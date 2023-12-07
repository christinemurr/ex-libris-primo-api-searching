

# prepare data
# these are steps specific to how the data is received at Bates

prepareBookList <- function(data, filename = "", authorVar = "in_author", titleVar = "in_title", isbnVar = "in_isbn", 
                            yearVar = "in_copyright",
                            noText = "*** NO TEXT ***",
                            skipAuthors = c("NONBOOK", "STAFF","TURNING TECHNOLOGIES",
                                            "POLL EVERYWHERE", "RITE IN THE RAIN", "BARCHARTS"),
                            skipTitles = c("COURSEPACK", "MODEL KIT", "LAB NOTEBOOK",
                                           "LAB MANUAL","WORKBOOK", "LABORATORY MANUAL", 
                                           "WKBK", "Safety Glasses, Aquilus OTG")){
  
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(stringr))
  
  # standardize column names
  
  names(data)[which(names(data) == authorVar)] <- "in_author"
  names(data)[which(names(data) == titleVar)] <- "in_title"
  names(data)[which(names(data) == isbnVar)] <- "in_isbn" 
  names(data)[which(names(data) == yearVar)] <- "in_copyright" 
  
  
  #get rid of "no text"
  data <- data[which(!data$in_title %in% noText),] 
  
  # remove dummy courses, non-confirmed
  
  data <- data[which(!grepl("TBA",data$course_name)),]
  data <- data[which(!is.na(data$confirmed_date)),]
  
  # filter out NONBOOK/STAFF in in_author
  
  skipAuthorsRegEx <- paste0(skipAuthors, collapse = "|")
  data <- data[!grepl(skipAuthorsRegEx, data$in_author, ignore.case = TRUE),]
  
  #filter out coursepacks
  
  skipTitlesRegEx <- paste0(skipTitles, collapse = "|")
  data <- data[!grepl(skipTitlesRegEx, data$in_title, ignore.case = TRUE),]  %>% 
  # add unique identifier so results can be merged back to original file
  # data$uniqid <- group_indices(data, in_isbn, in_title)
    mutate(clean_title  = cleanTitle(in_title)) %>% 
    # remove optionals if same title is also required (getting rid of ebooks)
    group_by(clean_title) %>% 
    filter(!(required_code == "O" & n() > 1)) %>% 
    ungroup() %>% 
    group_by(clean_title, in_isbn) %>% 
    dplyr::mutate(search_id = str_c(filename, str_pad(cur_group_id(), 3, "left", 0), sep = "_"))
  
  
  data <- data %>% 
    ungroup() %>% 
    group_by(search_id) %>%
    mutate(course = paste(course_title, section_number)) %>%
    dplyr::mutate(allCourses = str_c(str_sort(course), collapse = "; "), 
           totalExpectedStudents = sum(as.numeric(section_estimated_enrollment))) %>%
    select(!c(campus_description, college_name, college_id, estimated_sales, 
              in_serial_code, itm_vendor_price, in_cw_suffix, college_id, 
              section_actual_enrollment, course_continuation, new_rental, used_rental, in_net_pricing ))
  
  # add whether the book is required for any course
    data <- group_by(data, search_id) %>%
    dplyr::mutate(anyRequired = any(required_code != "O", grepl("library", 
                                                         paste(data$section_note,                                                          data$course_note, data$itm_book_note))))
    
    data <- data %>%
      filter(anyRequired) %>%
      distinct(search_id, .keep_all = TRUE) %>% 
      arrange(course_name, section_number)
    
    return(data)
}


# searching the Primo API by ISBN and then author/title

searchReservesPrimo <- function(data, id_field = "search_id", vid, scope, gateway = "api-na.hosted.exlibrisgroup.com", 
                                exl_key = getOption("reserves_search.exl_key", NULL), 
                                ws_key = getOption("reserves_search.ws_key", NULL)){
  
  # check that has in_isbn, in_title, in_author and the data object exists
  stopifnot("in_isbn" %in% names(data), "in_title" %in% names(data), 
            "in_author" %in% names(data), "in_copyright" %in% names(data),
            exists(deparse(substitute(data))))
 
  # rename id field if necessary 
  if (!id_field %in% names(data)){
    data$search_id <- 1:nrow(data)
  } else if (!"search_id" %in% names(data)) {
    names(data[id_field]) <- "search_id"
  }
  
  
  message("searching Primo by ISBN")
  
  #add any leading zeros that went missing in the isbn
  isbn <- data[["in_isbn"]] %>%
    str_pad(10, side= "left", pad = "0")
  

  isbnResults <- purrr::pmap_dfr(list(isbn, data$search_id, vid = vid, 
                                      scope = scope, gateway = gateway, exl_key = exl_key), 
                                 getBibsIsbnPrimo)

  
  #merge with data
  isbnResults_m <- merge(data, isbnResults) %>% 
    #only keep results
    filter(mms != "") 

  message("finished searching Primo by ISBN")
  
  #get rid of weirdnesses in title  
  
  title <- cleanTitle(data[["in_title"]])	
  
  data$clean_title <- title
  
  
  #pull out year of publication
  year <- data$in_copyright
  
  #pull out author's name
  last_name <- data$in_author %>%
    str_replace(" ET AL\\.?$", "") %>%
    str_replace_all("\\(.*\\)", "") %>%							  
    str_replace(" / ", " ") %>%
    #get rid of accents, etc.
    stri_trans_general("latin-ascii")
  
  data$last_name <- last_name
  
  message("searching Primo by author/title")
  
  authorTitleResults <- purrr::pmap_dfr(list(title, last_name, year, isbn, exl_key = exl_key, 
                                             vid = vid, gateway = gateway, ws_key = ws_key,
                                             scope = scope, search_id = data$search_id), 
                                        getBibsAuthorTitlePrimo)
  

  
  authorTitleResults_m <- merge(authorTitleResults, data, by = "search_id") %>% 
    #keep rows that either returned an MMS or returned none in ISBN search
    filter(mms != "" | !search_id %in% isbnResults$search_id[which(isbnResults$mms != "")]) %>% 
    mutate(otherEditions = str_replace(str_extract(mms, "Other editions:\\s+[0-9]{18}"), "Other editions:  ", "")) %>% 
    filter(is.na(otherEditions) | !otherEditions %in% isbnResults_m$mms) %>% 
    select(-otherEditions)

  # combining the two sets of results
  
  results <- bind_rows(authorTitleResults_m, isbnResults_m) %>% 
    distinct(search_id, delivery, mms, .keep_all = TRUE)
  
  
  return(results)
}


getBibsIsbnPrimo <- function(isbn, search_id = NA, vid, scope, check_availability = TRUE,
                                    gateway = "api-na.hosted.exlibrisgroup.com", 
                                    exl_key = getOption("reserves_search.exl_key", NULL)){
  
  suppressPackageStartupMessages(require(purrr))
  suppressPackageStartupMessages(require(tidyjson))
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(stringr))
  suppressPackageStartupMessages(require(stringi))
  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(xml2))
  

  query <- URLencode(paste0("isbn,contains,",isbn), reserved = TRUE)

  
  resp <- try(primo_api_helper(query = query, gateway = gateway, scope = scope, vid=vid,
                               exl_key = exl_key))
  
  
  
  if ("try-error" %in% class(resp)) {
    return(data.frame(search_id = search_id, mms = "", delivery = "", path = paste0("Problem with query:", query)))
  }
  
  nullResult <- data.frame(search_id = search_id, mms = "", delivery = "", path = resp$path) 
  
  
  if ( resp[["content"]][["info"]][["total"]] == 0) {
    return(nullResult)
  }
  
  #pull out records
  recs <- resp[["content"]][["docs"]]
  
  # look for Alma-E
  alma_e <- map(recs, pluck, "delivery", "deliveryCategory", .default = "none") %>% 
    map(unlist) %>% 
    map(function (x) any(x == "Alma-E")) %>% 
    unlist()
    
  
  online <- unlist(map(recs, pluck, "pnx", "display", "format", .default = "none")) %>% 
    str_detect("online")
  
  # even if the format does not contain online, make online if there is Alma-E in deliveryCategory
  online <- ifelse(alma_e, TRUE, online)
  
  mms <- unlist(map(resp[["content"]][["docs"]], pluck, "pnx", "display", "mms", .default = NA))
  
  callNumber <- map_chr(recs, pluck, "delivery", "bestlocation", "callNumber", .default = "")
  unavailable <- map_chr(recs, pluck, "delivery", "bestlocation", "availabilityStatus", .default = "") == "unavailable"
  locs <- map_chr(recs, pluck, "delivery", "bestlocation", "subLocation", .default = "")
  
  #use sprintf instead?
  callNums <- ifelse(callNumber != "", paste("print: ",locs, callNumber, sep = " "), "") 
  
  if (check_availability == TRUE){
    callNums <- ifelse(unavailable, paste0(callNums, " (not available)"), callNums)
  }
  
  delivery <- ifelse(online, "online", callNums)


  results <- data.frame(search_id, mms, delivery, path = resp$path)
  
  Sys.sleep(1)
  return(results)
  
}


getBibsAuthorTitlePrimo <- function(title, last_name, year, isbn, search_id = NA, check_availability = TRUE,
                                    vid, scope,
                                    gateway = "api-na.hosted.exlibrisgroup.com", 
                                    exl_key = getOption("reserves_search.exl_key", NULL), 
                                    ws_key = getOption("reserves_search.ws_key", NULL)){
  
  suppressPackageStartupMessages(require(rvest))
  suppressPackageStartupMessages(require(tidyjson))
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(stringr))
  suppressPackageStartupMessages(require(stringi))
  suppressPackageStartupMessages(require(xml2))
  suppressPackageStartupMessages(require(purrr))
  

  #pull only books and ebooks
  
  query <- URLencode(paste0("title,contains,",URLencode(title, reserved = TRUE),
                            ",AND;creator,contains,",URLencode(last_name, reserved = TRUE)))

  resp <- primo_api_helper(query = query, vid = vid, scope = scope, gateway = gateway, 
                           exl_key = exl_key)
  
  nullResult <- data.frame(search_id, mms = "", delivery = "", path = resp$path)
  
  

  if ( resp[["content"]][["info"]][["total"]] == 0) {

    # make so wskey is optional (skip this part if there is no wskey)
    if (is.null(ws_key)) {
      
      return(nullResult)
      
    } else {
    
    # get new info from WorldCat search    
    newReq <- getWorldCatbyISBN(isbn, wskey = ws_key)
    
    #if author and title are found in WorldCat; make new request
    if (is.null(newReq)){
      
      return(nullResult)
      
    } else {
      
      title <- newReq$title[[1]] %>%
        str_to_lower() %>%
        str_split(":|\\. ", simplify = TRUE) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        select("V1") %>%
        as.character() %>%
        str_replace("\\, or\\, .*", "") %>%
        str_replace("^the ","") %>%
        str_replace("^a[n]? ","") %>%
        str_replace_all('"', "") %>%
        str_replace_all(";|,", "")%>%
        str_trim() 
      
      #this is something about limiting length of title
      
      titlewords <- strsplit(title, "\\s")
      
      title_lengths <- lapply(lapply(titlewords, nchar), cumsum)
      
      wordsintitle <- lapply(title_lengths, function(x) which(x < 50))
      
      # replace with seq_along?
      for (i in 1:length(title)){
        title[i] <-  paste(titlewords[[i]][wordsintitle[[i]]], collapse = " ")
      }
      
      
      #put quotes around titles that contain not (or catalog will interpret as boolean) 
      # necessary in Primo API?
      
      title <- ifelse(grepl("\\bnot\\b", title), paste0('"', title, '"'), title)
      
      #able to simplify with str_locate(",")?
      # substr(author_name, 1, str_locate(author_name, ",")[1]-1)
      
      # last_name <- stri_trans_general(newReq$author[[1]], "latin-ascii") %>%
      #   str_split(",", simplify = TRUE) %>%
      #   as.data.frame(stringsAsFactors = FALSE) %>%
      #   select("V1") %>%
      #   as.character() %>%
      #   str_trim() 
      
      #pull out everything before the comma, if there is one
      last_name <- stri_trans_general(newReq$author[[1]], "latin-ascii") %>%
        ifelse(str_detect(., ","), str_sub(., 1, str_locate(., ",")[1]-1), .) %>% 
        str_trim()
       
        
      
      
      #pulls only books and ebooks
      if (length(last_name) > 0) {
        
        query <- URLencode(paste0("title,contains,",title,",AND;creator,contains,",last_name))
        
      } else {
        
        #replace with Primo search
        query <- URLencode(paste0("title,contains,",title))
        
      }
      
      resp <- primo_api_helper(query = query, vid = vid, scope = scope, gateway = gateway, 
                               exl_key = exl_key)
      
      # fix this
      nullResult$path <- resp$path
      

      
      if (resp[["content"]][["info"]][["totalResultsLocal"]] == 0) {
        return(nullResult)
      } 
        
    }
  }
}
  

  
  
  #create a simplified, shortened title    
  title_short <- str_to_lower(title) %>%
    # extract text before (semi-)colon
    ifelse(str_detect(., ":|;"), str_sub(., 1, str_locate(., ":|;")[1]-1), .) %>% 
    str_trim() %>%
    str_replace_all('"$|^"', "") %>%
    str_replace_all('[tT]he ', "") %>%
    str_replace('^[Aa]n? ', "") %>%	
    str_replace_all("'|,|\\.", "") %>%
    str_replace_all(" & ", " and ") %>%
    str_squish()
  


    
#    recs <- make a list of recs
	
 #see which records in results match the simplifed, shortened title   
    realTitleMatch <- unlist(map(resp[["content"]][["docs"]], pluck, "pnx", "display", "title", .default = NA)) %>%
      str_to_lower() %>%
      stri_trans_general("latin-ascii") %>%
      str_replace(";|,|\\?","") %>%
      str_replace( "/", " ") %>%
      str_replace_all('[tT]he ', "") %>%
	  str_replace('^[Aa]n? ', "") %>%							 
      str_replace_all("'|,|\\.", "") %>%
      str_replace_all("&", "and") %>%
      str_squish() %>% 
      str_detect(paste0("^", title_short))
	  
    
    #what if there is no real title match?
    if(!any(realTitleMatch)) {
      return(nullResult)
    }
    
    recs <- resp[["content"]][["docs"]][realTitleMatch]
    
    #weed out "readers"
    
    if (!grepl("reader|reading", title)){
      notReaders <- unlist(map(recs, pluck, "pnx", "display", "title", .default = NA)) %>%
        str_to_lower() %>%
        str_detect("reader\\b|reading\\b")
      
      recs <- recs[!notReaders]
    }
    
    # is it possible to have a null title?
 

    years <-  unlist(map(recs, pluck, "pnx", "sort", "creationdate", .default = ""))

    
    # whether the item matches the year of listed editions
    matchedYear <- years %>% 
      str_detect(paste(as.numeric(year) + -1:1, collapse="|"))

   # if there are no copies with year +/- 1 
  if(!any(matchedYear)){
    # detect ebooks
    
    alma_e <- map(recs, pluck, "delivery", "deliveryCategory", .default = "none") %>% 
      map(unlist) %>% 
      map(function (x) any(x == "Alma-E")) %>% 
      unlist()
    
    if (all(alma_e)){
      # if all the other editions are e, indicate e and return
      otherEditionsMMS <- paste("Other editions (e):", paste(unlist(map(recs, pluck, "pnx", "display", "mms", .default = NA))[!matchedYear], collapse = "; "))
      return(data.frame(search_id, mms = otherEditionsMMS, delivery = "online", path = resp$path))    
    }

    otherEditionsMMS <- paste("Other editions:", paste(unlist(map(recs, pluck, "pnx", "display", "mms", .default = NA))[!matchedYear], collapse = "; "))
    
    return(data.frame(search_id, mms = otherEditionsMMS, delivery = "", path = resp$path))
  }    
    
 #what if print matches year but the only available ebook does not match year?
# e.g. neighbors/gross
    
  
  online <- unlist(map(recs, pluck, "pnx", "display", "format", .default = "none")) %>% 
    str_detect("online")
  
  
  matched_recs <- recs[matchedYear]
  
  matched_online <- unlist(map(matched_recs, pluck, "pnx", "display", "format", .default = "none")) %>% 
    str_detect("online")
  
  alma_e <- map(matched_recs, pluck, "delivery", "deliveryCategory", .default = "none") %>% 
    map(unlist) %>% 
    map(function (x) any(x == "Alma-E")) %>% 
    unlist()

  
  # even if the format does not contain online, make online if there is Alma-E in deliveryCategory
  matched_online <- ifelse(alma_e, TRUE, matched_online)

  
  mms <- unlist(map(matched_recs, pluck, "pnx", "display", "mms", .default = NA))

  callNumber <- map_chr(matched_recs, pluck, "delivery", "bestlocation", "callNumber", .default = "")
  unavailable <- map_chr(matched_recs, pluck, "delivery", "bestlocation", "availabilityStatus", .default = "") == "unavailable"
  
  locs <- map_chr(matched_recs, pluck, "delivery", "bestlocation", "subLocation", .default = "")


callNums <- ifelse(callNumber != "", paste("print: ",locs, callNumber, sep = " "), "")

if (check_availability == TRUE){
  callNums <- ifelse(unavailable, paste0(callNums, " (not available)"), callNums)
}
  

# make online and call number one column?

delivery <- ifelse(matched_online, "online", callNums)


# if there are online versions but none of those versions match the year
if(any(online) & !any(matchedYear & online)){
  
  unmatched_online_recs <- recs[online]
  unmatched_mms <- unlist(map(unmatched_online_recs, pluck, "pnx", "display", "mms", .default = NA))
  otherEditionsMMS <- paste("Other editions (e): ", paste(unmatched_mms, collapse = "; "))
  unmatched_online <- data.frame(search_id, mms = otherEditionsMMS, delivery = "online", path = resp$path)
  results <- data.frame(search_id, mms, delivery, path = resp$path) %>% 
    bind_rows(unmatched_online)
} else {
  results <- data.frame(search_id, mms, delivery, path = resp$path)
  }



  Sys.sleep(1)
  return(results)

}





findNewReserves <- function(most_recent_file, penultimate_file = "",  dir = "", 
                            skipAuthors = c("NONBOOK",  "STAFF","TURNING TECHNOLOGIES",
                                            "POLL EVERYWHERE","RITE IN THE RAIN"),
                            skipTitles = c("COURSEPACK", "MODEL KIT", "LAB NOTEBOOK","LAB MANUAL",
                                           "WORKBOOK", "LABORATORY MANUAL", "WKBK", "CHEM 101")){
  
  suppressMessages(require(purrr))
  suppressMessages(require(dplyr))
  suppressMessages(require(readxl))
  
  skipTitlesRegEx <- paste0(skipTitles, collapse = "|")
  skipAuthorsRegEx <- paste0(skipAuthors, collapse = "|")
  
  update_data <- read_excel(paste0("input\\",most_recent_file), col_types = c("text")) %>%
    filter(!is.na(confirmed_date))
  
  update_data <- update_data[!grepl(skipTitlesRegEx, update_data$in_title, ignore.case = TRUE),]
  update_data <- update_data[!grepl(skipAuthorsRegEx, update_data$in_author, ignore.case = TRUE),]
  
  #catch any changed to cu_lname
  names(update_data)[grep("cu_lname",names(update_data))] <- "cu_name"
  names(update_data)[grep("course_display",names(update_data))] <- "course_title"
#  update_data <- update_data[ , -which(names(update_data) %in% c("course_id"))]

  if(!grepl("xlsx",penultimate_file)){
    
    message("Only one file processed")
    return(list(newBooks = update_data))
    
  }  else {
    
  
  
  original <- read_excel(paste0("input\\",penultimate_file), col_types = c("text")) %>%
    filter(!is.na(confirmed_date))
  
  original <- original[!grepl(skipTitlesRegEx, original$in_title, ignore.case = TRUE),]
  original <- original[!grepl(skipAuthorsRegEx, original$in_author, ignore.case = TRUE),]
  
  #catch any changed to cu_lname
  names(original)[grep("cu_lname",names(original))] <- "cu_name"
  names(original)[grep("course_display",names(original))] <- "course_title"
  
  last_date <- max(original$confirmed_date)
  
  # all books in new sheet that have a later confirmed date than the last sheet
  newBooks <- update_data[which(update_data$confirmed_date > last_date),]
  
  # something to do with books confirmed on the same date
  # sameDate <- bind_rows(update_data[which(update_data$confirmed_date == last_date),], 
  #                       original[which(original$confirmed_date > last_date),]) %>%
  #   distinct(in_title, course_title, in_isbn, .keep_all = TRUE)
  
  # should be this?
  overlappingDate <- anti_join(update_data[which(update_data$confirmed_date == last_date),],
                               original[which(original$confirmed_date == last_date),],
                               by = c("in_isbn", "course_title"))   
  
  newBooks <- bind_rows(newBooks, overlappingDate)
  
  
  # newBooks <- anti_join(update_data, original, by = c("campus_description", "college_name", "term_desc", 
  #                                                "course_name",  "section_number", "cu_name", "college_id",
  #                                                "in_title", "in_edition",  "required_code", "in_copyright", 
  #                                                "in_author", "itm_vendor_price", "in_isbn",  "ve_name",  
  #                                                "in_publisher", "itm_id", "section_id", "course_description",  
  #                                                "section_course_reference_nbr", "college_depart_code",  
  #                                                "course_title"))

    deleted <- anti_join(original, update_data, by = c("campus_description", "term_desc", 
                                                  "course_name",  "section_number", "cu_name", "college_id",
                                                  "in_title", "in_edition",  "required_code", "in_copyright", 
                                                  "in_author", "in_isbn",   
                                                  "in_publisher", "itm_id", "section_id", "course_description",  
                                                  "college_depart_code",  
                                                  "course_title"))
    
  already_there <- newBooks[newBooks$in_isbn %in% original$in_isbn,]
  
  if(nrow(deleted) > 0) {
    message("The following books may have been removed from the reserves list")
    print(data.frame(course = paste(deleted$course_title,deleted$section_number), deleted[c("cu_name","in_title", "in_edition",  "in_copyright", "in_author", "in_isbn",  "course_description")]))
  }
  
  if(nrow(already_there) > 0) {
    message("The following books may have already been put on reserve for another course")
    print(data.frame(course = paste(already_there$course_title,already_there$section_number), already_there[c("cu_name","in_title", "in_edition",  "in_copyright", "in_author", "in_isbn",  "course_description")]))
  }
  
  return(list(newBooks = newBooks, deleted = deleted, already_there = already_there))
  } 
}

#get correct title/author by search WorldCat by isbn
getWorldCatbyISBN <- function(isbn, wskey){
  require(xml2)
  
  # check if a bad isbn snuck through
  if (grepl("\\w", isbn)){
    return(NULL)
  }
  
  isbn <- paste0("srw.bn+%3D%22", isbn, "%22")
  reqURLIsbn <- paste0("https://www.worldcat.org/webservices/catalog/search/sru?query=", isbn,"&maximiumRecords=100&frbrGrouping=off&recordSchema=info%3Asrw%2Fschema%2F1%2Fdc&wskey=",wskey)
 
  resultsList <- try(read_xml(reqURLIsbn))
  
  #wait and see if worldcat works again?
  if ("try-error" %in% class(resultsList)) {
    Sys.sleep(5)
    resultsList <- try(read_xml(reqURLIsbn))
  }
  
  if ("try-error" %in% class(resultsList)) {
    return(NULL)
  }
  
  
  
  resultsList <- as_list(resultsList)
   
  if(resultsList$searchRetrieveResponse$numberOfRecords > 0){
    record <- resultsList$searchRetrieveResponse$records[[1]]$recordData$oclcdcs
    if (!is.null(record$creator)|!is.null(record$contributor)){
      return(c(author = ifelse(length(record$creator) > 0, record$creator, record$contributor), 
               title = record$title))     
    } else {
      return(c(author = "", title = record$title))
    }

  }
}

getEbookIsbnsWorldCat <- function(title, author, isbn, uniqid = 0, wskey = getOption("reserves_search.ws_key", NULL)) {
  
  require(stringr, quietly=TRUE)
  require(dplyr, quietly = TRUE)
  require(plyr, quietly = TRUE)
  require(xml2, quietly = TRUE)
  require(stringi, quietly = TRUE)
  require(utils)
  
  #use short title
  title <- title %>%
    str_replace("^#", "") %>%
    str_split(":|;", simplify = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    select("V1") %>%
    as.character() %>%
    str_trim() %>%
    str_replace_all('"', "") %>%
    str_replace_all(",|\\.", "") %>%
    str_replace_all(" & ", " and ") %>%
    str_squish()
  
  
  
  
  search_title <- paste0("srw.ti+%3D+%22", URLencode(title, reserved = TRUE),"%22")
  
  name <-paste0("srw.au+%3D+%22",  URLencode(as.character(author), reserved = TRUE), "%22")
  
  
  reqURLTitleAuthor <- paste0("https://www.worldcat.org/webservices/catalog/search/sru?query=srw.dt%3D%22url%22+and+srw.la+all+%22eng%22+and+",
                              name,"+and+",search_title,"&maximiumRecords=100&frbrGrouping=off&recordSchema=info%3Asrw%2Fschema%2F1%2Fdc","&wskey=",wskey)
  
  
  resultsList <- try(read_xml(reqURLTitleAuthor))
  
  
  if ("try-error" %in% class(resultsList)){
    message(paste("WorldCat error: ", isbn))
    return(data.frame(isbnsForGobi = "WorldCat error", title = title,
                      author = author,
                      in_isbn = isbn, uniqid))
  }
  
  resultsList <- as_list(resultsList)
  
  numResults <- resultsList$searchRetrieveResponse$numberOfRecords[[1]]
  
  #if there are no results
  if (numResults=="0") {
    
    # get title and author from ISBN search?
    newReq <- getWorldCatbyISBN(isbn, wskey)
    
    if (is.null(newReq)) {
      return(data.frame(isbnsForGobi = "Bad isbn", in_isbn = isbn, uniqid, 
                        title = title, author = author))
    }
    
    title <- newReq$title[[1]] %>%
      str_to_lower() %>%
      str_split(":|=", simplify = TRUE) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      select("V1") %>%
      as.character() %>%
      str_replace("^the ","") %>%
      str_replace("^a[n]? ","") %>%
      str_replace_all('"', "") %>%
      str_replace_all(";|,", "")%>%
      str_replace("book [1-9]", "") %>%
      str_squish() 
    
    author <- stri_trans_general(newReq$author[[1]], "latin-ascii") %>%
      str_split(",", simplify = TRUE) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      select("V1") %>%
      as.character() %>%
      str_trim() 
    
    search_title <- paste0("srw.ti+%3D+%22", URLencode(title, reserved = TRUE),"%22")
    name <-paste0("srw.au+%3D+%22",  URLencode(author, reserved = TRUE), "%22")
    
    reqURLTitleAuthor <- paste0("https://www.worldcat.org/webservices/catalog/search/sru?query=srw.dt%3D%22url%22+and+srw.la+all+%22eng%22+and+",
                                name,"+and+", search_title, "&maximiumRecords=100&frbrGrouping=off&recordSchema=info%3Asrw%2Fschema%2F1%2Fdc","&wskey=",wskey)
  
    
    resultsList <- try(read_xml(reqURLTitleAuthor), silent = TRUE)
    
    if ("try-error" %in% class(resultsList)){
      message(paste("WorldCat error: ", isbn))
      return(data.frame(isbnsForGobi = "WorldCat error", title = title,
                        author = author,in_isbn = isbn, uniqid))
    }
    
    resultsList <- as_list(resultsList)
    
    numResults <- resultsList$searchRetrieveResponse$numberOfRecords[[1]]
    
    if (numResults == 0) {
      return(data.frame(isbnsForGobi = "ebook not found in WorldCat", in_isbn = isbn, uniqid))
    }
    
  }
  
  resultSets <- ceiling(as.numeric(numResults)/10)
  isbnsForGobi <- character(0)
  
  
  
  for (i in 1:length(resultsList$searchRetrieveResponse$records)) {
    isbns <- try(getIsbnsFromWorldCatRec(resultsList, recNum = i, title = title), silent = TRUE)
    if ("try-error" %in% class(isbns)){
      message(title)
    }
    newIsbns <- isbns[which(isbns != isbn)]
    isbnsForGobi <- c(isbnsForGobi, newIsbns)
  }
  
  if (resultSets > 1) {
    for (i in 1:(resultSets-1)){
      
      nextReqURL <- paste0("https://www.worldcat.org/webservices/catalog/search/sru?query=srw.dt%3D%22url%22+and+srw.la+all+%22eng%22+and+",
                           name,"+and+",search_title,"&maximiumRecords=100&frbrGrouping=off&&startRecord=",i * 10, "&recordSchema=info%3Asrw%2Fschema%2F1%2Fdc","&wskey=",wskey)
      
      nextResultsList <- try(read_xml(nextReqURL), silent= TRUE)
      
      #just wait a mo if WC does not respond?
      if ("try-error" %in% class(nextResultsList)){
        Sys.sleep(5)
        nextResultsList <- try(read_xml(nextReqURL), silent= TRUE)
      }
      
      
      nextResultsList <- as_list(nextResultsList)
      
      for (i in 1:length(nextResultsList$searchRetrieveResponse$records)) {
        isbns <- getIsbnsFromWorldCatRec(nextResultsList, recNum = i, title = title)
        newIsbns <- isbns[which(isbns != isbn)]
        isbnsForGobi <- c(isbnsForGobi, newIsbns)
      }
      
    }
  }
  
  
  if (length(isbnsForGobi) == 0) {
    isbnsForGobi <- c("ebook not found in WorldCat")
  } 
  
  forSearch <- data.frame(isbnsForGobi = unique(isbnsForGobi), 
                          title = str_to_title(title), 
                          author = author, in_isbn = isbn, uniqid, stringsAsFactors = FALSE)
  message(uniqid)
  Sys.sleep(.5)
  return(forSearch)
  
}

getAllIsbnsWorldCat <- function(title, author, isbn, uniqid = 0, wskey) {
  
  require(stringr, quietly=TRUE)
  require(plyr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(xml2, quietly = TRUE)
  require(stringi, quietly = TRUE)
  require(utils)
  
  #use short title
  title <- title %>%
    str_replace("^#", "") %>%
    str_split(":|;", simplify = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    select("V1") %>%
    as.character() %>%
    str_trim() %>%
    str_replace_all('"', "") %>%
    str_replace_all(",|\\.", "") %>%
    str_replace_all(" & ", " and ") %>%
    str_squish()
  
  search_title <- paste0("srw.ti+%3D+%22", URLencode(title, reserved = TRUE),"%22")
  
  name <- str_to_lower(author) %>%
    str_split(" ", simplify = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    select("V1") %>%
    as.character() %>%
    str_trim()
  
  name <-paste0("srw.au+%3D+%22",  URLencode(as.character(name), reserved = TRUE), "%22")
  
  
  reqURLTitleAuthor <- paste0("https://www.worldcat.org/webservices/catalog/search/sru?query=srw.la+all+%22eng%22+and+",
                              name,"+and+",search_title,"&maximiumRecords=100&frbrGrouping=off&recordSchema=info%3Asrw%2Fschema%2F1%2Fdc",wskey)

  #re-write to catch error
  resultsList <- as_list(read_xml(reqURLTitleAuthor))
  
  
  
  numResults <- resultsList$searchRetrieveResponse$numberOfRecords[[1]]
  
  #if there are no results
  if (numResults=="0") {
    
    # get title and author from ISBN search?
    newReq <- getWorldCatbyISBN(isbn, wskey)
    
    if (is.null(newReq)) {
      return(data.frame(isbnsForGobi = "Bad isbn", in_isbn = isbn, uniqid))
    }
    
    title <- newReq$title[[1]] %>%
      str_to_lower() %>%
      str_split(":|=", simplify = TRUE) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      select("V1") %>%
      as.character() %>%
      str_replace("^the ","") %>%
      str_replace("^a[n]? ","") %>%
      str_replace_all('"', "") %>%
      str_replace_all(";|,", "")%>%
      str_replace("book [1-9]", "") %>%
      str_squish() 
    
    author <- stri_trans_general(newReq$author[[1]], "latin-ascii") %>%
      str_split(",", simplify = TRUE) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      select("V1") %>%
      as.character() %>%
      str_trim() 
    
    search_title <- paste0("srw.ti+%3D+%22", URLencode(title, reserved = TRUE),"%22")
    name <-paste0("srw.au+%3D+%22",  URLencode(author, reserved = TRUE), "%22")
    
    reqURLTitleAuthor <- paste0("https://www.worldcat.org/webservices/catalog/search/sru?query=srw.la+all+%22eng%22+and+",
                                name,"+and+", search_title, "&maximiumRecords=100&frbrGrouping=off&recordSchema=info%3Asrw%2Fschema%2F1%2Fdc",wskey)
    
    resultsList <- as_list(read_xml(reqURLTitleAuthor))
    
    numResults <- resultsList$searchRetrieveResponse$numberOfRecords[[1]]
    
    if (numResults == 0) {
      return(data.frame(isbnsForGobi = "ebook not found in WorldCat", in_isbn = isbn, uniqid))
    }
    
  }
  
  resultSets <- ceiling(as.numeric(numResults)/10)
  # limit to ten pages of results
  resultSets <- ifelse(resultSets > 10, 10, resultSets)
  isbnsForGobi <- character(0)
  
  
  
  for (i in 1:length(resultsList$searchRetrieveResponse$records)) {
    isbns <- try(getIsbnsFromWorldCatRec(resultsList, recNum = i, title = title), silent = TRUE)
    if ("try-error" %in% class(isbns)){
      message(title)
    }
    newIsbns <- isbns[which(isbns != isbn)]
    isbnsForGobi <- c(isbnsForGobi, newIsbns)
  }
  
  if (resultSets > 1) {
    for (i in 1:(resultSets-1)){
      
      nextReqURL <- paste0("https://www.worldcat.org/webservices/catalog/search/sru?query=srw.la+all+%22eng%22+and+",
                           name,"+and+",search_title,"&maximiumRecords=100&frbrGrouping=off&&startRecord=",i * 10, "&recordSchema=info%3Asrw%2Fschema%2F1%2Fdc",wskey)
      
      nextResultsList <- as_list(read_xml(nextReqURL))
      
      for (i in 1:length(nextResultsList$searchRetrieveResponse$records)) {
        isbns <- getIsbnsFromWorldCatRec(nextResultsList, recNum = i, title = title)
        newIsbns <- isbns[which(isbns != isbn)]
        isbnsForGobi <- c(isbnsForGobi, newIsbns)
      }
      
    }
  }
  
  
  if (length(isbnsForGobi) == 0) {
    isbnsForGobi <- c("ebook not found in WorldCat")
  } 
  
  forSearch <- data.frame(isbnsForGobi = unique(isbnsForGobi), in_isbn = isbn, uniqid, stringsAsFactors = FALSE)
  message(uniqid)
  Sys.sleep(.5)
  return(forSearch)
  
}

getIsbnsFromWorldCatRec <- function(resultsList, recNum, title) {
  record <- resultsList$searchRetrieveResponse$records[[recNum]]$recordData$oclcdcs
  # check that format = "online resource"?
  #get rid of apostrophes in both
  title <- str_replace_all(title, "'", "")
  record_title <- str_to_lower(record$title) %>%
    str_replace("^#", "") %>%
    str_replace_all(";|,|\\.", "") %>%
    str_replace("^the ","") %>%
    str_replace("^a[n]? ","") %>%
    str_replace_all('"', "") %>%
    str_replace( "/", " ") %>%
    str_replace_all( "'", "") %>%
    str_squish()
  format <- ifelse(length(record$type) > 0, record$type, "")
  publisher <- ifelse(length(record$publisher) > 0, record$publisher, "")
  publisher <- !publisher %in% c("CNPeReading", "Cliffs Notes", "Bokish Ltd", "Crowood")
  #    print(record_title)
  #make sure in_title is actually in title
  if(grepl(paste0("^",title), record_title, ignore.case = TRUE) & grepl("Text", format) & publisher & !grepl("summary & analysis", record_title)){
    #   print(i)
    isbns <-  unname(unlist(record[which(names(record)=="identifier")]))
    isbns <- isbns[which(nchar(isbns) == 13)]
    return(isbns)
  }
}

cleanTitle <- function(in_title, max_char = 50) {
  
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(stringr))
  suppressPackageStartupMessages(require(stringi))
  

  
  title <- str_to_lower(in_title) %>%
    str_replace_all("\\(.*\\)", "") %>%
    str_squish() %>% 
    str_replace(": poems$", "") %>% 
    #if on two lines, take second line
    str_replace( "^.*\\r\\n", "") %>%
    str_replace(" new edition ?$", "") %>%
    str_replace("updtd & expd", "") %>% 
    str_replace("original ed$", "") %>%
    str_replace(" 3rd rev$", "") %>%
    str_replace(": centennial edition[ \\d-]*", "") %>%
    str_replace(" new ed$", "") %>%
    str_replace("very short intro$", "very short introduction") %>%
    str_replace(" rev & updtd", "") %>%
    str_replace(" book \\d\\d?$", "") %>%
    str_replace("; or .*$", "") %>%
    str_replace(": integrated course in elem japanese 1", "") %>%
    str_replace("w/new intro.*$", "") %>%
    str_replace(" elem ", " elementary ") %>%
    str_replace(" 2nd$", "") %>%
    str_replace("vol one", "") %>%
    str_replace("vol 1.*", "") %>%
    str_replace("vol\\.? \\d", "") %>%
    str_replace(" enhanced$", "") %>%								 									 
    str_replace("p\\.s\\. ed$", "") %>%
    str_replace(": new edition$", "") %>%
    str_replace_all("\\!|\\?", "") %>%
    str_replace_all("\\([^\\)]*$", "") %>%
    str_replace("[0-9]+th anniv ed.*","") %>%
    str_replace("with hdbk","") %>%
    str_replace("& sketches","") %>%
    str_replace("updtd","") %>%
    str_replace("w/new afterword","") %>%
    str_replace("w/ handbook","") %>%
    str_replace("w/ readings","") %>%
    str_replace("w/rdgs & hdbk","") %>%
    str_replace("w/afterword","") %>%
    str_replace("w/new foreword","") %>%
    str_replace("w/new preface","") %>%
    str_replace("w/foreword","") %>%
    str_replace("w/documents","") %>%
    str_replace("and other stories$", "") %>%
    str_replace("[2-3][nr]d update", "") %>%
    str_replace("new expd 21st century ed$", "") %>%
    str_replace("special commemorative ed$", "") %>%
    str_replace("new ed$","") %>%
    str_replace(" \\.\\.\\.","") %>%
    str_replace( "/", " ") %>%
    str_trim() %>%
    stri_trans_general("latin-ascii") %>%
    str_replace("\\bprin\\b", "principles ") %>%
    str_replace("\\bintro ", " introduction ") %>%
    str_replace(", the$|, an?$|[2-9]e", "") %>%
    str_replace(";|,", "") %>%
    str_replace(" 20[0-9]{2}$", "") %>%
    str_replace("20th", "twentieth") %>%
    str_replace("19th", "nineteenth") %>%
    str_replace("fortieth anniv ed", "") %>% 
    str_replace("25th anniv", "") %>%   
    str_replace_all("&", "and") %>%
    str_replace(" etc\\.?$", "") %>%
    str_replace(" etch$", "") %>%
    str_replace(" appl$", " applications") %>%
    str_replace(" amer ", " american ") %>%
    str_replace("norton anthology of (.*) lit$", "norton anthology of \\1 literature")%>%
    str_replace("norton anth engl lit", "norton anthology of english literature") %>%
    str_replace(" vol [a-z0-9]", " ") %>%
    str_replace(" gde", " guide") %>%
    str_replace("ebook:\\s*https?://cbbcat.net/record=b[0-9]*~s19\\s*", "") %>%
    str_replace(" expd", "") %>%
    str_replace(" 1$", "") %>%
    str_squish()
  
  #try to fix extra long titles
  titlewords <- strsplit(title, "\\s")
  
  title_lengths <- lapply(lapply(titlewords, nchar), cumsum)
  
  # find how many words until the title is 50 characters long (or max_char)
  wordsintitle <- lapply(title_lengths, function(x) which(x < max_char))
  
# for each title, take the first words that add up to less than max_char
# way to use map instead?
# map2_chr(titlewords, wordsintitle, function(x,y) paste(x[y], collapse = " "))  
  
   for (i in seq_along(title)){
     title[i] <-  paste(titlewords[[i]][wordsintitle[[i]]], collapse = " ")
   }
  
  
  #put quotes around titles that contain not (or catalog will interpret as boolean)  
  title <- ifelse(grepl("\\bnot\\b", title), paste0('"', title, '"'), title)
  return(title)
}

cleanName <- function(in_author){
  last_name <- in_author %>%
    str_replace(" ET AL\\.?$", "") %>%
    str_replace_all("\\(.*\\)", "") %>%							  
    str_replace(" / ", " ") %>%
    #get rid of accents, etc.
    stri_trans_general("latin-ascii")
  return(last_name)
}



getEbooksFromGobi <- function(gobifile, ownedByTitle, licenses = c("Unlimited", "DRM free"), 
                              max_price = 300, price_diff = 50, 
                              outputVariables = c("cu_name", "title", "in_author", "in_copyright",
                                                  "in_isbn", "required_code",
                                                  "allCourses",  "section_note",
                                                  "course_note",  "ve_name", "course_description",
                                                  "section_course_reference_nbr",
                                                   "in_listprice_new", "mms",
                                                  "delivery" )) {
  suppressMessages(require(plyr))
  suppressMessages(require(stringr))
  suppressMessages(require(purrr))
  suppressMessages(require(readr))
  suppressMessages(require(dplyr))
  suppressMessages(require(reshape2))
  
  gobi <- suppressWarnings(read_delim(paste0("input\\" ,gobiFile), 
                                      "\t", escape_double = FALSE, col_types = cols(ISBN = col_character()), 
                                      trim_ws = TRUE) %>%
                             filter(Publisher != "INSTAREAD" & Publisher != "JOOSR" & Publisher != "MOBILEREFERENCE COM" & Publisher != "BRIGHTSUMMARIES COM"))
  
  ebookOptions <- gobi[which(!is.na(gobi$ISBN)), c("ISBN",  "Pub_Year", names(gobi)[grep("ItemVendor_[1-6]\\.(Supplier|List_Price|Library_Availability|Purchase_Option)",names(gobi))])]
  
  ebookOptions_m <- melt(ebookOptions, id.vars = c("ISBN", "Pub_Year"))
  ebookOptions_m <- ebookOptions_m[which(!is.na(ebookOptions_m$value)),]
  ebookOptions_m$item <- substr(ebookOptions_m$variable, 12,12)
  ebookOptions_m <- ebookOptions_m[order(ebookOptions_m$ISBN),]
  ebookOptions_m$variable <- str_replace(ebookOptions_m$variable, "ItemVendor_[1-6]\\.", "")
  ebookOptions_m <- ebookOptions_m[which(ebookOptions_m$variable %in% c("List_Price", "Purchase_Option", "Library_Availability", "Supplier")),]
  ebookOptions_m$value <- ifelse(grepl("DRM-free", ebookOptions_m$value), "DRM free", ebookOptions_m$value)
  ebookOptions_m$value <- ifelse(grepl("Concurrent|Linear", ebookOptions_m$value), "Concurrent access/Non-linear", ebookOptions_m$value)
  ebookOptions_m$value <- ifelse(ebookOptions_m$value %in% c("Unlimited User", "Unlimited User Access", "Unlimited Access"), "Unlimited", ebookOptions_m$value)
  
  # add number of courses, total enrollments, whether the book is required
  
  
  ebooksForSale <- distinct(ebookOptions_m, ISBN, Pub_Year, variable, value, item) %>%
    dcast(ISBN + Pub_Year + item ~ variable) %>%
    filter(Library_Availability == "Contract on file") %>%
    # filter out concurrent or other undesired licenses
    filter(Purchase_Option %in% licenses) 
  
  licenses_available <- licenses[licenses %in% ebooksForSale$Purchase_Option]
  
   ebooksForSale <- ebooksForSale %>%
     filter(List_Price != "Not Known") %>% 
     dplyr::mutate(List_Price = as.numeric(str_replace(List_Price, " USD", ""))) %>%
     #multiply the JSTOR price by 3
     dplyr::mutate(List_Price = ifelse(Supplier == "JSTOR", List_Price * 3, List_Price)) %>%
    # filter out above max price
    filter(List_Price < max_price) %>%
    # join in titles from isbnsToSearchGobi?
    left_join(isbnsToSearchGobi[c("isbnsForGobi", "title", "author", "uniqid")], by = c("ISBN" = "isbnsForGobi")) %>%
    arrange(ISBN, List_Price) %>%
    # group by author/title instead of ISBN?
    group_by(uniqid, Purchase_Option) %>%
     dplyr::mutate(value = min(List_Price)) %>%
    select(ISBN, Pub_Year, uniqid, author, title, Purchase_Option, value) %>%
    # concatenate isbns of different editions
    group_by(author, title, Purchase_Option) %>%
    dplyr::mutate(volIsbns = str_c(str_sort(ISBN), collapse = "; ")) %>%
    distinct(author, title, Purchase_Option, .keep_all = TRUE) %>%
    dcast(volIsbns + Pub_Year + title + author + uniqid ~ Purchase_Option) %>%
     #bring in info from Primo search and book list, joins by title
    left_join(ownedByTitle) %>%
     
    # mark ebooks whose pub dates don't match
    dplyr::mutate(in_copyright = as.numeric(in_copyright)) %>%
    dplyr::mutate(badPubDate = abs(Pub_Year - as.numeric(in_copyright)) > 1) %>%
    group_by(uniqid) %>%
    #warning following
    # mutate(across(licenses, ~min(.x, na.rm = TRUE))) %>%
    #mutate(across(licenses, function(x)suppressWarnings(min(x, na.rm = TRUE)))) %>%
    dplyr::mutate(across(licenses_available, function(x)suppressWarnings(min(x, na.rm = TRUE)))) %>%
    dplyr::mutate(allIsbns = str_c(str_sort(volIsbns), collapse = "; ")) %>%
    #  mutate(allIsbns = str_c("'",str_sort(ISBN), collapse = "; ")) %>%
    group_by(allIsbns) %>%
    select(-volIsbns) %>%
    distinct(author, title, delivery, allIsbns, .keep_all = TRUE) %>%
#     distinct(author, title, allIsbns, .keep_all = TRUE) %>%
    dplyr::mutate(across(licenses_available, ~min(.x, na.rm = TRUE))) %>%
    dplyr::mutate(min = min(across(licenses_available))) %>%
    filter(min < (as.numeric(in_listprice_new) + price_diff))
  
  # make all infinite values NA
  is.na(ebooksForSale) <- sapply(ebooksForSale, is.infinite)
  
  #is the ebook ISBN part working?
  
  ebookIsbns <- ebooksForSale$allIsbns %>%
    str_split(";")
  
  ebookIsbns <- sapply(ebookIsbns, function(x){str_c(str_sort(unique(str_trim(x))), collapse = ";")})
  
  ebooksForSale$ebookIsbns <- ebookIsbns
  
  #only add columns for desired licenses
  
  for (i in 1:length(licenses)){
    if(!licenses[i] %in% names(ebooksForSale)){
      ebooksForSale[licenses[i]] <- NA
    }
  }
  
  ebooksForSale$flag <- ifelse(ebooksForSale$badPubDate, "Double-check this title!", "")

  
  
  
  ebooksForSale <- list(ebooksForSale[c("ebookIsbns", "Pub_Year", "flag", outputVariables,
                                   "totalExpectedStudents", licenses)], ebooksForSale$uniqid)
  return(ebooksForSale)
  
}

# get portfolio, collection and service ids from mms

getCollectionIds <- function(mms_id, exl_key = getOption("reserves_search.exl_key", NULL)){
  if(is.na(mms_id) | substr(mms_id,1,2) != "99"){
    return(data.frame(mms_id, portfolio_id = NA, collection_id= NA, service_id = NA))
  } else {
    resp <- try(alma_portfolios_helper(mms_id, exl_key = exl_key))
    
    if ("try-error" %in% class(resp) | !is.null(resp[["content"]][["errorsExist"]])) {
      return(data.frame(mms_id, portfolio_id = NA, collection_id= NA, service_id = NA))
    }
    
    #portfolio_id <- resp[["content"]][["portfolio"]][[1]][["id"]]
    portfolio_id <- unlist(map(resp[["content"]][["portfolio"]], pluck, "id", .default = "none"))
    # collection_id <- resp[["content"]][["portfolio"]][[1]][["electronic_collection"]][["id"]][["value"]]
    collection_id <- unlist(map(resp[["content"]][["portfolio"]], pluck, "electronic_collection","id", "value",.default = "none"))
    #service_id <- resp[["content"]][["portfolio"]][[1]][["electronic_collection"]][["service"]][["value"]]
    service_id <- unlist(map(resp[["content"]][["portfolio"]], pluck, "electronic_collection","service", "value",.default = "none"))
    Sys.sleep(.5)
    return(data.frame(mms_id, portfolio_id, collection_id, service_id))
  }
}

getPublicAccess <- function(mms_id, collection_id, service_id, portfolio_id, exl_key) {
  resp <- try(alma_collections_helper(collection_id = collection_id, service_id = service_id,
                                      portfolio_id = portfolio_id, 
                                      exl_key = getOption("reserves_search.exl_key", NULL)))
  
  if ("try-error" %in% class(resp)) {
    return(data.frame(public_access_model = NA, path = paste0("Problem with query:", query)))
  }
  
  public_access_model <- resp[["content"]][["public_access_model"]][["value"]]
  Sys.sleep(.5)
  return(data.frame(mms_id, public_access_model, path = resp$path))
}

alma_portfolios_helper <- function(mms_id, 
                                   gateway = "api-na.hosted.exlibrisgroup.com", 
                                   exl_key){
  suppressPackageStartupMessages(require(httr))
  
  reqUrl <- paste0("https://", gateway,
                   "/almaws/v1/bibs/", mms_id, "/portfolios?","&apikey=", exl_key)
  
  resp <- httr::GET(reqUrl)
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  structure(
    list(content = parsed,
         path = resp$url,
         response = resp),
    class = "alma_api"
  )
}

alma_collections_helper <- function(collection_id, service_id, portfolio_id, 
                                    gateway = "api-na.hosted.exlibrisgroup.com", 
                                    exl_key){
  suppressPackageStartupMessages(require(httr))
  
  reqUrl <- paste0("https://", gateway,
                   "/almaws/v1/electronic/e-collections/", 
                   collection_id, "/e-services/", service_id, 
                   "/portfolios/", portfolio_id,"?&apikey=", exl_key)
  
  resp <- httr::GET(reqUrl)
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  structure(
    list(content = parsed,
         path = resp$url,
         response = resp),
    class = "alma_api"
  )
}

primo_api_helper <- function(query, gateway = "api-na.hosted.exlibrisgroup.com", exl_key = getOption("reserves_search.exl_key", NULL), 
                             vid, limit = 100, scope="MyInstitution", 
                             tab = "LibraryCatalog", facet = "facet_rtype,exact,books"){
  
  if (!is.numeric(limit)) {
    stop("limit must be numeric", call. = FALSE)
  }
  
  suppressPackageStartupMessages(require(httr))
  
  reqUrl <- paste0("https://", gateway,
                   "/primo/v1/search?vid=", vid,"&tab=",tab,"&scope=",scope,"&q=",
                   query, "&pcAvailability=false&conVoc=false&limit=",limit,"&qInclude=",facet,"&apikey=", exl_key)
  
  resp <- httr::GET(reqUrl)
  
  if (http_type(resp) == "text/plain") {
    # error seem to be in xml instad of json
  parsed <- read_xml(resp$content)
  xml2::xml_ns_strip(parsed)
  
  if (class(xml_find_first(parsed, "//web_service_result/errorList/error/errorCod")) == "xml_missing"){
  
stop( 
  sprintf(
    "Primo API request failed\n[%s]\n%s", 
    xml_find_first(parsed, "//web_service_result/errorList/error/errorCode") %>% xml_text(),
    xml_find_first(parsed, "//web_service_result/errorList/error/errorMessage") %>% xml_text()
  ),
  call. = FALSE
)  
  } else {
    stop("API did not return json", call. = FALSE)
  }
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "Primo API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(content = parsed,
         path = resp$url,
         response = resp),
    class = "primo_api"
  )
}

alma_e_collection_helper <- function(mms_id, gateway = "api-na.hosted.exlibrisgroup.com", exl_key){
  suppressPackageStartupMessages(require(httr))
  
  reqUrl <- paste0("https://", gateway,
                   "almaws/v1/bibs/", mms_id, "/e-collections","&apikey=", exl_key)
  
  resp <- httr::GET(reqUrl)
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  structure(
    list(content = parsed,
         path = resp$url,
         response = resp),
    class = "primo_api"
  )
}



# un-exported internal function to get HTTP response
# from libbib

# alma_get_http_response <- function(aurl, print.api.responses=FALSE){
#   req <- curl::curl_fetch_memory(aurl)
#   status <- req$status_code
#   # make more cases
#   if(status!=200)
#     stop("API returned non-OK status code ", status)
#   if(print.api.responses){
#     message("\nHTTP response status code: ", req$status_code)
#   }
#   resp <- list(content=rawToChar(req$content),
#                http_status_code=req$status_code)
#   resp
# }




# prepareQueryFromWorldCat <- function(newReq, nullResult) {
#   
#   #if author and title are found in WorldCat; make new request
#   if (is.null(newReq)){
#     
#     return(nullResult)
#     } else {
#     
#     title <- newReq$title[[1]] %>%
#       str_to_lower() %>%
#       str_split(":|\\. ", simplify = TRUE) %>%
#       as.data.frame(stringsAsFactors = FALSE) %>%
#       select("V1") %>%
#       as.character() %>%
#       str_replace("\\, or\\, .*", "") %>%
#       str_replace("^the ","") %>%
#       str_replace("^a[n]? ","") %>%
#       str_replace_all('"', "") %>%
#       str_replace_all(";|,", "")%>%
#       str_trim() 
#     
#     #this is something about limiting length of title
#     
#     titlewords <- strsplit(title, "\\s")
#     
#     title_lengths <- lapply(lapply(titlewords, nchar), cumsum)
#     
#     wordsintitle <- lapply(title_lengths, function(x) which(x < 50))
#     
#     # replace with seq_along?
#     for (i in 1:length(title)){
#       title[i] <-  paste(titlewords[[i]][wordsintitle[[i]]], collapse = " ")
#     }
#     
#     
#     #put quotes around titles that contain not (or catalog will interpret as boolean) 
#     # necessary in Primo API?
#     
#     title <- ifelse(grepl("\\bnot\\b", title), paste0('"', title, '"'), title)
#     
#     #able to simplify with str_locate(",")?
#     # substr(author_name, 1, str_locate(author_name, ",")[1]-1)
#     
#     # last_name <- stri_trans_general(newReq$author[[1]], "latin-ascii") %>%
#     #   str_split(",", simplify = TRUE) %>%
#     #   as.data.frame(stringsAsFactors = FALSE) %>%
#     #   select("V1") %>%
#     #   as.character() %>%
#     #   str_trim() 
#     
#     #pull out everything before the comma, if there is one
#     last_name <- stri_trans_general(newReq$author[[1]], "latin-ascii") %>%
#       ifelse(str_detect(., ","), str_sub(., 1, str_locate(., ",")[1]-1), .) %>% 
#       str_trim()
#     
#     
#     
#     # titleURL <- URLencode(title, reserved = TRUE)
#     # 
#     # nameURL <- URLencode(last_name, reserved = TRUE)
#     
#     #pulls only books and ebooks
#     # changed to bates only
#     if (length(last_name) > 0) {
#       
#       query <- URLencode(paste0("title,contains,",title,",AND;creator,contains,",last_name))
#       
#     } else {
#       
#       #replace with Primo search
#       query <- URLencode(paste0("title,contains,",title))
#       
#     }
#     
#     }
#   
#   return(query)
# }


