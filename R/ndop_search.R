ndop_search <- function(search_payload){
    SEARCH_URL <- paste0("https://portal23.nature.cz/nd/find.php",
                         "?akce=seznam&opener=&vztazne_id=0")
    if (!exists("isop_loginhash") || is.na(isop_loginhash)) {
        isop_login()
    }

    for (i in 1:2){
        filter_page <- httr::POST(SEARCH_URL,
                           body = search_payload,
                           config = httr::set_cookies(
                                            isop_loginhash = isop_loginhash))
    }
    
    Sys.sleep(3)

    filter_page_cont <- httr::content(filter_page)
    
    num_rec_ind <- gregexpr("záznam ze \\d+", 
                            filter_page_cont)
    num_rec <- as.numeric(
        substring(
            filter_page_cont,
            num_rec_ind[[1]][1] + 10,
            num_rec_ind[[1]][1] + attr(num_rec_ind[[1]], "match.length")[1] - 1)
        )
    ndtoken_ind <- gregexpr("ndtoken=\\w", filter_page_cont)
    ndtoken <- substring(
           filter_page_cont,
           ndtoken_ind[[1]][1] + 8,
           ndtoken_ind[[1]][1] + attr(ndtoken_ind[[1]], "match.length")[1] + 30)
    filter_session_info <- list(records = num_rec,
                                isop_loginhash = isop_loginhash,
                                ndtoken = ndtoken,
                                PHPSESSID = filter_page$cookies[1,"value"])
    cat(paste0(num_rec," records found\n"))
    return(filter_session_info)
}
