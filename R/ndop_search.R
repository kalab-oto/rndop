ndop_search <- function(taxon){
    SEARCH_URL <- paste0("https://portal.nature.cz/nd/find.php",
                         "?akce=seznam&opener=&vztazne_id=0")
    if (!exists("isop_loginhash")) {
       isop_login()
    }
    search_payload <- list(
        rfTaxon = taxon,
        but_action = 'Filtrovat',
        but_co = 'rf',
        pagesizeX = 1000
    )
    for (i in 1:2){
        filter_page <- httr::POST(SEARCH_URL,
                           body = search_payload,
                           config = httr::set_cookies(
                                            isop_loginhash = isop_loginhash))
    }

    num_rec_ind <- gregexpr("záznam ze \\d+", 
                            httr::content(filter_page, 
                            "text"))
    num_rec <- as.numeric(
        substring(
            httr::content(filter_page, "text"),
            num_rec_ind[[1]][1] + 10,
            num_rec_ind[[1]][1] + attr(num_rec_ind[[1]], "match.length")[1] - 1)
        )
    ndtoken_ind <- gregexpr("ndtoken=\\w", httr::content(filter_page, "text"))
    ndtoken <- substring(
           httr::content(filter_page, "text"),
           ndtoken_ind[[1]][1] + 8,
           ndtoken_ind[[1]][1] + attr(ndtoken_ind[[1]], "match.length")[1] + 30)
    filter_session_info <- list(records = num_rec,
                                isop_loginhash = isop_loginhash,
                                ndtoken = ndtoken,
                                PHPSESSID = filter_page$cookies[1,"value"])
    return(filter_session_info)
}