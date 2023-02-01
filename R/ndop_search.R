set_search_payload <- function(rfTaxon, rfCeledi, rfKategorie) {
    search_payload <- list(
            but_action =  'Filtrovat',
            but_co = 'rf',
            pagesizeX =  1000)
    
    if (hasArg(rfTaxon)) {
        list_lines <- grep(paste(rfTaxon, collapse = "|"),
                                 ndop_list("species"))
        search_payload$rfTaxon <- ndop_list("species")[list_lines]
        rfTaxon_num <- length(search_payload$rfTaxon)
        if (rfTaxon_num > 1) {
            search_payload$rfTaxon <- paste(search_payload$rfTaxon,
                                            collapse = " || ")
            cat(paste("Processing",rfTaxon_num,"species:\n"))
            cat(paste0(ndop_list("species")[list_lines],","))
        }       
    }

    if (hasArg(rfCeledi)) {
        list_lines <- grep(paste(rfCeledi, collapse = "|"),
                                 ndop_list("family")[,2])
        search_payload$rfCeledi <- ndop_list("family")[list_lines,1]
        rfCeledi_num <- length(search_payload$rfCeledi)
        if (rfCeledi_num > 1) {
            search_payload$rfCeledi <- paste(search_payload$rfCeledi,
                                            collapse = " || ")
            cat(paste("Processing",rfCeledi_num,"families:\n"))
            cat(paste0(ndop_list("family")[list_lines,2],","))
        }
    }

    if (hasArg(rfKategorie)) {
        list_lines <- grep(paste(rfKategorie,collapse = "|"),
                                 ndop_list("group")[,2])

        search_payload$rfKategorie <- ndop_list("group")[list_lines,1]
        search_payload$rfMesiceOd <- 1
        search_payload$rfMesiceDo <- 12
        rfKategorie <- length(search_payload$rfKategorie)
        if (rfKategorie > 1) {
            search_payload$rfKategorie <- paste(search_payload$rfKategorie,
                                            collapse = ",")
            cat(paste("Processing",rfKategorie,"families:\n"))
            cat(paste0(ndop_list("group")[list_lines,2],","))
        }
    }

    return(search_payload)
} 



ndop_search <- function(search_payload){
    SEARCH_URL <- paste0("https://portal.nature.cz/nd/find.php",
                         "?akce=seznam&opener=&vztazne_id=0")
    if (!exists("isop_loginhash")) {
       isop_login()
    }

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