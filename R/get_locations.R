get_locations <- function(filter_session_info) {
    LOCATIONS_URL <- "https://portal23.nature.cz/nd/export_lokalizaci.php"

    filter_PHPSESSID <- filter_session_info$PHPSESSID

    locat <- httr::GET(LOCATIONS_URL,
                      config = httr::set_cookies(PHPSESSID = filter_PHPSESSID),
                      httr::user_agent(USER_AGENT)
                      )
    if (httr::content(locat,"text") == "\n\n\n\n\n\n") {
       cat("No locations available")
    } else {
        locat_ind <- gregexpr("export[^>]*zip", httr::content(locat,"text"))
        cat(length(locat_ind[[1]]))
        sf_list <- list()
        for (i in 1:length(locat_ind[[1]])) {
            locat_url <- paste0("https://portal23.nature.cz/nd/",
                                substring(httr::content(locat, "text"),
                                   locat_ind[[1]][i],
                                   locat_ind[[1]][i] + attr(locat_ind[[1]],
                                                          "match.length")[i]-1)
                                )
            cat(locat_url)
            locat_temp_zip <- tempfile()
            download.file(url = locat_url, destfile = locat_temp_zip)
            locat_temp_dir <- tempfile()
            unzip(locat_temp_zip, exdir = locat_temp_dir)
            sf_list[[i]] <- sf::st_read(locat_temp_dir)
            sf_list <- lapply(sf_list, function(x) sf::st_set_crs(x, 5514))
        }
    return(sf_list)
    }
}