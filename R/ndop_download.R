#' Download occurrence data from NDOP
#'
#' Download data from NDOP. Default data source is table data containing all
#'  available informations about records. Optionally is also possible to 
#' download raw localizations.
#' @param species search by species or genus name
#' @param family search by family name
#' @param group search by predefined higher taxon categories
#' @param locations enables download raw localizations provided by NDOP in 
#' ESRI shapefiles. The localizations are provided as points, lines and polygons
#' layers, and contains only geometries and lcalization id. Options: 1 - 
#' download only localization (without table data); 2 - download localization 
#' and table data; NULL - do not download localization (default)
#' @param search_payload allows to enter a list of keys and values of POST request directly. Use \code{\link{ndop_download}} function for genarating tepmlate
#' @param num_rec_only if TRUE, returns only number of records found
#' @section Coordinates:
#' Table data includes XY coordinates in S-JTSK (EPSG:5514). Source of some 
#' records are polygons or lines, in that case XY coordinates in table are 
#' centroids of source geometries. See `locations` parameter for downloading
#' raw geometries. 
#' @return `data.frame` with table data or/and list of `sf` objects with 
#' localizations (depend on `locations` parameter)
#' @export ndop_download
#' @examples
#' # Example with table data
#' mr <- ndop_download("mantis religiosa")
#' head(mr)
#'
#' require(sf)
#' sf_mr <- st_as_sf(x = mr,
#'                coords = c("X", "Y"),
#'                crs = st_crs(5514))
#'
#' plot(sf_mr$geometry)
#'
#' sf_mr$year <- as.numeric(substring(sf_mr$DATUM_OD,1,4))
#' hist(sf_mr$year,100)
#' plot(sf_mr["year"])
#' 
#' # Example with localization data
#'
#' locs <- ndop_download("mantis religiosa", locations = 1)
#' plot(locs[[1]]$geometry)

ndop_download <- function(species, family, group, locations = 0, search_payload, num_rec_only) {
    
    if (!hasArg(search_payload)) {
        search_payload <- set_search_payload(rfTaxon = species,
                                             rfCeledi = family,
                                             rfKategorie = group)
    }

    pagesize <- search_payload$pagesizeX

    filter_session <- ndop_search(search_payload)

    ndtoken <- filter_session$ndtoken
    isop_loginhash <- filter_session$isop_loginhash
    num_rec <- filter_session$records

    if (hasArg(num_rec_only)) {
        return(num_rec)
    }

    if (locations == 1) {
        sf_list <- get_locations(filter_session)
        return(sf_list)
    }

    table_payload = list(
        meziexport_druh = 'nalezy',
        meziexport_sloupec_CXAKCE_AUTOR = 1,
        meziexport_sloupec_CXAKCE_DATI_DO = 1,
        meziexport_sloupec_CXAKCE_DATI_OD = 1,
        meziexport_sloupec_CXAKCE_ZDROJ = 1,
        meziexport_sloupec_CXEVD = 1,
        meziexport_sloupec_CXKATASTR_NAZEV = 1,
        meziexport_sloupec_CXLOKAL_ID = 1,
        meziexport_sloupec_CXLOKAL_KVADRAT_XY = 1,
        meziexport_sloupec_CXLOKAL_NAZEV = 1,
        meziexport_sloupec_CXLOKAL_POZN = 1,
        meziexport_sloupec_CXLOKAL_X = 1,
        meziexport_sloupec_CXLOKAL_Y = 1,
        meziexport_sloupec_CXLOKAL_Z = 1,
        meziexport_sloupec_CXMTD_DTB = 1,
        meziexport_sloupec_CXOSOBY_ZAPSAL = 1,
        meziexport_sloupec_CXPRESNOST = 1,
        meziexport_sloupec_CXPROJ_NAZEV = 1,
        meziexport_sloupec_CXREDLIST = 1,
        meziexport_sloupec_CXTAXON_IDX_CATEG = 1,
        meziexport_sloupec_CXTAXON_NAME = 1,
        meziexport_sloupec_CXTAXON_NAME_CZ = 1,
        meziexport_sloupec_CXVALIDACE = 1,
        meziexport_sloupec_CXVYHL = 1,
        meziexport_sloupec_HOD_VEROH = 1,
        meziexport_sloupec_ID_ND_NALEZ = 1,
        meziexport_sloupec_NEGATIVNI = 1,
        meziexport_sloupec_ODHAD = 1,
        meziexport_sloupec_POCET = 1,
        meziexport_sloupec_POKRYVNOST = 1,
        meziexport_sloupec_POP_POC = 1,
        meziexport_sloupec_POZ_HAB = 1,
        meziexport_sloupec_POZ_HABLOK = 1,
        meziexport_sloupec_POZNAMKA = 1,
        meziexport_sloupec_REL_POC = 1,
        meziexport_sloupec_STRUKT_POZN = 1,
        meziexport_sloupec_TAX_NOTE = 1,
        meziexport_sloupec_VEROH = 1,
        meziexport_sloupec_DATI_OD = 1,
        meziexport_sloupec_KODHN = 1,
        meziexport_sloupec_VAL_POZN = 1,
        meziexport_sloupec_GAR_POZN = 1,
        meziexport_sloupec_DATI_INSERT = 1,
        meziexport_sloupec_DATI_UPDATE = 1,
        meziexport_sloupec_UPDATOR = 1,
        meziexport_tlacitko = 'Exportovat',
        meziexport_typ_exportu = 'csv',
        ndtokenexport = ndtoken
    )
    tables_num <- ceiling(num_rec / pagesize)
    table_df_list <- vector("list", length = tables_num)
    cat("Downloading:\n")
    for (i in 1:tables_num) {
        frompage <- seq(0, num_rec, pagesize)[i]

        if (frompage + pagesize < num_rec) {
           to <- frompage + pagesize
        } else {
           to <- num_rec
        }
        cat(paste0(frompage + 1, " - ", format(to, scientific = FALSE), "\n"))
        if (num_rec == frompage + pagesize) {
           cat(num_rec)
        }
        table_url <- paste0("https://portal23.nature.cz/nd/find.php?",
                            "akce=seznam&opener=&vztazne_id=0&",
                            "order=ID_ND_NALEZ&orderhow=DESC&frompage=",
                            format(frompage, scientific = FALSE),
                            "&pagesize=",
                            format(pagesize, scientific = FALSE),
                            "&filtering=&searching=&export=1&ndtoken=",
                            ndtoken)
        table_post <- httr::POST(url = table_url,
                           body = table_payload,
                           config = httr::set_cookies(
                                            isop_loginhash = isop_loginhash))
        table_resp <- httr::content(table_post,
                                    type = "text",
                                    encoding = 'cp1250')
        table_df_list[[i]] <- read.delim(text = table_resp,
                                         sep = ";",
                                         dec = ",",
                                         stringsAsFactors = FALSE)
        hidden_coords <- grep("<i>Skrytá lokalizace</i>",
                                 table_df_list[[i]]$Y)
        table_df_list[[i]][hidden_coords,c("X","Y")] <-  NA
        table_df_list[[i]]$X <- as.numeric(gsub(",",".",table_df_list[[i]]$X))
        table_df_list[[i]]$Y <- as.numeric(gsub(",",".",table_df_list[[i]]$Y))    
    }
    table_df <- do.call(rbind, table_df_list)
    if (locations == 2) {
        sf_list <- get_locations(filter_session)
        sf_df <- list(table_df, sf_list)
        return(sf_df)
    }
    return(table_df)
}
