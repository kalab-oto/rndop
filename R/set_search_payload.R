#' Generate a template for user search payload for POST request
#'
#' Allows to generate a template of list of keys and values of user POST request for \code{\link{ndop_download}} function. This is useful for users who wants experiment, or are familiar with the content of the POST request sent to the. 
#' @return `list`
#' @export set_search_payload
#' @examples
#' # Prepare search payload for 6175 KFME field
#' pld <- set_search_payload(rfKvadrat = 6175, num_rec_only = T)

set_search_payload <- function(
            but_action =  'Filtrovat',
            but_co = 'rf',
            pagesizeX =  10000,
            aopkOrtho = 0,
            searchSelector = 'ku',
            rfMesiceOd = '',
            rfMesiceDo = '',
            rfDatumOd = '1.1.0001',
            rfDatumDo = '1.1.9999',
            rfAkce = '',
            rfNalez = '',
            rfIdLokalizace = '',
            rfLokalizace = '',
            rfAutor = '',
            rfZdroj = '',
            rfProjekt = '',
            rfMetadata = '',
            rfZapsal = '',
            rfTaxon = '',
            rfKategorie = '',
            rfCeledi = '',
            rfKatastr = '',
            rfMZCHU = '',
            rfVZCHU = '',
            rfEVL = '',
            rfPO = '',
            rfZahrUzemi = '',
            rfKraj = '',
            rfPusobnost = '',
            rfSpatialX = '',
            rfSpatial = '',
            rfKvadrat = '',
            rfPresnost = '',
            rfNalezyCr = '',
            rfFotky = '',
            rfKO = '',
            rfSO = '',
            rfO = '',
            rfCcsCR = '',
            rfCcsEN = '',
            rfCcsVU = '',
            rfCcsNT = '',
            rfCcsEX = '',
            rfCcsDD = '',
            rfEVDII = '',
            rfEVDIV = '',
            rfEVDV = '',
            rfEVDI = '',
            rfBL = '',
            rfGL = '',
            rfWL = '',
            rf1143 = '',
            `text+inputObec` = '',
            parametryZakresu = '',
            existujeZakres = '',
            karta_id = '',
            karta_vztazne_id = '',
            idAkce = '',
            lpass = ''){

    search_payload <- c(as.list(environment()))

    if (hasArg(rfTaxon) && !is.null(rfTaxon)) {
        list_vals  <- grep(paste(rfTaxon, collapse = "|"),
                                 ndop_list("species"),
                          ignore.case=TRUE,
                          value=TRUE)
        
        if (length(rfTaxon) > length(list_vals)) {
            cat(paste("species not found in NDOP database:\n",
                        paste(rfTaxon[!(rfTaxon %in% list_vals)], collapse = ", "),
                        "\ncheck `?ndop_list('species')`\n"
                        )
            )
        }
        
        search_payload$rfTaxon <- list_vals
        rfTaxon_num <- length(search_payload$rfTaxon)

        search_payload$rfTaxon <- paste(list_vals, collapse = " || ")
        cat(paste("Found",rfTaxon_num,"species:\n"))
        cat(paste0(list_vals, collapse = ", "), "\n")
  
    }

    if (hasArg(rfCeledi) && !is.null(rfCeledi)) {
        list_lines <- grep(paste(rfCeledi, collapse = "|"),
                                 ndop_list("family")[,2],
                          ignore.case=TRUE)

        if (length(rfCeledi) > length(list_lines)) {
            cat(paste("families not found in NDOP database:\n",
                      paste(rfCeledi[!(rfCeledi %in% ndop_list("family")[list_lines, 2])], collapse = ", "),
                      "\ncheck `?ndop_list('family')`\n"
                      )
            )
        }
        
        search_payload$rfCeledi <- ndop_list("family")[list_lines,1]

        rfCeledi_num <- length(search_payload$rfCeledi)

        search_payload$rfCeledi <- paste(search_payload$rfCeledi,
                                        collapse = " || ")
        cat(paste("Found",rfCeledi_num,"families:\n"))
        cat(paste0(ndop_list("family")[list_lines, 2], collapse = ", "), "\n")
    }

    if (hasArg(rfKategorie) && !is.null(rfKategorie)) {
        list_lines <- grep(paste(rfKategorie,collapse = "|"),
                                 ndop_list("group")[,2],
                          ignore.case=TRUE)

        if (length(rfKategorie) > length(list_lines)) {
            cat(paste("groups not found in NDOP database:\n",
                      paste(rfKategorie[!(rfKategorie %in% ndop_list("group")[list_lines, 2])], collapse = ", "),
                      "\ncheck `?ndop_list('group')`\n"
                      )
            )
        }

        search_payload$rfKategorie <- ndop_list("group")[list_lines,1]
        
        rfKategorie_num <- length(search_payload$rfKategorie)
        search_payload$rfKategorie <- paste(search_payload$rfKategorie,
                                        collapse = ",")
        cat(paste("Found", rfKategorie_num, "groups:\n"))
        cat(paste(ndop_list("group")[list_lines, 2], collapse = ", " ), "\n")
    }

    return(search_payload)
}