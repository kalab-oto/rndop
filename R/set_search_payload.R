#' Generate a template for user search payload for POST request
#'
#' Allows to generate a template of list of keys and values of user POST request for \code{\link{ndop_download}} function. This is useful for users who wants experiment, or are familiar with the content of the POST request sent to the. 
#' @return `list`
#' @export set_search_payload
#' @examples
#' # Prepare search payload for 6175 KFME field
#' pld <- set_search_payload()
#' pld$rfKvadrat <- 6175

set_search_payload <- function(rfTaxon, rfCeledi, rfKategorie) {
    search_payload <- list(
            but_action =  'Filtrovat',
            but_co = 'rf',
            pagesizeX =  10000,
            aopkOrtho = 0,
            searchSelector = 'ku',
            rfMesiceOd = 1,
            rfMesiceDo = 12,
            rfDatumOd = '',
            rfDatumDo = '',
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
            'text+inputObec' = '',
            parametryZakresu = '',
            existujeZakres = '',
            karta_id = '',
            karta_vztazne_id = '',
            idAkce = '',
            lpass = '')

    if (hasArg(rfTaxon)) {
        list_vals  <- grep(paste(rfTaxon, collapse = "|"),
                                 ndop_list("species"),
                          ignore.case=TRUE,
                          value=TRUE)
        search_payload$rfTaxon <- list_vals
        rfTaxon_num <- length(search_payload$rfTaxon)
        if (rfTaxon_num > 1) {
            search_payload$rfTaxon <- paste(list_vals,
                                            collapse = " || ")
            cat(paste("Processing",rfTaxon_num,"species:\n"))
            cat(paste0(list_vals,","))
        }       
    }

    if (hasArg(rfCeledi)) {
        list_lines <- grep(paste(rfCeledi, collapse = "|"),
                                 ndop_list("family")[,2],
                          ignore.case=TRUE)
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
                                 ndop_list("group")[,2],
                          ignore.case=TRUE)
        search_payload$rfKategorie <- ndop_list("group")[list_lines,1]
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