#' Show promt value lists
#'
#' Check all available values for NDOP queries of species, families, and groups.
#' @param list_type entry category of values: "species" for list of available 
#' species values; "family" for families; and "group" for higher taxonomic 
#' groups defined in NDOP
#' @section Note:
#' Some of returned dataframes contains also `payload_val`, which is value used
#' in the `search_payload` list i.e. in POST requests during querying the server. For entering values in 
#' \code{\link{ndop_download}} use values from `name_lat`.
#' 
#' Some group entries have multiple groups in one row (e.g. `Collembola, 
#' Protura, Diplura` with `payload_val` `119`), you can entry only one of them,
#'  but is not possible to split the request, becaues the request use 
#' the `payload_val`.
#' @export ndop_list
#' @examples
#' # Example with table data
#' ndop_list("group")

ndop_list <- function(list_type){
    if (list_type == "species") {
       return(species_list)
    }
    if (list_type == "family") {
       return(family_list)
    }
    if (list_type == "group") {
       return(group_list)
    }
}

