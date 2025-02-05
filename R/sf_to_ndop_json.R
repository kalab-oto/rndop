sf_to_ndop_json <- function(sf_poly) {
    sf_poly <- sf::st_transform(sf_poly, 5514)
    geom <- sf::st_geometry(sf_poly)[[1]]
    if (sf::st_area(geom)/1000000 > 100) {
        stop("Maximum allowed area for polygon is 100 km², choose smaller polygon")
    }
    coords <- lapply(geom, function(ring) as.matrix(ring))
    bbox <- sf::st_bbox(sf_poly)
  
    ndop_json <- list(
        type = "polygon",
        coordinates = coords[[1]],
        xmin = bbox$xmin,
        xmax = bbox$xmax,
        ymin = bbox$ymin,
        ymax = bbox$ymax
    )

    ndop_json <- as.character(jsonlite::toJSON(ndop_json, auto_unbox = TRUE))
    ndop_json <- gsub("\\[\\[", "[[[", ndop_json)
    ndop_json <- gsub("]]", "]]]", ndop_json)

    return(ndop_json)
}
