USER_AGENT <- paste0("rndop/",packageVersion("rndop")," (https://github.com/kalab-oto/rndop)")

# polygons for spatial query

poly_1_df <- data.frame(x = c(-529744, -528744),
                        y = c(-1115254, -1116254))

poly_1 <- sf::st_as_sfc(
    sf::st_bbox(
        sf::st_as_sf(poly_1_df, coords = c("x", "y"), crs = 5514))
        )

poly_120_df <- data.frame(x = c(-529744 - 5000, -528744 + 5000),
                        y = c(-1115254 + 5000, -1116254 - 5000))

poly_120 <- sf::st_as_sfc(
    sf::st_bbox(
        sf::st_as_sf(poly_120_df, coords = c("x", "y"), crs = 5514))
        )

sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, c("USER_AGENT", "poly_1", "poly_120")), file = "R/sysdata.rda")
