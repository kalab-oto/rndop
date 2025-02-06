# Group

r <- httr::GET("https://portal23.nature.cz/redlist/v_nd_taxon_category.php?", httr::user_agent(USER_AGENT))

html_parse <-  gsub(paste0('^.*<select name="skupina" style=',
                           '"width: 360px; padding: 2px;">\\s*|\\s*<td>.*$'),
                    '',
                    httr::content(r,  "text"))
html_parse <- strsplit(html_parse,"option value")

group_df <- as.data.frame(html_parse)
group_df <- read.table(text = group_df[,1],
                        sep = '-',
                        fill = T,
                        h = F,
                        dec = '/')[-c(1:2,61),]
group_df$V1 <- gsub('=','',group_df$V1)
group_df$V2 <- trimws(group_df$V2)
group_df$V2 <- gsub('</option><','',group_df$V2)
group_df$V2 <- gsub('</option>      <','',group_df$V2)
group_df$V2 <- gsub('/select>','',group_df$V2 )

group_list <- cbind(read.table(text = group_df[,1],
                               sep = '>',
                               fill = T,
                               h = F,
                               dec = '/'),
                               group_df$V2)

names(group_list) <- c("payload_val","name_cz","name_lat")
group_list <- group_list[c(1,3)]

# Family

family_cat <- list()

for (i in seq(nrow(group_list))) {
    r <- httr::GET(paste0("https://portal23.nature.cz/inc/components/",
                           "modals/modals.php?opener=rfCeledi&promka=&",
                           "id_kategorie=",group_list$payload_val[i]), httr::user_agent(USER_AGENT))
    family_cat[[i]] <- jsonlite::fromJSON(httr::content(r,"text"))$items
}

family_list <- do.call("rbind",family_cat)
names(family_list) <- c("payload_val","name_lat","group_name_cz")
family_list <- family_list[1:2]

# Species

r <- httr::GET(paste0("https://portal23.nature.cz/inc/components/modals/",
                      "modals.php?opener=rfTaxon&promka=&id_kategorie="), httr::user_agent(USER_AGENT))

species_list <- jsonlite::fromJSON(httr::content(r,"text"))$items
species_list <- unique(species_list[,1])

# Area

r <- httr::GET(paste0("https://portal23.nature.cz/inc/components/modals/",
                      "modals.php?opener=multiple&promka=&id_kategorie="), httr::user_agent(USER_AGENT))

area_list <- jsonlite::fromJSON(httr::content(r,"text"))$items
area_list <- as.data.frame(area_list)
names(area_list) <- c("payload_val","type","name")
# Save data

usethis::use_data(species_list,family_list,group_list,area_list
                  internal = TRUE,
                  overwrite = T)
