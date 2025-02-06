USER_AGENT <- paste0("rndop/",packageVersion("rndop")," (https://github.com/kalab-oto/rndop)")

usethis::use_data(USER_AGENT,
                  internal = TRUE,
                  overwrite = T)
