USER_AGENT <- paste0("rndop/",packageVersion("rndop")," (https://github.com/kalab-oto/rndop)")
                  
sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, "USER_AGENT"), file = "R/sysdata.rda")
