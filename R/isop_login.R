#' Login to ISOP
#'
#' This function is used to log in to ISOP. Account can be created at 
#' <https://idm.nature.cz/idm/#/registration>. You can use the function 
#' separately, otherwise the function will start automatically if a login is 
#' required.
#' @param username Username for ISOP login. See `Authentication` section for 
#' details
#' @param password Password for ISOP login. See `Authentication` section for 
#' details
#' @param store Store the `username` and `password` in the `.Renviron`. Default
#'  is TRUE.
#' @section Authentication:
#' There are two option how to set login authentication:
#' 1. Set login credentials in the `.Renviron` file. This can be done 
#' manually by editing the file, or just use the function without any paramatres 
#' (`isop_login()`). In case that login credentials are not stored in 
#' `.Renviron`, you will be promted, and the login will be than stored. R must 
#' be restarted to take into account the changes in the `.Renviron` file.
#' 
#' 2. Simply pass strings to each of the parameters. However, this is not 
#' recommended, because the strings will be stored in `.Rhistory` file, which 
#' can be easily, accidentally shared by user.
#'  
#' @return defined global variable `isop_loginhash` with NDOP login hash cookie 
#' @export isop_login
#' @examples
#' # recommended
#' isop_login()
#' 
#' # not recommended
#' usr <- "isop_username"
#' pwd <- "isop_password"
#' isop_login(usr, pwd)

isop_login <- function(username = NULL,
                       password = NULL,
                       store = TRUE) {
    LOGIN_URL <- "https://login.nature.cz/login.php?appid=59"
    if (Sys.getenv("NDOP_USER") != "") {
       username <- Sys.getenv("NDOP_USER")
    }
    if (Sys.getenv("NDOP_PWD") != "") {
       password <- Sys.getenv("NDOP_PWD")
    }

    if (missing(username)) {
        cat(paste0("You are not logged in. Enter username and password.",
                   " For more details see `?isop_login`\n"))
       username <- readline(prompt = "Username: ")
    }
    if (missing(password)) {
       password <- getPass::getPass(msg = "Password: ")
        if (store) {
            ndop_user <- paste("NDOP_USER", "=", username)
            write(ndop_user, file = " ~/.Renviron", append = TRUE)
            ndop_pwd <- paste("NDOP_PWD","=", password)
            write(ndop_pwd, file = "~/.Renviron", append = TRUE)
        }
    }
    login_payload <- list(
            isop_user = username,
            isop_password = password,
            isop_login = "+Přihlásit+se+"
        )

    l <- httr::POST(LOGIN_URL, body = login_payload)
    if (is.na(l$cookies$value[2])) {
       cat(("Login failed"))
    }
    assign('isop_loginhash',l$cookies$value[2], envir = .GlobalEnv)
}
