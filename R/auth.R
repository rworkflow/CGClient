#' Authorization
#' @param key The Authentication Token of your account from CGC.
#' @import httr
#' @export
auth <- function(key){
    a <- GET("https://cgc-api.sbgenomics.com/v2/user",
             add_headers("X-SBG-Auth-Token" = key,
                         "Content-Type" = "application/json"))
    a <- content(a)
    if("message" %in% names(a) && a$message == "Unauthorized"){
        stop("Unauthorized")
    }else{
        options(cgc_auth_key = key)
        return(responseList(a))
    }
}

.check_auth <- function(key){
    if(is.null(key)){
        key <- getOption("cgc_auth_key")
    }
    if(is.null(key)) stop("Auth key is not set.")
    return(key)
}
