#' get file
#' @param id The file ID.
#' @param key The authentication token.
#' @export
get_file <- function(id, key = NULL){
    key <- .check_auth(key)
    ff <- GET(paste0("https://cgc-api.sbgenomics.com/v2/files/", id),
                add_headers("X-SBG-Auth-Token" = key,
                            "Content-Type" = "application/json"))
    ff <- content(ff)
    return(responseList(ff))
}

#' list folder
#' @param id The folder ID to list.
#' @param key The authentication token.
#' @export
list_folder <- function(id, key = NULL){
    key <- .check_auth(key)
    ff <- GET(paste0("https://cgc-api.sbgenomics.com/v2/files/", id, "/list"),
                add_headers("X-SBG-Auth-Token" = key,
                            "Content-Type" = "application/json"))
    ff <- content(ff)
    return(responseList(ff))
}
