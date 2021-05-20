#' get App details
#' @param project The project ID in the following format: {project_owner}/{project}.
#' @param app The ID for the app you are querying.
#' @param key The Authentication Token.
#' @export
get_app <- function(project, app, key = NULL){
    key <- .check_auth(key)
    app1 <- GET("https://cgc-api.sbgenomics.com",
                add_headers("X-SBG-Auth-Token" = key,
                            "Content-Type" = "application/json"),
                path = paste("v2/apps", project, app, sep = "/"))
    app1 <- content(app1)
    return(responseList(app1$raw))
}
