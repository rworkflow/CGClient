#' get App details
#' @param project The project ID in the following format: {project_owner}/{project}.
#' @param app_id The ID for the app you are querying.
#' @param key The Authentication Token.
#' @export
get_app <- function(project, app_id, key = NULL){
    key <- .check_auth(key)
    app1 <- GET("https://cgc-api.sbgenomics.com",
                add_headers("X-SBG-Auth-Token" = key,
                            "Content-Type" = "application/json"),
                path = paste("v2/apps", project, app_id, sep = "/"))
    app1 <- content(app1)
    return(responseList(app1))
}

#' add App
#'
#' @param project TThe project ID in the following format: {project_owner}/{project}.
#' @param app The app JSON file to upload. Or a `cwlProcess` from `Rcwl`.
#' @importFrom Rcwl writeCWL
#' @export
add_app <- function(project, app, app_id, key = NULL){
    key <- .check_auth(key)
    if(is.character(app) && file.exists(app)){
        app_file <- app
    }else if(is(app, "cwlProcess")){
        if(is(app, "cwlWorkflow")){
            app_file <- cwlpack(app)
        }else{
            jn <- toJSON(Rcwl:::cwlToList(app), auto_unbox = TRUE)
            app_file <- paste0(tempfile(), ".json")
            write(jn, app_file)
        }
    }
    app0 <- get_app(project, app_id)
    if("revision" %in% names(app0)){
        vid <- app0$revision + 1
    }else{
        vid <- 0
    }
    
    app1 <- POST(paste0("https://cgc-api.sbgenomics.com/v2/apps/", project,
                        "/", app_id, "/", vid, "/raw"),
                 add_headers("X-SBG-Auth-Token" = key,
                             "Content-Type" = "application/json"),
                 body = upload_file(app_file),
                 encode = "json")
    app1 <- content(app1)
    return(responseList(app1))
}


#' require sbpack to wrap cwl workflow
#' @importFrom yaml read_yaml
cwlpack <- function(cwl, name = NULL){
    if(is.null(name)){
        name = deparse(substitute(cwl))
    }
    app_file <- writeCWL(cwl, prefix = name)[1]
    tmp <- paste0(tempfile(), ".cwl")
    re <- system(paste("cwlpack", app_file, ">", tmp))
    stopifnot(re == 0)
    tmp_j <- sub(".cwl", ".json", tmp)
    write(toJSON(read_yaml(tmp), auto_unbox = TRUE), tmp_j)
    return(tmp_j)
}
