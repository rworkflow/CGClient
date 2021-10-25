#' create new task
#'
#' https://docs.cancergenomicscloud.org/docs/create-a-new-task
#' 
#' @param project The project ID in the following format: {project_owner}/{project}.
#' @param app The ID for the app you are querying.
#' @param key The Authentication Token.
#' @param inputs The list of input parameters for the App.
#' @param batch_input The ID from the inputs for batch run.
#' @param batch_by The type of batch.
#' @param extlist The other requests in a list.
#' @return A response list.
#' @importFrom methods is
#' @importFrom jsonlite toJSON
#' @export
new_task <- function(project, app, inputs, batch_input = NULL, batch_by = NULL,
                     key = NULL, extlist = NULL){
    key <- .check_auth(key)
    inputsList <- lapply(inputs, function(x){
        if(is(x, "list")){
            if(!"class" %in% names(x)){
                ctype <- "File"
            }else{
                ctype <- x$class
            }
            if(length(x$id) > 1 | is(x$id, "list")){
                lapply(x$id, function(i)list(class = ctype, path = i))
            }else{
                list(class = ctype,
                     path = x$id)
            }
        }else{
            x
        }
        
    })
    if(!grepl("\\/", app)){
        app <- paste(project, app, sep = "/")
    }
    Inputs <- list(name = paste(sub(".*\\/", "", app), date()),
                   project = project,
                   app = app,
                   inputs = inputsList,
                   batch_input = batch_input,
                   batch_by = batch_by)
    if(!is.null(extlist)){
        Inputs <- c(Inputs, extlist)
    }
                   
    Inputs <- Inputs[lengths(Inputs)>0]
    
    post_task <- POST("https://cgc-api.sbgenomics.com/v2/tasks",
                      add_headers("X-SBG-Auth-Token" = key,
                                  "Content-Type" = "application/json"),
                      body = toJSON(Inputs, auto_unbox = T),
                      encode = "json", verbose())
    responseList(content(post_task))
}

#' Run a task
#'
#' @param task The response list with task ID.
#' @param key The authentication token.
#' @export
run_task <- function(task, key = NULL){
    key <- .check_auth(key)
    run <- POST(paste0("https://cgc-api.sbgenomics.com/v2/tasks/",
                       task$id, "/actions/run"),
                add_headers("X-SBG-Auth-Token" = key,
                            "Content-Type" = "application/json"))
    responseList(content(run))
}
