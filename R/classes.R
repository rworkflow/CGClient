#' Response Json
#' @rdname responseList
#' @importFrom yaml as.yaml
#' @importFrom methods new
#' @export
setClass("responseList", contains = "list")

responseList <- function(res){
    new("responseList", res)
}

setMethod("show", "responseList", function(object){
    cat(as.yaml(object))
})
