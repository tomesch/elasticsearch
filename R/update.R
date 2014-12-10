#' Update a document with a script
#'
#' \code{update} updates a document based on a script provided.
#'
#' @param index A string representing the index
#' @param type A string representing the type
#' @param id A string representing the id
#' @param body A string representing the body
#' @param routing A string allowing to control the _routing aspect when indexing data and explicit routing control is required.
#' @param parent A string pointing to the parent type this child relates to.
#' @param timeout A string representing the value of the timeout 
#' @param refresh A boolean that allows to explicitly refresh one or more index, making all operations performed since the last refresh. 
#' @param fields A string representing the fields.
#' @param version A string representing the return of a version for each search hit.
#' @param validate.params A boolean indicating the need to validate the passing parameters or not.
#'
#' @examples
#' update("twitter", "tweet", 1)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-update.html}
#'
#' @export
update <- function (index, type, id, body, routing = NULL, parent = NULL,
                    timeout = "1m", refresh = FALSE, fields = NULL,
                    version = NULL, validate.params = TRUE) {
  if (missing(index) || missing(type) || missing(id) || missing(script)) {
    stop()
  } else {
    url = getOption("url")

    path = paste(index, type, id, "_update", sep="/")

    if (!is.null(fields)) {
      fields = paste(fields, collapse = ",")
    }
    args = list(routing = routing, parent = parent, timeout = timeout,
                refresh = refresh, fields = fields, version = version)

    if (validate.params) {
      validateArgs(args)
    }

    args = prepareArgs(args)

    url = httr::modify_url(url, "path" = path, "query" = args)

    res <- httr::POST(url, body=script, content_type_json())
    stop_for_status(res)

    formatESResult(res, raw)
  }
}
