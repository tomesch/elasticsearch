#' Get a document from an index based on its id
#'
#' \code{get} searches a document in an index based on its id and returns it.
#'
#' @param index A string representing the index.
#' @param type A string representing the type.
#' @param id A string representing the id.
#' @param fields A string representing the fields.
#' @param source A boolean representing our choice to return the contents of the _source field or not.
#' @param source_include A string representing the source_include; include parameters that you want to display in the returned source.  
#' @param source_exclude A string representing the source_exclude; filter parameters that you do not want to display in the returned source.
#' @param realtime A boolean representing if the result should be realtime or not
#' @param routing A string allowing to control the _routing aspect when indexing data and explicit routing control is required.
#' @param preference A string that controls a preference of which shard replicas to execute the search request on. 
#' @param refresh A boolean that allows to explicitly refresh one or more index, making all operations performed since the last refresh available for search. 
#' @param version A string representing the return of a version for each search hit.
#' @param exists A boolean that returns documents that have at least one non-null value in the original field.
#' @param raw A boolean that indicates if the format of the response should be in json or not.
#' @param validate.params A boolean indicating the need to validate the passing parameters or not.
#'
#' @examples
#' get("twitter", id="1")
#' get("twitter", "tweet", "1")
#' get("twitter", "tweet", "1", 'c("name", "date")')
#' get("twitter", "tweet", "1", 'c("name", "date")', TRUE)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-get.html}
#'
#' @export
get <- function (index, type = "_all", id, fields = NULL, source = TRUE,
                 source_include = NULL, source_exclude = NULL, realtime = TRUE, routing = NULL, preference = NULL,
                 refresh = FALSE, version = NULL, exists = FALSE, raw = FALSE,
                 validate.params = TRUE, get.source = FALSE) {
  if (missing(index) || missing(id)) {
    stop()
  }
  url = getOption("res_url")

  path = paste(index, paste(type, collapse = ","), id, sep = "/")
  if (get.source) {
    path = paste(path, '_source', sep = "/")
  }

  if (!is.null(fields)) {
    fields = paste(fields, collapse = ",")
  }
  if (!is.null(source_include)) {
    source_include = paste(source_include, collapse = ",")
  }
  if (!is.null(source_exclude)) {
    source_exclude = paste(source_exclude, collapse = ",")
  }

  args = list("_source_exclude" = source_exclude,
              "_source_include" = source_include, "_source" = source,
              fields = fields, realtime = realtime, routing = routing,
              preference = preference, refresh = refresh, version = version)

  if (validate.params) {
    validateArgs(args)
  }

  args = prepareArgs(args)

  url = httr::modify_url(url, "path" = path, "query" = args)

  if (exists) {
    res = httr::HEAD(url)
    if (res['status_code'] == 404) {
      return(FALSE)
    } else if (res['status_code'] == 200) {
      return(TRUE)
    }
  } else {
    res = httr::GET(url)
    httr::stop_for_status(res)
  }

  formatESResult(res, raw)
}
