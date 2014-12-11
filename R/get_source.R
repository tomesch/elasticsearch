#'  Retrieve source from index
#'
#' \code{getSource} retrieves source from index
#'
#'@param index A string representing the index.
#'@param type A string representing the type.
#'@param id A string representing the id
#'@param fields A string representing the fields.
#'@param source A boolean representing our choice to return the contents of the _source field or not.
#'@param realtime A boolean representing if the result should be realtime or not
#'@param routing A string allowing to control the _routing aspect when indexing data and explicit routing control is required.
#'@param preference A string that controls a preference of which shard replicas to execute the search request on. 
#'@param refresh A boolean that allows to explicitly refresh one or more index, making all operations performed since the last refresh available for search. 
#'@param version A string representing the return of a version for each search hit.
#'@param exists A boolean that returns documents that have at least one non-null value in the original field.
#'@param raw A boolean that indicates if the format of the response should be in json or not.
#'@param validate.params A boolean indicating the need to validate the passing parameters or not.
#'@param source_include A string representing the source_include; include parameters that you want to display in the returned source.  
#'@param source_exclude A string representing the source_exclude; filter parameters that you do not want to display in the returned source.

#' @export
getSource <- function (index, type = "_all", id, fields = NULL, source = TRUE,
                       realtime = TRUE, routing = NULL, preference = NULL,
                       refresh = FALSE, version = NULL, exists = FALSE, raw = FALSE,
                       validate.params = TRUE, source_include = NULL,
                       source_exclude = NULL) {
  args = as.list(match.call())
  args[[1]] = NULL
  args[['get.source']] = TRUE
  do.call(get, args)
}
