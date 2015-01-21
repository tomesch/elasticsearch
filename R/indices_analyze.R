#' indices.analyze
#'
#' Perform the analysis process on a text and return the tokens breakdown of the text.
#'
#' @param index String The name of the index to scope the operation
#' @param analyzer String The name of the analyzer to use
#' @param char_filters String, String[], Logical A comma-separated list of character filters to use for the analysis
#' @param field String Use the analyzer configured for this field (instead of passing the analyzer name)
#' @param filters String, String[], Logical A comma-separated list of filters to use for the analysis
#' @param text String The text on which the analysis should be performed (when request body is not used)
#' @param tokenizer The name of the tokenizer to use for the analysis
#' @param format String Format of the output (detailed or text)
#' @param validate_params Logical
#' @param raw Logical
#'
#' @examples
#' indices.analyze(es, "will you analyse this for me please ?")
#' indices.analyze(es, index = "test", text = "will you analyse this for me please ?", format="detailed")
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-analyze.html}
#'
#' @export
indices.analyze <- function (client, ...) {
  UseMethod("indices.analyze", client)
}

#' @rdname indices.analyze
#' @export
indices.analyze.elasticsearch <- function (client, text, index, analyzer, char_filters, field, filters, format, tokenizer, raw = FALSE, validate_params = TRUE) {
  if (missing(index)) {
    path = "_analyze"
  } else {
    path = paste(index, "_analyze", sep = "/")
  }

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)
  res = httr::GET(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
