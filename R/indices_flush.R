#' indices.flush
#'
#' Explicitly flush one or more indices.
#'
#' @param index String, String[], Logical A comma-separated list of index names; use _all or empty string for all indices
#' @param force Logical Whether a flush should be forced even if it is not necessarily needed
#' @param full Logical If set to true a new index writer is created and settings that have been changed related to the index writer will be refreshed.
#' @param wait_if_ongoing Logical If set to true the flush operation will block until the flush can be executed if another flush operation is already executing.
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices.
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both.
#' @param validate_params Logical
#' @param raw Logical
#'
#' @examples
#' indices.flush(es, "test")
#' indices.flush(es, "test", force =  TRUE)
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-flush.html}
#'
#' @export
indices.flush <- function (client, ...) {
  UseMethod("indices.flush", client)
}

#' @rdname indices.flush
#' @export
indices.flush.elasticsearch <- function (client, index, force, full, wait_if_ongoing, ignore_unavailable, allow_no_indices, expand_wildcards, raw = FALSE, validate_params = TRUE) {
  if (missing(index)) {
    path = "_flush"
  } else {
    path = paste(paste(index, collapse = ","), "_flush", sep = "/")
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

