#' indices.optimize
#'
#' Explicitly optimize one or more indices.
#' @param index String The name of the index
#' @param flush Logical Specify whether the index should be flushed after performing the operation (default: true)
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both
#' @param max_num_segments Number The number of segments the index should be merged into (default: dynamic)
#' @param only_expunge_deletes Logical Specify whether the operation should only expunge deleted documents
#' @param wait_for_merge Logical Specify whether the request should block until the merge process is finished (default: true)
#' @export
indices.optimize <- function (client, ...) {
  UseMethod("indices.optimize", client)
}

#' @rdname indices.optimize
#' @export
indices.optimize.elasticsearch <- function (client, index = "_all", ignore_unavailable = NULL, allow_no_indices = NULL, expand_wildcards = "open", max_num_segments = NULL,
                                            only_expunge_deletes = FALSE, flush = TRUE,
                                            wait_for_merge = TRUE, raw = FALSE, validate_params = TRUE) {
  path = paste(paste(index, collapse = ","), "_optimize", sep = "/")

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)
  res = httr::POST(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
