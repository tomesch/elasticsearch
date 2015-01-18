#' indices.close
#'
#' Close an index to remove its overhead from the cluster. Closed index is blocked for read/write operations.
#' @param index String The name of the index
#' @param timeout Date, Number Explicit operation timeout
#' @param master_timeout Date, Number Specify timeout for connection to master
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both
#'
#' @rdname indices.close
#' @export
indices.close <- function (client, ...) {
  UseMethod("indices.close", client)
}

#' @rdname indices.close
#' @export
indices.close.elasticsearch <- function (client, index = "_all", timeout = NULL, master_timeout = NULL, allow_no_indices = NULL, expand_wildcards = "open", ignore_unavailable = FALSE,
                           raw = FALSE, validate_params = TRUE) {
  path = paste(paste(index, collapse = ","), "_close", sep = "/")

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
