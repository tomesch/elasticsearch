#' indices.open
#'
#' Open a closed index, making it available for search.
#' @param index String The name of the index
#' @param timeout Date, Number Explicit operation timeout
#' @param master_timeout Date, Number Specify timeout for connection to master
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both
#' @export
indices.open <- function (client, ...) {
  UseMethod("indices.open", client)
}

#' @rdname indices.open
#' @export
indices.open.elasticsearch <- function (client, index = "_all", timeout = NULL, master_timeout = NULL, allow_no_indices = NULL, expand_wildcards = "closed", ignore_unavailable = FALSE,
                                        raw = FALSE, validate_params = TRUE) {
  path = paste(paste(index, collapse = ","), "_open", sep = "/")

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
