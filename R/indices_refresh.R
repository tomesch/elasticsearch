#' indices.refresh
#'
#' Explicitly refresh one or more index, making all operations performed since the last refresh available for search.
#' @param index String The name of the index
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both
#' @param force Logical Force a merge operation to run, even if there is a single segment in the index (default: false)
#' @export
indices.refresh <- function (client, ...) {
  UseMethod("indices.refresh", client)
}

#' @rdname indices.refresh
#' @export
indices.refresh.elasticsearch <- function (client, index = "_all", ignore_unavailable = NULL, allow_no_indices = NULL, expand_wildcards = "open", force = NULL, raw = FALSE, validate_params = TRUE) {
  path = paste(paste(index, collapse = ","), "_refresh", sep = "/")

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
