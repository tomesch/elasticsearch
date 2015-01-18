#' indices.get
#'
#' @param index String The name of the index
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both
#' @param local Logical Return local information, do not retrieve the state from master node (default: false)
#' @param feature String, String[], Logical A comma-separated list of features
#' @export
indices.get <- function (client, ...) {
  UseMethod("indices.get", client)
}

#' @rdname indices.get
#' @export
indices.get.elasticsearch <- function (client, index = "_all", local = FALSE, ignore_unavailable = FALSE, allow_no_indices = FALSE, expand_wildcards = "open", feature = NULL, raw = FALSE, validate_params = TRUE) {
  path = paste(index, collapse = ",")

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)
  print(url)
  res = httr::GET(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
