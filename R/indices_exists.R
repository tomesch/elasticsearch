#' indices.exists
#'
#' Return a boolean indicating whether given index exists.
#' @param index String The name of the index
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both
#' @param local Logical Return local information, do not retrieve the state from master node (default: false)
#' @export
indices.exists <- function (client, ...) {
  UseMethod("indices.exists", client)
}

#' @rdname indices.exists
#' @export
indices.exists.elasticsearch <- function (client, index, ignore_unavailable = NULL, allow_no_indices = NULL, expand_wildcards = "open", local = FALSE, validate_params = TRUE) {
  if (missing(index)) {
    stop()
  }
  path = paste(index, collapse = ",")

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)
  res = httr::HEAD(url)
  if (res['status_code'] >= 400) {
    list(exists = FALSE, url = url)
  } else if (res['status_code'] == 200) {
    list(exists = TRUE, '_url' = url)
  }
}

