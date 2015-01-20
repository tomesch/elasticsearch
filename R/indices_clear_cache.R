#' indices.clearCache
#'
#' Clear either all caches or specific cached associated with one ore more indices.
#'
#' @param index String, String[], Logical A comma-separated list of index name to limit the operation
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both
#' @param fields String, String[], Logical A comma-separated list of fields to clear when using the field_data parameter (default: all)
#' @param field_data Logical Clear field data
#' @param filter Logical Clear filter caches
#' @param id_cache Logical Clear ID caches for parent/child
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param query_cache Logical Clear query cache
#' @param validate_params Logical
#' @param raw Logical
#'
#' @examples
#' indices.clearCache(es)
#' indices.clearCache(es, "twitter")
#'
#' @references
#' \url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-clearcache.html}
#'
#' @export
indices.clearCache <- function (client, ...) {
  UseMethod("indices.clearCache", client)
}

#' @rdname indices.clearCache
#' @export
indices.clearCache.elasticsearch <- function (client, index, allow_no_indices, expand_wildcards, fields, field_data, filter, id_cache, ignore_unavailable, query_cache, raw = FALSE, validate_params = TRUE) {
  if (missing(index)) {
    path = "_cache/clear"
  } else {
    path = paste(paste(index, collapse = ","), "_cache/clear", sep = "/")
  }

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

