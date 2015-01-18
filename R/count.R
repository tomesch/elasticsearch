#' count
#'
#' Get the number of documents for the cluster, index, type, or a query.
#'
#' \code{count} counts items that match a query.
#'
#' @param index String, String[] A vector of indices  to restrict the results
#' @param type String, String[] A vector of types to restrict the results
#' @param body String The query
#' @param min_score Number Include only documents with a specific _score value in the result
#' @param preference String Specify the node or shard the operation should be performed on (default: random)
#' @param routing String Specific routing value
#' @param expand_wildcards String Whether to expand wildcard expression to concrete indices that are open, closed or both
#' @param ignore_unavailable Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)
#' @param allow_no_indices Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)
#' @param raw A string representing the query
#' @param validate_params A string representing the query
#'
#' @export
count <- function (client, ...) {
  UseMethod("count", client)
}

#' @rdname count
#' @export
count.elasticsearch <- function (client, index, type, body, min_score, preference = "random", routing, expand_wildcards = "open", ignore_unavailable, allow_no_indices, raw = FALSE, validate_params = TRUE) {
  # Format path
  if (missing(index) && missing(type)) {
    path = paste("_count", sep="/")
  } else if (missing(index)) {
    path = paste("_all", paste(type, collapse = ","), "_count", sep="/")
  } else if (missing(type)) {
    path = paste(paste(index, collapse = ","), "_count", sep="/")
  } else {
    path = paste(paste(index, collapse = ","),
                 paste(type, collapse = ","), "_count", sep="/")
  }

  args = as.list(match.call())
  args = removeNonURLARgs(args)
  if (validate_params) {
    validateParams(args)
  }
  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path, "query" = args)

  # Send HTTP request
  if (!missing(body)) {
    res = httr::POST(url, body = body, encode = "json")
  } else {
    res = httr::POST(url)
  }
  httr::stop_for_status(res)

  # Return the result
  formatESResult(res, raw)
}
