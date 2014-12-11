#' Count items that match a query.
#'
#' \code{count} counts items that match a query.
#'
#' @param index A string representing the index.
#' @param type A string representing the type.
#' @param query A string representing the query
#' 
#' @export
count <- function (index, type, query) {
  url = getOption("res_url")

  # Format path
  path = ""
  if (missing(index) && missing(type)) {
    path = paste(path, "_count", sep="/")
  } else if (missing(index)) {
    path = paste(path, "_all", paste(type, collapse = ","), "_count", sep="/")
  } else if (missing(type)) {
    path = paste(path, paste(index, collapse = ","), "_count", sep="/")
  } else {
    path = paste(path, paste(index, collapse = ","),
                 paste(type, collapse = ","), "_count", sep="/")
  }

  url = httr::modify_url(url, "path" = path)

  # Send HTTP request
  if (!missing(query)) {
    res = httr::POST(url, body = query, encode = "json")
  } else {
    res = httr::POST(url)
  }
  httr::stop_for_status(res)

  # Return the result
  formatESResult(res, raw)
}
