#' @export
indices.exists <- function (client, index) {
  UseMethod("indices.exists", client)
}

#' @rdname indices.exists
#' @export
indices.exists.elasticsearch <- function (client, index) {
  if (missing(index)) {
    stop()
  }
  path = paste(index, collapse = ",")

  url = httr::modify_url(client$url, "path" = path)
  res = httr::HEAD(url)
  if (res['status_code'] == 404) {
    FALSE
  } else if (res['status_code'] == 200) {
    TRUE
  }
}

