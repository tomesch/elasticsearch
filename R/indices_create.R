#' @export
indices.create <- function (client, index, raw = FALSE) {
  UseMethod("indices.create", client)
}

#' @rdname indices.create
#' @export
indices.create.elasticsearch <- function (client, index, raw = FALSE) {
  if (missing(index)) {
    stop()
  } else {
    path = paste(index, collapse = ",")

    url = httr::modify_url(client$url, "path" = path)
    res = httr::PUT(url)
    httr::stop_for_status(res)

    formatESResult(res, raw)
  }
}
