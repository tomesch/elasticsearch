#' @export
indices.get <- function (client, index = "_all", raw = FALSE) {
  UseMethod("indices.get", client)
}

#' @rdname indices.get
#' @export
indices.get.elasticsearch <- function (client, index = "_all", raw = FALSE) {
  path = paste(index, collapse = ",")

  url = httr::modify_url(client$url, "path" = path)
  res = httr::GET(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
