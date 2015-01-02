#' @export
indices.refresh <- function (client, index = "_all", raw = FALSE) {
  UseMethod("indices.refresh", client)
}

#' @rdname indices.refresh
#' @export
indices.refresh.elasticsearch <- function (client, index = "_all", raw = FALSE) {
  path = paste(paste(index, collapse = ","), "_refresh", sep = "/")

  url = httr::modify_url(client$url, "path" = path)
  res = httr::POST(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
