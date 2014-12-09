#' @export
indices.create <- function (index, raw = FALSE) {
  if (missing(index)) {
    stop()
  } else {
    url = getOption("res_url")
    path = paste(index, collapse = ",")

    url = httr::modify_url(url, "path" = path)
    res = httr::PUT(url)
    httr::stop_for_status(res)

    formatESResult(res, raw)
  }
}
