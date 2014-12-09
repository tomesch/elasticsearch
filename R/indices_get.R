#' @export
indices.get <- function (index = "_all", raw = FALSE) {
  url = getOption("res_url")
  path = paste(index, collapse = ",")

  url = httr::modify_url(url, "path" = path)
  res = httr::GET(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
