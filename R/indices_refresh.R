#' @export
indices.refresh <- function (index = "_all", raw = FALSE) {
  url = getOption("res_url")
  path = paste(paste(index, collapse = ","), "_refresh", sep = "/")

  url = httr::modify_url(url, "path" = path)
  res = httr::POST(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
