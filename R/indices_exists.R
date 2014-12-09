#' @export
indices.exists <- function (index) {
  if (missing(index)) {
    stop()
  }
  url = getOption("res_url")
  path = paste(index, collapse = ",")

  url = httr::modify_url(url, "path" = path)
  res = httr::HEAD(url)
  if (res['status_code'] == 404) {
    FALSE
  } else if (res['status_code'] == 200) {
    TRUE
  }
}

