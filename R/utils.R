format_res <- function (res, raw) {
  if (raw) {
    httr::content(res, as="text")
  }
  else {
    jsonlite::fromJSON(httr::content(res, as="text"))
  }
}