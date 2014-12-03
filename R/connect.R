#' @export
connect <- function (protocol = "http", hostname = "127.0.0.1", path = NULL, port = 9200, raw = FALSE) {
  url = structure(list("scheme" = protocol, "hostname" = hostname, "port" = port, "path" = path), class="url")
  
  res <- httr::GET(url)
  httr::stop_for_status(res)
  configure(url)
  
  format_res(res, raw)
}

configure <- function (url) {
  options(res_url = url)
}