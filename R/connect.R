relastic_connect <- function (host="http://127.0.0.1", port=9200) {
  base_url = paste(host, port, sep=":")
  res <- GET(base_url)
  stop_for_status(res)
  print(content(res, as="parsed"))
  
  relastic_configure(base_url)
}

relastic_configure <- function (url) {
  options(relastic_url = url)
}