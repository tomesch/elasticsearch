#' @export
indices.open <- function (client, index = "_all", ignore_unavailable = FALSE,
                          raw = FALSE, validate.params = TRUE) {
  UseMethod("indices.open", client)
}

#' @rdname indices.open
#' @export
indices.open.elasticsearch <- function (client, index = "_all", ignore_unavailable = FALSE,
                           raw = FALSE, validate.params = TRUE) {
  path = paste(paste(index, collapse = ","), "_open", sep = "/")

  args = list(ignore_unavailable = ignore_unavailable)

  if (validate.params) {
    validateArgs(args)
  }

  args = prepareArgs(args)

  url = httr::modify_url(client$url, "path" = path)
  res = httr::POST(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
