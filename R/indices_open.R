#' @export
indices.open <- function (index = "_all", ignore_unavailable = FALSE,
                           raw = FALSE, validate.params = TRUE) {
  url = getOption("res_url")
  path = paste(paste(index, collapse = ","), "_open", sep = "/")

  args = list(ignore_unavailable = ignore_unavailable)

  if (validate.params) {
    validateArgs(args)
  }

  args = prepareArgs(args)

  url = httr::modify_url(url, "path" = path)
  res = httr::POST(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
