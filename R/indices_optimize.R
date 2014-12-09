#' @export
indices.optimize <- function (index = "_all", max_num_segments = NULL,
                          only_expunge_deletes = FALSE, flush = TRUE,
                          wait_for_merge = TRUE, raw = FALSE, validate.params = TRUE) {
  url = getOption("res_url")
  path = paste(paste(index, collapse = ","), "_optimize", sep = "/")

  args = list(max_num_segments = max_num_segments,
              only_expunge_deletes = only_expunge_deletes, flush = flush,
              wait_for_merge = wait_for_merge)

  if (validate.params) {
    validateArgs(args)
  }

  args = prepareArgs(args)

  url = httr::modify_url(url, "path" = path)
  res = httr::POST(url)
  httr::stop_for_status(res)

  formatESResult(res, raw)
}
