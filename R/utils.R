#' @export
formatESResult <- function (res, raw) {
  body = httr::content(res, as="text")
  if (body == "") {
    return()
  }
  if (raw) {
    body
  } else {
    jsonlite::fromJSON(body)
  }
}

#' @export
prepareArgs <- function (args) {
  args = args[!sapply(args, is.null)]
  rapply(args, function (x) {
    ifelse(is.logical(x), x * 1, x)
  }, how="replace")
}

#' @export
validateArgs <- function (args) {
  res = lapply(names(args), function (arg) {
    val = args[[arg]]
    if (!is.null(val)) {
      switch(arg,
             max_num_segments = { # WRITE TEST
               if (all.equal(val, as.integer(val)) != TRUE || val < 0) warning("lol", call. = FALSE, immediate = TRUE)
             },
             only_expunge_deletes = { # WRITE TEST
               if (!is.logical(val)) warning('"', val, '" is not a valid only_expunge_deletes value. Should be TRUE or FALSE', call. = FALSE, immediate = TRUE)
             },
             flush = { # WRITE TEST
               if (!is.logical(val)) warning('"', val, '" is not a valid flush value. Should be TRUE or FALSE', call. = FALSE, immediate = TRUE)
             },
             wait_for_merge = { # WRITE TEST
               if (!is.logical(val)) warning('"', val, '" is not a valid wait_for_merge value. Should be TRUE or FALSE', call. = FALSE, immediate = TRUE)
             },
             size = { # WRITE TEST
              if (all.equal(val, as.integer(val)) != TRUE || val < 0) warning("lol", call. = FALSE, immediate = TRUE)
             },
             from = { # WRITE TEST
               if (all.equal(val, as.integer(val)) != TRUE || val < 0) warning("lol", call. = FALSE, immediate = TRUE)
             },
             realtime = { # WRITE TEST
              if (!is.logical(val)) warning('"', val, '" is not a valid realtime value. Should be TRUE or FALSE', call. = FALSE, immediate = TRUE)
             },
             replication = {
               if (!(val %in% c("async", "sync"))) warning('"', val, '" is not a valid replication value. Should be "async" or "sync"', call. = FALSE, immediate = TRUE)
             },
             consistency = {
               if (!(val %in% c("one", "quorum", "all"))) warning('"', val, '" is not a valid consistency value. Should be one of "one", "quorum" or "all"', call. = FALSE, immediate = TRUE)
             },
             ignore_unavailable = { # WRITE TEST
               if (!is.logical(val)) warning('"', val, '" is not a valid ignore_unavailable value. Should be TRUE or FALSE', call. = FALSE, immediate = TRUE)
             },
             refresh = {
               if (!is.logical(val)) warning('"', val, '" is not a valid refresh value. Should be TRUE or FALSE', call. = FALSE, immediate = TRUE)
             },
             timeout = {
               if(!grepl("^[0-9]+[y, M, w, d, h, m, s]{0,1}$", val)) warning('"', val, '" is not a valid timeout value. Reference: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/common-options.html#time-units', call. = FALSE, immediate = TRUE)
             },
             version_type = {
               if (!(val %in% c("internal", "external", "external_gt",
                                "external_gte", "force"))) warning('"', val, '" is not a valid version type value. Should be one of "internal", "external", "external_gt", "external_gte" or "force',call. = FALSE, immediate = TRUE)
             },
             op_type = {
               if (!(val %in% c("create"))) warning('"', val, '" is not a valid operation type value. Should be "create"', call. = FALSE, immediate = TRUE)
             },
             timestamp = {
               #TODO
             },
             ttl = {
               if(!grepl("^[0-9]+[y, M, w, d, h, m, s]{0,1}$", val)) warning('"', val, '" is not a valid ttl value. Reference: http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/common-options.html#time-units', call. = FALSE, immediate = TRUE)
             },
             fields = {
               if (!is.vector(val)) warning(call. = FALSE, immediate = TRUE)
             })
    }
  })
  args
}
