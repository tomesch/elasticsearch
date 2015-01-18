#' @export
formatESResult <- function (res, raw = FALSE) {
  url = res$url
  body = httr::content(res, as="text")
  if (body == "") {
    return()
  }
  if (raw) {
    body
  } else {
    res = jsonlite::fromJSON(body)
    res$'_url' = url
    res
  }
}

#' @export
prepareArgs <- function (args) {
  if (!is.null(args) && length(args) > 0) {
    args = args[!sapply(args, is.null)]
    rapply(args, function (x) {
      ifelse(is.logical(x), x * 1, x)
    }, how="replace")
  } else {
    NULL
  }
}

#' @export
removeNonURLARgs <- function (args) {
  args[1] = NULL
  args$client = NULL
  args$index = NULL
  args$type = NULL
  args$id = NULL
  args$body = NULL
  args$validate_params = NULL
  args$raw = NULL
  args$exists = NULL
  args$get_source = NULL

  args$'_source_include' = eval(args$'source_include')
  args$'_source_exclude' = eval(args$'source_exclude')
  args[['_source']] = eval(args[['source']])

  args$'source' = NULL
  args$source_exclude = NULL
  args$source_include = NULL

  if (!is.null(args$fields)) {
    args$fields = paste(args$fields, collapse = ",")
  }
  if (!is.null(args$'_source_include')) {
    args$'_source_include' = paste(args$'_source_include', collapse = ",")
  }
  if (!is.null(args$'_source_exclude')) {
    args$'_source_exclude' = paste(args$'_source_exclude', collapse = ",")
  }

  args
}

#' @export
validateParams <- function (args) {
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
