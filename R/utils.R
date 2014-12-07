format_res <- function (res, raw) {
  if (raw) {
    httr::content(res, as="text")
  }
  else {
    jsonlite::fromJSON(httr::content(res, as="text"))
  }
}

validate_args <- function (args) {
  res = lapply(names(args), function (arg) {
    val = args[[arg]]
    if (!is.null(val)) {
      switch(arg,
             realtime = { # WRITE TEST
              if (!is.logical(val)) warning(call. = FALSE)
             },
             replication = {
               if (!(val %in% c("async", "sync"))) warning(call. = FALSE)
             },
             consistency = {
               if (!(val %in% c("one", "quorum", "all"))) warning(call. = FALSE)
             },
             refresh = {
               if (!is.logical(val)) warning(call. = FALSE)
             },
             timeout = {
               if(!grepl("^[0-9]+[y, M, w, d, h, m, s]{0,1}$", val)) warning(call. = FALSE)
             },
             version_type = {
               if (!(val %in% c("internal", "external", "external_gt",
                                "external_gte", "force"))) warning(call. = FALSE)
             },
             op_type = {
               if (!(val %in% c("create"))) warning(call. = FALSE)
             },
             timestamp = {
               #TODO
             },
             ttl = {
               if(!grepl("^[0-9]+[y, M, w, d, h, m, s]{0,1}$", val)) warning(call. = FALSE)
             },
             fields = {
               if (!is.vector(val)) warning(call. = FALSE)
             })
    }
  })
}
