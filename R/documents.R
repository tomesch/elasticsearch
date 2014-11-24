relastic_index <- function (index, type, id=NULL, document) {
  if (missing(index) || missing(type) || missing(document)) {
    stop()
  }
  else {
    base_url = getOption("relastic_url")
    req_url = paste(base_url, index, type, sep="/")
    if (is.null(id)) {      
      res = POST(req_url, body=document, content_type_json())
      stop_for_status(res)
      content(res, as="parsed")
    }
    else {
      req_url = paste(req_url, id, sep="/")
      res = PUT(req_url, body=document, content_type_json())
      stop_for_status(res)
      content(res, as="parsed")
    }
  }
}

relastic_get <- function (index, type="_all", id, fields=NULL, raw=FALSE) {
  if (exists("index") && exists("id")) {
    base_url = getOption("relastic_url")
    req_url = paste(base_url, index, type, id, sep="/")
    
    if (is.vector(fields)) {
      fields_str = paste(fields, collapse=",")
      req_url = paste(req_url, "?fields=", fields_str, sep="")
    }
    
    res <- GET(req_url)
    stop_for_status(res)
   
    if (raw) {
      content(res, as="text")
    }
    else {
      content(res, as="parsed")
    }
  }
}

relastic_delete <- function (index, type, id) {
  if (missing(index) || missing(type) || missing(id)) {
    stop()
  }
  else {
    base_url = getOption("relastic_url")
    req_url = paste(base_url, index, type, id, sep="/")
    
    res <- DELETE(req_url)
    stop_for_status(res)
    content(res, as="parsed")
  }
}

relastic_update <- function (index, type, id, script) {
  if (missing(index) || missing(type) || missing(id) || missing(script)) {
    stop()
  }
  else {
    base_url = getOption("relastic_url")
    req_url = paste(base_url, index, type, id, "_update", sep="/")
    
    res <- POST(req_url, body=script, content_type_json())
    stop_for_status(res)
    content(res, as="parsed")
  }
}