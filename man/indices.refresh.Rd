% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/indices_refresh.R
\name{indices.refresh}
\alias{indices.refresh}
\alias{indices.refresh.elasticsearch}
\title{indices.refresh}
\usage{
indices.refresh(client, ...)

\method{indices.refresh}{elasticsearch}(client, index = "_all",
  ignore_unavailable = NULL, allow_no_indices = NULL,
  expand_wildcards = "open", raw = FALSE, validate_params = TRUE)
}
\arguments{
\item{index}{String The name of the index}

\item{ignore_unavailable}{Logical Whether specified concrete indices should be ignored when unavailable (missing or closed)}

\item{allow_no_indices}{Logical Whether to ignore if a wildcard indices expression resolves into no concrete indices. (This includes _all string or when no indices have been specified)}

\item{expand_wildcards}{String Whether to expand wildcard expression to concrete indices that are open, closed or both}
}
\description{
Explicitly refresh one or more index, making all operations performed since the last refresh available for search.
}

