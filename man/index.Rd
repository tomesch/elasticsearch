% Generated by roxygen2 (4.0.2): do not edit by hand
\name{index}
\alias{index}
\title{Add or update a document in a specific index}
\usage{
index(index, type, id = NULL, document)
}
\arguments{
\item{index}{A string representing the index under wi}

\item{type}{A string representing the index under wi}

\item{id}{A string representing the index under wi}

\item{document}{A string representing the index under wi}
}
\description{
\code{index} adds or updates a document in a specific index.
}
\examples{
index("twitter", "tweet", 1, '{"test_index": "text_index"}')
index("twitter", "tweet", document='{"test_index": "text_index"}')
}
\references{
\url{http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-index_.html}
}
