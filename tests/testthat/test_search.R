context("search API")

es = ElasticsearchClient("http://localhost:9200")
bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_search"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_search"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_search"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}
')

test_that("a basic search request works", {
  res = search(es, "test_r_elasticsearch")

  expect_false(is.null(res$hits))
})

test_that("a search request with a from parameter works", {
  res = search(es, "test_r_elasticsearch", from=0)

  expect_false(is.null(res$hits))
  expect_equal(grep('from=0', res$'_url'), 1)
})

test_that("a search request with a size parameter works", {
  res = search(es, "test_r_elasticsearch", size=1)

  expect_false(is.null(res$hits))
  expect_equal(grep('size=1', res$'_url'), 1)
})

test_that("a search request with a fields parameter works", {
  res = search(es, "test_r_elasticsearch", fields=c("cp", "blk"))

  expect_false(is.null(res$hits))
  expect_equal(grep('fields=cp%2Cblk', res$'_url'), 1)
})

test_that("a search request with a source parameter works", {
  res = search(es, "test_r_elasticsearch", source=FALSE)

  expect_false(is.null(res$hits))
  expect_equal(grep('_source=0', res$'_url'), 1)
})

test_that("a search request with a default_operator parameter works", {
  res = search(es, "test_r_elasticsearch", default_operator="OR")

  expect_false(is.null(res$hits))
  expect_equal(grep('default_operator=OR', res$'_url'), 1)
})

test_that("a search request with a explain parameter works", {
  res = search(es, "test_r_elasticsearch", explain=FALSE)

  expect_false(is.null(res$hits))
  expect_equal(grep('explain=0', res$'_url'), 1)
})

test_that("a search request with a lowercase_expanded_terms parameter works", {
  res = search(es, "test_r_elasticsearch", lowercase_expanded_terms=TRUE)

  expect_false(is.null(res$hits))
  expect_equal(grep('lowercase_expanded_terms=1', res$'_url'), 1)
})

test_that("a search request with a preference parameter works", {
  res = search(es, "test_r_elasticsearch", preference="random")

  expect_false(is.null(res$hits))
  expect_equal(grep('preference=random', res$'_url'), 1)
})

test_that("a search request with a analyzer parameter works", {
  res = search(es, "test_r_elasticsearch", analyzer="default")

  expect_false(is.null(res$hits))
  expect_equal(grep('analyzer=default', res$'_url'), 1)
})

test_that("a search request with a analyze_wildcard parameter works", {
  res = search(es, "test_r_elasticsearch", analyze_wildcard=FALSE)

  expect_false(is.null(res$hits))
  expect_equal(grep('analyze_wildcard=0', res$'_url'), 1)
})

test_that("a search request with a ignore_unavailable parameter works", {
  res = search(es, c("test_r_elasticsearch", "test_r_elasticsearch_nope"), ignore_unavailable=TRUE)

  expect_false(is.null(res$hits))
  expect_equal(grep('ignore_unavailable=1', res$'_url'), 1)
})

test_that("a search request with a timeout parameter works", {
  res = search(es, c("test_r_elasticsearch"), timeout="1m")

  expect_false(is.null(res$hits))
  expect_equal(grep('timeout=1m', res$'_url'), 1)
})

test_that("a search request with a source_include parameter works", {
  res = search(es, c("test_r_elasticsearch"), source_include=c("cp", "age"))

  expect_false(is.null(res$hits))
  expect_equal(grep('_source_include=cp%2Cage', res$'_url'), 1)
})

test_that("a search request with a source_exclude parameter works", {
  res = search(es, c("test_r_elasticsearch"), source_exclude=c("cp", "age"))

  expect_false(is.null(res$hits))
  expect_equal(grep('_source_exclude=cp%2Cage', res$'_url'), 1)
})

test_that("a search request with a allow_no_indices parameter works", {
  res = search(es, "test_r_elasticsearch", source_include=c("cp", "age"))

  expect_false(is.null(res$hits))
  expect_equal(grep('source_include=cp%2Cage', res$'_url'), 1)
})

test_that("a search request with a allow_no_indices parameter works", {
  indices.close(es, "test_r_elasticsearch")

  expect_error(search(es, "test_r_elasticsearc*", allow_no_indices=FALSE))
  indices.open(es, "test_r_elasticsearch")
})

test_that("a search request with a expand_wildcards parameter works", {
  indices.close(es, "test_r_elasticsearch")

  expect_error(search(es, "test_r_elasticsearc*", expand_wildcards="closed"))
  indices.open(es, "test_r_elasticsearch")
})

test_that("a search request with a search_type parameter works", {
  res = search(es, "test_r_elasticsearch", search_type="dfs_query_then_fetch")

  expect_false(is.null(res$hits))
  expect_equal(grep('search_type=dfs_query_then_fetch', res$'_url'), 1)
})

test_that("a search request with a track_scores parameter works", {
  res = search(es, "test_r_elasticsearch", track_scores=TRUE)

  expect_false(is.null(res$hits))
  expect_equal(grep('track_scores=1', res$'_url'), 1)
})

test_that("a search request with a sort parameter works", {
  res = search(es, "test_r_elasticsearch", sort=c("cp:asc", "age:desc"))

  expect_false(is.null(res$hits))
  expect_equal(grep('sort=cp%3Aasc%2Cage%3Adesc', res$'_url'), 1)
})

test_that("a search request with a query_cache parameter works", {
  res = search(es, "test_r_elasticsearch", query_cache=TRUE)

  expect_false(is.null(res$hits))
  expect_equal(grep('query_cache=1', res$'_url'), 1)
})
