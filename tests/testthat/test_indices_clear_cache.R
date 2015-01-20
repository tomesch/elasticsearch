context("indices.clearCache API")

es = ElasticsearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.create(es, "test_r_elasticsearch")
}

index(es, "test_r_elasticsearch", "test_indices_clear_cache", body = '{"hi": "it works", "hi2": "still works"}')


test_that("a basic index cache cleaning request works", {
  res = indices.clearCache(es, "test_r_elasticsearch")

  expect_false(is.null(res$'_shards'))
})

test_that("an index cache cleaning request with an allow_no_indices paramater works", {
  res = indices.clearCache(es, "test_r_elasticsearch", allow_no_indices = TRUE)

  expect_false(is.null(res$'_shards'))
  expect_equal(grep('allow_no_indices=1', res$'_url'), 1)
})

test_that("an index cache cleaning request with an expand_wildcards paramater works", {
  res = indices.clearCache(es, "test_r_elasticsearch", expand_wildcards = "open")

  expect_false(is.null(res$'_shards'))
  expect_equal(grep('expand_wildcards=open', res$'_url'), 1)
})

test_that("an index cache cleaning request with a fields paramater works", {
  res = indices.clearCache(es, "test_r_elasticsearch", fields = c("hi", "hi2"))

  expect_false(is.null(res$'_shards'))
  expect_equal(grep('fields=hi%2Chi2', res$'_url'), 1)
})

test_that("an index cache cleaning request with a field_data paramater works", {
  res = indices.clearCache(es, "test_r_elasticsearch", field_data = TRUE)

  expect_false(is.null(res$'_shards'))
  expect_equal(grep('field_data=1', res$'_url'), 1)
})

test_that("an index cache cleaning request with an id_cache paramater works", {
  res = indices.clearCache(es, "test_r_elasticsearch", id_cache = TRUE)

  expect_false(is.null(res$'_shards'))
  expect_equal(grep('id_cache=1', res$'_url'), 1)
})

test_that("an index cache cleaning request with an ignore_unavailable paramater works", {
  res = indices.clearCache(es, "test_r_elasticsearch", ignore_unavailable = TRUE)

  expect_false(is.null(res$'_shards'))
  expect_equal(grep('ignore_unavailable=1', res$'_url'), 1)
})

test_that("an index cache cleaning request with an query_cache paramater works", {
  res = indices.clearCache(es, "test_r_elasticsearch", query_cache = TRUE)

  expect_false(is.null(res$'_shards'))
  expect_equal(grep('query_cache=1', res$'_url'), 1)
})
