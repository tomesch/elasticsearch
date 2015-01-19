context("indices.refresh API")

es = ElasticSearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.create(es, "test_r_elasticsearch")
}

test_that("a basic index refresh request works", {
  res = indices.refresh(es, "test_r_elasticsearch")

  expect_true(!is.null(res$'_shards'))
})

test_that("an index refresh request with ignore_unavailable parameter works", {
  res = indices.refresh(es, "test_r_elasticsearch", ignore_unavailable = TRUE)

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('ignore_unavailable=1', res$'_url'), 1)
})

test_that("an index refresh request with allow_no_indices parameter works", {
  res = indices.refresh(es, "test_r_elasticsearch", allow_no_indices = TRUE)

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('allow_no_indices=1', res$'_url'), 1)
})

test_that("an index refresh request with expand_wildcards parameter works", {
  res = indices.refresh(es, "test_r_elasticsearch", expand_wildcards = "open")

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('expand_wildcards=open', res$'_url'), 1)
})
