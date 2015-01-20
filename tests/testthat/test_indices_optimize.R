context("indices.optimize API")

es = ElasticsearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.create(es, "test_r_elasticsearch")
} else {
  indices.open(es, "test_r_elasticsearch")
}

test_that("a basic index optimization request works", {
  res = indices.optimize(es, "test_r_elasticsearch")

  expect_true(!is.null(res$'_shards'))
})

test_that("an index optimization request with ignore_unavailable parameter works", {
  res = indices.optimize(es, "test_r_elasticsearch", ignore_unavailable=TRUE)

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('ignore_unavailable=1', res$'_url'), 1)
})

test_that("an index optimization request with allow_no_indices parameter works", {
  res = indices.optimize(es, "test_r_elasticsearch", allow_no_indices=TRUE)

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('allow_no_indices=1', res$'_url'), 1)
})

test_that("an index optimization request with expand_wildcards parameter works", {
  res = indices.optimize(es, "test_r_elasticsearch", expand_wildcards="open")

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('expand_wildcards=open', res$'_url'), 1)
})

test_that("an index optimization request with max_num_segments parameter works", {
  res = indices.optimize(es, "test_r_elasticsearch", max_num_segments=1)

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('max_num_segments=1', res$'_url'), 1)
})

test_that("an index optimization request with only_expunge_deletes parameter works", {
  res = indices.optimize(es, "test_r_elasticsearch", only_expunge_deletes=TRUE)

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('only_expunge_deletes=1', res$'_url'), 1)
})

test_that("an index optimization request with flush parameter works", {
  res = indices.optimize(es, "test_r_elasticsearch", flush=TRUE)

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('flush=1', res$'_url'), 1)
})

test_that("an index optimization request with wait_for_merge parameter works", {
  res = indices.optimize(es, "test_r_elasticsearch", wait_for_merge=TRUE)

  expect_true(!is.null(res$'_shards'))
  expect_equal(grep('wait_for_merge=1', res$'_url'), 1)
})
