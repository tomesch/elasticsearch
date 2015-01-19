context("indices.open API")

es = ElasticSearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.create(es, "test_r_elasticsearch")
  indices.close(es, "test_r_elasticsearch")
}

test_that("a basic index opening request works", {
  res = indices.open(es, "test_r_elasticsearch")

  expect_true(res$acknowledged)
  indices.close(es, "test_r_elasticsearch")
})

test_that("an index opening request with timeout parameter works", {
  res = indices.open(es, "test_r_elasticsearch", timeout="1m")

  expect_true(res$acknowledged)
  expect_equal(grep('timeout=1m', res$'_url'), 1)
  indices.close(es, "test_r_elasticsearch")
})

test_that("an index opening request with master_timeout parameter works", {
  res = indices.open(es, "test_r_elasticsearch", master_timeout="1m")

  expect_true(res$acknowledged)
  expect_equal(grep('master_timeout=1m', res$'_url'), 1)
  indices.close(es, "test_r_elasticsearch")
})

test_that("an index opening request with allow_no_indices parameter works", {
  res = indices.open(es, "test_r_elasticsearch", allow_no_indices=TRUE)

  expect_true(res$acknowledged)
  expect_equal(grep('allow_no_indices=1', res$'_url'), 1)
  indices.close(es, "test_r_elasticsearch")
})

test_that("an index opening request with expand_wildcards parameter works", {
  res = indices.open(es, "test_r_elasticsearch", expand_wildcards="open")

  expect_true(res$acknowledged)
  expect_equal(grep('expand_wildcards=open', res$'_url'), 1)
  indices.close(es, "test_r_elasticsearch")
})

test_that("an index opening request with ignore_unavailable parameter works", {
  res = indices.open(es, "test_r_elasticsearch", ignore_unavailable=TRUE)

  expect_true(res$acknowledged)
  expect_equal(grep('ignore_unavailable=1', res$'_url'), 1)
  indices.close(es, "test_r_elasticsearch")
})
