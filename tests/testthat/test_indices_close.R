context("indices.close API")

es = ElasticSearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")) {
  indices.create(es, "test_r_elasticsearch")
}

test_that("a basic index closing request works", {
  res = indices.close(es)
  expect_true(res$acknowledged)
  indices.open(es)
})

test_that("an index closing request with index parameter works", {
  res = indices.close(es, "test_r_elasticsearch")
  expect_true(res$acknowledged)
  indices.open(es, "test_r_elasticsearch")
})

test_that("an index closing request with master_timeout parameter works", {
  res = indices.close(es, "test_r_elasticsearch", master_timeout = "1m")
  expect_true(res$acknowledged)
  indices.open(es, "test_r_elasticsearch")
})

test_that("an index closing request with allow_no_indices parameter works", {
  res = indices.close(es, allow_no_indices = TRUE)
  expect_true(res$acknowledged)
  indices.open(es)
})

test_that("an index closing request with expand_wildcards parameter works", {
  res = indices.close(es, expand_wildcards = "open")
  expect_true(res$acknowledged)
  indices.open(es)
})

test_that("an index closing request with ignore_unavailable parameter works", {
  res = indices.close(es, ignore_unavailable = TRUE)
  expect_true(res$acknowledged)
  indices.open(es)
})
