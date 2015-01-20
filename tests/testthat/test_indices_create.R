context("indices.create API")

es = ElasticsearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.create(es, "test_r_elasticsearch")
}

test_that("a basic index deletion request works", {
  res = indices.delete(es, "test_r_elasticsearch")

  expect_true(res$acknowledged)
  indices.create(es, "test_r_elasticsearch")
})

test_that("an index deletion request with a timeout paramater works", {
  res = indices.delete(es, "test_r_elasticsearch", timeout="1m")

  expect_true(res$acknowledged)
  expect_equal(grep('timeout=1m', res$'_url'), 1)
  indices.create(es, "test_r_elasticsearch")
})

test_that("an index deletion request with a master_timeout paramater works", {
  res = indices.delete(es, "test_r_elasticsearch", master_timeout="1m")

  expect_true(res$acknowledged)
  expect_equal(grep('master_timeout=1m', res$'_url'), 1)
  indices.create(es, "test_r_elasticsearch")
})
