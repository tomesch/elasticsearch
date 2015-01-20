context("indices.exists API")

es = ElasticsearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.create(es, "test_r_elasticsearch")
}

test_that("a basic exists request works", {
  res = indices.exists(es, "test_r_elasticsearch")

  expect_true(res$exists)
})

test_that("an exists request with ignore_unavailable paramater works", {
  res = indices.exists(es, "test_r_elasticsearch", ignore_unavailable = TRUE)

  expect_true(res$exists)
  expect_equal(grep('ignore_unavailable=1', res$'_url'), 1)
})

test_that("an exists request with ignore_unavailable paramater works", {
  res = indices.exists(es, "test_r_elasticsearch", allow_no_indices = TRUE)

  expect_true(res$exists)
  expect_equal(grep('allow_no_indices=1', res$'_url'), 1)
})

test_that("an exists request with expand_wildcards paramater works", {
  res = indices.exists(es, "test_r_elasticsearch", expand_wildcards = "open")

  expect_true(res$exists)
  expect_equal(grep('expand_wildcards=open', res$'_url'), 1)
})

test_that("an exists request with local paramater works", {
  res = indices.exists(es, "test_r_elasticsearch", local = TRUE)

  expect_true(res$exists)
  expect_equal(grep('local=1', res$'_url'), 1)
})
