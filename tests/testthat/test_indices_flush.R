context("indices.flush API")

es = ElasticsearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.create(es, "test_r_elasticsearch")
}

test_that("a basic flush request works", {
  res = indices.flush(es, "test_r_elasticsearch")

  expect_equal(grep('/test_r_elasticsearch/', res$'_url'), 1)
})

test_that("a flush request with a force paramater works", {
  res = indices.flush(es, "test_r_elasticsearch", force = TRUE)

  expect_equal(grep('force=1', res$'_url'), 1)
})

test_that("a flush request with a full paramater works", {
  res = indices.flush(es, "test_r_elasticsearch", full = TRUE)

  expect_equal(grep('full=1', res$'_url'), 1)
})

test_that("a flush request with a wait_if_ongoing paramater works", {
  res = indices.flush(es, "test_r_elasticsearch", wait_if_ongoing = TRUE)

  expect_equal(grep('wait_if_ongoing=1', res$'_url'), 1)
})

test_that("a flush request with a ignore_unavailable paramater works", {
  res = indices.flush(es, "test_r_elasticsearch", ignore_unavailable = TRUE)

  expect_equal(grep('ignore_unavailable=1', res$'_url'), 1)
})

test_that("a flush request with a allow_no_indices paramater works", {
  res = indices.flush(es, "test_r_elasticsearch", allow_no_indices = TRUE)

  expect_equal(grep('allow_no_indices=1', res$'_url'), 1)
})

test_that("a flush request with a expand_wildcards paramater works", {
  res = indices.flush(es, "test_r_elasticsearch", expand_wildcards = "open")

  expect_equal(grep('expand_wildcards=open', res$'_url'), 1)
})
