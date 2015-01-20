context("info API")

es = ElasticsearchClient("http://localhost:9200")

test_that("a basic info request works", {
  res = info(es)

  expect_equal(res$status, 200)
})
