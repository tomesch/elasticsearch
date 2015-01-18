context("ElasticSearch client creation")

test_that("the ElasticSearch client creation works", {
  es = ElasticSearchClient("http://localhost:9200")
  expect_that(class(es), equals("elasticsearch"))
})

test_that("the ElasticSearch client creation fails if given a wrong url", {
  expect_error(ElasticSearchClient("http://testest.com"))
})
