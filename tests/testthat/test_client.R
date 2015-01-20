context("Elasticsearch client creation")

test_that("the Elasticsearch client creation works", {
  es = ElasticsearchClient("http://localhost:9200")
  expect_that(class(es), equals("elasticsearch"))
})

test_that("the Elasticsearch client creation fails if given a wrong url", {
  expect_error(ElasticsearchClient("http://testest.com"))
})
