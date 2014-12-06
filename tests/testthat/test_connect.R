context("Connection to ElasticSearch server")

test_that("the connection to the ElasticSearch server works", {
  res = res::connect()
  expect_equal(res$status, 200)
})

test_that("the connection to the ElasticSearch fails when given wrong url", {
  expect_error(res::connect("http://google.fr"))
  expect_error(res::connect("sdsqsd"))
})

test_that("the result is return in the correct format", {
  res = res::connect()
  expect_is(res, "list")

  res = res::connect(raw=TRUE)
  expect_is(res, "character")
})
