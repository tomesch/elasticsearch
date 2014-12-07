context("Connection to ElasticSearch server")

test_that("the connection to the ElasticSearch server works", {
  res = Connect()
  expect_equal(res$status, 200)
})

test_that("the connection to the ElasticSearch fails when given wrong url", {
  expect_error(Connect("http://google.fr"))
  expect_error(Connect("sdsqsd"))
})

test_that("the result is return in the correct format", {
  res = Connect()
  expect_is(res, "list")

  res = Connect(raw=TRUE)
  expect_is(res, "character")
})
