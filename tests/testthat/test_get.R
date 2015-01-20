context("get API")

es = ElasticsearchClient("http://localhost:9200")

bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_get","_id":"1"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_get","_id":"2", "_routing":"tests"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
')

test_that("a basic get request works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1)
  expect_true(res$found)
})

test_that("a get request with fields parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, fields="cp")

  expect_true(res$found)
  expect_equal(res$fields$cp, "0000")
  expect_equal(grep('fields=cp', res$'_url'), 1)
})

test_that("a get request with source parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, source=FALSE)

  expect_true(res$found)
  expect_null(res$'_source')
  expect_equal(grep('source=0', res$'_url'), 1)
})

test_that("a get request with source_include parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, source_include=c("age", "blk"))

  expect_true(res$found)
  expect_null(res$'_source'$na)
  expect_false(is.null(res$'_source'$age))
  expect_false(is.null(res$'_source'$blk))
  expect_equal(grep('_source_include=age%2Cblk', res$'_url'), 1)
  expect_equal(grep('_source=', res$'_url'), integer(0))
})

test_that("a get request with source_exclude parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, source_exclude=c("age", "blk"))

  expect_true(res$found)
  expect_null(res$'_source'$age)
  expect_null(res$'_source'$blk)
  expect_false(is.null(res$'_source'$na))
  expect_false(is.null(res$'_source'$cp))
  expect_equal(grep('_source_exclude=age%2Cblk', res$'_url'), 1)
  expect_equal(grep('_source=', res$'_url'), integer(0))
})

test_that("a get request with realtime parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, realtime=TRUE)

  expect_true(res$found)
  expect_equal(grep('realtime=1', res$'_url'), 1)
})

test_that("a get request with routing parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 2, routing="tests")

  expect_true(res$found)
  expect_equal(grep('routing=tests', res$'_url'), 1)
})

test_that("a get request with preference parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, preference="random")

  expect_true(res$found)
  expect_equal(grep('preference=random', res$'_url'), 1)
})

test_that("a get request with refresh parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, refresh=TRUE)

  expect_true(res$found)
  expect_equal(grep('refresh=1', res$'_url'), 1)
})

test_that("a get request with version parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, version=1)

  expect_true(res$found)
  expect_equal(grep('version=1', res$'_url'), 1)
})

test_that("a get request with version_type parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, version_type="internal")

  expect_true(res$found)
  expect_equal(grep('version_type=internal', res$'_url'), 1)
})

test_that("a get request with exists parameter works", {
  res = get(es, "test_r_elasticsearch", "test_get", 1, exists=TRUE)

  expect_true(res)
})
