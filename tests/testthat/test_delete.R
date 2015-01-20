context("delete API")

es = ElasticsearchClient("http://localhost:9200")

bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_delete","_id":"1"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_delete","_id":"2"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_delete","_id":"3"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_delete","_id":"4", "_routing":"tests"}}
{"cp":"0003","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_delete","_id":"5"}}
{"cp":"0004","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_delete","_id":"6"}}
{"cp":"0005","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_delete","_id":"7"}}
{"cp":"0006","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_delete","_id":"8"}}
{"cp":"0007","na":"","age":"1.1","blk":"ASCII"}
')

test_that("a basic deletion works", {
  res = delete(es, "test_r_elasticsearch", "test_delete", 1)
  expect_true(res$found)
})

test_that("a deletion with version_type parameter works", {
  res = delete(es, "test_r_elasticsearch", "test_delete", 2, version_type="internal")

  expect_true(res$found)
  expect_equal(grep('version_type=internal', res$'_url'), 1)
})

test_that("a deletion with version parameter works", {
  res = delete(es, "test_r_elasticsearch", "test_delete", 3, version=1)

  expect_true(res$found)
  expect_equal(grep('version=1', res$'_url'), 1)
})

test_that("a deletion with routing parameter works", {
  res = delete(es, "test_r_elasticsearch", "test_delete", 4, routing="tests")

  expect_true(res$found)
  expect_equal(grep('routing=tests', res$'_url'), 1)
})

test_that("a deletion with replication parameter works", {
  res = delete(es, "test_r_elasticsearch", "test_delete", 5, replication="sync")

  expect_true(res$found)
  expect_equal(grep('replication=sync', res$'_url'), 1)
})

test_that("a deletion with refresh parameter works", {
  res = delete(es, "test_r_elasticsearch", "test_delete", 6, refresh=TRUE)

  expect_true(res$found)
  expect_equal(grep('refresh=1', res$'_url'), 1)
})

test_that("a deletion with timeout parameter works", {
  res = delete(es, "test_r_elasticsearch", "test_delete", 7, timeout="2m")

  expect_true(res$found)
  expect_equal(grep('timeout=2m', res$'_url'), 1)
})

test_that("a deletion with consistency parameter works", {
  res = delete(es, "test_r_elasticsearch", "test_delete", 8, consistency="one")

  expect_true(res$found)
  expect_equal(grep('consistency=one', res$'_url'), 1)
})
