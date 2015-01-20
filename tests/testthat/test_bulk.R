context("bulk API")

es = ElasticsearchClient("http://localhost:9200")

test_that("a basic bulk request works", {
  res = bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}')

  expect_that(res$errors, equals(FALSE))
})

test_that("a bulk request with an index works", {
  res = bulk(es, body = '{"index":{"_type":"test_bulk"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_type":"test_bulk"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_type":"test_bulk"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}', index = "test_r_elasticsearch");

  expect_that(res$errors, equals(FALSE))
})

test_that("a bulk request with a type an index works", {
  res = bulk(es, body = '{"index":{}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}', index = "test_r_elasticsearch", type = "test_bulk");

  expect_that(res$errors, equals(FALSE))
})

test_that("a bulk request with consistency parameter works", {
  res = bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}', consistency = "one");

  expect_that(res$errors, equals(FALSE))
  expect_equal(grep('consistency=one', res$'_url'), 1)
})

test_that("a bulk request with replication parameter works", {
  res = bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}', replication = "sync");

  expect_that(res$errors, equals(FALSE))
  expect_equal(grep('replication=sync', res$'_url'), 1)
})

test_that("a bulk request with routing parameter works", {
  res = bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}', routing = "test");

  expect_that(res$errors, equals(FALSE))
  expect_equal(grep('routing=test', res$'_url'), 1)
})

test_that("a bulk request with refresh parameter works", {
  res = bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}', refresh = TRUE);

  expect_that(res$errors, equals(FALSE))
  expect_equal(grep('refresh=1', res$'_url'), 1)
})

test_that("a bulk request with timeout parameter works", {
  res = bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_bulk"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}', timeout = "1m");

  expect_that(res$errors, equals(FALSE))
  expect_equal(grep('timeout=1m', res$'_url'), 1)
})
