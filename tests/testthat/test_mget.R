context("mget API")

es = ElasticsearchClient("http://localhost:9200")

bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_mget1","_id":"1"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_mget1","_id":"2"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_mget1","_id":"3", "_routing": "tests"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_mget1","_id":"4", "_routing": "tests"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_mget2","_id":"1"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
     ')

test_that("a basic mget request works", {
  res = mget(es, body = '{"docs": [
             {"_index": "test_r_elasticsearch", "_type": "test_mget1", "_id": "1" },
             {"_index": "test_r_elasticsearch", "_type": "test_mget1", "_id": "2" },
             {"_index": "test_r_elasticsearch", "_type": "test_mget2", "_id": "1" }
             ]}')

  expect_equal(length(res$docs$found), 3)
})

test_that("an mget request with index parameter works", {
  res = mget(es, index = "test_r_elasticsearch", body = '{"docs": [
             {"_type": "test_mget1", "_id": "1" },
             {"_type": "test_mget1", "_id": "2" }
             ]}')

  expect_equal(length(res$docs$found), 2)
})

test_that("an mget request with index and type parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"docs": [
             {"_id": "1" },
             {"_id": "2" }
             ]}')

  expect_equal(length(res$docs$found), 2)
})

test_that("an mget request with index and type parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["1", "2"]}')

  expect_equal(length(res$docs$found), 2)
})

test_that("an mget request with fields parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["1", "2"]}', fields = c('age', 'blk'))

  expect_equal(grep('fields=age%2Cblk', res$'_url'), 1)
})

test_that("an mget request with source parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["1", "2"]}', source = FALSE)

  expect_equal(grep('source=0', res$'_url'), 1)
})

test_that("an mget request with source_include parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["1", "2"]}', source_include = c('age', 'blk'))

  expect_equal(grep('_source_include=age%2Cblk', res$'_url'), 1)
})

test_that("an mget request with source_exclude parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["1", "2"]}', source_exclude = c('age', 'blk'))

  expect_equal(grep('_source_exclude=age%2Cblk', res$'_url'), 1)
})

test_that("an mget request with realtime parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["1", "2"]}', realtime = TRUE)

  expect_equal(grep('realtime=1', res$'_url'), 1)
})

test_that("an mget request with preference parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["1", "2"]}', preference = "random")

  expect_equal(grep('preference=random', res$'_url'), 1)
})

test_that("an mget request with refresh parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["1", "2"]}', refresh = TRUE)

  expect_equal(grep('refresh=1', res$'_url'), 1)
})

test_that("an mget request with routing parameter works", {
  res = mget(es, index = "test_r_elasticsearch", type = "test_mget1", body = '{"ids" : ["3", "4"]}', routing = "tests")

  expect_equal(grep('routing=tests', res$'_url'), 1)
})
