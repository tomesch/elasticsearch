context("count API")

es = ElasticsearchClient("http://localhost:9200")
res = bulk(es, body = '{"index":{"_index":"test_r_elasticsearch","_type":"test_count"}}
{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_count"}}
{"cp":"0001","na":"","age":"1.1","blk":"ASCII"}
{"index":{"_index":"test_r_elasticsearch","_type":"test_count", "_routing":"tests"}}
{"cp":"0002","na":"","age":"1.1","blk":"ASCII"}');

test_that("a basic count request works", {
  res = count(es, body='{"query": {"match" : {"blk": "ASCII"}}}')

  expect_equal(res$'_shards'$failed, 0)
})

test_that("a count request with index works", {
  res = count(es, index="test_r_elasticsearch")

  expect_equal(res$'_shards'$failed, 0)
})

test_that("a count request with index and type parameters works", {
  res = count(es, index="test_r_elasticsearch", type="test_count")

  expect_equal(res$'_shards'$failed, 0)
})

test_that("a count request with min_score parameter works", {
  res = count(es, body='{"query": {"match" : {"blk": "ASCII"}}}', min_score=1)

  expect_equal(res$'_shards'$failed, 0)
  expect_equal(grep('min_score=1', res$'_url'), 1)
})

test_that("a count request with preference parameter works", {
  res = count(es, body='{"query": {"match" : {"blk": "ASCII"}}}', preference="random")

  expect_equal(res$'_shards'$failed, 0)
  expect_equal(grep('preference=random', res$'_url'), 1)
})

test_that("a count request with routing parameter works", {
  res = count(es, body='{"query": {"match" : {"blk": "ASCII"}}}', routing="tests")

  expect_equal(res$'_shards'$failed, 0)
  expect_equal(grep('routing=tests', res$'_url'), 1)
})

test_that("a count request with expand_wildcards parameter works", {
  res = count(es, body='{"query": {"match" : {"blk": "ASCII"}}}', expand_wildcards="open")

  expect_equal(res$'_shards'$failed, 0)
  expect_equal(grep('expand_wildcards=open', res$'_url'), 1)
})

test_that("a count request with ignore_unavailable parameter works", {
  res = count(es, body='{"query": {"match" : {"blk": "ASCII"}}}', ignore_unavailable=TRUE)

  expect_equal(res$'_shards'$failed, 0)
  expect_equal(grep('ignore_unavailable=1', res$'_url'), 1)
})

test_that("a count request with allow_no_indices parameter works", {
  res = count(es, body='{"query": {"match" : {"blk": "ASCII"}}}', allow_no_indices=TRUE)

  expect_equal(res$'_shards'$failed, 0)
  expect_equal(grep('allow_no_indices=1', res$'_url'), 1)
})
