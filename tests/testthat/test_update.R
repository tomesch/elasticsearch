context("update API")

es = ElasticSearchClient("http://localhost:9200/")
index(es, "test_r_elasticsearch", "test_update", 1, body = '{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}');
index(es, "test_r_elasticsearch", "test_update", 2, body = '{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}', routing="tests");
index(es, "test_r_elasticsearch", "test_update", 3, body = '{"cp":"0000","na":"","age":"1.1","blk":"ASCII"}');

test_that("a basic update request works", {
  res = update(es, "test_r_elasticsearch", "test_update", 1, body='{"doc": {"age" : 1.2}}')

  expect_false(is.null(res))
})

test_that("an update request with a routing parameter works", {
  res = update(es, "test_r_elasticsearch", "test_update", 2, body='{"doc": {"age" : 1.2}}', routing="tests")

  expect_false(is.null(res))
  expect_equal(grep('routing=tests', res$'_url'), 1)
})

test_that("an update request with a timeout parameter works", {
  res = update(es, "test_r_elasticsearch", "test_update", 1, body='{"doc": {"age" : 1.2}}', timeout="1m")

  expect_false(is.null(res))
  expect_equal(grep('timeout=1m', res$'_url'), 1)
})

test_that("an update request with a refresh parameter works", {
  res = update(es, "test_r_elasticsearch", "test_update", 1, body='{"doc": {"age" : 1.2}}', refresh=TRUE)

  expect_false(is.null(res))
  expect_equal(grep('refresh=1', res$'_url'), 1)
})

test_that("an update request with a fields parameter works", {
  res = update(es, "test_r_elasticsearch", "test_update", 1, body='{"doc": {"age" : 1.2}}', fields=c('age', 'blk'))

  expect_false(is.null(res))
  expect_equal(grep('fields=age%2Cblk', res$'_url'), 1)
})

test_that("an update request with a version parameter works", {
  res = update(es, "test_r_elasticsearch", "test_update", 3, body='{"doc": {"age" : 1.2}}', version=1)

  expect_false(is.null(res))
  expect_equal(grep('version=1', res$'_url'), 1)
})

test_that("an update request with a version_type parameter works", {
  res = update(es, "test_r_elasticsearch", "test_update", 1, body='{"doc": {"age" : 1.2}}', version_type="internal")

  expect_false(is.null(res))
  expect_equal(grep('version_type=internal', res$'_url'), 1)
})
