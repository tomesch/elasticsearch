context("index API")

es = ElasticsearchClient("http://localhost:9200")

test_that("a basic indexation without id works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}')

  expect_that(res$created, equals(TRUE))
})

test_that("a basic indexation with id works", {
  res = index(es, "test_r_elasticsearch", "test_index", 1, body = '{"hi":"it works"}')

  expect_that(res$created, equals(TRUE))
})

test_that("an indexation with consistency parameter works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}', consistency = "one")

  expect_that(res$created, equals(TRUE))
  expect_equal(grep('consistency=one', res$'_url'), 1)
})

test_that("an indexation with replication parameter works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}', replication = "sync")

  expect_that(res$created, equals(TRUE))
  expect_equal(grep('replication=sync', res$'_url'), 1)
})

test_that("an indexation with version parameter works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}', version = 0)

  expect_that(res$created, equals(TRUE))
  expect_equal(grep('version=0', res$'_url'), 1)
})

test_that("an indexation with version_type parameter works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}', version_type = "internal")

  expect_that(res$created, equals(TRUE))
  expect_equal(grep('version_type=internal', res$'_url'), 1)
})

test_that("an indexation with routing parameter works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}', routing = "test")

  expect_that(res$created, equals(TRUE))
  expect_equal(grep('routing=test', res$'_url'), 1)
})

test_that("an indexation with timeout parameter works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}', timeout = "1m")

  expect_that(res$created, equals(TRUE))
  expect_equal(grep('timeout=1m', res$'_url'), 1)
})

test_that("an indexation with refresh parameter works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}', refresh = TRUE)

  expect_that(res$created, equals(TRUE))
  expect_equal(grep('refresh=1', res$'_url'), 1)
})

test_that("an indexation with ttl parameter works", {
  res = index(es, "test_r_elasticsearch", "test_index", body = '{"hi":"it works"}', ttl = "1m")

  expect_that(res$created, equals(TRUE))
  expect_equal(grep('ttl=1m', res$'_url'), 1)
})

# Parent
