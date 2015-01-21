context("indices.analyze API")

es = ElasticsearchClient("http://localhost:9200")
if (!indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.create(es, "test_r_elasticsearch")
}

test_that("a basic analyze request works", {
  res = indices.analyze(es, "analyze")

  expect_equal(res$tokens[[1]], "analyze")
})

test_that("an analyze request with an index paramater works", {
  res = indices.analyze(es, text = "analyze", index = "test_r_elasticsearch")

  expect_equal(res$tokens[[1]], "analyze")
  expect_equal(grep('/test_r_elasticsearch/', res$'_url'), 1)
})

test_that("an analyze request with an analyzer paramater works", {
  res = indices.analyze(es, text = "analyze", analyzer = "standard")

  expect_equal(res$tokens[[1]], "analyze")
  expect_equal(grep('analyzer=standard', res$'_url'), 1)
})

test_that("an analyze request with an char_filters paramater works", {
  res = indices.analyze(es, text = "analyze", char_filters = "html_strip")

  expect_equal(res$tokens[[1]], "analyze")
  expect_equal(grep('char_filters=html_strip', res$'_url'), 1)
})

# Need to create a mapping first
# test_that("an analyze request with an field paramater works", {
#   res = indices.analyze(es, text = "analyze", field = "field")
#
#   expect_equals(res$tokens[[1]], "analyze")
#   expect_equal(grep('field=field', res$'_url'), 1)
# })

test_that("an analyze request with a filters paramater works", {
  res = indices.analyze(es, text = "analyze", filters = "lowercase")

  expect_equal(res$tokens[[1]], "analyze")
  expect_equal(grep('filters=lowercase', res$'_url'), 1)
})

test_that("an analyze request with a format paramater works", {
  res = indices.analyze(es, text = "analyze", format = "text")

  expect_equal(res$tokens[[1]], "analyze")
  expect_equal(grep('format=text', res$'_url'), 1)
})

test_that("an analyze request with a tokenizer paramater works", {
  res = indices.analyze(es, text = "analyze", tokenizer = "keyword")

  expect_equal(res$tokens[[1]], "analyze")
  expect_equal(grep('tokenizer=keyword', res$'_url'), 1)
})
