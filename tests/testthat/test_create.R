context("create API")

es = ElasticSearchClient("http://localhost:9200")

test_that("a basic create request works", {
  res = create(es, "test_r_elasticsearch", "test_create", body='{"test":"test"}')

  print(res)
})
