context("Document indexation API")

test_that("ids are automaticly created", {
  res::connect()
  
  res = res::index("tests", "test1", document='{"testing": "hard"}')
  expect_match(res["created"], "TRUE")
  expect_true(nchar(res["_id"]) > 0)
})

test_that("version can be set", {
  res::connect()
  
  res = res::index("tests", "test1", document='{"testing": "hard"}')
  expect_match(res["created"], "TRUE")
  
  res = res::index("tests", "test1", res["_id"], document='{"testing": "hard"}', version = 1)
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "2")
  
  expect_error(res::index("tests", "test1", res["_id"], document='{"testing": "hard"}', version = 1))
})

test_that("version type can be set", {
  res::connect()
  
  res = res::index("tests", "test1", document='{"testing": "hard"}', version_type = "external", version = 5)
  expect_match(res["created"], "TRUE")
  expect_match(res["_version"], "5")
  
  res = res::index("tests", "test1", res["_id"], document='{"testing": "hard"}', version_type = "external", version = 6)
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "6")
  
  res = res::index("tests", "test1", res["_id"], document='{"testing": "hard"}', version_type = "external_gte", version = 7)
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "7")
  
  res = res::index("tests", "test1", res["_id"], document='{"testing": "hard"}', version_type = "external_gte", version = 7)
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "7")
  
  res = res::index("tests", "test1", res["_id"], document='{"testing": "hard"}', version = 1, version_type = "force")
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "1")  
})

test_that("operation type can be set", {
  res::connect()
  
  res = res::index("tests", "test1", document='{"testing": "hard"}')
  expect_match(res["created"], "TRUE")
  
  expect_error(res::index("tests", "test1", res["_id"], document='{"testing": "hard"}', op_type="create"))
})

test_that("routing can be set", {
  #TODO
})

test_that("timestamp can be set", {
  ## TODO
})

test_that("time to live can be set", {
  ## need maping
})