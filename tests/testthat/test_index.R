context("Document indexation API")

connect()

test_that("the document is correctly encoded", {
  res = index("tests", "test1",
                   document='{"testing": "hard", "list" : [1, 2, 3]}')
  expect_match(res["created"], "TRUE")

  res2 = index("tests", "test1",
                    document=list(testing = "hard", list = c(1, 2, 3)))
  expect_match(res2["created"], "TRUE")

  expect_equal(get("tests", "test1", res["_id"])["_source"],
               get("tests", "test1", res2["_id"])["_source"])
})

test_that("ids are automaticly created", {
  res = index("tests", "test1", document='{"testing": "hard"}')
  expect_match(res["created"], "TRUE")
  expect_true(nchar(res["_id"]) > 0)
})

test_that("version can be set", {
  res = index("tests", "test1", document='{"testing": "hard"}')
  expect_match(res["created"], "TRUE")

  res = index("tests", "test1", res["_id"], document='{"testing": "hard"}',
                   version = 1)
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "2")

  expect_error(index("tests", "test1", res["_id"],
                          document='{"testing": "hard"}', version = 1))
})

test_that("version type can be set", {
  res = index("tests", "test1", document='{"testing": "hard"}',
                   version_type = "external", version = 5)
  expect_match(res["created"], "TRUE")
  expect_match(res["_version"], "5")

  res = index("tests", "test1", res["_id"], document='{"testing": "hard"}',
                   version_type = "external", version = 6)
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "6")

  res = index("tests", "test1", res["_id"], document='{"testing": "hard"}',
                   version_type = "external_gte", version = 7)
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "7")

  res = index("tests", "test1", res["_id"], document='{"testing": "hard"}',
                   version_type = "external_gte", version = 7)
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "7")

  res = index("tests", "test1", res["_id"], document='{"testing": "hard"}',
                   version = 1, version_type = "force")
  expect_match(res["created"], "FALSE")
  expect_match(res["_version"], "1")
})

test_that("operation type can be set", {
  res = index("tests", "test1", document='{"testing": "hard"}')
  expect_match(res["created"], "TRUE")

  expect_error(index("tests", "test1", res["_id"],
                          document='{"testing": "hard"}', op_type="create"))
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
