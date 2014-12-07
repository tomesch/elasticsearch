context("Document deletion API")

res::connect()

test_that("the document is correctly removed", {
  res = res::index("tests", "test1",
                   document='{"testing": "hard", "list" : [1, 2, 3]}')
  expect_match(res["created"], "TRUE")

  res = res::delete("tests", "test1", res["_id"])
  expect_match(res["found"], "TRUE")
})
