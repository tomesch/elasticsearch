context("Document deletion API")

Connect()

test_that("the document is correctly removed", {
  res = Index("tests", "test1",
                   document='{"testing": "hard", "list" : [1, 2, 3]}')
  expect_match(res["created"], "TRUE")

  res = Delete("tests", "test1", res["_id"])
  expect_match(res["found"], "TRUE")
})
