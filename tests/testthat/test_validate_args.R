context("Common API parameters validation")

test_that("a wrong ttl value produces a warning", {
  args = list(ttl = "1x")
  expect_warning(ValidateArgs(args))

  args = list(ttl = "")
  expect_warning(ValidateArgs(args))

  args = list(ttl = "ad")
  expect_warning(ValidateArgs(args))

  args = list(ttl = "d")
  expect_warning(ValidateArgs(args))

  args = list(ttl = "d1")
  expect_warning(ValidateArgs(args))

  args = list(ttl = "1dw")
  expect_warning(ValidateArgs(args))

  args = list(ttl = "1d")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(ttl = "121d")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(ttl = "9y")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(ttl = "234M")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(ttl = "12w")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(ttl = "176h")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(ttl = "98m")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(ttl = "432s")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(ttl = "944")
  expect_that(ValidateArgs(args), not(gives_warning()))
})

test_that("a wrong consistency value produces a warning", {
  args = list(consistency = "test")
  expect_warning(ValidateArgs(args))

  args = list(consistency = "")
  expect_warning(ValidateArgs(args))

  args = list(consistency = "one")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(consistency = "quorum")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(consistency = "all")
  expect_that(ValidateArgs(args), not(gives_warning()))
})

test_that("a wrong timeout value produces a warning", {
  args = list(timeout = "1x")
  expect_warning(ValidateArgs(args))

  args = list(timeout = "")
  expect_warning(ValidateArgs(args))

  args = list(timeout = "ad")
  expect_warning(ValidateArgs(args))

  args = list(timeout = "d")
  expect_warning(ValidateArgs(args))

  args = list(timeout = "d1")
  expect_warning(ValidateArgs(args))

  args = list(timeout = "1dw")
  expect_warning(ValidateArgs(args))

  args = list(timeout = "1d")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(timeout = "121d")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(timeout = "9y")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(timeout = "234M")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(timeout = "12w")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(timeout = "176h")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(timeout = "98m")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(timeout = "432s")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(timeout = "944")
  expect_that(ValidateArgs(args), not(gives_warning()))
})

test_that("a wrong refresh value produces a warning", {
  args = list(refresh = "test")
  expect_warning(ValidateArgs(args))

  args = list(refresh = "")
  expect_warning(ValidateArgs(args))

  args = list(refresh = TRUE)
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(refresh = FALSE)
  expect_that(ValidateArgs(args), not(gives_warning()))
})

test_that("a wrong replication value produces a warning", {
  args = list(replication = "test")
  expect_warning(ValidateArgs(args))

  args = list(replication = "")
  expect_warning(ValidateArgs(args))

  args = list(replication = "async")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(replication = "sync")
  expect_that(ValidateArgs(args), not(gives_warning()))
})

test_that("a wrong version type value produces a warning", {
  args = list(version_type = "test")
  expect_warning(ValidateArgs(args))

  args = list(version_type = "")
  expect_warning(ValidateArgs(args))

  args = list(version_type = "internal")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(version_type = "external")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(version_type = "external_gt")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(version_type = "external_gte")
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(version_type = "force")
  expect_that(ValidateArgs(args), not(gives_warning()))
})

test_that("a wrong from value produces a warning", {
  args = list(from = "test")
  expect_warning(ValidateArgs(args))

  args = list(from = "")
  expect_warning(ValidateArgs(args))

  args = list(from = -1)
  expect_warning(ValidateArgs(args))

  args = list(from = 1.1)
  expect_warning(ValidateArgs(args))

  args = list(from = -1.1)
  expect_warning(ValidateArgs(args))

  args = list(from = 0)
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(from = 1)
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(from = 923)
  expect_that(ValidateArgs(args), not(gives_warning()))
})

test_that("a wrong size value produces a warning", {
  args = list(size = "test")
  expect_warning(ValidateArgs(args))

  args = list(size = "")
  expect_warning(ValidateArgs(args))

  args = list(size = -1)
  expect_warning(ValidateArgs(args))

  args = list(size = 1.1)
  expect_warning(ValidateArgs(args))

  args = list(size = -1.1)
  expect_warning(ValidateArgs(args))

  args = list(size = 0)
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(size = 1)
  expect_that(ValidateArgs(args), not(gives_warning()))

  args = list(size = 923)
  expect_that(ValidateArgs(args), not(gives_warning()))
})

test_that("a wrong operation type value produces a warning", {
  args = list(op_type = "test")
  expect_warning(ValidateArgs(args))

  args = list(op_type = "")
  expect_warning(ValidateArgs(args))

  args = list(op_type = "create")
  expect_that(ValidateArgs(args), not(gives_warning()))
})
