test_that("get_acled_data validates date inputs before any network call", {
  expect_error(
    get_acled_data(
      email.address = "user@example.com",
      password = "secret",
      start.date = "2024-02-01",
      end.date = NULL
    ),
    "Supply both start.date and end.date"
  )

  expect_error(
    get_acled_data(
      email.address = "user@example.com",
      password = "secret",
      start.date = "2024-02-01",
      end.date = "2024-01-01"
    ),
    "start.date cannot be after end.date"
  )
})

test_that("get_acled_data validates query parameter types before any network call", {
  expect_error(
    get_acled_data(
      email.address = "user@example.com",
      password = "secret",
      country = 404
    ),
    "`country` must be character vector of names."
  )

  expect_error(
    get_acled_data(
      email.address = "user@example.com",
      password = "secret",
      iso = "404"
    ),
    "`iso` must be numeric vector."
  )
})

test_that("get_acled_data exposes auth_method argument", {
  expect_identical(
    eval(formals(get_acled_data)$auth_method),
    c("auto", "oauth", "cookie")
  )
})
