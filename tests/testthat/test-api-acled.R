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

test_that("ACLED follows cursors even when an intermediate response is short", {
  urls <- character()
  local_mocked_bindings(req_perform = function(req, ...) {
    if (grepl('/oauth/token', req$url, fixed = TRUE)) {
      body <- '{"access_token":"test-token"}'
    } else {
      urls <<- c(urls, req$url)
      body <- if (length(urls) == 1L) {
        '{"status":200,"data":[{"event_id_cnty":"CMR1","year":2024}],"next_cursor":"12345"}'
      } else {
        '{"status":200,"data":[{"event_id_cnty":"CMR2","year":2025}],"next_cursor":null}'
      }
    }
    httr2::response(status_code = 200, headers = list(`content-type` = "application/json"),
                    body = charToRaw(body))
  }, .package = "httr2")
  result <- get_acled_data("user@example.com", "secret", iso = 120)
  expect_equal(result$event_id_cnty, c("CMR1", "CMR2"))
  expect_length(urls, 2)
  expect_false(any(grepl("[?&]page=", urls)))
  expect_false(grepl("[?&]cursor=", urls[1]))
  expect_match(urls[2], "cursor=12345")
})

test_that("ACLED stops at a null cursor even for a full response", {
  requests <- 0L
  local_mocked_bindings(req_perform = function(req, ...) {
    if (grepl('/oauth/token', req$url, fixed = TRUE)) {
      body <- '{"access_token":"test-token"}'
    } else {
      requests <<- requests + 1L
      expect_equal(requests, 1L)
      body <- jsonlite::toJSON(list(status = 200,
        data = data.frame(event_id_cnty = paste0("CMR", seq_len(5000))),
        next_cursor = NULL), auto_unbox = TRUE, null = "null")
    }
    httr2::response(status_code = 200, headers = list(`content-type` = "application/json"),
                    body = charToRaw(body))
  }, .package = "httr2")
  result <- get_acled_data("user@example.com", "secret", iso = 120)
  expect_equal(nrow(result), 5000)
  expect_equal(requests, 1)
})
