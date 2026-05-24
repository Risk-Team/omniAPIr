#' Get ACLED Conflict Data
#'
#' @description
#' Retrieves conflict event data from the Armed Conflict Location & Event Data
#' Project (ACLED) API with support for pagination and filtering by country,
#' ISO code, and date range.
#'
#' @importFrom magrittr %>%
#'
#' @param email.address Character. Your ACLED login email address.
#' @param password Character. Your ACLED password (OAuth).
#' @param country Character vector. Country names (e.g., c("Kenya", "Togo")).
#'   Default is NULL.
#' @param iso Numeric vector. ISO country codes (e.g., 404 for Kenya).
#'   Default is NULL.
#' @param start.date Character. Start date in "YYYY-MM-DD" format. Default is NULL.
#' @param end.date Character. End date in "YYYY-MM-DD" format. Default is NULL.
#' @param auth_method Character. Authentication mode: `"auto"` (default),
#'   `"oauth"`, or `"cookie"`. `"auto"` tries OAuth first and falls back to
#'   ACLED's cookie-auth flow if the endpoint rejects the OAuth token with the
#'   known permission error.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A data.frame containing ACLED event data with coordinates and metadata,
#'   or invisible(NULL) if no data found.
#'
#' @details
#' API Documentation: \url{https://apidocs.acleddata.com/}
#'
#' The function automatically handles pagination (5000 rows per page) and retrieves
#' all available data matching the filters. By default it tries OAuth password
#' grant first, then falls back to ACLED's documented cookie-auth flow when the
#' endpoint rejects an otherwise valid OAuth token with the known permission error.
#'
#' If both start.date and end.date are provided, the function filters events
#' within that date range. If only country or iso is provided, all events for
#' those locations are returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all events for Kenya
#' acled_data <- get_acled_data(
#'   email.address = "your.email@example.com",
#'   password = "your_password",
#'   country = "Kenya"
#' )
#'
#' # Get events for multiple countries with date range
#' acled_data <- get_acled_data(
#'   email.address = "your.email@example.com",
#'   password = "your_password",
#'   country = c("Kenya", "Ethiopia"),
#'   start.date = "2023-01-01",
#'   end.date = "2023-12-31"
#' )
#'
#' # Enable verbose output for debugging
#' acled_data <- get_acled_data(
#'   email.address = "your.email@example.com",
#'   password = "your_password",
#'   country = "Kenya",
#'   verbose = TRUE
#' )
#'
#' # Force cookie auth if your ACLED account cannot read via OAuth
#' acled_data <- get_acled_data(
#'   email.address = "your.email@example.com",
#'   password = "your_password",
#'   country = "Kenya",
#'   auth_method = "cookie"
#' )
#' }


get_acled_data <- function(
  email.address, # your ACLED login email
  password, # your ACLED password (OAuth)
  country = NULL, # character vector of country names, e.g. c("Kenya","Togo")
  iso = NULL, # numeric vector of ISO codes (e.g. 404 for Kenya)
  start.date = NULL, # "YYYY-MM-DD"
  end.date = NULL, # "YYYY-MM-DD"
  auth_method = c("auto", "oauth", "cookie"),
  verbose = FALSE, # print detailed progress messages
  max_retries = 3 # maximum retry attempts
) {
  # ---- Validate basics ----------------------------------------------------
  auth_method <- match.arg(auth_method)

  if (!is.null(start.date) || !is.null(end.date)) {
    if (is.null(start.date) || is.null(end.date)) {
      stop("Supply both start.date and end.date, or neither.", call. = FALSE)
    }
    if (as.Date(start.date) > as.Date(end.date)) {
      stop("start.date cannot be after end.date.", call. = FALSE)
    }
  }
  # ---- Build query params -------------------------------------------------
  params <- list()
  # Countries (prefer names) or ISO numeric codes
  if (!is.null(country)) {
    if (!is.character(country)) {
      stop("`country` must be character vector of names.", call. = FALSE)
    }
    params$country <- paste(country, collapse = "|") # multiple values in ONE filter
  }
  if (!is.null(iso)) {
    if (!is.numeric(iso)) {
      stop("`iso` must be numeric vector.", call. = FALSE)
    }
    params$iso <- paste(iso, collapse = "|")
  }
  # Date range → BETWEEN
  if (!is.null(start.date) && !is.null(end.date)) {
    params$event_date <- paste0(start.date, "|", end.date)
    params$event_date_where <- "BETWEEN"
  }
  # Fields - standard set with coordinates always included
  params$fields <- paste(
    c(
      "event_id_cnty",
      "region",
      "country",
      "year",
      "event_date",
      "source",
      "admin1",
      "admin2",
      "admin3",
      "location",
      "latitude",
      "longitude",
      "event_type",
      "sub_event_type",
      "interaction",
      "fatalities",
      "tags",
      "timestamp"
    ),
    collapse = "|"
  )

  base_url <- "https://acleddata.com/api/acled/read?_format=json"

  is_known_oauth_permission_error <- function(message_text) {
    grepl("HTTP 403 Forbidden", message_text, fixed = TRUE) ||
      grepl("restful get acled_api_endpoint", message_text, fixed = TRUE)
  }

  fetch_acled_oauth_token <- function() {
    httr2::request("https://acleddata.com/oauth/token") %>%
      httr2::req_body_form(
        username = email.address,
        password = password,
        grant_type = "password",
        client_id = "acled"
      ) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
  }

  perform_cookie_login <- function() {
    cookie_jar <- tempfile(fileext = ".txt")
    login_resp <- httr2::request("https://acleddata.com/user/login?_format=json") %>%
      httr2::req_headers(`Content-Type` = "application/json") %>%
      httr2::req_body_json(list(name = email.address, pass = password)) %>%
      httr2::req_cookie_preserve(path = cookie_jar) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()

    if (httr2::resp_status(login_resp) >= 400) {
      stop(
        sprintf(
          "ACLED cookie login failed with HTTP %s.",
          httr2::resp_status(login_resp)
        ),
        call. = FALSE
      )
    }

    cookie_jar
  }

  auth_state <- list(
    mode = auth_method,
    access_token = NULL,
    cookie_jar = NULL
  )

  if (auth_method %in% c("auto", "oauth")) {
    token_resp <- fetch_acled_oauth_token()
    auth_state$mode <- "oauth"
    auth_state$access_token <- token_resp$access_token
  } else {
    auth_state$mode <- "cookie"
    auth_state$cookie_jar <- perform_cookie_login()
  }

  page_limit <- 5000
  params$limit <- page_limit
  all_data <- list()
  page <- 1
  total_fetched <- 0

  repeat {
    params$page <- page

    if (verbose) {
      message(sprintf(
        "Fetching ACLED data via %s auth - page %d (limit: %d)...",
        auth_state$mode,
        page,
        page_limit
      ))
    }

    # Retry logic for current page
    retry_attempt <- 1
    success <- FALSE
    resp <- NULL

    while (retry_attempt <= max_retries && !success) {
      tryCatch(
        {
          req <- httr2::request(base_url) %>%
            httr2::req_url_query(!!!params)

          if (identical(auth_state$mode, "oauth")) {
            req <- req %>%
              httr2::req_auth_bearer_token(auth_state$access_token)
          } else {
            req <- req %>%
              httr2::req_cookie_preserve(path = auth_state$cookie_jar) %>%
              httr2::req_error(is_error = function(resp) FALSE)
          }

          resp <- httr2::req_perform(req)

          if (identical(auth_state$mode, "oauth") && httr2::resp_status(resp) == 403) {
            out_403 <- httr2::resp_body_json(resp, simplifyVector = TRUE)
            message_403 <- paste(
              c(
                if (!is.null(out_403$message)) out_403$message else NULL,
                if (!is.null(out_403$messages)) out_403$messages else NULL
              ),
              collapse = "; "
            )

            if (
              identical(auth_method, "auto") &&
                is_known_oauth_permission_error(message_403)
            ) {
              if (verbose) {
                message(
                  "ACLED endpoint rejected OAuth token with permission error; switching to cookie authentication."
                )
              }
              auth_state$mode <- "cookie"
              auth_state$cookie_jar <- perform_cookie_login()
              resp <- NULL  # clear 403 response; outer repeat will retry this page with cookies
              success <- TRUE  # exit inner retry while loop
            }
          }

          success <- TRUE
        },
        error = function(e) {
          if (
            identical(auth_state$mode, "oauth") &&
              identical(auth_method, "auto") &&
              is_known_oauth_permission_error(conditionMessage(e))
          ) {
            if (verbose) {
              message(
                "ACLED endpoint rejected OAuth request with HTTP 403; switching to cookie authentication."
              )
            }
            auth_state$mode <<- "cookie"
            auth_state$cookie_jar <<- perform_cookie_login()
            return(NULL)
          }

          if (retry_attempt < max_retries) {
            wait_time <- 2^retry_attempt # Exponential backoff: 2, 4, 8 seconds
            if (verbose) {
              message(sprintf(
                "Request failed (attempt %d/%d): %s. Retrying in %d seconds...",
                retry_attempt,
                max_retries,
                conditionMessage(e),
                wait_time
              ))
            }
            Sys.sleep(wait_time)
            retry_attempt <<- retry_attempt + 1
          } else {
            stop(
              sprintf(
                "Failed to fetch ACLED data after %d attempts. Last error: %s",
                max_retries,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        }
      )
    }

    if (is.null(resp)) next  # switched to cookie auth mid-page; retry with cookies

    out <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    if (httr2::resp_status(resp) >= 400 || (!is.null(out$status) && out$status != 200)) {
      error_msg <- if (!is.null(out$message) && nzchar(out$message)) {
        out$message
      } else if (!is.null(out$messages) && length(out$messages) > 0) {
        paste(out$messages, collapse = "; ")
      } else {
        "unknown error"
      }
      stop(
        sprintf(
          "ACLED API error %s: %s",
          httr2::resp_status(resp),
          error_msg
        ),
        call. = FALSE
      )
    }

    page_data <- out$data

    if (
      is.null(page_data) || length(page_data) == 0L || nrow(page_data) == 0L
    ) {
      if (page == 1) {
        message("No data found for the supplied filters.")
        return(invisible(NULL))
      }
      if (verbose) {
        message(sprintf("No more data at page %d. Stopping pagination.", page))
      }
      break
    }

    all_data[[page]] <- page_data
    rows_fetched <- nrow(page_data)
    total_fetched <- total_fetched + rows_fetched

    if (verbose) {
      message(sprintf(
        "Retrieved %d rows from page %d (total so far: %d)",
        rows_fetched,
        page,
        total_fetched
      ))
    }

    if (rows_fetched < page_limit) {
      if (verbose) {
        message(sprintf(
          "Received %d rows (less than limit %d). All data retrieved.",
          rows_fetched,
          page_limit
        ))
      }
      break
    }

    page <- page + 1
  }

  if (length(all_data) == 0) {
    message("No data found for the supplied filters.")
    return(invisible(NULL))
  }

  df <- dplyr::bind_rows(all_data)

  if (!is.null(df$event_date)) {
    rng <- range(as.character(df$event_date), na.rm = TRUE)
    message(sprintf(
      "✓ ACLED data retrieved successfully: %s rows (%d page%s) | Date range: %s to %s",
      format(nrow(df), big.mark = ","),
      length(all_data),
      ifelse(length(all_data) > 1, "s", ""),
      rng[1],
      rng[2]
    ))
  } else {
    message(sprintf(
      "✓ ACLED data retrieved successfully: %s rows (%d page%s)",
      format(nrow(df), big.mark = ","),
      length(all_data),
      ifelse(length(all_data) > 1, "s", "")
    ))
  }

  return(df)
}
