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
#' all available data matching the filters. Authentication is required via OAuth
#' password grant using your ACLED credentials.
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
#' }
get_acled_data <- function(
  email.address, # your ACLED login email
  password, # your ACLED password (OAuth)
  country = NULL, # character vector of country names, e.g. c("Kenya","Togo")
  iso = NULL, # numeric vector of ISO codes (e.g. 404 for Kenya)
  start.date = NULL, # "YYYY-MM-DD"
  end.date = NULL, # "YYYY-MM-DD"
  verbose = FALSE, # print detailed progress messages
  max_retries = 3 # maximum retry attempts
) {
  # ---- Validate basics ----------------------------------------------------
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
  # ---- Build request with pagination ---------------------------------------
  base_url <- "https://acleddata.com/api/acled/read?_format=json"
  # OAuth password grant; client_id is "acled"
  token_url <- "https://acleddata.com/oauth/token"
  # Set limit to API maximum (5000 rows per page)
  page_limit <- 5000
  params$limit <- page_limit
  # Initialize pagination variables
  all_data <- list()
  page <- 1
  total_fetched <- 0
  # ---- Pagination loop with retry logic ------------------------------------
  repeat {
    # Calculate page number for offset-based pagination
    params$page <- page

    if (verbose) {
      message(sprintf(
        "Fetching ACLED data - page %d (limit: %d)...",
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
          # Build and execute request
          req <- httr2::request(base_url) %>%
            httr2::req_oauth_password(
              client = httr2::oauth_client("acled", token_url),
              username = email.address,
              password = password
            ) %>%
            httr2::req_url_query(!!!params)

          resp <- httr2::req_perform(req)
          success <- TRUE
        },
        error = function(e) {
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

    # ---- Parse response ---------------------------------------------------
    out <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # Check for API errors
    if (!is.null(out$status) && out$status != 200) {
      error_msg <- if (!is.null(out$message)) out$message else "unknown error"
      stop(
        sprintf(
          "ACLED API error %s: %s",
          out$status,
          error_msg
        ),
        call. = FALSE
      )
    }

    # Extract data from current page
    page_data <- out$data

    # If no data returned, we've reached the end
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

    # Add page data to collection
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

    # If we got fewer rows than the limit, we've reached the end
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

    # Move to next page
    page <- page + 1
  }
  # ---- Combine all pages ---------------------------------------------------
  if (length(all_data) == 0) {
    message("No data found for the supplied filters.")
    return(invisible(NULL))
  }
  # Combine all pages into single data frame
  df <- dplyr::bind_rows(all_data)

  # Summary message (always shown)
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
