#' List Available Indicators for UN Data Sources
#'
#' @description
#' Retrieves the list of available indicators and their descriptions from
#' various UN data sources (World Bank, UN SDG, UNDP, ILO, WHO).
#'
#' @importFrom magrittr %>%
#'
#' @param source Character. Data source name. Options: "WorldBank", "UNSDG",
#'   "UNDP", "ILO", "WHO". Case-insensitive.
#' @param search Character. Optional search term to filter indicators by name
#'   or description. Default is NULL (returns all indicators).
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#' @param conda_env Character. Conda environment name for World Bank data source.
#'   Required when source = "WorldBank". Default is NULL.
#' @param database Integer. World Bank database ID. Default is NULL (all databases).
#'   Use 88 for Food & Price Database, 2 for WDI, etc. Only applies to WorldBank source.
#'
#' @return A data.frame with columns varying by source, but typically including:
#'   \itemize{
#'     \item code: Indicator code
#'     \item name/description: Indicator name or description
#'     \item database: Database name (for World Bank)
#'     \item database_id: Database ID (for World Bank)
#'     \item Additional metadata fields specific to each source
#'   }
#'
#' @details
#' This helper function makes it easier to discover available indicators without
#' needing to consult external documentation.
#'
#' **World Bank:** Requires Python with wbgapi package installed in a conda environment.
#'   By default, fetches indicators from ALL available World Bank databases (WDI, Doing Business,
#'   Debt Statistics, Food & Price, etc.). Use the `database` parameter to query a specific
#'   database (e.g., 88 for Food & Price Database, 2 for WDI).
#'
#' **UNDP:** Requires API key (pass via `apikey` parameter or set UNDP_API_KEY
#' environment variable).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List all WHO indicators
#' who_indicators <- list_un_indicators("WHO")
#'
#' # Search for unemployment indicators in ILO
#' ilo_unemployment <- list_un_indicators("ILO", search = "unemployment")
#'
#' # List World Bank indicators from all databases (requires conda environment)
#' wb_indicators <- list_un_indicators("WorldBank", conda_env = "your_env_name")
#'
#' # Search for GDP indicators across all databases
#' gdp_indicators <- list_un_indicators("WorldBank", search = "GDP", conda_env = "your_env_name")
#'
#' # List indicators from World Bank Food & Price Database (database 88)
#' food_price_indicators <- list_un_indicators(
#'   "WorldBank",
#'   conda_env = "your_env_name",
#'   database = 88
#' )
#'
#' # Search for cost indicators in Food & Price Database
#' cost_indicators <- list_un_indicators(
#'   "WorldBank",
#'   search = "cost",
#'   conda_env = "your_env_name",
#'   database = 88
#' )
#' }
list_un_indicators <- function(
  source,
  search = NULL,
  verbose = FALSE,
  max_retries = 3,
  conda_env = NULL,
  database = NULL
) {
  # Normalize source name
  source <- tolower(trimws(source))

  if (verbose) {
    message(sprintf("Fetching indicator list for: %s", toupper(source)))
  }

  # Validate conda_env for World Bank
  if (source == "worldbank" && is.null(conda_env)) {
    stop(
      "conda_env parameter is required for World Bank data source. ",
      "Specify the conda environment name containing wbgapi package.",
      call. = FALSE
    )
  }

  # Route to appropriate metadata fetcher based on source
  result <- switch(
    source,
    "worldbank" = .fetch_wb_indicators(search, verbose, max_retries, conda_env, database),
    "unsdg" = .fetch_unsdg_indicators(search, verbose, max_retries),
    "undp" = .fetch_undp_indicators(search, verbose, max_retries),
    "ilo" = .fetch_ilo_indicators(search, verbose, max_retries),
    "who" = .fetch_who_indicators(search, verbose, max_retries),
    stop(
      sprintf(
        "Unknown source: '%s'. Valid options: 'WorldBank', 'UNSDG', 'UNDP', 'ILO', 'WHO'",
        source
      ),
      call. = FALSE
    )
  )

  if (verbose) {
    message(sprintf(
      "✓ Retrieved %s indicators for %s",
      format(nrow(result), big.mark = ","),
      toupper(source)
    ))
  }

  return(result)
}

# Internal helper functions for fetching indicators from each source

.fetch_wb_indicators <- function(search, verbose, max_retries, conda_env, database = NULL) {
  if (!is.null(conda_env)) {
    reticulate::use_condaenv(conda_env, required = TRUE)
  }
  wb <- reticulate::import("wbgapi")

  empty_tbl <- tibble::tibble(
    code = character(),
    name = character(),
    database_id = character()
  )

  one_db <- function(db_id) {
    if (isTRUE(verbose)) {
      message(sprintf("DB %d: fetching indicators…", db_id))
    }
    # set current database
    suppressWarnings(wb$db <- as.integer(db_id))

    # pull featureset -> compact 2-col table
    fs <- wb$series$info()
    tbl <- reticulate::py_to_r(fs$table())

    if (length(tbl) == 0) {
      return(empty_tbl)
    }

    code <- vapply(tbl, function(x) as.character(x[[1]]), character(1))
    name <- vapply(
      tbl,
      function(x) if (length(x) >= 2) as.character(x[[2]]) else NA_character_,
      character(1)
    )

    tibble::tibble(
      code = code,
      name = name,
      database_id = as.character(db_id)
    ) %>%
      dplyr::filter(!is.na(code), !is.na(name), nzchar(code), nzchar(name)) %>%
      dplyr::distinct()
  }

  # Determine which databases to query
  db_range <- if (!is.null(database)) {
    # Query only the specified database
    as.integer(database)
  } else {
    # Query all databases (1:63)
    1:63
  }

  # run across db_range, swallow errors per-db and return empty tibble when they occur
  out <- purrr::map_dfr(
    db_range,
    purrr::possibly(one_db, otherwise = empty_tbl)
  ) %>%
    dplyr::distinct()

  # Optional search (case-insensitive)
  if (!is.null(search) && nzchar(search)) {
    pat <- paste0("(?i)", search)
    out <- out %>%
      dplyr::filter(
        grepl(pat, .data$code) |
          grepl(pat, .data$name) |
          grepl(pat, .data$database_id)
      )
  }

  out
}

.fetch_unsdg_indicators <- function(search, verbose, max_retries) {
  base_url <- "https://unstats.un.org/sdgapi/v1/sdg/Series/List"

  retry_attempt <- 1
  success <- FALSE
  resp <- NULL

  while (retry_attempt <= max_retries && !success) {
    tryCatch(
      {
        resp <- httr2::request(base_url) |>
          httr2::req_perform()
        httr2::resp_check_status(resp)
        success <- TRUE
      },
      error = function(e) {
        if (retry_attempt < max_retries) {
          wait_time <- 2^retry_attempt
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
              "Failed to fetch UN SDG indicators after %d attempts: %s",
              max_retries,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      }
    )
  }

  content <- httr2::resp_body_string(resp)
  json <- jsonlite::fromJSON(content, flatten = TRUE)

  result <- tibble::as_tibble(json) %>%
    dplyr::select(code, description) %>%
    dplyr::rename(name = description)

  # Apply search filter if provided
  if (!is.null(search) && nchar(search) > 0) {
    search_pattern <- paste0("(?i)", search)
    result <- result %>%
      dplyr::filter(
        grepl(search_pattern, code) | grepl(search_pattern, name)
      )
  }

  return(result)
}

.fetch_undp_indicators <- function(search, verbose, max_retries) {
  # UNDP requires API key - check environment variable
  apikey <- Sys.getenv("UNDP_API_KEY")
  if (apikey == "") {
    stop(
      "UNDP API key required. Set UNDP_API_KEY environment variable or pass apikey parameter",
      call. = FALSE
    )
  }

  base_url <- "https://hdrdata.org/api/CompositeIndices/query-detailed"

  retry_attempt <- 1
  success <- FALSE
  resp <- NULL

  while (retry_attempt <= max_retries && !success) {
    tryCatch(
      {
        resp <- httr2::request(base_url) |>
          httr2::req_url_query(apikey = apikey) |>
          httr2::req_perform()
        httr2::resp_check_status(resp)
        success <- TRUE
      },
      error = function(e) {
        if (retry_attempt < max_retries) {
          wait_time <- 2^retry_attempt
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
              "Failed to fetch UNDP indicators after %d attempts: %s",
              max_retries,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      }
    )
  }

  content <- httr2::resp_body_string(resp)
  json <- jsonlite::fromJSON(content, flatten = TRUE)

  # Extract unique indicators
  result <- json %>%
    dplyr::distinct(indicator_id, indicator_name) %>%
    dplyr::rename(code = indicator_id, name = indicator_name) %>%
    tibble::as_tibble()

  # Apply search filter if provided
  if (!is.null(search) && nchar(search) > 0) {
    search_pattern <- paste0("(?i)", search)
    result <- result %>%
      dplyr::filter(
        grepl(search_pattern, code) | grepl(search_pattern, name)
      )
  }

  return(result)
}

.fetch_ilo_indicators <- function(search, verbose, max_retries) {
  base_url <- "https://rplumber.ilo.org/metadata/toc/indicator/"

  retry_attempt <- 1
  success <- FALSE
  response <- NULL

  while (retry_attempt <= max_retries && !success) {
    tryCatch(
      {
        response <- httr2::request(base_url) |>
          httr2::req_url_query(lang = "en", format = ".csv") |>
          httr2::req_user_agent("fetch_ilo_indicators/1.0") |>
          httr2::req_perform()
        httr2::resp_check_status(response)
        success <- TRUE
      },
      error = function(e) {
        if (retry_attempt < max_retries) {
          wait_time <- 2^retry_attempt
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
              "Failed to fetch ILO indicators after %d attempts: %s",
              max_retries,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      }
    )
  }

  # Parse CSV from octet-stream (or gzipped) as raw bytes
  raw_content <- httr2::resp_body_raw(response)

  # Quick guard: bail if we actually got HTML
  if (
    grepl(
      "^\\s*<!DOCTYPE html>|^\\s*<html",
      rawToChar(raw_content[seq_len(min(200L, length(raw_content)))]),
      ignore.case = TRUE
    )
  ) {
    stop(
      "Server returned HTML instead of CSV. Check the URL/params or try again.",
      call. = FALSE
    )
  }

  df <- suppressMessages(readr::read_csv(raw_content, show_col_types = FALSE))

  # Map columns to desired output (robust to minor header variants)
  code_col <- if ("id" %in% names(df)) {
    "id"
  } else if ("indicator_code" %in% names(df)) {
    "indicator_code"
  } else {
    stop(
      "Could not find indicator code column (expected 'id' or 'indicator_code').",
      call. = FALSE
    )
  }

  name_col <- if ("indicator.label" %in% names(df)) {
    "indicator.label"
  } else if ("indicator_label" %in% names(df)) {
    "indicator_label"
  } else {
    stop(
      "Could not find indicator label column (expected 'indicator.label' or 'indicator_label').",
      call. = FALSE
    )
  }

  result <- tibble::tibble(
    code = df[[code_col]],
    name = df[[name_col]]
  ) %>%
    dplyr::distinct()

  # Apply search filter if provided
  if (!is.null(search) && nchar(search) > 0) {
    search_pattern <- paste0("(?i)", search)
    result <- result %>%
      dplyr::filter(grepl(search_pattern, code) | grepl(search_pattern, name))
  }

  return(result)
}

.fetch_who_indicators <- function(search, verbose, max_retries) {
  base_url <- "https://ghoapi.azureedge.net/api/Indicator"

  retry_attempt <- 1
  success <- FALSE
  response <- NULL

  while (retry_attempt <= max_retries && !success) {
    tryCatch(
      {
        response <- httr2::request(base_url) |>
          httr2::req_perform()
        httr2::resp_check_status(response)
        success <- TRUE
      },
      error = function(e) {
        if (retry_attempt < max_retries) {
          wait_time <- 2^retry_attempt
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
              "Failed to fetch WHO indicators after %d attempts: %s",
              max_retries,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      }
    )
  }

  json <- httr2::resp_body_json(response)

  # Extract indicator codes and names
  result <- tibble::tibble(
    code = purrr::map_chr(json$value, ~ .x$IndicatorCode),
    name = purrr::map_chr(json$value, ~ .x$IndicatorName)
  )

  # Apply search filter if provided
  if (!is.null(search) && nchar(search) > 0) {
    search_pattern <- paste0("(?i)", search)
    result <- result %>%
      dplyr::filter(
        grepl(search_pattern, code) | grepl(search_pattern, name)
      )
  }

  return(result)
}

#' Get World Bank Development Indicators Data
#'
#' @description
#' Retrieves development indicators from the World Bank API using the wbgapi
#' Python package via reticulate.
#'
#' @importFrom magrittr %>%
#'
#' @param indicators Character vector. World Bank indicator codes (required).
#' @param iso3 Character. ISO3 country code. Default is NULL (all countries).
#' @param mrv Integer. Most Recent Values - number of years to retrieve. Default is 23.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param conda_env Character. Conda environment name containing wbgapi package (required).
#' @param exclude_aggregates Logical. If TRUE (default), filters out regional and income group aggregates,
#'   returning only data for individual countries with valid ISO3 codes.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests. Default is 3.
#' @param database Integer. World Bank database ID. Default is 2 (WDI). Use 88 for Food & Price Database.
#'
#' @return A data.frame with columns: isocode, Year, value, indicator.
#'   By default, only includes individual countries (aggregates excluded).
#'
#' @details
#' API Documentation: \url{https://github.com/tgherzog/wbgapi}
#'
#' **Python Required:** This function requires Python and the wbgapi package
#' installed in a conda environment. Specify the conda environment name using
#' the `conda_env` parameter.
#'
#' **Aggregate Filtering:** By default, the function excludes regional and income group
#' aggregates (e.g., "World", "Sub-Saharan Africa", "High income") to return only
#' individual country data. Set `exclude_aggregates = FALSE` to include all entities.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get GDP data for a specific country (excludes aggregates by default)
#' wb_data <- get_wb_data(
#'   indicators = "NY.GDP.MKTP.CD",
#'   iso3 = "KEN",
#'   mrv = 10,
#'   conda_env = "your_env_name"
#' )
#'
#' # Get multiple indicators for all countries
#' wb_data <- get_wb_data(
#'   indicators = c("NY.GDP.MKTP.CD", "SP.POP.TOTL"),
#'   mrv = 5,
#'   conda_env = "your_env_name"
#' )
#'
#' # Include regional aggregates in the results
#' wb_data_with_aggregates <- get_wb_data(
#'   indicators = "NY.GDP.MKTP.CD",
#'   mrv = 10,
#'   exclude_aggregates = FALSE,
#'   conda_env = "your_env_name"
#' )
#'
#' # Get Food & Price Database indicators (database 88)
#' food_price_data <- get_wb_data(
#'   indicators = c("CoCA_PPP", "CoHD_fexp"),
#'   mrv = 10,
#'   conda_env = "your_env_name",
#'   database = 88
#' )
#' }
get_wb_data <- function(
  indicators,
  iso3 = NULL,
  mrv = 23,
  verbose = FALSE,
  conda_env,
  exclude_aggregates = TRUE,
  max_retries = 3,
  database = 2
) {
  # Validate conda_env parameter
  if (missing(conda_env)) {
    stop(
      "conda_env parameter is required. Specify the conda environment name containing wbgapi package.",
      call. = FALSE
    )
  }

  reticulate::use_condaenv(conda_env)
  wb <- reticulate::import("wbgapi")
  
  # Set database (default is 2 = WDI, but can be changed to 88 for Food & Price, etc.)
  wb$db <- as.integer(database)

  if (verbose) {
    message(sprintf("Fetching World Bank data from database %d for last %d years", database, mrv))
  }

  # Process each indicator separately to maintain clear structure
  indicator_data <- purrr::map_dfr(indicators, function(indicator) {
    # Retry logic for each indicator
    retry_attempt <- 1
    success <- FALSE
    raw_data <- NULL

    while (retry_attempt <= max_retries && !success) {
      tryCatch(
        {
          if (!is.null(iso3)) {
            raw_data <- wb$data$DataFrame(
              indicator,
              iso3,
              mrv = mrv,
              labels = TRUE
            ) %>%
              tibble::rownames_to_column(var = "isocode")
          } else {
            raw_data <- wb$data$DataFrame(indicator, mrv = mrv, labels = TRUE) %>%
              tibble::rownames_to_column(var = "isocode")
          }
          success <- TRUE
        },
        error = function(e) {
          if (retry_attempt < max_retries) {
            wait_time <- 2^retry_attempt
            if (verbose) {
              message(sprintf(
                "Request failed for indicator %s (attempt %d/%d): %s. Retrying in %d seconds...",
                indicator,
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
                "Failed to fetch World Bank data for indicator %s after %d attempts: %s",
                indicator,
                max_retries,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        }
      )
    }

    # Only pivot columns that start with "YR" (year columns)
    year_cols <- grep("^YR", names(raw_data), value = TRUE)

    if (length(year_cols) > 0) {
      raw_data %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(year_cols),
          names_to = "Year",
          values_to = "value"
        ) %>%
        dplyr::mutate(
          Year = as.numeric(stringr::str_remove(Year, "YR")),
          indicator = indicator # Keep track of which indicator this is
        )
    } else {
      # Return empty structure if no year columns
      tibble::tibble(
        isocode = character(0),
        Year = numeric(0),
        value = numeric(0),
        indicator = character(0)
      )
    }
  })

  # Filter out aggregates by validating ISO codes (if requested)
  if (exclude_aggregates) {
    initial_rows <- nrow(indicator_data)

    indicator_data <- indicator_data %>%
      dplyr::mutate(
        is_valid_country = !is.na(countrycode::countrycode(
          isocode,
          origin = "iso3c",
          destination = "country.name",
          warn = FALSE
        ))
      ) %>%
      dplyr::filter(is_valid_country) %>%
      dplyr::select(-is_valid_country)

    filtered_rows <- initial_rows - nrow(indicator_data)

    if (verbose && filtered_rows > 0) {
      message(sprintf(
        "  Excluded %s aggregate rows",
        format(filtered_rows, big.mark = ",")
      ))
    }
  }

  message(sprintf(
    "✓ World Bank data retrieved successfully: %s rows",
    format(nrow(indicator_data), big.mark = ",")
  ))

  return(indicator_data)
}


#' Get UN Sustainable Development Goals Data
#'
#' @description
#' Retrieves SDG indicator data from the United Nations Statistics Division API
#' with automatic conversion between ISO3 and UN M49 country codes.
#'
#' @param indicators Character vector. UN SDG indicator codes (required).
#' @param iso3 Character vector. ISO3 country codes. Default is NULL (all countries).
#' @param mrv Integer. Most Recent Values - number of years to retrieve. Default is 23.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#' @param exclude_aggregates Logical. If TRUE (default), filters out regional and income group aggregates,
#'   returning only data for individual countries with valid ISO3 codes.
#'
#' @return A data.frame with columns: indicator, series, seriesDescription, iso3,
#'   area, year, value, unit, source.
#'   By default, only includes individual countries (aggregates excluded).
#'
#' @details
#' API Documentation: \url{https://unstats.un.org/SDGAPI/swagger/}
#'
#' The function automatically converts ISO3 country codes to UN M49 numeric codes
#' required by the API, retrieves data for the specified time period, and returns
#' the most recent values.
#'
#' **Aggregate Filtering:** By default, the function excludes regional and income group
#' aggregates (e.g., "World", "Sub-Saharan Africa") to return only individual country data.
#' Set `exclude_aggregates = FALSE` to include all entities.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get SDG data for poverty indicators (excludes aggregates by default)
#' sdg_data <- get_unsdg_data(
#'   indicators = c("1.1.1", "1.2.1"),
#'   iso3 = "KEN",
#'   mrv = 10
#' )
#'
#' # Get data for all countries and regions
#' sdg_data_all <- get_unsdg_data(
#'   indicators = "1.1.1",
#'   mrv = 5,
#'   exclude_aggregates = FALSE
#' )
#' }

get_unsdg_data <- function(
  indicators,
  iso3 = NULL,
  mrv = 23,
  verbose = FALSE,
  max_retries = 3,
  exclude_aggregates = TRUE
) {
  if (verbose) {
    message(sprintf(
      "Fetching UNSDG data • indicators: %s%s • MRV: %d",
      paste(indicators, collapse = ", "),
      if (!is.null(iso3)) {
        sprintf(" • iso3: %s", paste(iso3, collapse = ", "))
      } else {
        ""
      },
      mrv
    ))
  }

  # Convert ISO-3 → UN M49 numeric (required by UNSD API) if iso3 provided
  areas <- if (!is.null(iso3)) {
    areas <- countrycode::countrycode(
      iso3,
      origin = "iso3c",
      destination = "un"
    )

    # Handle conversion errors
    if (any(is.na(areas))) {
      bad <- iso3[is.na(areas)]
      message(sprintf(
        "Some iso3 codes could not be converted to UN M49: %s",
        paste(bad, collapse = ", ")
      ))
      areas <- areas[!is.na(areas)]
      if (!length(areas)) return(tibble::tibble())
    }
    areas
  } else {
    NULL
  }

  # Calculate time periods for the last MRV years
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  time_periods <- list((current_year - mrv + 1):current_year)

  fetch_one <- function(
    ind,
    area = NULL,
    time_periods,
    base = "https://unstats.un.org/SDGAPI/v1/sdg/Indicator/Data"
  ) {
    # Build query parameters according to Swagger docs
    q <- list(
      indicator = ind,
      timePeriod = time_periods # Add time periods for latest MRV years
    )

    if (!is.null(area)) {
      q$areaCode <- area # Correct parameter name per Swagger docs
    }

    # Retry logic
    retry_attempt <- 1
    success <- FALSE
    resp <- NULL

    while (retry_attempt <= max_retries && !success) {
      tryCatch(
        {
          resp <- httr2::request(base) |>
            httr2::req_url_query(!!!q) |>
            httr2::req_perform()
          if (!httr2::resp_is_error(resp)) {
            success <- TRUE
          } else {
            stop(sprintf("HTTP error %d", httr2::resp_status(resp)))
          }
        },
        error = function(e) {
          if (retry_attempt < max_retries) {
            wait_time <- 2^retry_attempt
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
            message(sprintf(
              "Request failed for indicator=%s area=%s after %d attempts",
              ind,
              ifelse(is.null(area), "ALL", area),
              max_retries
            ))
            retry_attempt <<- retry_attempt + 1 # Exit loop
          }
        }
      )
    }

    if (!success) {
      return(tibble::tibble())
    }

    # Parse response according to Swagger schema
    response_content <- httr2::resp_body_string(resp)
    parsed_response <- jsonlite::fromJSON(response_content, flatten = TRUE)

    # Extract data array from response
    dat <- parsed_response$data
    if (is.null(dat) || !length(dat)) {
      return(tibble::tibble())
    }

    dplyr::as_tibble(dat) %>%
      dplyr::mutate(
        indicator = ind,
        year = suppressWarnings(as.integer(timePeriodStart)), # Correct column name
        value = suppressWarnings(as.numeric(value)),
        iso3 = countrycode::countrycode(as.numeric(geoAreaCode), "un", "iso3c")
      )
  }

  # Gather data (all countries vs specific)
  results <- if (is.null(areas)) {
    purrr::map_dfr(indicators, ~ fetch_one(.x, NULL, time_periods))
  } else {
    purrr::map_dfr(indicators, function(ind) {
      purrr::map_dfr(areas, ~ fetch_one(ind, .x, time_periods))
    })
  }

  if (!nrow(results)) {
    return(results)
  }

  # Select and rename columns based on actual API response
  final_results <- results %>%
    dplyr::select(
      indicator,
      series,
      seriesDescription,
      iso3,
      area = geoAreaName,
      year,
      value,
      unit = attributes.Units,
      source
    ) %>%
    dplyr::group_by(indicator, series, iso3) %>%
    dplyr::slice_max(order_by = year, n = mrv, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(indicator, iso3, year)

  # Filter out aggregates by validating ISO codes (if requested)
  if (exclude_aggregates) {
    initial_rows <- nrow(final_results)

    final_results <- final_results %>%
      dplyr::mutate(
        is_valid_country = !is.na(countrycode::countrycode(
          iso3,
          origin = "iso3c",
          destination = "country.name",
          warn = FALSE
        ))
      ) %>%
      dplyr::filter(is_valid_country) %>%
      dplyr::select(-is_valid_country)

    filtered_rows <- initial_rows - nrow(final_results)

    if (verbose && filtered_rows > 0) {
      message(sprintf(
        "  Excluded %s aggregate rows",
        format(filtered_rows, big.mark = ",")
      ))
    }
  }

  message(sprintf(
    "✓ UNSDG data retrieved successfully: %s rows",
    format(nrow(final_results), big.mark = ",")
  ))

  return(final_results)
}

#' Get UNDP Human Development Reports Data
#'
#' @description
#' Retrieves human development indicators from the UNDP Human Development Reports
#' API. Requires an API key.
#'
#' @importFrom magrittr %>%
#'
#' @param indicator Character. UNDP indicator code. Default is NULL.
#' @param apikey Character. UNDP API key (required).
#' @param iso3 Character vector. ISO3 country code(s). Default is NULL (all countries).
#' @param metadata Logical. If TRUE, returns metadata instead of data. Default is FALSE.
#' @param mrv Integer. Most Recent Values - number of years to retrieve. Default is 23.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#' @param exclude_aggregates Logical. If TRUE (default), filters out regional and income group aggregates,
#'   returning only data for individual countries with valid ISO3 codes.
#'
#' @return A tibble containing UNDP data with year, country, and indicator values.
#'   By default, only includes individual countries (aggregates excluded).
#'
#' @details
#' API Documentation: \url{https://hdr.undp.org/data-center/documentation-and-downloads}
#'
#' **Authentication Required:** Obtain an API key from UNDP HDR data portal.
#'
#' The function implements smart year discovery similar to other get_* functions.
#'
#' **Aggregate Filtering:** By default, the function excludes regional and income group
#' aggregates to return only individual country data. Set `exclude_aggregates = FALSE`
#' to include all entities.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get HDI data for a specific country (excludes aggregates by default)
#' hdi_data <- get_undp_data(
#'   indicator = "hdi",
#'   apikey = "your_api_key",
#'   iso3 = "KEN",
#'   mrv = 10
#' )
#'
#' # Include regional aggregates in the results
#' hdi_data_all <- get_undp_data(
#'   indicator = "hdi",
#'   apikey = "your_api_key",
#'   mrv = 10,
#'   exclude_aggregates = FALSE
#' )
#' }

get_undp_data <- function(
  indicator = NULL,
  apikey,
  iso3 = NULL,
  metadata = FALSE,
  mrv = 23,
  verbose = FALSE,
  max_retries = 3,
  exclude_aggregates = TRUE
) {
  # Always use the detailed endpoint
  base_url <- "https://hdrdata.org/api/CompositeIndices/query-detailed"

  if (metadata) {
    resp <- httr2::request(base_url) |>
      httr2::req_url_query(apikey = apikey) |>
      httr2::req_perform()
    httr2::resp_check_status(resp)
    return(
      jsonlite::fromJSON(
        httr2::resp_body_string(resp),
        flatten = TRUE
      ) |>
        tibble::as_tibble()
    )
  }

  if (verbose) {
    message(sprintf("Fetching UNDP data for last %d years", mrv))
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- (current_year - mrv + 1):current_year

  # Helper function to try fetching data with specific years
  try_fetch_undp_data <- function(years_to_try) {
    # Build query params
    q <- list(
      apikey = apikey,
      indicator = indicator
    )
    if (!is.null(iso3) && length(iso3)) {
      q$countryOrAggregation <- paste(unique(iso3), collapse = ",")
    }
    if (!is.null(years_to_try) && length(years_to_try)) {
      q$year <- paste(unique(as.integer(years_to_try)), collapse = ",")
    }

    # Retry logic
    retry_attempt <- 1
    success <- FALSE
    resp <- NULL

    while (retry_attempt <= max_retries && !success) {
      tryCatch(
        {
          resp <- httr2::request(base_url) |>
            httr2::req_url_query(!!!q) |>
            httr2::req_perform()
          httr2::resp_check_status(resp)
          success <- TRUE
        },
        error = function(e) {
          if (retry_attempt < max_retries) {
            wait_time <- 2^retry_attempt
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
                "Failed to fetch UNDP data after %d attempts. Last error: %s",
                max_retries,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        }
      )
    }

    txt <- httr2::resp_body_string(resp)
    json <- jsonlite::fromJSON(txt, flatten = TRUE)
    # Extract data frame from JSON
    df <- if (is.data.frame(json)) {
      tibble::as_tibble(json)
    } else if (!is.null(json$data)) {
      tibble::as_tibble(json$data)
    } else {
      tibble::as_tibble(jsonlite::flatten(json))
    }
    return(df)
  }

  # Try fetching data with the requested years first
  result <- try_fetch_undp_data(years)
  # If no data returned, implement smart year discovery
  if (nrow(result) == 0) {
    if (verbose) {
      message(
        "No data found for requested years. Trying to discover available years..."
      )
    }

    # Try progressively larger year ranges to find available data
    year_ranges <- list(
      (current_year - 10):current_year, # Last 10 years
      (current_year - 20):current_year, # Last 20 years
      (current_year - 30):current_year, # Last 30 years
      1990:current_year, # Since 1990 (HDI data typically starts around this time)
      1980:current_year, # Since 1980
      1970:current_year, # Since 1970
      1960:current_year # All available years since UNDP data might start
    )

    for (year_range in year_ranges) {
      if (verbose) {
        message(sprintf(
          "Trying years: %d-%d",
          min(year_range),
          max(year_range)
        ))
      }
      result <- try_fetch_undp_data(year_range)

      if (nrow(result) > 0) {
        # Found data! Now return the most recent values within our mrv limit
        # Check if the result has a year column (it might be named differently)
        year_col <- NULL
        if ("year" %in% names(result)) {
          year_col <- "year"
        } else if ("Year" %in% names(result)) {
          year_col <- "Year"
        } else if ("time" %in% names(result)) {
          year_col <- "time"
        }

        if (!is.null(year_col)) {
          available_years <- sort(unique(result[[year_col]]), decreasing = TRUE)
          years_to_return <- head(available_years, mrv)
          result <- result %>%
            dplyr::filter(!!rlang::sym(year_col) %in% years_to_return)

          if (verbose) {
            message(sprintf(
              "Found data for years: %s",
              paste(sort(years_to_return), collapse = ", ")
            ))
          }
        } else {
          if (verbose) {
            message(
              "Found data but could not identify year column for filtering"
            )
          }
        }
        break
      }
    }

    if (nrow(result) == 0) {
      message("UNDP HDR API returned no rows even with expanded year range.")
      return(result)
    }
  }

  # Filter out aggregates by validating ISO codes (if requested)
  if (exclude_aggregates && nrow(result) > 0) {
    # Try to find the ISO3 column (might be named differently)
    iso_col <- NULL
    possible_names <- c(
      "iso3",
      "iso_code",
      "isocode",
      "country_code",
      "countryCode",
      "country"
    )

    for (col_name in possible_names) {
      if (col_name %in% names(result)) {
        iso_col <- col_name
        break
      }
    }

    if (!is.null(iso_col)) {
      initial_rows <- nrow(result)

      result <- result %>%
        dplyr::mutate(
          is_valid_country = !is.na(countrycode::countrycode(
            !!rlang::sym(iso_col),
            origin = "iso3c",
            destination = "country.name",
            warn = FALSE
          ))
        ) %>%
        dplyr::filter(is_valid_country) %>%
        dplyr::select(-is_valid_country)

      filtered_rows <- initial_rows - nrow(result)

      if (verbose && filtered_rows > 0) {
        message(sprintf(
          "  Excluded %s aggregate rows",
          format(filtered_rows, big.mark = ",")
        ))
      }
    } else if (verbose) {
      message("  Could not identify ISO3 column for aggregate filtering")
    }
  }

  message(sprintf(
    "✓ UNDP data retrieved successfully: %s rows",
    format(nrow(result), big.mark = ",")
  ))

  return(result)
}

#' Get ILO Labor Statistics Data
#'
#' @description
#' Retrieves labor statistics from the International Labour Organization (ILO)
#' API with automatic year discovery and smart fallback for data availability.
#'
#' @importFrom magrittr %>%
#'
#' @param iso3 Character vector. ISO3 country codes to filter data.
#'   Default is NULL (all countries).
#' @param indicators Character vector. ILO indicator codes (required).
#' @param mrv Integer. Most Recent Values - number of years to retrieve.
#'   Default is 23.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#' @param exclude_aggregates Logical. If TRUE (default), filters out regional and income group aggregates,
#'   returning only data for individual countries with valid ISO3 codes.
#'
#' @return A data.frame with columns: isocode, Year, Value, and additional
#'   indicator-specific columns from the ILO API.
#'   By default, only includes individual countries (aggregates excluded).
#'
#' @details
#' API Documentation: \url{https://rplumber.ilo.org/__docs__/}
#'
#' The function implements smart year discovery: if no data is found for the
#' requested years, it progressively expands the year range (10, 20, 30 years,
#' back to 1960) until data is found, then returns the most recent values up
#' to the mrv limit.
#'
#' **Aggregate Filtering:** By default, the function excludes regional and income group
#' aggregates to return only individual country data. Set `exclude_aggregates = FALSE`
#' to include all entities.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get unemployment rate for Kenya (excludes aggregates by default)
#' ilo_data <- get_ilo_data(
#'   iso3 = "KEN",
#'   indicators = "UNE_DEAP_SEX_AGE_RT_A",
#'   mrv = 10
#' )
#'
#' # Get data for all countries and regions
#' ilo_data_all <- get_ilo_data(
#'   indicators = "UNE_DEAP_SEX_AGE_RT_A",
#'   mrv = 10,
#'   exclude_aggregates = FALSE
#' )
#' }
get_ilo_data <- function(
  iso3 = NULL,
  indicators,
  mrv = 23,
  verbose = FALSE,
  max_retries = 3,
  exclude_aggregates = TRUE
) {
  # Validate inputs
  if (missing(indicators) || length(indicators) == 0) {
    stop("indicators parameter is required and must be a non-empty vector")
  }

  if (verbose) {
    message(sprintf("Fetching ILO data for last %d years", mrv))
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- (current_year - mrv + 1):current_year

  # Helper function to try fetching data with specific years
  try_fetch_ilo_data <- function(years_to_try) {
    # Create simple data frame for indicators
    indicators_df <- tibble::tibble(indicator = indicators)
    # Base URL for ILO API
    base_url <- "https://rplumber.ilo.org/data/indicator/"

    # Fetch data
    ilo_data <- indicators_df %>%
      dplyr::mutate(
        api_response = purrr::map(
          indicator,
          ~ {
            # URL encode the indicator ID (replace _ with %5F)
            encoded_indicator <- gsub("_", "%5F", .x)
            # Build URL with proper parameters
            url <- paste0(
              base_url,
              "?id=",
              encoded_indicator,
              "&type=code&format=.csv"
            )

            # Retry logic for each indicator
            retry_attempt <- 1
            success <- FALSE
            response <- NULL

            while (retry_attempt <= max_retries && !success) {
              tryCatch(
                {
                  response <- httr2::request(url) |>
                    httr2::req_perform()
                  if (httr2::resp_status(response) == 200) {
                    content <- httr2::resp_body_string(response)
                    if (nchar(content) > 0 && !grepl("^\\s*$", content)) {
                      success <- TRUE
                      return(utils::read.csv(textConnection(content)))
                    } else {
                      if (verbose) {
                        message(sprintf("Empty response for indicator: %s", .x))
                      }
                      return(NULL)
                    }
                  } else {
                    if (retry_attempt < max_retries) {
                      wait_time <- 2^retry_attempt
                      if (verbose) {
                        message(sprintf(
                          "HTTP %d for indicator %s (attempt %d/%d). Retrying in %d seconds...",
                          httr2::resp_status(response),
                          .x,
                          retry_attempt,
                          max_retries,
                          wait_time
                        ))
                      }
                      Sys.sleep(wait_time)
                      retry_attempt <- retry_attempt + 1
                    } else {
                      if (verbose) {
                        message(sprintf(
                          "HTTP error %d for indicator: %s after %d attempts",
                          httr2::resp_status(response),
                          .x,
                          max_retries
                        ))
                      }
                      return(NULL)
                    }
                  }
                },
                error = function(e) {
                  if (retry_attempt < max_retries) {
                    wait_time <- 2^retry_attempt
                    if (verbose) {
                      message(sprintf(
                        "Error for indicator %s (attempt %d/%d): %s. Retrying in %d seconds...",
                        .x,
                        retry_attempt,
                        max_retries,
                        conditionMessage(e),
                        wait_time
                      ))
                    }
                    Sys.sleep(wait_time)
                    retry_attempt <<- retry_attempt + 1
                  } else {
                    if (verbose) {
                      message(sprintf(
                        "Error fetching data for indicator %s after %d attempts: %s",
                        .x,
                        max_retries,
                        conditionMessage(e)
                      ))
                    }
                    return(NULL)
                  }
                }
              )
            }
            return(NULL)
          }
        )
      ) %>%
      dplyr::filter(!purrr::map_lgl(api_response, is.null))
    # Check if we have any valid data
    if (nrow(ilo_data) == 0) {
      if (verbose) {
        message("No data retrieved from ILO API for the specified indicators")
      }
      return(data.frame())
    }

    # Unnest the API responses (remove original indicator column to avoid conflicts)
    ilo_data <- ilo_data %>%
      dplyr::select(-indicator) %>%
      tidyr::unnest(api_response)

    # Filter by country if specified
    if (!is.null(iso3)) {
      ilo_data <- ilo_data %>%
        dplyr::filter(ref_area %in% iso3)
    }

    # Filter by years and return ALL columns (with standardized key column names)
    if ("time" %in% names(ilo_data)) {
      ilo_data <- ilo_data %>%
        dplyr::filter(time %in% years_to_try) %>%
        # Rename key columns for consistency but keep ALL other columns
        dplyr::rename(
          isocode = ref_area,
          Year = time,
          Value = obs_value
        )
    } else {
      if (verbose) {
        message(sprintf(
          "No 'time' column found in ILO data. Available columns: %s",
          paste(names(ilo_data), collapse = ", ")
        ))
      }
      return(data.frame())
    }
    return(ilo_data)
  }

  # Try fetching data with the requested years first
  result <- try_fetch_ilo_data(years)

  # If no data returned, implement smart year discovery
  if (nrow(result) == 0) {
    if (verbose) {
      message(
        "No data found for requested years. Trying to discover available years..."
      )
    }

    # Try progressively larger year ranges to find available data
    year_ranges <- list(
      (current_year - 10):current_year, # Last 10 years
      (current_year - 20):current_year, # Last 20 years
      (current_year - 30):current_year, # Last 30 years
      1990:current_year, # Since 1990
      1980:current_year, # Since 1980
      1970:current_year, # Since 1970
      1960:current_year # All available years since ILO data typically starts
    )

    for (year_range in year_ranges) {
      if (verbose) {
        message(sprintf(
          "Trying years: %d-%d",
          min(year_range),
          max(year_range)
        ))
      }
      result <- try_fetch_ilo_data(year_range)

      if (nrow(result) > 0) {
        # Found data! Now return the most recent values within our mrv limit
        available_years <- sort(unique(result$Year), decreasing = TRUE)
        years_to_return <- head(available_years, mrv)
        result <- result %>%
          dplyr::filter(Year %in% years_to_return)

        if (verbose) {
          message(sprintf(
            "Found data for years: %s",
            paste(sort(years_to_return), collapse = ", ")
          ))
        }
        break
      }
    }

    if (nrow(result) == 0) {
      message("No data found even with expanded year range")
      return(result)
    }
  }

  # Filter out aggregates by validating ISO codes (if requested)
  if (exclude_aggregates && nrow(result) > 0 && "isocode" %in% names(result)) {
    initial_rows <- nrow(result)

    result <- result %>%
      dplyr::mutate(
        is_valid_country = !is.na(countrycode::countrycode(
          isocode,
          origin = "iso3c",
          destination = "country.name",
          warn = FALSE
        ))
      ) %>%
      dplyr::filter(is_valid_country) %>%
      dplyr::select(-is_valid_country)

    filtered_rows <- initial_rows - nrow(result)

    if (verbose && filtered_rows > 0) {
      message(sprintf(
        "  Excluded %s aggregate rows",
        format(filtered_rows, big.mark = ",")
      ))
    }
  }

  message(sprintf(
    "✓ ILO data retrieved successfully: %s rows",
    format(nrow(result), big.mark = ",")
  ))

  return(result)
}
#' Get WHO Global Health Observatory Data
#'
#' @description
#' Retrieves health indicators from the World Health Organization Global Health
#' Observatory (WHO GHO) API with automatic year discovery.
#'
#' @importFrom magrittr %>%
#'
#' @param iso3 Character. ISO3 country code to filter data. Default is NULL (all countries).
#' @param indicators Character vector. WHO GHO indicator codes (required).
#' @param mrv Integer. Most Recent Values - number of years to retrieve. Default is 23.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#' @param exclude_aggregates Logical. If TRUE (default), filters out regional and income group aggregates,
#'   returning only data for individual countries with valid ISO3 codes.
#'
#' @return A data.frame with columns: isocode, Year, Value, indicator, and all
#'   available fields from the WHO API response.
#'   By default, only includes individual countries (aggregates excluded).
#'
#' @details
#' API Documentation: \url{https://www.who.int/data/gho/info/gho-odata-api}
#'
#' The function preserves all available fields from the WHO API while adding
#' standardized columns (isocode, Year, Value). Implements smart year discovery
#' similar to get_ilo_data().
#'
#' **Aggregate Filtering:** By default, the function excludes regional and income group
#' aggregates to return only individual country data. Set `exclude_aggregates = FALSE`
#' to include all entities.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get life expectancy data for a specific country (excludes aggregates by default)
#' who_data <- get_who_data(
#'   iso3 = "KEN",
#'   indicators = "WHOSIS_000001",
#'   mrv = 10
#' )
#'
#' # Get data for all countries and regions
#' who_data_all <- get_who_data(
#'   indicators = "WHOSIS_000001",
#'   mrv = 10,
#'   exclude_aggregates = FALSE
#' )
#' }
get_who_data <- function(
  iso3 = NULL,
  indicators,
  mrv = 23,
  verbose = FALSE,
  max_retries = 3,
  exclude_aggregates = TRUE
) {
  # Validate inputs
  if (missing(indicators) || length(indicators) == 0) {
    stop("indicators parameter is required and must be a non-empty vector")
  }

  if (verbose) {
    message(sprintf("Fetching WHO data for last %d years", mrv))
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- (current_year - mrv + 1):current_year

  # Helper function to try fetching data with specific years
  try_fetch_who_data <- function(years_to_try) {
    # Create simple data frame for indicators
    indicators_df <- tibble::tibble(indicator = indicators)

    # Fetch data - either for specific country or all countries
    who_data <- indicators_df %>%
      dplyr::mutate(
        api_response = purrr::map(
          indicator,
          ~ {
            if (!is.null(iso3)) {
              # Fetch data for specific country
              encoded_iso3 <- utils::URLencode(iso3, reserved = TRUE)
              url <- paste0(
                "https://ghoapi.azureedge.net/api/",
                .x,
                "?$filter=SpatialDim%20eq%20'",
                encoded_iso3,
                "'"
              )
            } else {
              # Fetch data for all countries
              url <- paste0("https://ghoapi.azureedge.net/api/", .x)
            }

            # Retry logic for each indicator
            retry_attempt <- 1
            success <- FALSE
            result <- NULL

            while (retry_attempt <= max_retries && !success) {
              tryCatch(
                {
                  result <- httr2::request(url) |>
                    httr2::req_perform() |>
                    httr2::resp_body_json()
                  success <- TRUE
                },
                error = function(e) {
                  if (retry_attempt < max_retries) {
                    wait_time <- 2^retry_attempt
                    if (verbose) {
                      message(sprintf(
                        "Error for indicator %s (attempt %d/%d): %s. Retrying in %d seconds...",
                        .x,
                        retry_attempt,
                        max_retries,
                        conditionMessage(e),
                        wait_time
                      ))
                    }
                    Sys.sleep(wait_time)
                    retry_attempt <<- retry_attempt + 1
                  } else {
                    if (verbose) {
                      message(sprintf(
                        "Error fetching data for indicator %s after %d attempts: %s",
                        .x,
                        max_retries,
                        conditionMessage(e)
                      ))
                    }
                    result <<- NULL
                  }
                }
              )
            }
            return(result)
          }
        ),
        all_values = purrr::map(
          api_response,
          ~ {
            if (length(.x$value) > 0) .x$value else NULL
          }
        )
      ) %>%
      dplyr::filter(!purrr::map_lgl(all_values, is.null)) %>%
      dplyr::select(indicator, all_values) %>%
      tidyr::unnest_longer(all_values) %>%
      dplyr::mutate(
        # Extract all available fields from WHO API response safely
        extracted_data = purrr::map(
          all_values,
          ~ {
            # Create a safe extraction function for WHO API response
            safe_extract <- function(obj, field, default = NA) {
              if (field %in% names(obj) && !is.null(obj[[field]])) {
                return(obj[[field]])
              } else {
                return(default)
              }
            }
            # Extract all available fields from the response
            result <- list()
            for (field_name in names(.x)) {
              result[[field_name]] <- safe_extract(.x, field_name)
            }
            # Add standardized column names while preserving originals
            result$isocode <- safe_extract(.x, "SpatialDim")
            result$Year <- as.integer(safe_extract(.x, "TimeDim", "0"))
            result$Value <- as.numeric(safe_extract(.x, "NumericValue"))
            # Convert to data frame with consistent structure
            data.frame(result, stringsAsFactors = FALSE)
          }
        )
      ) %>%
      dplyr::select(indicator, extracted_data) %>%
      tidyr::unnest(extracted_data) %>%
      # Remove duplicates - take most recent entry for same indicator/year/country
      dplyr::group_by(isocode, indicator, Year) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Year %in% years_to_try)
    return(who_data)
  }
  # Try fetching data with the requested years first
  result <- try_fetch_who_data(years)

  # If no data returned, implement smart year discovery
  if (nrow(result) == 0) {
    if (verbose) {
      message(
        "No data found for requested years. Trying to discover available years..."
      )
    }

    # Try progressively larger year ranges to find available data
    year_ranges <- list(
      (current_year - 10):current_year, # Last 10 years
      (current_year - 20):current_year, # Last 20 years
      (current_year - 30):current_year, # Last 30 years
      1990:current_year, # Since 1990
      1980:current_year, # Since 1980
      1970:current_year, # Since 1970
      1960:current_year # All available years since WHO data typically starts
    )

    for (year_range in year_ranges) {
      if (verbose) {
        message(sprintf(
          "Trying years: %d-%d",
          min(year_range),
          max(year_range)
        ))
      }
      result <- try_fetch_who_data(year_range)

      if (nrow(result) > 0) {
        # Found data! Now return the most recent values within our mrv limit
        available_years <- sort(unique(result$Year), decreasing = TRUE)
        years_to_return <- head(available_years, mrv)
        result <- result %>%
          dplyr::filter(Year %in% years_to_return)

        if (verbose) {
          message(sprintf(
            "Found data for years: %s",
            paste(sort(years_to_return), collapse = ", ")
          ))
        }
        break
      }
    }

    if (nrow(result) == 0) {
      message("No data found even with expanded year range")
      return(result)
    }
  }

  # Filter out aggregates by validating ISO codes (if requested)
  if (exclude_aggregates && nrow(result) > 0 && "isocode" %in% names(result)) {
    initial_rows <- nrow(result)

    result <- result %>%
      dplyr::mutate(
        is_valid_country = !is.na(countrycode::countrycode(
          isocode,
          origin = "iso3c",
          destination = "country.name",
          warn = FALSE
        ))
      ) %>%
      dplyr::filter(is_valid_country) %>%
      dplyr::select(-is_valid_country)

    filtered_rows <- initial_rows - nrow(result)

    if (verbose && filtered_rows > 0) {
      message(sprintf(
        "  Excluded %s aggregate rows",
        format(filtered_rows, big.mark = ",")
      ))
    }
  }

  message(sprintf(
    "✓ WHO data retrieved successfully: %s rows",
    format(nrow(result), big.mark = ",")
  ))

  return(result)
}
