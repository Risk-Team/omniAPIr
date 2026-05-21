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
    "worldbank" = .fetch_wb_indicators(
      search,
      verbose,
      max_retries,
      conda_env,
      database
    ),
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

.fetch_wb_indicators <- function(
  search,
  verbose,
  max_retries,
  conda_env,
  database = NULL
) {
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


.fetch_who_indicators <- function(
  search = NULL,
  verbose = FALSE,
  max_retries = 3
) {
  base_url <- "https://ghoapi.azureedge.net/api/Indicator"

  if (verbose) {
    message("Requesting WHO indicators: ", base_url)
  }

  fetch_with_retry <- function() {
    last_err <- NULL
    for (i in seq_len(max_retries)) {
      if (verbose && max_retries > 1) {
        message(sprintf("  Attempt %d/%d", i, max_retries))
      }

      out <- tryCatch(
        {
          resp <- httr2::request(base_url) |>
            httr2::req_perform()
          httr2::resp_check_status(resp)
          httr2::resp_body_json(resp)
        },
        error = function(e) {
          last_err <<- e
          if (i < max_retries) {
            Sys.sleep(2^i)
          }
          NULL
        }
      )

      if (!is.null(out)) return(out)
    }

    stop(
      sprintf(
        "Failed to fetch WHO indicators after %d attempts: %s",
        max_retries,
        conditionMessage(last_err)
      ),
      call. = FALSE
    )
  }

  json <- fetch_with_retry()

  if (is.null(json$value) || length(json$value) == 0) {
    if (verbose) {
      message("No indicator metadata returned by WHO.")
    }
    return(tibble::tibble(code = character(), name = character()))
  }

  # One row per indicator (handle potential NULLs gracefully)
  df <- purrr::map_dfr(
    json$value,
    function(rec) {
      rec_list <- as.list(rec)
      # Only keep the fields we care about and replace NULL with NA
      code <- rec_list$IndicatorCode %||% NA_character_
      name <- rec_list$IndicatorName %||% NA_character_
      tibble::tibble(code = code, name = name)
    }
  ) |>
    dplyr::distinct()

  # Apply search filter if provided
  if (!is.null(search) && nchar(search) > 0) {
    pattern <- paste0("(?i)", search)
    df <- df |>
      dplyr::filter(
        grepl(pattern, code) | grepl(pattern, name)
      )
  }

  if (verbose) {
    message(sprintf(
      "✓ WHO indicators retrieved: %s rows",
      format(nrow(df), big.mark = ",")
    ))
  }

  df
}


#' Normalize World Bank Data Structure Across Databases
#'
#' @description
#' Internal helper function that normalizes raw data from different World Bank
#' databases into a consistent structure. Different databases return data in
#' different formats (e.g., ISO codes in rownames vs. columns).
#'
#' **Adding Support for New Databases:**
#' To add support for a new database, add a new case in the switch statement below
#' with the database ID as a string (e.g., "57" for Debt Statistics). Document the
#' database structure and any column transformations needed. The goal is to always
#' return data with an 'isocode' column + year columns (YR2020, YR2021, etc.).
#'
#' @param raw_data Data frame returned by wb$data$DataFrame()
#' @param database Integer. World Bank database ID
#' @param verbose Logical. If TRUE, prints normalization details
#'
#' @return Data frame with standardized structure (isocode column + year columns)
#'
#' @keywords internal
#' @noRd
.normalize_wb_data_structure <- function(raw_data, database, verbose = FALSE) {
  # ============================================================================
  # Database-Specific Transformations
  # ============================================================================
  # Add new databases as switch cases below. Each case should:
  # 1. Document the database structure
  # 2. Transform to have an 'isocode' column
  # 3. Remove any unnecessary metadata columns
  # 4. Return a clean data frame ready for pivoting
  # ============================================================================

  normalized_data <- switch(
    as.character(database),

    # ========================================================================
    # Database 2: WDI (World Development Indicators)
    # ========================================================================
    # Structure:
    #   - ISO codes in rownames (e.g., "KEN", "USA")
    #   - Year columns: YR2020, YR2021, etc.
    #   - Country metadata columns: Country, Classification
    # Transformation:
    #   - Move rownames to 'isocode' column
    # ========================================================================
    "2" = {
      if (verbose) {
        message("  Normalizing WDI (database 2) structure")
      }
      raw_data %>%
        tibble::rownames_to_column(var = "isocode")
    },

    # ========================================================================
    # Database 88: Food & Price Database
    # ========================================================================
    # Structure varies based on labels parameter:
    #   With labels=TRUE:
    #     - 'Country' column with full country names (e.g., "Kenya", "United States")
    #     - 'Classification' column: "Food Prices for Nutrition 4.0"
    #   With labels=FALSE:
    #     - 'economy' column with ISO codes (e.g., "KEN", "USA")
    #     - 'classification' column: "FPN 4.0"
    # Transformation:
    #   - Convert Country names → ISO codes using countrycode
    #   - OR rename 'economy' → 'isocode'
    #   - Remove 'Classification'/'classification' column (metadata not needed)
    # ========================================================================
    "88" = {
      if (verbose) {
        message("  Normalizing Food & Price (database 88) structure")
      }
      result <- raw_data

      # Case 1: labels=TRUE returns 'Country' column with full names
      if ("Country" %in% names(result)) {
        result <- result %>%
          dplyr::mutate(
            isocode = countrycode::countrycode(
              Country,
              origin = "country.name",
              destination = "iso3c",
              warn = FALSE
            )
          ) %>%
          dplyr::select(-Country)
      } else if ("economy" %in% names(result)) {
        # Case 2: labels=FALSE returns 'economy' column with ISO codes
        result <- result %>% dplyr::rename(isocode = economy)
      }

      # Remove classification column if present (not needed in final output)
      if ("Classification" %in% names(result)) {
        result <- result %>% dplyr::select(-Classification)
      }
      if ("classification" %in% names(result)) {
        result <- result %>% dplyr::select(-classification)
      }

      result
    },

    # ========================================================================
    # Default: Auto-detect structure for unspecified databases
    # ========================================================================
    # Heuristic-based detection:
    #   1. If 'Country' column exists → convert country names to ISO codes
    #   2. If 'economy' column exists → assume it contains ISO codes
    #   3. Otherwise → assume ISO codes are in rownames (WDI-style)
    #
    # Note: For best results, add explicit case above for your database
    # ========================================================================
    {
      if (verbose) {
        message(sprintf("  Auto-detecting structure for database %d", database))
      }

      # Priority 1: 'Country' column with full names (labels=TRUE style)
      if ("Country" %in% names(raw_data)) {
        if (verbose) {
          message(
            "    Detected 'Country' column, converting names to ISO codes"
          )
        }
        result <- raw_data %>%
          dplyr::mutate(
            isocode = countrycode::countrycode(
              Country,
              origin = "country.name",
              destination = "iso3c",
              warn = FALSE
            )
          ) %>%
          dplyr::select(-Country)

        # Remove classification if present
        if ("Classification" %in% names(result)) {
          result <- result %>% dplyr::select(-Classification)
        }
        if ("classification" %in% names(result)) {
          result <- result %>% dplyr::select(-classification)
        }

        result
      } else if ("economy" %in% names(raw_data)) {
        # Priority 2: 'economy' column with ISO codes (labels=FALSE style)
        if (verbose) {
          message(
            "    Detected 'economy' column, using Food & Price-style normalization"
          )
        }
        result <- raw_data %>% dplyr::rename(isocode = economy)

        # Remove classification if present
        if ("classification" %in% names(result)) {
          result <- result %>% dplyr::select(-classification)
        }

        result
      } else {
        # Priority 3: ISO codes in rownames (WDI-style)
        if (verbose) {
          message(
            "    No 'Country' or 'economy' column found, using WDI-style normalization"
          )
        }
        raw_data %>% tibble::rownames_to_column(var = "isocode")
      }
    }
  )

  return(normalized_data)
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
#' **Multiple Databases:** This function automatically normalizes data structures across
#' different World Bank databases to provide consistent output format. Database-specific
#' logic is handled internally via `.normalize_wb_data_structure()` helper function.
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
    message(sprintf(
      "Fetching World Bank data from database %d for last %d years",
      database,
      mrv
    ))
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
            )
          } else {
            raw_data <- wb$data$DataFrame(indicator, mrv = mrv, labels = TRUE)
          }

          # Normalize data structure based on database type
          # This ensures consistent output format regardless of source database
          raw_data <- .normalize_wb_data_structure(raw_data, database, verbose)

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
#'   area, year, value, unit, sex, age, location, reporting_type, source.
#'   Dimension columns contain NA if not applicable to the indicator:
#'   - `sex`: "BOTHSEX", "MALE", "FEMALE", etc.
#'   - `age`: "ALLAGE", "15-49", "<5Y", etc.
#'   - `location`: "ALLAREA", "URBAN", "RURAL"
#'   - `reporting_type`: "G" (Global), "N" (National)
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

  # Calculate year range for filtering results (client-side)
  # The SDG API works best without timePeriod filter - we fetch all and filter locally

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  min_year <- current_year - mrv + 1

  fetch_one <- function(
    ind,
    area = NULL,
    base = "https://unstats.un.org/SDGAPI/v1/sdg/Indicator/Data"
  ) {
    # Build query parameters - keep it simple like the working curl example
    # Don't pass timePeriod; filter client-side instead
    # Request large page size to minimize pagination
    q <- list(indicator = ind, pageSize = 10000)

    if (!is.null(area)) {
      q$areaCode <- area
    }

    all_data <- list()
    page <- 1
    total_pages <- 1

    # Fetch all pages

    while (page <= total_pages) {
      q$page <- page

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
                "Request failed for indicator=%s area=%s page=%d after %d attempts",
                ind,
                ifelse(is.null(area), "ALL", area),
                page,
                max_retries
              ))
              retry_attempt <<- retry_attempt + 1 # Exit loop
            }
          }
        )
      }

      if (!success) {
        break
      }

      # Parse response according to Swagger schema
      response_content <- httr2::resp_body_string(resp)
      parsed_response <- jsonlite::fromJSON(response_content, flatten = TRUE)

      # Get pagination info from first page
      if (page == 1) {
        total_pages <- parsed_response$totalPages %||% 1
        if (verbose) {
          message(sprintf(
            "  Indicator %s: %d total records across %d pages",
            ind,
            parsed_response$totalElements %||% 0,
            total_pages
          ))
        }
      }

      # Extract data array from response
      dat <- parsed_response$data
      if (!is.null(dat) && length(dat) > 0) {
        all_data[[page]] <- dat
      }

      page <- page + 1
    }

    # Combine all pages
    if (length(all_data) == 0) {
      return(tibble::tibble())
    }

    combined <- dplyr::bind_rows(all_data)

    dplyr::as_tibble(combined) %>%
      dplyr::mutate(
        indicator = ind,
        year = suppressWarnings(as.integer(timePeriodStart)),
        value = suppressWarnings(as.numeric(value)),
        iso3 = countrycode::countrycode(as.numeric(geoAreaCode), "un", "iso3c")
      )
  }

  # Gather data (all countries vs specific)
  results <- if (is.null(areas)) {
    purrr::map_dfr(indicators, ~ fetch_one(.x, NULL))
  } else {
    purrr::map_dfr(indicators, function(ind) {
      purrr::map_dfr(areas, ~ fetch_one(ind, .x))
    })
  }

  if (!nrow(results)) {
    message("UNSDG API returned no rows for the requested indicators")
    return(results)
  }

  # Filter to requested year range (client-side filtering)
  results <- results %>%
    dplyr::filter(!is.na(year), year >= min_year)

  if (!nrow(results)) {
    message(sprintf("No data found for years >= %d", min_year))
    return(tibble::tibble())
  }

  # Select and rename columns based on actual API response
  # Use dplyr::any_of() to handle columns that may not exist
  available_cols <- names(results)

  final_results <- results %>%
    dplyr::mutate(
      # Create unit column from attributes.Units if it exists, otherwise NA
      unit = if ("attributes.Units" %in% available_cols) {
        .data$attributes.Units
      } else {
        NA_character_
      },
      # Extract dimension columns (Sex, Age, Location, Reporting Type, etc.)
      sex = if ("dimensions.Sex" %in% available_cols) {
        .data$`dimensions.Sex`
      } else {
        NA_character_
      },
      age = if ("dimensions.Age" %in% available_cols) {
        .data$`dimensions.Age`
      } else {
        NA_character_
      },
      location = if ("dimensions.Location" %in% available_cols) {
        .data$`dimensions.Location`
      } else {
        NA_character_
      },
      reporting_type = if ("dimensions.Reporting Type" %in% available_cols) {
        .data$`dimensions.Reporting Type`
      } else {
        NA_character_
      }
    ) %>%
    dplyr::select(
      indicator,
      dplyr::any_of(c("series", "seriesDescription")),
      iso3,
      area = geoAreaName,
      year,
      value,
      unit,
      sex,
      age,
      location,
      reporting_type,
      dplyr::any_of("source")
    ) %>%
    dplyr::group_by(
      indicator,
      dplyr::across(dplyr::any_of(c("series", "sex", "age", "location"))),
      iso3
    ) %>%
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
#' @param timeout_s Numeric. Per-request timeout in seconds. Default is 30.
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
  timeout_s = 30,
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

  # Helper function to fetch each indicator once. The ILO endpoint returns the
  # full indicator series; year fallback therefore filters locally rather than
  # re-downloading for each candidate year range.
  fetch_ilo_data <- function() {
    fetch_one <- function(id) {
      url <- sprintf("https://rplumber.ilo.org/files/indicator/%s.rds", id)

      if (verbose) {
        message("Requesting: ", url)
      }

      for (attempt in seq_len(max_retries)) {
        resp <- tryCatch(
          {
            httr2::request(url) |>
              httr2::req_timeout(timeout_s) |>
              httr2::req_user_agent("omniAPIr/1.0") |>
              httr2::req_error(is_error = function(r) FALSE) |>
              httr2::req_perform()
          },
          error = function(e) {
            if (attempt < max_retries) {
              wait_time <- 2^attempt
              if (verbose) {
                message(sprintf(
                  "  Request failed for %s (attempt %d/%d): %s. Retrying in %ds...",
                  id, attempt, max_retries, conditionMessage(e), wait_time
                ))
              }
              Sys.sleep(wait_time)
            } else {
              warning(sprintf(
                "Failed to fetch ILO data for indicator %s: %s",
                id, conditionMessage(e)
              ))
            }
            NULL
          }
        )

        if (is.null(resp)) next

        status <- httr2::resp_status(resp)

        if (status == 200) {
          raw_bytes <- httr2::resp_body_raw(resp)
          df <- tryCatch(
            {
              tmp <- tempfile(fileext = ".rds")
              on.exit(unlink(tmp), add = TRUE)
              writeBin(raw_bytes, tmp)
              readRDS(tmp)
            },
            error = function(e) {
              if (verbose) {
                message(sprintf(
                  "Failed to parse RDS for %s: %s", id, conditionMessage(e)
                ))
              }
              NULL
            }
          )
          if (!is.null(df)) {
            df$indicator <- id
            return(tibble::as_tibble(df))
          }
          return(NULL)
        }

        if (status %in% c(408, 429, 500, 502, 503, 504) && attempt < max_retries) {
          wait_time <- 2^attempt
          if (verbose) {
            message(sprintf(
              "HTTP %d for %s (attempt %d/%d). Retrying in %ds...",
              status, id, attempt, max_retries, wait_time
            ))
          }
          Sys.sleep(wait_time)
          next
        }

        if (verbose) {
          message(sprintf("HTTP error %d for indicator: %s", status, id))
        }
        return(NULL)
      }
      NULL
    }

    out <- purrr::map_dfr(indicators, fetch_one)

    if (nrow(out) == 0) {
      if (verbose) {
        message("No data retrieved from ILO API for the specified indicators")
      }
      return(data.frame())
    }

    # Filter by country if specified
    if (!is.null(iso3)) {
      out <- out %>% dplyr::filter(ref_area %in% iso3)
    }

    if (!"time" %in% names(out)) {
      if (verbose) {
        message(sprintf(
          "No 'time' column found in ILO data. Available columns: %s",
          paste(names(out), collapse = ", ")
        ))
      }
      return(data.frame())
    }

    out %>%
      dplyr::rename(
        isocode = ref_area,
        Year = time,
        Value = obs_value
      ) %>%
      dplyr::mutate(
        isocode = as.character(isocode),
        Year = as.integer(as.character(Year)),
        Value = as.numeric(as.character(Value))
      )
  }

  raw_result <- fetch_ilo_data()
  if (nrow(raw_result) == 0) {
    message("No data found even with expanded year range")
    return(raw_result)
  }

  # Try the requested years first
  result <- raw_result %>% dplyr::filter(Year %in% years)

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
      result <- raw_result %>% dplyr::filter(Year %in% year_range)

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
#' Get WHO Global Health Observatory data
#'
#' Simple wrapper around the WHO GHO API:
#' - builds the URL(s)
#' - fetches raw data
#' - standardises isocode / Year / Value
#' - keeps the most recent `mrv` years
#' - optionally drops aggregates.
#'
#' @param indicators Character vector of WHO GHO indicator codes (required).
#' @param iso3 Optional ISO3 country code (e.g. "KEN") to filter on SpatialDim.
#'   If NULL (default), returns all entities.
#' @param mrv Integer. Number of most recent years per country/indicator to keep.
#'   Default is 10.
#' @param exclude_aggregates Logical. If TRUE (default), drop regional/income
#'   aggregates using `countrycode` on ISO3 codes.
#' @param verbose Logical. If TRUE, prints URLs and a final row count.
#'
#' @return A tibble with all original WHO fields plus:
#'   - `indicator`
#'   - `isocode`
#'   - `Year`
#'   - `Value`
#' @export
get_who_data <- function(
  indicators,
  iso3 = NULL,
  mrv = 10,
  exclude_aggregates = TRUE,
  verbose = FALSE
) {
  if (missing(indicators) || length(indicators) == 0) {
    stop("`indicators` must be a non-empty character vector.")
  }

  indicators <- unique(indicators)

  # ---- internal helper: fetch one indicator ---------------------------------
  fetch_one_indicator <- function(ind, max_retries = 3) {
    base_url <- sprintf("https://ghoapi.azureedge.net/api/%s", ind)

    if (!is.null(iso3)) {
      # Filter only by SpatialDim (ISO3)
      url <- sprintf(
        "%s?$filter=SpatialDim%%20eq%%20'%s'",
        base_url,
        iso3
      )
    } else {
      url <- base_url
    }

    if (verbose) {
      message("Requesting: ", url)
    }

    # Retry logic with error handling
    resp <- NULL
    for (attempt in seq_len(max_retries)) {
      resp <- tryCatch(
        {
          httr2::request(url) |>
            httr2::req_perform() |>
            httr2::resp_body_json()
        },
        error = function(e) {
          if (attempt < max_retries) {
            wait_time <- 2^attempt
            if (verbose) {
              message(sprintf(
                "  Request failed for %s (attempt %d/%d): %s. Retrying in %ds...",
                ind,
                attempt,
                max_retries,
                conditionMessage(e),
                wait_time
              ))
            }
            Sys.sleep(wait_time)
          } else {
            warning(sprintf(
              "Failed to fetch WHO data for indicator %s: %s",
              ind,
              conditionMessage(e)
            ))
          }
          NULL
        }
      )
      if (!is.null(resp)) break
    }

    # Return NULL if all retries failed
    if (is.null(resp)) {
      return(NULL)
    }

    # WHO returns records under `$value`
    if (is.null(resp$value) || length(resp$value) == 0) {
      if (verbose) {
        message("  (no rows returned)")
      }
      return(NULL)
    }

    # One row per record, replacing NULL fields with NA
    df <- purrr::map_dfr(
      resp$value,
      function(rec) {
        rec_list <- as.list(rec)
        # Replace NULL elements with NA so tibble is happy
        null_idx <- vapply(rec_list, is.null, logical(1))
        rec_list[null_idx] <- NA
        tibble::as_tibble(rec_list, .name_repair = "unique")
      }
    )

    df$indicator <- ind
    df
  }

  # ---- fetch all indicators and bind ----------------------------------------
  out_list <- lapply(indicators, fetch_one_indicator)
  out <- dplyr::bind_rows(out_list)

  if (nrow(out) == 0) {
    if (verbose) {
      message("No data returned by WHO API for these parameters.")
    }
    return(out)
  }

  # ---- standardise key columns ----------------------------------------------
  if (!"SpatialDim" %in% names(out)) {
    out$SpatialDim <- NA_character_
  }
  if (!"TimeDim" %in% names(out)) {
    out$TimeDim <- NA
  }
  if (!"NumericValue" %in% names(out)) {
    out$NumericValue <- NA
  }

  out <- out |>
    dplyr::mutate(
      isocode = SpatialDim,
      Year = suppressWarnings(as.integer(TimeDim)),
      Value = suppressWarnings(as.numeric(NumericValue))
    )

  # ---- keep most recent `mrv` years per country & indicator -----------------
  if (!is.null(mrv) && mrv > 0) {
    out <- out |>
      dplyr::group_by(isocode, indicator) |>
      dplyr::arrange(dplyr::desc(Year), .by_group = TRUE) |>
      dplyr::slice_head(n = mrv) |>
      dplyr::ungroup()
  }

  # ---- drop aggregates if requested -----------------------------------------
  if (exclude_aggregates && "isocode" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        is_valid_country = !is.na(
          countrycode::countrycode(
            isocode,
            origin = "iso3c",
            destination = "country.name",
            warn = FALSE
          )
        )
      ) |>
      dplyr::filter(is_valid_country) |>
      dplyr::select(-is_valid_country)
  }

  if (verbose) {
    message(sprintf(
      "✓ WHO data retrieved: %s rows",
      format(nrow(out), big.mark = ",")
    ))
  }

  out
}
