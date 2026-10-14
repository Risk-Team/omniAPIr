#' GEMStat indicator services that can be queried as station points.
#'
#' Dissolved oxygen, total nitrogen, and total phosphorus are the parameters
#' published as annual station means. Nitrate, phosphate, and biochemical
#' oxygen demand are not in these services.
.gemstat_services <- c(
  "O2-Dis" = "GEMS_INDICATOR_STATIONS_DO",
  "TN" = "GEMS_INDICATOR_STATIONS_NITROGEN",
  "TP" = "GEMS_INDICATOR_STATIONS_PHOSPHORUS"
)

.gemstat_fields <- c(
  "GEMS_STATION_CODE",
  "STATION_NAME",
  "COUNTRY_CODE",
  "WATER_TYPE",
  "SAMPLE_YEAR",
  "PARAMETER_CODE",
  "PARAMETER_NAME",
  "MEAN_VALUE",
  "UNIT",
  "INDICATOR_CLASS"
)

.gemstat_chr <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
}

.gemstat_num <- function(x) {
  if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)
}

.gemstat_int <- function(x) {
  if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x)
}

.gemstat_empty_sf <- function() {
  sf::st_sf(
    GEMS_STATION_CODE = character(),
    STATION_NAME = character(),
    COUNTRY_CODE = character(),
    WATER_TYPE = character(),
    SAMPLE_YEAR = integer(),
    PARAMETER_CODE = character(),
    PARAMETER_NAME = character(),
    MEAN_VALUE = numeric(),
    UNIT = character(),
    INDICATOR_CLASS = numeric(),
    geometry = sf::st_sfc(crs = 4326)
  )
}

.gemstat_features_to_sf <- function(features) {
  if (length(features) == 0) {
    return(.gemstat_empty_sf())
  }

  rows <- lapply(features, function(feature) {
    if (is.null(feature$geometry) || is.null(feature$geometry$coordinates)) {
      return(NULL)
    }
    props <- feature$properties
    coords <- feature$geometry$coordinates
    tibble::tibble(
      GEMS_STATION_CODE = .gemstat_chr(props$GEMS_STATION_CODE),
      STATION_NAME = .gemstat_chr(props$STATION_NAME),
      COUNTRY_CODE = .gemstat_chr(props$COUNTRY_CODE),
      WATER_TYPE = .gemstat_chr(props$WATER_TYPE),
      SAMPLE_YEAR = .gemstat_int(props$SAMPLE_YEAR),
      PARAMETER_CODE = .gemstat_chr(props$PARAMETER_CODE),
      PARAMETER_NAME = .gemstat_chr(props$PARAMETER_NAME),
      MEAN_VALUE = .gemstat_num(props$MEAN_VALUE),
      UNIT = .gemstat_chr(props$UNIT),
      INDICATOR_CLASS = .gemstat_num(props$INDICATOR_CLASS),
      lon = .gemstat_num(coords[[1]]),
      lat = .gemstat_num(coords[[2]])
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(.gemstat_empty_sf())
  }

  sf::st_as_sf(
    dplyr::bind_rows(rows),
    coords = c("lon", "lat"),
    crs = 4326,
    remove = TRUE
  )
}

.gemstat_keep_latest <- function(stations) {
  if (nrow(stations) == 0) {
    return(stations)
  }

  stations %>%
    dplyr::group_by(.data$GEMS_STATION_CODE, .data$PARAMETER_CODE) %>%
    dplyr::filter(.data$SAMPLE_YEAR == max(.data$SAMPLE_YEAR, na.rm = TRUE)) %>%
    dplyr::ungroup()
}

.gemstat_perform <- function(
  service,
  where,
  page_size,
  offset,
  timeout_s,
  max_retries,
  verbose
) {
  url <- sprintf(
    "https://geoportal.bafg.de/arcgis3/rest/services/GEMSTAT/%s/MapServer/0/query",
    service
  )

  resp <- NULL
  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch(
      {
        httr2::request(url) |>
          httr2::req_user_agent("omniAPIr") |>
          httr2::req_timeout(timeout_s) |>
          httr2::req_url_query(
            where = where,
            outFields = paste(.gemstat_fields, collapse = ","),
            returnGeometry = "true",
            outSR = "4326",
            f = "geojson",
            resultRecordCount = page_size,
            resultOffset = offset,
            orderByFields = "OBJECTID"
          ) |>
          httr2::req_perform() |>
          httr2::resp_body_json(simplifyVector = FALSE)
      },
      error = function(e) {
        if (attempt < max_retries) {
          wait_time <- 2^attempt
          if (verbose) {
            message(sprintf(
              "GEMStat request failed (attempt %d/%d): %s. Retrying in %ds...",
              attempt,
              max_retries,
              conditionMessage(e),
              wait_time
            ))
          }
          Sys.sleep(wait_time)
          return(NULL)
        }
        stop(
          sprintf(
            "Failed to fetch GEMStat data after %d attempts: %s",
            max_retries,
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
    if (!is.null(resp)) {
      break
    }
  }

  if (!is.null(resp$error)) {
    message_text <- resp$error$message
    if (is.null(message_text) || !nzchar(message_text)) {
      message_text <- "GEMStat query failed"
    }
    stop(message_text, call. = FALSE)
  }

  resp
}

.gemstat_fetch_parameter <- function(
  parameter,
  iso3,
  page_size,
  timeout_s,
  max_retries,
  verbose
) {
  service <- .gemstat_services[[parameter]]
  where <- sprintf(
    "COUNTRY_CODE='%s' AND PARAMETER_CODE='%s'",
    iso3,
    parameter
  )
  if (verbose) {
    message("Requesting GEMStat ", parameter, " for ", iso3)
  }

  offset <- 0L
  pages <- list()
  repeat {
    resp <- .gemstat_perform(
      service = service,
      where = where,
      page_size = page_size,
      offset = offset,
      timeout_s = timeout_s,
      max_retries = max_retries,
      verbose = verbose
    )
    features <- resp$features
    if (is.null(features) || length(features) == 0) {
      break
    }
    pages[[length(pages) + 1]] <- features
    # A full page, or an explicit limit flag, means another page follows.
    # Advance by the rows received so a short page does not skip stations.
    if (!isTRUE(resp$exceededTransferLimit) && length(features) < page_size) {
      break
    }
    offset <- offset + length(features)
    if (offset > page_size * 40L) {
      stop(
        "GEMStat query exceeded 40 pages for ",
        where,
        call. = FALSE
      )
    }
  }

  if (length(pages) == 0) {
    return(.gemstat_empty_sf())
  }
  .gemstat_features_to_sf(unlist(pages, recursive = FALSE))
}

#' Download GEMStat water-quality station means
#'
#' Retrieves annual station means from the UNEP GEMS/Water GEMStat indicator
#' services for one country. Dissolved oxygen (`O2-Dis`) covers rivers, lakes,
#' reservoirs, wetlands, and groundwater. Total nitrogen (`TN`) and total
#' phosphorus (`TP`) cover rivers and lakes.
#'
#' @param iso3 ISO3 country code.
#' @param parameters Character vector of parameter codes. Defaults to
#'   dissolved oxygen, total nitrogen, and total phosphorus.
#' @param latest_only Logical. If `TRUE` (default), keep the latest year for
#'   each station and parameter.
#' @param verbose Logical. Print request progress. Default is `FALSE`.
#' @param max_retries Integer. Retry attempts for each page request. Default is 3.
#' @param timeout_s Numeric. Request timeout in seconds. Default is 60.
#' @param page_size Integer. Records per ArcGIS page. The service maximum is 5000.
#'
#' @return An `sf` point table in EPSG:4326. `WATER_TYPE` uses the GEMStat
#'   labels `River station`, `Lake station`, `Reservoir station`,
#'   `Wetland station`, and `Groundwater station`. Dissolved oxygen is in
#'   mg/l; total nitrogen and total phosphorus are in µg/l. An empty table is
#'   returned when the country has no open stations for the requested parameters.
#'
#' @export
get_gemstat_data <- function(
  iso3,
  parameters = c("O2-Dis", "TN", "TP"),
  latest_only = TRUE,
  verbose = FALSE,
  max_retries = 3,
  timeout_s = 60,
  page_size = 5000
) {
  if (missing(iso3) || length(iso3) != 1 || !is.character(iso3)) {
    stop("`iso3` must be one ISO3 country code.", call. = FALSE)
  }
  iso3 <- toupper(iso3)
  if (!grepl("^[A-Z]{3}$", iso3)) {
    stop("`iso3` must be one ISO3 country code.", call. = FALSE)
  }
  if (!is.character(parameters) || length(parameters) == 0) {
    stop("`parameters` must be a non-empty character vector.", call. = FALSE)
  }

  parameters <- unique(parameters)
  unknown <- setdiff(parameters, names(.gemstat_services))
  if (length(unknown) > 0) {
    stop(
      "`parameters` must be one or more of: ",
      paste(names(.gemstat_services), collapse = ", "),
      ". Unknown: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(page_size) || length(page_size) != 1 || page_size < 1 || page_size > 5000) {
    stop("`page_size` must be between 1 and 5000.", call. = FALSE)
  }

  stations <- lapply(
    parameters,
    .gemstat_fetch_parameter,
    iso3 = iso3,
    page_size = as.integer(page_size),
    timeout_s = timeout_s,
    max_retries = max_retries,
    verbose = verbose
  )
  stations <- Filter(function(x) nrow(x) > 0, stations)
  if (length(stations) == 0) {
    return(.gemstat_empty_sf())
  }
  stations <- do.call(rbind, stations)
  if (isTRUE(latest_only)) {
    stations <- .gemstat_keep_latest(stations)
  }
  stations
}
