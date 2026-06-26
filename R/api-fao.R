# Environment to cache the FAOSTAT guest token
.faostat_token_cache <- new.env(parent = emptyenv())
.faostat_token_cache$token <- NULL
.faostat_token_cache$expires_at <- NULL

#' Fetch a guest token for the FAOSTAT API
#' @return Character. A valid Bearer token string.
#' @keywords internal
.get_faostat_token <- function(verbose = FALSE) {
  # Return cached token if still valid (with 60s buffer)
  if (!is.null(.faostat_token_cache$token) &&
      !is.null(.faostat_token_cache$expires_at) &&
      Sys.time() < .faostat_token_cache$expires_at) {
    if (verbose) message("Using cached FAOSTAT guest token")
    return(.faostat_token_cache$token)
  }

  if (verbose) message("Fetching new FAOSTAT guest token...")

  token <- NULL
  for (attempt in 1:3) {
    tryCatch({
      resp <- httr2::request("https://faostatservices.fao.org/api/v1/auth/guest") |>
        httr2::req_headers(
          Accept = "application/json, text/javascript, */*; q=0.01",
          Origin = "https://www.fao.org",
          Referer = "https://www.fao.org/",
          `User-Agent` = "Mozilla/5.0"
        ) |>
        httr2::req_perform()
      body <- httr2::resp_body_json(resp)
      token <- body$token
      break
    }, error = function(e) {
      if (attempt < 3) {
        if (verbose) message(sprintf("Token request failed (attempt %d/3), retrying in %ds...", attempt, 2^attempt))
        Sys.sleep(2^attempt)
      }
    })
  }

  if (is.null(token) || token == "") {
    stop("Failed to obtain FAOSTAT guest token after 3 attempts. The FAOSTAT API may be temporarily unavailable.")
  }

  # Decode expiry from JWT payload (middle segment)
  parts <- strsplit(token, "\\.")[[1]]
  payload_b64 <- parts[2]
  # Add base64 padding if needed
  pad <- nchar(payload_b64) %% 4
  if (pad > 0) payload_b64 <- paste0(payload_b64, strrep("=", 4 - pad))
  payload <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(payload_b64)))
  exp_time <- as.POSIXct(payload$exp, origin = "1970-01-01", tz = "UTC")

  # Cache with 60-second safety buffer
  .faostat_token_cache$token <- token
  .faostat_token_cache$expires_at <- exp_time - 60

  if (verbose) {
    message(sprintf("FAOSTAT token cached, expires at %s UTC", format(exp_time, "%H:%M:%S")))
  }

  token
}

.faostat_api_get <- function(path, query = list(), verbose = FALSE, max_retries = 3) {
  base_url <- paste0("https://faostatservices.fao.org/api/v1/en/", path)
  query <- query[!vapply(query, is.null, logical(1))]

  if (length(query) > 0) {
    qs <- paste0(
      names(query),
      "=",
      vapply(query, utils::URLencode, "", reserved = TRUE),
      collapse = "&"
    )
    url <- paste0(base_url, "?", qs)
  } else {
    url <- base_url
  }

  if (verbose) {
    message(sprintf("FAOSTAT API URL: %s", url))
  }

  token <- .get_faostat_token(verbose = verbose)
  for (attempt in seq_len(max_retries)) {
    response <- tryCatch(
      httr2::request(url) |>
        httr2::req_headers(
          Authorization = paste("Bearer", token),
          Origin = "https://www.fao.org",
          Referer = "https://www.fao.org/",
          `User-Agent` = "Mozilla/5.0"
        ) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(),
      error = function(e) e
    )

    if (!inherits(response, "error") && httr2::resp_status(response) == 200) {
      return(httr2::resp_body_json(response, simplifyVector = TRUE))
    }

    status_or_error <- if (inherits(response, "error")) {
      conditionMessage(response)
    } else {
      httr2::resp_status(response)
    }

    if (!inherits(response, "error") &&
        httr2::resp_status(response) %in% c(401, 403)) {
      .faostat_token_cache$token <- NULL
      .faostat_token_cache$expires_at <- NULL
      token <- .get_faostat_token(verbose = verbose)
    }

    if (attempt < max_retries) {
      if (verbose) {
        message(sprintf(
          "Request failed (attempt %d/%d): %s. Retrying...",
          attempt,
          max_retries,
          status_or_error
        ))
      }
      Sys.sleep(2^attempt)
    }
  }

  stop(
    sprintf(
      "Failed to fetch FAOSTAT metadata after %d attempts from %s",
      max_retries,
      url
    ),
    call. = FALSE
  )
}

.faostat_data_frame <- function(response) {
  if (is.null(response$data)) {
    return(data.frame())
  }
  if (is.data.frame(response$data)) {
    return(response$data)
  }
  tibble::as_tibble(response$data)
}

.clean_faostat_aggregate_label <- function(label) {
  label <- sub(" \\+ \\(Total\\)$", "", label)
  label <- sub(" > \\(List\\)$", "", label)
  label
}

#' List FAOSTAT Items
#'
#' @description
#' Lists item codes and names for a FAOSTAT database. For Food Balance Sheets
#' (FBS), aggregate food groups such as "Fish, Seafood" are returned from the
#' FAOSTAT aggregate item code list.
#'
#' @param database Character. FAOSTAT database/domain code. Default is "FBS".
#' @param item_cs Character. Optional item coding system. Defaults to "FBS"
#'   for FBS item listings, otherwise NULL.
#' @param include_aggregates Logical. If TRUE, include aggregate item codes from
#'   the `itemsagg` code list where available. Default is TRUE.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A data.frame with item_code, item_name, code, label, aggregate_type,
#'   and code_list columns.
#' @export
#'
#' @examples
#' \dontrun{
#' fbs_items <- list_faostat_items("FBS")
#' subset(fbs_items, grepl("Fish", item_name))
#' }
list_faostat_items <- function(
  database = "FBS",
  item_cs = NULL,
  include_aggregates = TRUE,
  verbose = FALSE,
  max_retries = 3
) {
  if (is.null(database) || !nzchar(database)) {
    stop("database is required.", call. = FALSE)
  }

  if (is.null(item_cs) && toupper(database) == "FBS") {
    item_cs <- "FBS"
  }

  query <- list(show_lists = "false", item_cs = item_cs)
  items <- .faostat_data_frame(.faostat_api_get(
    paste0("codes/items/", database, "/"),
    query = query,
    verbose = verbose,
    max_retries = max_retries
  ))
  items$code_list <- "items"

  if (include_aggregates) {
    aggregates <- tryCatch(
      .faostat_data_frame(.faostat_api_get(
        paste0("codes/itemsagg/", database, "/"),
        query = query,
        verbose = verbose,
        max_retries = max_retries
      )),
      error = function(e) data.frame()
    )

    if (nrow(aggregates) > 0) {
      aggregates$code_list <- "itemsagg"
      items <- dplyr::bind_rows(items, aggregates)
    }
  }

  if (nrow(items) == 0) {
    return(items)
  }

  items %>%
    dplyr::mutate(
      item_code = as.character(code),
      item_name = .clean_faostat_aggregate_label(label)
    ) %>%
    dplyr::select(
      item_code,
      item_name,
      code,
      label,
      dplyr::everything()
    )
}

#' List FAOSTAT Metadata
#'
#' @description
#' Lists FAOSTAT databases, elements, or items from the live FAOSTAT API.
#'
#' @param type Character. Metadata type: "databases", "elements", or "items".
#' @param database Character. FAOSTAT database/domain code. Required for
#'   "elements" and "items".
#' @param item_cs Character. Optional item coding system passed to item listing.
#' @param include_aggregates Logical. If TRUE, include aggregate item codes for
#'   item listings where available. Default is TRUE.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A data.frame containing code and label columns, plus metadata-specific
#'   code/name columns.
#' @export
#'
#' @examples
#' \dontrun{
#' databases <- list_faostat_metadata("databases")
#' fbs_elements <- list_faostat_metadata("elements", database = "FBS")
#' fbs_items <- list_faostat_metadata("items", database = "FBS")
#' }
list_faostat_metadata <- function(
  type = c("databases", "elements", "items"),
  database = NULL,
  item_cs = NULL,
  include_aggregates = TRUE,
  verbose = FALSE,
  max_retries = 3
) {
  type <- match.arg(type)

  if (type == "databases") {
    databases <- .faostat_data_frame(.faostat_api_get(
      "groupsanddomains",
      verbose = verbose,
      max_retries = max_retries
    ))

    return(databases %>%
      dplyr::mutate(
        code = domain_code,
        label = domain_name,
        database_code = domain_code,
        database_name = domain_name
      ) %>%
      dplyr::select(
        code,
        label,
        database_code,
        database_name,
        dplyr::everything()
      ))
  }

  if (is.null(database) || !nzchar(database)) {
    stop("database is required for FAOSTAT elements and items.", call. = FALSE)
  }

  if (type == "elements") {
    elements <- .faostat_data_frame(.faostat_api_get(
      paste0("codes/elements/", database, "/"),
      query = list(show_lists = "false"),
      verbose = verbose,
      max_retries = max_retries
    ))

    return(elements %>%
      dplyr::mutate(
        element_code = as.character(code),
        element_name = label
      ) %>%
      dplyr::select(
        code,
        label,
        element_code,
        element_name,
        dplyr::everything()
      ))
  }

  list_faostat_items(
    database = database,
    item_cs = item_cs,
    include_aggregates = include_aggregates,
    verbose = verbose,
    max_retries = max_retries
  )
}

#' Get FAOSTAT Agriculture Data
#'
#' @description
#' Retrieves agricultural statistics from the Food and Agriculture Organization
#' Statistics (FAOSTAT) API with built-in lookup tables for common items.
#'
#' **Note:** This function is designed for specific datasets and is not comprehensive.
#' It works best with QCL, FBS, FS, CAHD, RFN, RFB, RP, and RL databases and common agricultural elements (2111, 2413, 2510, 2515, 7209, 664, 674, 645, 6120).
#'
#' @importFrom magrittr %>%
#'
#' @param element Character or numeric. Element code specifying the type of
#'   measurement (e.g., "2111" for livestock stocks, "2413" for crop yield, "2510" for production quantity, "6120" for value indicators).
#' @param item Character or numeric. Item code(s) or common names. When use_lookup
#'   is TRUE, accepts friendly names like "cattle", "wheat", "healthy_diet", etc.
#' @param database Character. FAOSTAT database name (required).
#' @param item_cs Character. Item classification system. Default is NULL.
#' @param use_lookup Logical. Whether to use built-in lookup tables to convert
#'   common names to item codes. Default is TRUE.
#' @param mrv Integer. Most Recent Values - number of years to retrieve. Default is 50.
#' @param iso3 Character. ISO3 country code to filter data. Default is NULL (all countries).
#' @param release Character. Release version for CAHD database (e.g., "7S2025" for July 2025).
#'   Default is NULL (auto-set to "7S2025" for CAHD).
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A data.frame with columns: isocode, item_code, Item, element_code,
#'   element_name, Year, Value, Unit.
#'
#' @details
#' API Documentation: \url{https://www.fao.org/faostat/en/#data}
#'
#' **IMPORTANT: This function is NOT comprehensive and only supports specific datasets.**
#' It currently works with a limited set of FAOSTAT databases and elements.
#'
#' **Currently Supported Data:**
#' \itemize{
#'   \item \strong{Database: QCL (Crops and Livestock Products)} - Primary supported database
#'   \item \strong{Database: FBS (Food Balance Sheets)} - Food supply and nutrition data
#'   \item \strong{Database: FS (Food Security)} - Food security indicators (3-year averages)
#'   \item \strong{Database: CAHD (Cost and Affordability of a Healthy Diet)} - Diet cost indicators
#'   \item \strong{Element 2111 (Stocks)} - Livestock population data
#'   \item \strong{Element 2413 (Yield)} - Crop yield data
#'   \item \strong{Element 2510 (Production Quantity)} - Crop and fertilizer production data where available
#'   \item \strong{Element 2515 (Agricultural Use)} - Fertilizer and pesticide use
#'   \item \strong{Element 7209 (Share in Land area)} - Land use share statistics
#'   \item \strong{Element 664 (Food supply kcal/capita/day)} - Dietary energy supply
#'   \item \strong{Element 674 (Protein supply quantity)} - Protein supply data
#'   \item \strong{Element 645 (Food supply quantity kg/capita/year)} - Food supply by food group
#'   \item \strong{Element 6120 (Cost indicators)} - Food security indicators (FS) and diet cost (CAHD)
#' }
#'
#' **Built-in lookup tables** support:
#' \itemize{
#'   \item \strong{Animals (Element 2111):} cattle, sheep, chicken, goats, pigs, horses, buffalo, camels, rabbits, ducks
#'   \item \strong{Crops (Elements 2413 and 2510, QCL database):} wheat, rice, maize, barley, oats, rye, millet, sorghum, soybeans, sunflower, rapeseed, cotton, sugarcane, sugar_beet, potatoes, cassava
#'   \item \strong{Fertilizer nutrients (Element 2515, RFN database):} nitrogen, phosphate, potash
#'   \item \strong{Fertilizer products (Elements 2510 and 2515, RFB database):} npk, urea, ammonium_sulfate
#'   \item \strong{Pesticides (Element 2515, RP database):} pesticides_total
#'   \item \strong{Land use (Element 7209, RL database):} agricultural_land, forest_land
#'   \item \strong{Food Balance Sheets (Elements 645, 664, 674):} grand_total, total, cereals, starchy_roots, pulses, treenuts, vegetables, fruits, eggs, meat, fish, milk, vegetable_oils (aggregated food groups)
#'   \item \strong{Food Security SDG indicators (Element 6120 - FS database):} undernourishment/pou/sdg_2_1_1 (SDG 2.1.1), food_insecurity/fies/sdg_2_1_2 (SDG 2.1.2), cereals_roots_tubers (3-year averages)
#'   \item \strong{CAHD Cost Indicators (Element 6120 - CAHD database):} healthy_diet, starchy_staples, animal_source_food, vegetables, fruits (cost in PPP$/person/day)
#'   \item \strong{CAHD Affordability Indicators (Element 6120 - CAHD database):} people_unable_afford/nua_millions (millions), percent_unable_afford/pua_percent (%)
#' }
#'
#' **Limitations:**
#' \itemize{
#'   \item Only works with specific element codes (2111, 2413, 2510, 2515, 7209, 664, 674, 645, 6120)
#'   \item Primarily designed for QCL, FBS, FS, CAHD, RFN, RFB, RP, and RL databases
#'   \item Lookup tables are limited to common agricultural items
#'   \item For other databases/elements, use \code{use_lookup = FALSE} and provide exact codes
#'   \item FS database uses 3-year averages (e.g., 2000 represents 2000-2002)
#'   \item CAHD database requires release parameter (auto-set to "7S2025" if not specified)
#' }
#'
#' **Discovering Available Data:**
#'
#' Use \code{list_faostat_metadata()} to discover available databases, elements, and items:
#' \itemize{
#'   \item \code{list_faostat_metadata("databases")} - List all databases
#'   \item \code{list_faostat_metadata("elements", database = "QCL")} - List elements
#'   \item \code{list_faostat_metadata("items", database = "QCL")} - List items
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Discover available data using helper function
#' databases <- list_faostat_metadata("databases")
#' qcl_elements <- list_faostat_metadata("elements", database = "QCL")
#' qcl_items <- list_faostat_metadata("items", database = "QCL")
#'
#' # Get cattle population data using lookup
#' cattle_data <- get_faostat_data(
#'   element = "2111",
#'   item = "cattle",
#'   database = "QCL",
#'   iso3 = "KEN",
#'   mrv = 20
#' )
#'
#' # Get crop yield for multiple crops
#' crop_data <- get_faostat_data(
#'   element = "2413",
#'   item = c("wheat", "maize", "rice"),
#'   database = "QCL"
#' )
#'
#' # Use item codes directly (without lookup)
#' wheat_data <- get_faostat_data(
#'   element = "2510",
#'   item = "15",
#'   database = "QCL",
#'   use_lookup = FALSE
#' )
#'
#' # Get Food Balance Sheets data (dietary energy supply)
#' fbs_data <- get_faostat_data(
#'   element = "664",
#'   item = "grand_total",
#'   database = "FBS",
#'   iso3 = "KEN",
#'   mrv = 20
#' )
#'
#' # Get Food Balance Sheets data (protein supply and dietary energy)
#' fbs_nutrition <- get_faostat_data(
#'   element = "664,674",
#'   item = "grand_total",
#'   database = "FBS",
#'   iso3 = "KEN"
#' )
#'
#' # Get Food Balance Sheets food supply quantity (kg/capita/year) by food group
#' fbs_food_supply <- get_faostat_data(
#'   element = "645",
#'   item = c("cereals", "meat", "fish", "vegetables", "fruits"),
#'   database = "FBS",
#'   iso3 = "KEN",
#'   mrv = 20
#' )
#'
#' # Get Food Security data (3-year averages)
#' fs_data <- get_faostat_data(
#'   element = "6120",
#'   item = "cereals_roots_tubers",
#'   database = "FS",
#'   iso3 = "KEN",
#'   mrv = 20
#' )
#'
#' # Get SDG 2.1.1 - Prevalence of undernourishment (%)
#' pou_data <- get_faostat_data(
#'   element = "6120",
#'   item = "undernourishment",  # or "pou" or "sdg_2_1_1"
#'   database = "FS",
#'   iso3 = "KEN",
#'   mrv = 20
#' )
#'
#' # Get SDG 2.1.2 - Prevalence of moderate or severe food insecurity (%)
#' fies_data <- get_faostat_data(
#'   element = "6120",
#'   item = "food_insecurity",  # or "fies" or "sdg_2_1_2"
#'   database = "FS",
#'   iso3 = "KEN",
#'   mrv = 20
#' )
#'
#' # Get both SDG food security indicators together
#' sdg_food <- get_faostat_data(
#'   element = "6120",
#'   item = c("undernourishment", "food_insecurity"),
#'   database = "FS",
#'   iso3 = "KEN"
#' )
#'
#' # Get Cost and Affordability of a Healthy Diet (CAHD) data - cost indicators
#' cahd_cost <- get_faostat_data(
#'   element = "6120",
#'   item = c("healthy_diet", "starchy_staples", "animal_source_food", "vegetables", "fruits"),
#'   database = "CAHD",
#'   iso3 = "KEN",
#'   mrv = 10
#' )
#'
#' # Get CAHD affordability indicators - number and percent unable to afford
#' cahd_afford <- get_faostat_data(
#'   element = "6120",
#'   item = c("people_unable_afford", "percent_unable_afford"),
#'   database = "CAHD",
#'   iso3 = "KEN",
#'   mrv = 10
#' )
#'
#' # Get all CAHD indicators for a country
#' cahd_all <- get_faostat_data(
#'   element = "6120",
#'   item = c("healthy_diet", "people_unable_afford", "percent_unable_afford"),
#'   database = "CAHD",
#'   iso3 = "KEN"
#' )
#' }
get_faostat_data <- function(
  element,
  item,
  database = NULL,
  item_cs = NULL,
  use_lookup = TRUE,
  mrv = 50,
  iso3 = NULL,
  release = NULL,
  verbose = FALSE,
  max_retries = 3
) {
  if (verbose) {
    message(sprintf("Automatically fetching last %d years of data", mrv))
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- (current_year - mrv + 1):current_year

  # Auto-set item_cs for FBS database if not specified
  if (is.null(item_cs) && !is.null(database) && toupper(database) == "FBS") {
    item_cs <- "FBS"
  }

  # Auto-set release for CAHD database if not specified.
  if (is.null(release) && !is.null(database) && toupper(database) == "CAHD") {
    release <- "7S2025"
    if (verbose) {
      message(sprintf("Using CAHD release: %s", release))
    }
  }

  # Item lookup tables (defined once, outside inner function)
  animal_lookup <- list(
    "cattle" = "866",
    "sheep" = "976",
    "chicken" = "1057",
    "chickens" = "1057",
    "goats" = "1016",
    "goat" = "1016",
    "pigs" = "1034",
    "swine" = "1034",
    "horses" = "1096",
    "buffalo" = "946",
    "camels" = "1126",
    "rabbits" = "1140",
    "rabbits_hares" = "1140",
    "ducks" = "1068"
  )

  crop_lookup <- list(
    "wheat" = "15",
    "rice" = "27",
    "maize" = "56",
    "barley" = "44",
    "oats" = "75",
    "rye" = "71",
    "millet" = "79",
    "sorghum" = "83",
    "soybeans" = "236",
    "sunflower" = "267",
    "rapeseed" = "270",
    "cotton" = "328",
    "sugarcane" = "156",
    "sugar_beet" = "157",
    "potatoes" = "116",
    "cassava" = "125"
  )

  fertilizer_nutrient_lookup <- list(
    "nitrogen" = "3102",
    "nutrient_nitrogen" = "3102",
    "phosphate" = "3103",
    "phosphorus" = "3103",
    "nutrient_phosphate" = "3103",
    "potash" = "3104",
    "potassium" = "3104",
    "nutrient_potash" = "3104"
  )

  fertilizer_product_lookup <- list(
    "npk" = "4021",
    "npk_fertilizers" = "4021",
    "urea" = "4001",
    "ammonium_sulfate" = "4002",
    "ammonium_sulphate" = "4002"
  )

  pesticide_lookup <- list(
    "pesticides_total" = "1357",
    "pesticides" = "1357",
    "total" = "1357"
  )

  land_use_lookup <- list(
    "agricultural_land" = "6610",
    "forest_land" = "6646"
  )

  fbs_lookup <- list(
    "grand_total" = "2901",
    "total" = "2901",
    "cereals" = "2905",
    "cereals_excluding_beer" = "2905",
    "starchy_roots" = "2907",
    "roots" = "2907",
    "pulses" = "2911",
    "treenuts" = "2912",
    "tree_nuts" = "2912",
    "vegetables" = "2918",
    "fruits" = "2919",
    "fruits_excluding_wine" = "2919",
    "meat" = "2943",
    "milk" = "2948",
    "milk_excluding_butter" = "2948",
    "eggs" = "2949",
    "vegetable_oils" = "2914",
    "veg_oils" = "2914",
    "fish" = "2960",
    "fish_seafood" = "2960",
    "seafood" = "2960"
  )

  fs_lookup <- list(
    # SDG indicators
    "undernourishment" = "21004",
    "pou" = "21004",
    "sdg_2_1_1" = "21004",
    "prevalence_undernourishment" = "21004",
    "food_insecurity" = "21009",
    "moderate_severe_food_insecurity" = "21009",
    "sdg_2_1_2" = "21009",
    "fies" = "21009",
    # Dietary energy indicators
    "cereals_roots_tubers" = "21012",
    "dietary_energy_cereals" = "21012",
    "share_cereals" = "21012"
  )

  # CAHD (Cost and Affordability of a Healthy Diet) lookup - Element 6120
  # Cost indicators in PPP dollars per person per day
  # Affordability indicators (NUA = Number Unable to Afford, PUA = Percent Unable to Afford)
  cahd_lookup <- list(
    # Cost indicators (PPP$/person/day)
    "healthy_diet" = "7004",
    "cost_healthy_diet" = "7004",
    "starchy_staples" = "7007",
    "cost_starchy_staples" = "7007",
    "animal_source_food" = "7008",
    "cost_animal_food" = "7008",
    "vegetables" = "7010",
    "cost_vegetables" = "7010",
    "fruits" = "7011",
    "cost_fruits" = "7011",
    # Affordability indicators
    "people_unable_afford" = "7006",
    "nua_millions" = "7006",
    "number_unable_afford" = "7006",
    "percent_unable_afford" = "7005",
    "pua_percent" = "7005",
    "share_unable_afford" = "7005"
  )

  # Helper function to try fetching data with specific years
  try_fetch_data <- function(years_to_try) {
    # FAOSTAT Item Code Lookup - uses lookup tables from outer scope
    if (use_lookup && is.character(item)) {
      # Determine which lookup table to use based on element
      # Handle multiple elements (comma-separated) by checking if any match
      element_str <- as.character(element)
      element_codes <- base::trimws(base::strsplit(element_str, ",")[[1]])

      lookup_table <- NULL
      for (elem_code in element_codes) {
        # Check database to distinguish between FS and CAHD (both use element 6120)
        if (
          elem_code == "6120" &&
            !is.null(database) &&
            toupper(database) == "CAHD"
        ) {
          temp_table <- cahd_lookup # CAHD cost indicators
        } else {
          temp_table <- switch(
            elem_code,
            "2111" = animal_lookup, # Livestock
            "2413" = crop_lookup, # Crop yield
            "2510" = if (!is.null(database) && toupper(database) == "RFB") {
              fertilizer_product_lookup # Fertilizer production by product
            } else if (!is.null(database) && toupper(database) == "RFN") {
              fertilizer_nutrient_lookup # Fertilizer production by nutrient
            } else {
              crop_lookup # Crop production quantity (QCL)
            },
            "2515" = if (!is.null(database) && toupper(database) == "RFB") {
              fertilizer_product_lookup # Agricultural use by fertilizer product
            } else if (!is.null(database) && toupper(database) == "RFN") {
              fertilizer_nutrient_lookup # Agricultural use by fertilizer nutrient
            } else if (!is.null(database) && toupper(database) == "RP") {
              pesticide_lookup # Agricultural use by pesticide category
            } else {
              c(fertilizer_nutrient_lookup, fertilizer_product_lookup, pesticide_lookup)
            },
            "7209" = land_use_lookup, # Land use
            "664" = fbs_lookup, # Food supply (kcal/capita/day) - FBS aggregated food groups
            "674" = fbs_lookup, # Protein supply quantity - FBS aggregated food groups
            "645" = fbs_lookup, # Food supply quantity (kg/capita/year) - FBS aggregated food groups
            "6120" = fs_lookup, # Food Security indicators (3-year averages) - FS database
            NULL
          )
        }
        if (!is.null(temp_table)) {
          lookup_table <- temp_table
          break # Use first matching lookup table
        }
      }

      if (!is.null(lookup_table)) {
        # Handle both character vectors and comma-separated strings
        if (length(item) > 1) {
          # item is already a character vector
          item_names <- base::trimws(item)
        } else {
          # item is a single string, split by comma if multiple items
          item_names <- base::trimws(base::strsplit(item, ",")[[1]])
        }

        # Check for invalid items
        invalid_items <- item_names[!item_names %in% names(lookup_table)]
        if (length(invalid_items) > 0) {
          valid_options <- paste(names(lookup_table), collapse = ", ")
          stop(paste(
            "Invalid item(s) for element",
            element,
            ":",
            paste(invalid_items, collapse = ", "),
            "\nValid options are:",
            valid_options
          ))
        }

        # Convert names to codes
        item_codes <- lookup_table[item_names]
        item <- paste(item_codes, collapse = ",")
      }
    }

    # elements:
    # 2111 = Stocks Live animals
    # 2413 = crop yield
    # 2510 = Production Quantity
    # 2515 = Agricultural Use (for fertilizers and pesticides)
    # 7209 = Share in Land area
    # 664 = Food supply (kcal/capita/day) - FBS
    # 674 = Protein supply quantity - FBS
    # 645 = Food supply quantity (kg/capita/year) - FBS
    # 6120 = Food Security indicators (3-year averages) - FS / Cost indicators (PPP$/person/day) - CAHD
    # Items
    #  "866,1057,1016,976" Cattle, sheep, chicken and goats
    # "572,176,661,56,79,92,270,27,71,83,236,156,97,15"  several crops
    # "6610,6646" # agricultural land and forestry
    # "3102,3103,3104" # RFN fertilizer nutrients
    # "4021,4001,4002" # RFB fertilizer products
    # "1357" # RP pesticides total
    # "2901" # Grand Total + (Total) - FBS
    # "2905,2907,2911,2912,2918,2919,2943,2948,2949,2914,2960" # FBS Food groups
    # "21004" # FS: Prevalence of undernourishment (SDG 2.1.1) (%) - 3-year average
    # "21009" # FS: Prevalence of moderate or severe food insecurity (SDG 2.1.2) (%) - 3-year average
    # "21012" # FS: Share of dietary energy from cereals, roots, and tubers (%) - 3-year average
    # "7004,7007,7008,7010,7011" # CAHD cost indicators (healthy diet, starchy staples, animal food, vegetables, fruits)
    # "7006" # CAHD: Number of people unable to afford a healthy diet (NUA), millions
    # "7005" # CAHD: Percent of population unable to afford a healthy diet (PUA), %
    base <- paste0("https://faostatservices.fao.org/api/v1/en/data/", database)

    # Set area parameter based on whether iso3 is specified
    # IMPORTANT: API requires FAOSTAT area codes in area, even when area_cs=ISO3.
    area_param <- if (!is.null(iso3)) {
      iso3_values <- toupper(iso3)
      faostat_codes <- countrycode::countrycode(
        iso3_values,
        origin = "iso3c",
        destination = "fao",
        warn = FALSE
      )

      if (any(is.na(faostat_codes))) {
        stop(paste(
          "Invalid ISO3 code:",
          paste(iso3_values[is.na(faostat_codes)], collapse = ", "),
          "\nCould not map ISO3 to a FAOSTAT area code with countrycode."
        ))
      }
      paste(faostat_codes, collapse = ",")
    } else {
      NULL
    }

    # Handle special year parameter for FS database (3-year averages)
    # FS uses year3 parameter with format like 20003 (for 2000-2002 average)
    if (!is.null(database) && toupper(database) == "FS") {
      # Convert years to 3-year average format (e.g., 2000 -> 20003)
      years_formatted <- paste0(years_to_try, "3")
      year_param_name <- "year3"
      year_param_value <- paste(years_formatted, collapse = ",")
    } else {
      year_param_name <- "year"
      year_param_value <- paste(years_to_try, collapse = ",")
    }

    params <- list(
      area_cs = "ISO3",
      element = element, #What kind of measurement/statistic you want (e.g., production, area harvested, yield, etc.)
      item = item #What specific products/crops/commodities you want data for (e.g., wheat, maize, rice, etc.)
    )

    if (!is.null(area_param)) {
      params$area <- area_param
    }

    # Add year parameter with appropriate name
    params[[year_param_name]] <- year_param_value

    # Add remaining parameters
    params$show_codes <- "true"
    params$show_unit <- "true"
    params$show_flags <- "true"
    params$show_notes <- "true"
    params$null_values <- "false"
    params$output_type <- "csv"
    params$caching <- "false"

    # Add item_cs only if specified (some databases don't use it)
    if (!is.null(item_cs)) {
      params$item_cs <- item_cs
    }

    # Add release parameter for CAHD database
    if (!is.null(release)) {
      params$release <- release
    }

    # build the query string
    qs <- paste0(
      names(params),
      "=",
      vapply(params, utils::URLencode, "", reserved = TRUE),
      collapse = "&"
    )

    # Build the full URL
    url <- paste0(base, "?", qs)

    if (verbose) {
      message(sprintf("API URL: %s", url))
      message(sprintf("Item code(s): %s", item))
      message(sprintf("Element code(s): %s", element))
    }

    # Get guest auth token
    token <- .get_faostat_token(verbose = verbose)

    # Fetch and parse the data with retry logic
    retry_attempt <- 1
    success <- FALSE
    response <- NULL

    while (retry_attempt <= max_retries && !success) {
      tryCatch(
        {
          response <- httr2::request(url) |>
            httr2::req_headers(
              Authorization = paste("Bearer", token),
              Origin = "https://www.fao.org",
              Referer = "https://www.fao.org/",
              `User-Agent` = "Mozilla/5.0"
            ) |>
            httr2::req_perform()
          success <- TRUE
        },
        error = function(e) {
          # On auth errors, invalidate cache and get a fresh token
          if (grepl("401|403|Unauthorized|Forbidden", conditionMessage(e))) {
            .faostat_token_cache$token <- NULL
            .faostat_token_cache$expires_at <- NULL
            token <<- .get_faostat_token(verbose = verbose)
            if (verbose) message("Token expired, refreshed and retrying...")
          }
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
                "Failed to fetch FAOSTAT data after %d attempts. Last error: %s",
                max_retries,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        }
      )
    }

    tryCatch(
      {
        if (httr2::resp_status(response) == 200) {
          content <- httr2::resp_body_string(response)

          if (nchar(content) > 0 && !grepl("^\\s*$", content)) {
            # Parse CSV data
            faostat_data <- utils::read.csv(
              textConnection(content),
              stringsAsFactors = FALSE
            )

            if (verbose) {
              message(sprintf("Raw data: %d rows", nrow(faostat_data)))
            }

            # Clean and standardize column names
            if (nrow(faostat_data) > 0) {
              # Rename columns for consistency
              faostat_data <- faostat_data %>%
                dplyr::rename_with(
                  ~ dplyr::case_when(
                    .x %in% c("Area.Code", "Area Code") ~ "area_code",
                    .x %in% c("Area", "area") ~ "area_name",
                    grepl("^Item\\.Code", .x) | .x %in% c("Item Code") ~ "item_code",
                    .x %in% c("Item", "item") ~ "item_name",
                    grepl("^Element\\.Code", .x) | .x %in% c("Element Code") ~ "element_code",
                    .x %in% c("Element", "element") ~ "element_name",
                    .x %in% c("Year", "year") ~ "year",
                    .x %in% c("Value", "value") ~ "value",
                    .x %in% c("Unit", "unit") ~ "unit",
                    .x %in% c("Flag", "flag") ~ "flag",
                    TRUE ~ .x
                  )
                )

              # Handle FS database special year format (e.g., "2014-2016" -> 2014)
              # FS database returns 3-year average periods like "2014-2016"
              if (!is.null(database) && toupper(database) == "FS") {
                # Extract start year from year range (e.g., "2014-2016" -> 2014)
                faostat_data <- faostat_data %>%
                  dplyr::mutate(
                    year_start = as.numeric(sub("-.*", "", year)),
                    year = year_start
                  ) %>%
                  dplyr::filter(year %in% years_to_try) %>%
                  dplyr::select(-year_start)
              } else {
                # Standard year filtering for other databases
                faostat_data <- faostat_data %>%
                  dplyr::filter(year %in% years_to_try)
              }

              faostat_data <- faostat_data %>%
                dplyr::mutate(
                  item_code = sub("^S", "", as.character(item_code)),
                  element_code = as.character(element_code),
                  value = as.numeric(value),
                  year = as.numeric(year)
                ) %>%
                dplyr::select(
                  Area.Code..ISO3.,
                  item_code,
                  item_name,
                  element_code,
                  element_name,
                  year,
                  value,
                  unit
                ) %>%
                dplyr::rename(
                  isocode = Area.Code..ISO3.,
                  Item = item_name,
                  Year = year,
                  Value = value,
                  Unit = unit
                )

              if (verbose) {
                message(sprintf("Filtered data: %d rows", nrow(faostat_data)))
              }
              return(faostat_data)
            } else {
              if (verbose) {
                message("No data in CSV response")
              }
              return(data.frame())
            }
          } else {
            if (verbose) {
              message("Empty response from API")
            }
            return(data.frame())
          }
        } else {
          # Get error details from response
          error_content <- httr2::resp_body_string(response)
          if (verbose) {
            message(sprintf("HTTP error %d", httr2::resp_status(response)))
            message(sprintf("Error: %s", error_content))
          }

          message(sprintf(
            "HTTP error %d from FAOSTAT API. Response: %s",
            httr2::resp_status(response),
            error_content
          ))
          return(data.frame())
        }
      },
      error = function(e) {
        if (verbose) {
          message(sprintf("Exception: %s", conditionMessage(e)))
        }
        message(sprintf("Error fetching FAOSTAT data: %s", conditionMessage(e)))
        return(data.frame())
      }
    )
  }

  # Try fetching data with the requested years first
  result <- try_fetch_data(years)

  # If no data returned, implement smart year discovery
  if (nrow(result) == 0) {
    if (verbose) {
      message(
        "No data found for requested years. Trying to discover available years..."
      )
    }

    # Try progressively expanded year ranges to find available data
    # Including more granular recent ranges for better data discovery
    year_ranges <- list(
      (current_year - 5):current_year, # Last 5 years (2020-2025)
      (current_year - 10):current_year, # Last 10 years (2015-2025)
      2010:2023, # FBS exact data range (2010-2023)
      (current_year - 15):current_year, # Last 15 years (2010-2025)
      (current_year - 20):current_year, # Last 20 years (2005-2025)
      (current_year - 25):current_year, # Last 25 years (2000-2025)
      (current_year - 30):current_year, # Last 30 years (1995-2025)
      (current_year - 35):current_year, # Last 35 years (1990-2025)
      1990:current_year, # Since 1990
      1985:current_year, # Since 1985
      1980:current_year, # Since 1980
      1975:current_year, # Since 1975
      1970:current_year, # Since 1970
      1965:current_year, # Since 1965
      1961:current_year # All available years since FAOSTAT start
    )

    for (year_range in year_ranges) {
      if (verbose) {
        message(sprintf(
          "Trying years: %d-%d",
          min(year_range),
          max(year_range)
        ))
      }
      result <- try_fetch_data(year_range)

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
      return(data.frame())
    }
  }

  message(sprintf(
    "✓ FAOSTAT data retrieved successfully: %s rows | Years: %d-%d",
    format(nrow(result), big.mark = ","),
    min(result$Year),
    max(result$Year)
  ))

  return(result)
}

#' Get FAO Forest Resources Assessment Data
#'
#' @description
#' Retrieves forest statistics from the FAO Forest Resources Assessment (FRA) API.
#'
#' @param data_type Character. Type of data: "forest_area" or "forest_change".
#' @param ref_year Numeric. Assessment reference year (e.g., 2020, 2025).
#' @param iso3 Character. ISO3 country code(s).
#' @param years Numeric vector. Years for forest area data.
#' @param years_groups Character vector. Year groups for forest change data
#'   (e.g., "2010-2015").
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A list containing the parsed JSON response from the FAO FRA API.
#'   For `forest_change`, values are returned under
#'   `forestAreaNetChangeFrom1a`.
#'
#' @details
#' API Documentation: \url{https://fra-data.fao.org/static/assets/fra-api-swagger.json}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get forest area data
#' fra_area <- get_fao_fra_data(
#'   data_type = "forest_area",
#'   ref_year = 2020,
#'   iso3 = "KEN",
#'   years = c(2000, 2010, 2020),
#'   years_groups = NULL
#' )
#' }
get_fao_fra_data <- function(
  data_type,
  ref_year,
  iso3,
  years,
  years_groups,
  verbose = FALSE,
  max_retries = 3
) {
  # Base URL for FAO FRA API
  base_url <- "https://fra-data.fao.org/api/cycle-data/table/table-data"

  if (verbose) {
    message(sprintf(
      "Fetching FAO FRA data (type: %s, year: %s)",
      data_type,
      ref_year
    ))
  }

  params <- switch(
    data_type,
    forest_area = list(
      countryIso = "WO",
      assessmentName = "fra",
      cycleName = as.character(ref_year),
      tableNames = "extentOfForest",
      countryISOs = iso3,
      variables = "forestArea",
      columns = as.character(years)
    ),
    forest_change = list(
      countryIso = "WO",
      assessmentName = "fra",
      cycleName = as.character(ref_year),
      tableNames = "forestAreaChange",
      countryISOs = iso3,
      variables = "forestAreaNetChangeFrom1a",
      columns = years_groups
    ),
    stop(
      "Invalid data_type. Must be 'forest_area' or 'forest_change'.",
      call. = FALSE
    )
  )

  format_param <- function(name, value) {
    if (
      length(value) > 1 ||
        name %in% c("tableNames", "countryISOs", "variables", "columns")
    ) {
      paste(
        sapply(value, function(v) {
          paste0(name, "[]=", v)
        }),
        collapse = "&"
      )
    } else {
      paste0(name, "=", value)
    }
  }

  query <- paste0(
    base_url,
    "?",
    paste(
      unlist(
        mapply(format_param, names(params), params, SIMPLIFY = FALSE)
      ),
      collapse = "&"
    )
  )

  # Retry logic
  retry_attempt <- 1
  success <- FALSE
  response <- NULL

  while (retry_attempt <= max_retries && !success) {
    tryCatch(
      {
        response <- httr2::request(query) |>
          httr2::req_perform()

        if (httr2::resp_status(response) == 200) {
          success <- TRUE
        } else {
          stop(sprintf("HTTP error %d", httr2::resp_status(response)))
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
          stop(
            sprintf(
              "Failed to retrieve FAO FRA data after %d attempts. Last error: %s",
              max_retries,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      }
    )
  }

  result <- jsonlite::fromJSON(httr2::resp_body_string(response))
  message(sprintf("✓ FAO FRA data retrieved successfully"))

  return(result)
}


# Format a vector as the comma-separated filter value expected by the EMPRES
# public endpoint. NULL and empty values are omitted from the request.
.empres_collapse_filter <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }

  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return(NULL)
  }

  paste(x, collapse = ",")
}

.empres_country_names <- function(country_iso3) {
  if (is.null(country_iso3) || length(country_iso3) == 0) {
    return(NULL)
  }

  iso3 <- toupper(as.character(country_iso3))
  countries <- countrycode::countrycode(
    iso3,
    origin = "iso3c",
    destination = "country.name.en",
    warn = FALSE
  )

  missing <- iso3[is.na(countries)]
  if (length(missing) > 0) {
    stop(
      "Country ISO3 code(s) not recognized: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  unname(countries)
}

.empres_valid_diseases <- function() {
  c(
    "African horse sickness",
    "African swine fever",
    "Akabane Disease",
    "Alaskapox virus",
    "American foulbrood of honey bees",
    "Anthrax",
    "Arena virus",
    "Argentine hemorrhagic fever (Junin virus)",
    "Atkinsiella spp",
    "Aujeszky's disease",
    "Avian chlamydiosis",
    "Avian infectious bronchitis",
    "Avian infectious laryngotracheitis",
    "Avian mycoplasmosis (M. gallisepticum)",
    "Avian mycoplasmosis (M. synoviae)",
    "Babesia spp",
    "Blackleg",
    "Bluetongue",
    "Botulism",
    "Bovine anaplasmosis",
    "Bovine babesiosis",
    "Bovine genital campylobacteriosis",
    "Bovine spongiform encephalopathy",
    "Bovine tuberculosis",
    "Bovine viral diarrhoea",
    "Brucellosis",
    "Brucellosis (Brucella abortus)",
    "Brucellosis (Brucella melitensis)",
    "Brucellosis (Brucella suis)",
    "Camelpox",
    "Caprine arthritis/encephalitis",
    "Chronic respiratory disease",
    "Classical swine fever",
    "Colibacillosis",
    "Contagious agalactia",
    "Contagious bovine pleuropneumonia",
    "Contagious caprine pleuropneumonia",
    "Contagious Ecthyma",
    "Contagious equine metritis",
    "COVID-19 (SARS-COV-2)",
    "Crimean Congo haemorrhagic fever",
    "Dourine",
    "Duck virus enteritis",
    "Duck virus hepatitis",
    "East Coast fever",
    "Ebola Virus",
    "Ebola-Reston",
    "Echinococcosis/hydatidosis",
    "Enterotoxemia",
    "Enzootic abortion of ewes (ovine chlamydiosis)",
    "Enzootic bovine leukosis",
    "Epizootic haemorrhagic disease",
    "Epizootic ulcerative syndrome",
    "Equine encephalomyelitis (Eastern)",
    "Equine encephalomyelitis (Western)",
    "Equine infectious anaemia",
    "Equine piroplasmosis",
    "Equine rhinopneumonitis",
    "Equine viral arteritis",
    "European foulbrood of honey bees",
    "Foot and mouth disease",
    "Fowl cholera",
    "Fowl typhoid",
    "Glanders",
    "Haemorrhagic Septicaemia",
    "Heartwater",
    "Hendra Virus Disease",
    "Infectious bovine rhinotracheitis",
    "Infectious Bronchitis",
    "Infectious bursal disease (Gumboro disease)",
    "Influenza - Avian",
    "Influenza - Equine",
    "Intoxication",
    "Japanese Encephalitis",
    "Kunjin virus",
    "Kyasanur forest disease",
    "Leishmaniosis",
    "Leprosy (Hansen's disease)",
    "Leptospirosis",
    "Lumpy skin disease",
    "Maedi-visna",
    "Malignant catarrhal fever",
    "Marburg Hemorrhagic Fever",
    "Marek's disease",
    "MERS-CoV",
    "Monkey Pox",
    "Myxomatosis",
    "New world screwworm (Cochliomyia hominivorax)",
    "Newcastle disease",
    "Nipah virus encephalitis",
    "Old world screwworm (Chrysomya bezziana)",
    "Oropouche fever",
    "Other bacterial diseases",
    "Ovine epididymitis (Brucella ovis)",
    "Paratuberculosis",
    "Pasteurellosis",
    "Peste des petits ruminants",
    "Poliovirus",
    "Porcine cysticercosis",
    "Porcine epidemic diarrhea",
    "Porcine reproductive and respiratory syndrome",
    "Psittacosis(Chlamydophila psittaci)",
    "Pullorum disease",
    "Q fever",
    "Rabbit haemorrhagic disease",
    "Rabies",
    "Rift Valley fever",
    "Rinderpest",
    "Salmonellosis (S. abortusovis)",
    "Schmallenberg",
    "Scrapie",
    "Sheep pox and goat pox",
    "Small hive beetle infestation (Aethina tumida)",
    "Starvation",
    "Strangles",
    "Streptococcus suis",
    "Surra (Trypanosoma evansi)",
    "Swine influenza",
    "Swine Novel Enteric Corona Virus disease",
    "Swine vesicular disease",
    "Teschovirus encephalomyelitis",
    "Theileriosis",
    "Tick-borne encephalitis",
    "Transmissible gastroenteritis",
    "Trichinellosis",
    "Trypanosomosis (tsetse-transmitted)",
    "Tuberculosis",
    "Tularemia",
    "Turkey rhinotracheitis",
    "Unknown disease",
    "Varroa mites",
    "Varroosis of honey bees",
    "Venezuelan equine encephalomyelitis",
    "Vesicular stomatitis",
    "Viral Haemorrhagic Fevers",
    "West Nile Fever",
    "Western equine encephalomyelitis",
    "Yellow fever",
    "Yezo virus"
  )
}

.empres_validate_disease <- function(disease) {
  if (is.null(disease) || length(disease) == 0) {
    return(disease)
  }

  if (length(disease) == 1 && identical(tolower(disease), "all")) {
    return("All")
  }

  valid_diseases <- .empres_valid_diseases()
  canonical <- character(length(disease))
  invalid <- character()

  for (i in seq_along(disease)) {
    value <- disease[[i]]
    parts <- strsplit(value, " -- ", fixed = TRUE)[[1]]
    disease_name <- parts[[1]]
    match_idx <- match(tolower(disease_name), tolower(valid_diseases))

    if (is.na(match_idx)) {
      invalid <- c(invalid, value)
    } else {
      canonical[[i]] <- paste(c(valid_diseases[[match_idx]], parts[-1]), collapse = " -- ")
    }
  }

  if (length(invalid) > 0) {
    stop(
      "Invalid EMPRES disease value(s): ",
      paste(invalid, collapse = ", "),
      "\nSupported disease values are: ",
      paste(valid_diseases, collapse = ", "),
      call. = FALSE
    )
  }

  canonical
}

.empres_validate_values <- function(values, valid_values, argument) {
  if (is.null(values) || length(values) == 0) {
    return(values)
  }

  canonical <- valid_values[match(tolower(values), tolower(valid_values))]
  invalid <- values[is.na(canonical)]
  if (length(invalid) > 0) {
    stop(
      "Invalid `", argument, "` value(s): ",
      paste(invalid, collapse = ", "),
      "\nSupported values are: ",
      paste(valid_values, collapse = ", "),
      call. = FALSE
    )
  }

  unname(canonical)
}

.empres_species_metadata_path <- function() {
  path <- system.file(
    "extdata",
    "FILTERS_SPECIE_VALUES.md",
    package = "omniAPIr",
    mustWork = FALSE
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "FILTERS_SPECIE_VALUES.md")
  }
  if (!file.exists(path)) {
    stop("EMPRES species metadata file not found.", call. = FALSE)
  }
  path
}

.empres_valid_species_metadata <- function() {
  lines <- readLines(.empres_species_metadata_path(), warn = FALSE)
  sections <- list(type = character(), class = character(), specie = character())
  current_section <- NULL

  for (line in lines) {
    if (identical(line, "## `type` Values")) {
      current_section <- "type"
    } else if (identical(line, "## `class` Values")) {
      current_section <- "class"
    } else if (identical(line, "## `specie` Values")) {
      current_section <- "specie"
    } else if (!is.null(current_section) && grepl("^- ", line)) {
      sections[[current_section]] <- c(sections[[current_section]], sub("^- ", "", line))
    }
  }

  sections
}

.empres_validate_species_values <- function(species) {
  if (is.null(species) || length(species) == 0) {
    return(species)
  }

  if (any(tolower(species) == "all")) {
    return("All")
  }

  valid_species <- .empres_valid_species_metadata()$specie
  .empres_validate_values(species, valid_species, "specie")
}

.empres_parse_species_tuple <- function(tuple) {
  matches <- gregexpr("<(type|class|specie):([^>]*)>", tuple, perl = TRUE)
  tags <- regmatches(tuple, matches)[[1]]
  if (length(tags) == 0) {
    stop("Invalid EMPRES species tuple: ", tuple, call. = FALSE)
  }

  result <- list(type = NULL, class = NULL, specie = NULL)
  for (tag in tags) {
    key <- sub("^<([^:]+):.*>$", "\\1", tag)
    value <- sub("^<[^:]+:([^>]*)>$", "\\1", tag)
    result[[key]] <- value
  }

  result
}

.empres_species_filter <- function(specie = NULL, specie_type = NULL, specie_class = NULL) {
  if (is.null(specie) && is.null(specie_type) && is.null(specie_class)) {
    return(NULL)
  }

  if (!is.null(specie) && any(tolower(specie) %in% c("all", "<all>", "<type:all>", "<specie:all>"))) {
    return("<all>")
  }

  has_tuple <- !is.null(specie) && all(grepl("^<", specie))
  if (has_tuple && is.null(specie_type) && is.null(specie_class)) {
    tuples <- unlist(strsplit(specie, ",", fixed = TRUE), use.names = FALSE)
    tuples <- trimws(tuples)
    metadata <- .empres_valid_species_metadata()

    canonical_tuples <- vapply(tuples, function(tuple) {
      parsed <- .empres_parse_species_tuple(tuple)
      type <- .empres_validate_values(parsed$type, metadata$type, "specie_type")
      class <- .empres_validate_values(parsed$class, metadata$class, "specie_class")
      species <- .empres_validate_values(parsed$specie, metadata$specie, "specie")

      parts <- character()
      if (!is.null(type)) {
        parts <- c(parts, paste0("<type:", type, ">"))
      }
      if (!is.null(class)) {
        parts <- c(parts, paste0("<class:", class, ">"))
      }
      if (!is.null(species)) {
        parts <- c(parts, paste0("<specie:", species, ">"))
      }
      paste0(parts, collapse = "")
    }, character(1))

    return(.empres_collapse_filter(canonical_tuples))
  }

  metadata <- .empres_valid_species_metadata()
  specie_type <- .empres_validate_values(
    specie_type,
    metadata$type,
    "specie_type"
  )
  specie_class <- .empres_validate_values(
    specie_class,
    metadata$class,
    "specie_class"
  )
  specie <- .empres_validate_species_values(specie)

  lengths <- c(length(specie %||% character()), length(specie_type %||% character()), length(specie_class %||% character()))
  lengths <- lengths[lengths > 0]
  if (length(lengths) == 0) {
    return(NULL)
  }
  n <- max(lengths)

  recycle_to <- function(x, n, name) {
    if (is.null(x)) {
      return(rep(NA_character_, n))
    }
    x <- as.character(x)
    if (length(x) == n) {
      return(x)
    }
    if (length(x) == 1) {
      return(rep(x, n))
    }
    stop("EMPRES species filter component `", name, "` must have length 1 or ", n, ".", call. = FALSE)
  }

  species <- recycle_to(specie, n, "specie")
  types <- recycle_to(specie_type, n, "specie_type")
  classes <- recycle_to(specie_class, n, "specie_class")

  tuples <- vapply(seq_len(n), function(i) {
    parts <- character()
    if (!is.na(types[i]) && nzchar(types[i])) {
      parts <- c(parts, paste0("<type:", types[i], ">"))
    }
    if (!is.na(classes[i]) && nzchar(classes[i])) {
      parts <- c(parts, paste0("<class:", classes[i], ">"))
    }
    if (!is.na(species[i]) && nzchar(species[i])) {
      parts <- c(parts, paste0("<specie:", species[i], ">"))
    }
    paste0(parts, collapse = "")
  }, character(1))

  .empres_collapse_filter(tuples)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Get FAO EMPRES-i Animal Disease Data
#'
#' @description
#' Retrieves animal disease outbreak data from the FAO Emergency Prevention System
#' for Animal Health (EMPRES-i) public API.
#'
#' @param country_iso3 Character. ISO3 country code. Default is NULL (all countries).
#' @param area Character vector. Country names or geographic groupings accepted by the
#'   EMPRES public API. If supplied, this is used instead of \code{country_iso3}.
#' @param disease Character vector. Disease names, or disease/subtype values using the
#'   public API separator \code{" -- "}. Default is NULL.
#' @param diagnosis_status Character vector. Diagnosis status filter. Accepted values are
#'   "confirmed", "denied", "suspected", and "tentative". Default is "confirmed".
#' @param diagnosis_source Character vector. Diagnosis source labels. Default is NULL.
#' @param has_human_affected Character. Public API human affected filter: "0" for no
#'   filter, "1" for no affected humans flag, or "2" for affected humans flag.
#' @param include_reporting_date Character. "1" reuses reporting-date bounds for missing
#'   observation-date bounds; "0" does not. Default is "0".
#' @param specie Character vector. Species values or full public API tag tuples such as
#'   \code{"<type:Domestic><class:Mammal><specie:Cattle>"}.
#' @param specie_type Character vector. Species type tags, for example "Domestic" or "Wild".
#' @param specie_class Character vector. Species class tags, for example "Mammal" or "Birds".
#' @param start_observation_date,end_observation_date Character date bounds in YYYY-MM-DD format.
#' @param start_creation_date,end_creation_date Character date bounds in YYYY-MM-DD format.
#' @param start_reporting_date,end_reporting_date Character date bounds in YYYY-MM-DD format.
#' @param empresi_version Character. EMPRES-i version: "all", "1", or "0". Default is "all".
#' @param offset Integer. Starting offset for API pagination. Default is 0.
#' @param paginate Logical. Whether to continue requesting pages until the API returns fewer
#'   than \code{page_size} records. Default is TRUE.
#' @param page_size Integer. Server page size used to advance \code{offset}. The public API
#'   currently documents offset-based paging with 1000-row increments.
#' @param max_records Integer. Maximum number of records to return. Default is Inf.
#' @param api_key Character. EMPRES public API key. Defaults to the \code{EMPRES_API_KEY}
#'   environment variable.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A data.frame containing animal disease outbreak data, or invisible(NULL) if no data found.
#'
#' @details
#' API Documentation: \url{https://fao-empp-data-explorer-be-51851897723.europe-west1.run.app/api/docs}
#'
#' The public API does not expose a field-selection parameter. The function requests the
#' endpoint's default fields and returns them as supplied by the API.
#'
#' Disease values, species values, species types, and species classes are validated locally against
#' the public filter metadata before the API request is sent. Invalid values raise
#' an error listing the supported values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get specific disease data
#' fmd_data <- get_empres_data(
#'   country_iso3 = "KEN",
#'   disease = "Foot and mouth disease"
#' )
#'
#' # Get domestic cattle events observed in 2025
#' cattle_events <- get_empres_data(
#'   country_iso3 = "KEN",
#'   specie = "Cattle",
#'   specie_type = "Domestic",
#'   specie_class = "Mammal",
#'   start_observation_date = "2025-01-01",
#'   end_observation_date = "2025-12-31"
#' )
#' }
get_empres_data <- function(
  country_iso3 = NULL,
  area = NULL,
  disease = NULL,
  diagnosis_status = "confirmed",
  diagnosis_source = NULL,
  has_human_affected = "0",
  include_reporting_date = "0",
  specie = NULL,
  specie_type = NULL,
  specie_class = NULL,
  start_observation_date = NULL,
  end_observation_date = NULL,
  start_creation_date = NULL,
  end_creation_date = NULL,
  start_reporting_date = NULL,
  end_reporting_date = NULL,
  empresi_version = "all",
  offset = 0,
  paginate = TRUE,
  page_size = 1000,
  max_records = Inf,
  api_key = Sys.getenv("EMPRES_API_KEY", unset = NA_character_),
  verbose = FALSE,
  max_retries = 3
) {
  base <- "https://fao-empp-data-explorer-be-51851897723.europe-west1.run.app/api/events"

  if (length(offset) != 1 || is.na(offset) || offset < 0) {
    stop("`offset` must be a single non-negative integer.", call. = FALSE)
  }
  if (length(page_size) != 1 || is.na(page_size) || page_size <= 0) {
    stop("`page_size` must be a single positive integer.", call. = FALSE)
  }
  if (length(max_records) != 1 || is.na(max_records) || max_records <= 0) {
    stop("`max_records` must be a single positive number or Inf.", call. = FALSE)
  }

  if (!is.null(area)) {
    area_name <- area
  } else {
    area_name <- .empres_country_names(country_iso3)
  }

  disease <- .empres_validate_disease(disease)

  valid_status <- c("confirmed", "denied", "suspected", "tentative")
  invalid_status <- setdiff(tolower(diagnosis_status), valid_status)
  if (length(invalid_status) > 0) {
    stop(
      "Invalid diagnosis_status value(s): ",
      paste(invalid_status, collapse = ", "),
      ". Accepted values are: ",
      paste(valid_status, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  empresi_version <- as.character(empresi_version)
  if (length(empresi_version) != 1 || !empresi_version %in% c("all", "1", "0")) {
    stop("`empresi_version` must be one of: all, 1, 0.", call. = FALSE)
  }

  params <- list(
    diagnosis_status = .empres_collapse_filter(tolower(diagnosis_status)),
    diagnosis_source = .empres_collapse_filter(diagnosis_source),
    disease = .empres_collapse_filter(disease),
    empresi_version = empresi_version,
    area = .empres_collapse_filter(area_name),
    has_human_affected = as.character(has_human_affected),
    include_reporting_date = as.character(include_reporting_date),
    specie = .empres_species_filter(specie, specie_type, specie_class),
    start_observation_date = start_observation_date,
    end_observation_date = end_observation_date,
    start_creation_date = start_creation_date,
    end_creation_date = end_creation_date,
    start_reporting_date = start_reporting_date,
    end_reporting_date = end_reporting_date
  )
  params <- params[!vapply(params, is.null, logical(1))]

  for (date_param in names(params)[grepl("^(start|end)_(observation|creation|reporting)_date$", names(params))]) {
    date_value <- params[[date_param]]
    if (length(date_value) != 1 || !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_value)) {
      stop("`", date_param, "` must use YYYY-MM-DD format.", call. = FALSE)
    }
  }

  if (length(has_human_affected) != 1 || !as.character(has_human_affected) %in% c("0", "1", "2")) {
    stop("`has_human_affected` must be one of: 0, 1, 2.", call. = FALSE)
  }
  if (length(include_reporting_date) != 1 || !as.character(include_reporting_date) %in% c("0", "1")) {
    stop("`include_reporting_date` must be one of: 0, 1.", call. = FALSE)
  }

  if (is.null(api_key) || length(api_key) != 1 || is.na(api_key) || !nzchar(api_key)) {
    stop(
      "The EMPRES public API currently requires an X-API-Key header. ",
      "Set EMPRES_API_KEY or pass api_key.",
      call. = FALSE
    )
  }

  if (verbose) {
    message("Fetching EMPRES-i data from public API...")
    message(sprintf("Endpoint: %s", base))
    message(sprintf("Offset: %s", offset))
  }

  fetch_page <- function(page_offset) {
    page_params <- c(params, list(offset = page_offset))
    request <- httr2::request(base) |>
      httr2::req_headers(
        `X-API-Key` = api_key,
        Accept = "application/json"
      )
    request <- do.call(httr2::req_url_query, c(list(request), page_params))

    retry_attempt <- 1
    repeat {
      tryCatch(
        {
          response <- request |>
            httr2::req_perform()

          status <- httr2::resp_status(response)
          if (status >= 400) {
            body <- httr2::resp_body_string(response)
            stop(sprintf("HTTP error %d: %s", status, body), call. = FALSE)
          }

          data <- jsonlite::fromJSON(httr2::resp_body_string(response), flatten = TRUE)
          if (is.data.frame(data)) {
            return(data)
          }
          for (field in c("data", "results", "items", "events")) {
            if (!is.null(data[[field]])) {
              if (is.data.frame(data[[field]])) {
                return(data[[field]])
              }
              return(as.data.frame(data[[field]]))
            }
          }
          if (is.list(data) && length(data) == 0) {
            return(data.frame())
          }
          as.data.frame(data)
        },
        error = function(e) {
          if (retry_attempt >= max_retries) {
            stop(
              sprintf(
                "Failed to fetch EMPRES-i data after %d attempts. Last error: %s",
                max_retries,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
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
        }
      )
    }
  }

  pages <- list()
  current_offset <- as.integer(offset)
  records_read <- 0

  repeat {
    page <- fetch_page(current_offset)
    if (!is.data.frame(page) || nrow(page) == 0) {
      break
    }

    pages[[length(pages) + 1]] <- page
    records_read <- records_read + nrow(page)

    if (!paginate || nrow(page) < page_size || records_read >= max_records) {
      break
    }

    current_offset <- current_offset + page_size
    if (verbose) {
      message(sprintf("Fetched %s records; requesting offset %s", records_read, current_offset))
    }
  }

  if (length(pages) == 0) {
    message("No EMPRES-i data found for the specified filters")
    return(invisible(NULL))
  }

  data <- dplyr::bind_rows(pages)
  if (is.finite(max_records) && nrow(data) > max_records) {
    data <- utils::head(data, max_records)
  }

  message(sprintf(
    "EMPRES-i data retrieved successfully: %s records",
    format(nrow(data), big.mark = ",")
  ))

  data
}


#' Get FAO Fisheries Statistics Data
#'
#' @description
#' Retrieves fisheries production data from the fishstatj R package, which contains
#' FAO fisheries statistics.
#'
#' @importFrom magrittr %>%
#'
#' @param iso3 Character. ISO3 country code to filter data. Default is NULL (all countries).
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#'
#' @return A data.frame with combined fisheries production data from multiple tables.
#'
#' @details
#' This function uses the fishstatj R package data. No API calls are made.
#' Documentation: \url{https://github.com/socialcopsdev/fishstat}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all fisheries data
#' fish_data <- get_fishstat_data()
#'
#' # Get data for specific country
#' fish_data_ken <- get_fishstat_data(iso3 = "KEN")
#' }
get_fishstat_data <- function(iso3 = NULL, verbose = FALSE) {
  if (verbose) {
    message("Fetching fishstat production data from package...")
  }

  prod_all <- fishstat::production %>%
    dplyr::left_join(fishstat::area, by = "area") %>%
    dplyr::left_join(fishstat::country, by = "country") %>%
    dplyr::left_join(fishstat::measure, by = "measure") %>%
    dplyr::left_join(fishstat::source, by = "source") %>%
    dplyr::left_join(fishstat::species, by = "species") %>%
    dplyr::left_join(fishstat::status, by = "status")

  # Filter by iso3 if provided
  if (!is.null(iso3)) {
    iso3_filter <- toupper(iso3)
    if (verbose) {
      message(sprintf("Filtering data for country: %s", iso3_filter))
    }
    prod_all <- prod_all %>%
      dplyr::filter(.data$iso3 %in% .env$iso3_filter)
  }

  message(sprintf(
    "✓ Fishstat data retrieved successfully: %s rows",
    format(nrow(prod_all), big.mark = ",")
  ))

  return(prod_all)
}
