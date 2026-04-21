# Internal helper function to get ISO3 to M49 code mapping
# FAOSTAT API requires M49 codes for area parameter, even when area_cs=ISO3
.get_iso3_to_m49_lookup <- function() {
  list(
    "AFG" = "2",
    "ALB" = "3",
    "DZA" = "4",
    "AND" = "7",
    "AGO" = "8",
    "ATG" = "9",
    "ARG" = "10",
    "ARM" = "11",
    "AUS" = "52",
    "AUT" = "12",
    "AZE" = "13",
    "BHS" = "16",
    "BHR" = "14",
    "BGD" = "57",
    "BRB" = "255",
    "BLR" = "15",
    "BEL" = "23",
    "BLZ" = "53",
    "BEN" = "18",
    "BTN" = "19",
    "BOL" = "80",
    "BIH" = "20",
    "BWA" = "21",
    "BRA" = "27",
    "BRN" = "233",
    "BGR" = "35",
    "BFA" = "115",
    "BDI" = "32",
    "CPV" = "33",
    "KHM" = "40",
    "CMR" = "351",
    "CAN" = "96",
    "CAF" = "128",
    "TCD" = "214",
    "CHL" = "41",
    "CHN" = "44",
    "COL" = "45",
    "COM" = "46",
    "COG" = "48",
    "COK" = "98",
    "CRI" = "49",
    "HRV" = "50",
    "CUB" = "167",
    "CYP" = "107",
    "CZE" = "116",
    "CIV" = "250",
    "PRK" = "54",
    "COD" = "72",
    "DNK" = "56",
    "DJI" = "58",
    "DMA" = "59",
    "DOM" = "60",
    "ECU" = "63",
    "EGY" = "209",
    "SLV" = "238",
    "GNQ" = "66",
    "ERI" = "67",
    "EST" = "68",
    "SWZ" = "70",
    "ETH" = "74",
    "FJI" = "75",
    "FIN" = "73",
    "FRA" = "79",
    "GAB" = "81",
    "GMB" = "84",
    "GEO" = "86",
    "DEU" = "89",
    "GHA" = "90",
    "GRC" = "175",
    "GRD" = "91",
    "GTM" = "93",
    "GIN" = "95",
    "GNB" = "97",
    "GUY" = "99",
    "HTI" = "100",
    "HND" = "101",
    "HUN" = "102",
    "ISL" = "103",
    "IND" = "104",
    "IDN" = "105",
    "IRN" = "106",
    "IRQ" = "109",
    "IRL" = "112",
    "ISR" = "108",
    "ITA" = "114",
    "JAM" = "83",
    "JPN" = "118",
    "JOR" = "113",
    "KAZ" = "120",
    "KEN" = "114",
    "KIR" = "119",
    "KWT" = "121",
    "KGZ" = "122",
    "LAO" = "123",
    "LVA" = "124",
    "LBN" = "126",
    "LSO" = "256",
    "LBR" = "129",
    "LBY" = "130",
    "LTU" = "131",
    "LUX" = "132",
    "MDG" = "134",
    "MWI" = "127",
    "MYS" = "136",
    "MDV" = "137",
    "MLI" = "138",
    "MLT" = "145",
    "MRT" = "141",
    "MUS" = "273",
    "MEX" = "143",
    "MNG" = "144",
    "MNE" = "28",
    "MAR" = "147",
    "MOZ" = "148",
    "MMR" = "149",
    "NAM" = "150",
    "NPL" = "153",
    "NLD" = "156",
    "NZL" = "157",
    "NIC" = "158",
    "NER" = "159",
    "NGA" = "154",
    "MKD" = "162",
    "NOR" = "221",
    "OMN" = "165",
    "PAK" = "166",
    "PAN" = "168",
    "PNG" = "169",
    "PRY" = "170",
    "PER" = "171",
    "PHL" = "173",
    "POL" = "174",
    "PRT" = "179",
    "PSE" = "275",
    "QAT" = "117",
    "KOR" = "146",
    "MDA" = "183",
    "ROU" = "185",
    "RUS" = "184",
    "RWA" = "188",
    "KNA" = "189",
    "LCA" = "191",
    "VCT" = "244",
    "WSM" = "193",
    "STP" = "194",
    "SAU" = "195",
    "SEN" = "272",
    "SRB" = "196",
    "SYC" = "197",
    "SLE" = "199",
    "SGP" = "198",
    "SVK" = "25",
    "SVN" = "202",
    "SLB" = "203",
    "SOM" = "38",
    "ZAF" = "207",
    "SSD" = "210",
    "ESP" = "211",
    "LKA" = "212",
    "SDN" = "208",
    "SUR" = "216",
    "SWE" = "176",
    "CHE" = "219",
    "SYR" = "220",
    "TJK" = "222",
    "THA" = "213",
    "TLS" = "227",
    "TGO" = "223",
    "TON" = "226",
    "TTO" = "230",
    "TUN" = "225",
    "TUR" = "229",
    "TKM" = "215",
    "UGA" = "231",
    "UKR" = "234",
    "ARE" = "235",
    "GBR" = "155",
    "TZA" = "236",
    "USA" = "237",
    "URY" = "249",
    "UZB" = "251",
    "VUT" = "242",
    "VEN" = "254",
    "VNM" = "251",
    "YEM" = "181",
    "ZMB" = "251",
    "ZWE" = "181"
  )
}

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

#' Get FAOSTAT Agriculture Data
#'
#' @description
#' Retrieves agricultural statistics from the Food and Agriculture Organization
#' Statistics (FAOSTAT) API with built-in lookup tables for common items.
#'
#' **Note:** This function is designed for specific datasets and is not comprehensive.
#' It works best with QCL, FBS, FS, and CAHD databases and common agricultural elements (2111, 2413, 2515, 7209, 664, 674, 645, 6120).
#'
#' @importFrom magrittr %>%
#'
#' @param element Character or numeric. Element code specifying the type of
#'   measurement (e.g., "2111" for livestock stocks, "2413" for crop production, "6120" for cost indicators).
#' @param item Character or numeric. Item code(s) or common names. When use_lookup
#'   is TRUE, accepts friendly names like "cattle", "wheat", "healthy_diet", etc.
#' @param database Character. FAOSTAT database name (required).
#' @param item_cs Character. Item classification system. Default is NULL.
#' @param use_lookup Logical. Whether to use built-in lookup tables to convert
#'   common names to item codes. Default is TRUE.
#' @param mrv Integer. Most Recent Values - number of years to retrieve. Default is 50.
#' @param iso3 Character. ISO3 country code to filter data. Default is NULL (all countries).
#' @param release Character. Release version for CAHD database (e.g., "7S2025" for July 2025).
#'   Default is NULL (auto-set to latest for CAHD).
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A data.frame with columns: isocode, Item, Year, Value, Unit, element_name.
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
#'   \item \strong{Element 2413 (Production)} - Crop production data
#'   \item \strong{Element 2515 (Agricultural Use)} - Fertilizer and pesticide use
#'   \item \strong{Element 7209 (Land Use)} - Land use statistics
#'   \item \strong{Element 664 (Food supply kcal/capita/day)} - Dietary energy supply
#'   \item \strong{Element 674 (Protein supply quantity)} - Protein supply data
#'   \item \strong{Element 645 (Food supply quantity kg/capita/year)} - Food supply by food group
#'   \item \strong{Element 6120 (Cost indicators)} - Food security indicators (FS) and diet cost (CAHD)
#' }
#'
#' **Built-in lookup tables** support:
#' \itemize{
#'   \item \strong{Animals (Element 2111):} cattle, sheep, chicken, goats, pigs, horses, buffalo, camels, rabbits, ducks
#'   \item \strong{Crops (Element 2413):} wheat, rice, maize, barley, oats, rye, millet, sorghum, soybeans, sunflower, rapeseed, cotton, sugarcane, sugar_beet, potatoes, cassava
#'   \item \strong{Fertilizers (Element 2515):} npk, nitrogen, phosphate, potash, urea, ammonium_sulfate, calcium_phosphate, pesticides_total
#'   \item \strong{Land use (Element 7209):} agricultural_land, forest_land
#'   \item \strong{Food Balance Sheets (Elements 664, 674):} grand_total, total (Item aggregated: Grand Total, code 2901)
#'   \item \strong{Food Balance Sheets (Element 645):} cereals, starchy_roots, pulses, treenuts, vegetables, fruits, eggs, meat, fish, vegetable_oils (aggregated food groups)
#'   \item \strong{Food Security SDG indicators (Element 6120 - FS database):} undernourishment/pou/sdg_2_1_1 (SDG 2.1.1), food_insecurity/fies/sdg_2_1_2 (SDG 2.1.2), cereals_roots_tubers (3-year averages)
#'   \item \strong{CAHD Cost Indicators (Element 6120 - CAHD database):} healthy_diet, starchy_staples, animal_source_food, vegetables, fruits (cost in PPP$/person/day)
#'   \item \strong{CAHD Affordability Indicators (Element 6120 - CAHD database):} people_unable_afford/nua_millions (millions), percent_unable_afford/pua_percent (%)
#' }
#'
#' **Limitations:**
#' \itemize{
#'   \item Only works with specific element codes (2111, 2413, 2515, 7209, 664, 674, 645, 6120)
#'   \item Primarily designed for QCL, FBS, FS, and CAHD databases
#'   \item Lookup tables are limited to common agricultural items
#'   \item For other databases/elements, use \code{use_lookup = FALSE} and provide exact codes
#'   \item FS database uses 3-year averages (e.g., 2000 represents 2000-2002)
#'   \item CAHD database requires release parameter (auto-set to latest if not specified)
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
#' # Get crop production for multiple crops
#' crop_data <- get_faostat_data(
#'   element = "2413",
#'   item = c("wheat", "maize", "rice"),
#'   database = "QCL"
#' )
#'
#' # Use item codes directly (without lookup)
#' wheat_data <- get_faostat_data(
#'   element = "5510",
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

  # Auto-set release for CAHD database if not specified (use latest December 2024 release)
  if (is.null(release) && !is.null(database) && toupper(database) == "CAHD") {
    release <- "12U2024"
    if (verbose) {
      message(sprintf("Using CAHD release: %s", release))
    }
  }

  # ISO3 to M49 code lookup table (defined once, outside inner function)
  # FAOSTAT API requires M49 codes for area parameter, even when area_cs=ISO3
  iso3_to_m49 <- .get_iso3_to_m49_lookup()

  # Item lookup tables (defined once, outside inner function)
  animal_lookup <- list(
    "cattle" = "866",
    "sheep" = "1057",
    "chicken" = "1016",
    "goats" = "976",
    "pigs" = "1034",
    "horses" = "1096",
    "buffalo" = "946",
    "camels" = "1126",
    "rabbits" = "1141",
    "ducks" = "1058"
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

  fertilizer_and_pesticides_lookup <- list(
    "npk" = "4021",
    "nitrogen" = "4009",
    "phosphate" = "4019",
    "potash" = "4029",
    "urea" = "4014",
    "ammonium_sulfate" = "4010",
    "calcium_phosphate" = "4020",
    "pesticides_total" = "1357",
    "total" = "1357"
  )

  land_use_lookup <- list(
    "agricultural_land" = "6610",
    "forest_land" = "6646"
  )

  fbs_lookup <- list(
    "grand_total" = "2901",
    "total" = "2901"
  )

  fbs_food_supply_kg_lookup <- list(
    "cereals" = "2905",
    "cereals_excluding_beer" = "2905",
    "starchy_roots" = "2949",
    "pulses" = "2960",
    "treenuts" = "2919",
    "vegetables" = "2943",
    "fruits" = "2911",
    "fruits_excluding_wine" = "2911",
    "eggs" = "2907",
    "meat" = "2912",
    "fish" = "2914",
    "fish_seafood" = "2914",
    "vegetable_oils" = "2918"
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
    "healthy_diet" = "7008",
    "cost_healthy_diet" = "7008",
    "starchy_staples" = "7011",
    "cost_starchy_staples" = "7011",
    "animal_source_food" = "7007",
    "cost_animal_food" = "7007",
    "vegetables" = "7010",
    "cost_vegetables" = "7010",
    "fruits" = "7004",
    "cost_fruits" = "7004",
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
            "2413" = crop_lookup, # Crop production
            "2515" = fertilizer_and_pesticides_lookup, # Agricultural use (fertilizers and pesticides)
            "7209" = land_use_lookup, # Land use
            "664" = fbs_lookup, # Food supply (kcal/capita/day) - uses Grand Total item (2901)
            "674" = fbs_lookup, # Protein supply quantity - uses Grand Total item (2901)
            "645" = fbs_food_supply_kg_lookup, # Food supply quantity (kg/capita/year) - aggregated food groups
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
    # 2413 = crop production
    # 2515 = Agricultural Use (for fertilizers)
    # 7209 = LUC
    # 664 = Food supply (kcal/capita/day) - FBS
    # 674 = Protein supply quantity - FBS
    # 645 = Food supply quantity (kg/capita/year) - FBS
    # 6120 = Food Security indicators (3-year averages) - FS / Cost indicators (PPP$/person/day) - CAHD
    # Items
    #  "866,1057,1016,976" Cattle, sheep, chicken and goats
    # "572,176,661,56,79,92,270,27,71,83,236,156,97,15"  several crops
    # "6610,6646" # agricultural land and forestry
    # "4021" # NPK fertilizers
    # "2901" # Grand Total + (Total) - FBS
    # "2905,2949,2960,2919,2943,2911,2907,2912,2914,2918" # FBS Food groups (kg/capita/year)
    # "21004" # FS: Prevalence of undernourishment (SDG 2.1.1) (%) - 3-year average
    # "21009" # FS: Prevalence of moderate or severe food insecurity (SDG 2.1.2) (%) - 3-year average
    # "21012" # FS: Share of dietary energy from cereals, roots, and tubers (%) - 3-year average
    # "7008,7011,7007,7010,7004" # CAHD cost indicators (healthy diet, starchy staples, animal food, vegetables, fruits)
    # "7006" # CAHD: Number of people unable to afford a healthy diet (NUA), millions
    # "7005" # CAHD: Percent of population unable to afford a healthy diet (PUA), %
    base <- paste0("https://faostatservices.fao.org/api/v1/en/data/", database)

    # Set area parameter based on whether iso3 is specified
    # IMPORTANT: API requires M49 codes in area parameter, even when area_cs=ISO3
    # iso3_to_m49 lookup is from outer scope
    area_param <- if (!is.null(iso3)) {
      # Convert ISO3 to M49 code using lookup from outer scope
      m49_code <- iso3_to_m49[[iso3]]
      if (is.null(m49_code)) {
        stop(paste(
          "Invalid ISO3 code:",
          iso3,
          "\nAvailable codes:",
          paste(names(iso3_to_m49)[1:20], collapse = ", "),
          "... (and more)"
        ))
      }
      m49_code
    } else {
      "2,3,4,7,8,9,1,10,11,52,12,13,16,14,57,255,15,23,53,18,19,80,20,21,26,27,233,29,35,115,32,33,37,39,40,351,96,128,214,41,44,45,46,47,48,98,49,50,167,51,107,116,250,54,72,55,56,58,59,60,61,178,63,209,238,62,64,66,67,68,69,70,74,75,73,79,81,84,86,87,89,90,175,91,93,95,97,99,100,101,102,103,104,105,106,109,110,112,108,114,83,118,113,120,119,121,122,123,124,126,256,129,130,131,132,133,134,127,135,136,137,138,145,141,273,143,144,28,147,148,149,150,153,156,157,158,159,160,154,162,221,165,299,166,168,169,170,171,173,174,177,179,117,146,183,185,184,182,188,189,191,244,193,194,195,272,186,196,197,200,199,198,25,201,202,277,203,38,276,206,207,210,211,212,208,216,176,217,218,219,220,222,213,227,223,228,226,230,225,229,215,231,234,235,155,236,237,249,248,251,181" # All countries
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
      area = area_param,
      area_cs = "ISO3",
      element = element, #What kind of measurement/statistic you want (e.g., production, area harvested, yield, etc.)
      item = item #What specific products/crops/commodities you want data for (e.g., wheat, maize, rice, etc.)
    )

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
                    .x %in% c("Item.Code", "Item Code") ~ "item_code",
                    .x %in% c("Item", "item") ~ "item_name",
                    .x %in% c("Element.Code", "Element Code") ~ "element_code",
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
                  value = as.numeric(value),
                  year = as.numeric(year)
                ) %>%
                dplyr::select(
                  Area.Code..ISO3.,
                  item_name,
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
      variables = "forestAreaNetChange",
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


#' Get FAO EMPRES-i Animal Disease Data
#'
#' @description
#' Retrieves animal disease outbreak data from the FAO Emergency Prevention System
#' for Animal Health (EMPRES-i) API.
#'
#' @param country_iso3 Character. ISO3 country code. Default is NULL (all countries).
#' @param animals Character vector. Animal type(s) or disease code(s). Accepts common names
#'   like "cattle", "pigs", "chicken", etc. Default is "All".
#' @param diagnosis_status Character. Diagnosis status filter: "confirmed", "suspected",
#'   or "both". Default is "confirmed".
#' @param confidentiality_level Character. Confidentiality level: "public", "confidential",
#'   or "both". Default is "both".
#' @param empresi_version Character. EMPRES-i version: "v1", "v2", or "all". Default is "all".
#' @param use_lookup Logical. Whether to use built-in lookup tables for animals and diseases.
#'   Default is TRUE.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A data.frame containing animal disease outbreak data, or invisible(NULL) if no data found.
#'
#' @details
#' API Documentation: \url{https://empres-i.apps.fao.org/}
#'
#' The function supports animal names and disease shortcuts when use_lookup=TRUE:
#' \itemize{
#'   \item Animals: cattle, pigs, chicken, sheep, goats, horses, buffalo, camels, etc.
#'   \item Disease shortcuts: fmd, asf, ai, ppr, ahs, newcastle, rabies, anthrax, etc.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get cattle disease data for Kenya
#' empres_data <- get_empres_data(
#'   country_iso3 = "KEN",
#'   animals = "cattle"
#' )
#'
#' # Get specific disease data
#' fmd_data <- get_empres_data(
#'   country_iso3 = "KEN",
#'   animals = "fmd"
#' )
#'
#' # Get data with verbose output
#' empres_data <- get_empres_data(
#'   country_iso3 = "KEN",
#'   animals = c("cattle", "pigs"),
#'   verbose = TRUE
#' )
#' }
get_empres_data <- function(
  country_iso3 = NULL,
  animals = "All",
  diagnosis_status = "confirmed",
  confidentiality_level = "both",
  empresi_version = "all",
  use_lookup = TRUE,
  verbose = FALSE,
  max_retries = 3
) {
  # Base URL for EMPRES API
  base <- "https://fao-empp-data-explorer-be-175434516411.europe-west1.run.app/events"

  # ISO3 to country name lookup table (based on EMPRES area dimension)
  if (use_lookup && !is.null(country_iso3)) {
    iso3_to_country <- list(
      "AFG" = "Afghanistan",
      "ALB" = "Albania",
      "DZA" = "Algeria",
      "AND" = "Andorra",
      "AGO" = "Angola",
      "ATG" = "Antigua and Barbuda",
      "ARG" = "Argentina",
      "ARM" = "Armenia",
      "AUS" = "Australia",
      "AUT" = "Austria",
      "AZE" = "Azerbaijan",
      "BHS" = "Bahamas",
      "BHR" = "Bahrain",
      "BGD" = "Bangladesh",
      "BRB" = "Barbados",
      "BLR" = "Belarus",
      "BEL" = "Belgium",
      "BLZ" = "Belize",
      "BEN" = "Benin",
      "BTN" = "Bhutan",
      "BOL" = "Bolivia (Plurinational State of)",
      "BIH" = "Bosnia and Herzegovina",
      "BWA" = "Botswana",
      "BRA" = "Brazil",
      "BRN" = "Brunei Darussalam",
      "BGR" = "Bulgaria",
      "BFA" = "Burkina Faso",
      "BDI" = "Burundi",
      "CPV" = "Cabo Verde",
      "KHM" = "Cambodia",
      "CMR" = "Cameroon",
      "CAN" = "Canada",
      "CAF" = "Central African Republic",
      "TCD" = "Chad",
      "CHL" = "Chile",
      "CHN" = "China",
      "COL" = "Colombia",
      "COM" = "Comoros",
      "COG" = "Congo",
      "COK" = "Cook Islands",
      "CRI" = "Costa Rica",
      "HRV" = "Croatia",
      "CUB" = "Cuba",
      "CYP" = "Cyprus",
      "CZE" = "Czechia",
      "CIV" = "Côte d'Ivoire",
      "PRK" = "Democratic People's Republic of Korea",
      "COD" = "Democratic Republic of the Congo",
      "DNK" = "Denmark",
      "DJI" = "Djibouti",
      "DMA" = "Dominica",
      "DOM" = "Dominican Republic",
      "ECU" = "Ecuador",
      "EGY" = "Egypt",
      "SLV" = "El Salvador",
      "GNQ" = "Equatorial Guinea",
      "ERI" = "Eritrea",
      "EST" = "Estonia",
      "SWZ" = "Eswatini",
      "ETH" = "Ethiopia",
      "FRO" = "Faroe Islands",
      "FJI" = "Fiji",
      "FIN" = "Finland",
      "FRA" = "France",
      "GAB" = "Gabon",
      "GMB" = "Gambia",
      "GEO" = "Georgia",
      "DEU" = "Germany",
      "GHA" = "Ghana",
      "GRC" = "Greece",
      "GRD" = "Grenada",
      "GTM" = "Guatemala",
      "GIN" = "Guinea",
      "GNB" = "Guinea-Bissau",
      "GUY" = "Guyana",
      "HTI" = "Haiti",
      "VAT" = "Holy See",
      "HND" = "Honduras",
      "HUN" = "Hungary",
      "ISL" = "Iceland",
      "IND" = "India",
      "IDN" = "Indonesia",
      "IRN" = "Iran (Islamic Republic of)",
      "IRQ" = "Iraq",
      "IRL" = "Ireland",
      "ISR" = "Israel",
      "ITA" = "Italy",
      "JAM" = "Jamaica",
      "JPN" = "Japan",
      "JOR" = "Jordan",
      "KAZ" = "Kazakhstan",
      "KEN" = "Kenya",
      "KIR" = "Kiribati",
      "KWT" = "Kuwait",
      "KGZ" = "Kyrgyzstan",
      "LAO" = "Lao People's Democratic Republic",
      "LVA" = "Latvia",
      "LBN" = "Lebanon",
      "LSO" = "Lesotho",
      "LBR" = "Liberia",
      "LBY" = "Libya",
      "LIE" = "Liechtenstein",
      "LTU" = "Lithuania",
      "LUX" = "Luxembourg",
      "MDG" = "Madagascar",
      "MWI" = "Malawi",
      "MYS" = "Malaysia",
      "MDV" = "Maldives",
      "MLI" = "Mali",
      "MLT" = "Malta",
      "MHL" = "Marshall Islands",
      "MRT" = "Mauritania",
      "MUS" = "Mauritius",
      "MEX" = "Mexico",
      "FSM" = "Micronesia (Federated States of)",
      "MCO" = "Monaco",
      "MNG" = "Mongolia",
      "MNE" = "Montenegro",
      "MAR" = "Morocco",
      "MOZ" = "Mozambique",
      "MMR" = "Myanmar",
      "NAM" = "Namibia",
      "NRU" = "Nauru",
      "NPL" = "Nepal",
      "NLD" = "Netherlands (Kingdom of the)",
      "NZL" = "New Zealand",
      "NIC" = "Nicaragua",
      "NER" = "Niger",
      "NGA" = "Nigeria",
      "NIU" = "Niue",
      "MKD" = "North Macedonia",
      "NOR" = "Norway",
      "OMN" = "Oman",
      "PAK" = "Pakistan",
      "PLW" = "Palau",
      "PSE" = "Palestine",
      "PAN" = "Panama",
      "PNG" = "Papua New Guinea",
      "PRY" = "Paraguay",
      "PER" = "Peru",
      "PHL" = "Philippines",
      "POL" = "Poland",
      "PRT" = "Portugal",
      "PRI" = "Puerto Rico",
      "PSE" = "Gaza Strip (Palestinian Territory)",
      "QAT" = "Qatar",
      "KOR" = "Republic of Korea",
      "MDA" = "Republic of Moldova",
      "ROU" = "Romania",
      "RUS" = "Russian Federation",
      "RWA" = "Rwanda",
      "KNA" = "Saint Kitts and Nevis",
      "LCA" = "Saint Lucia",
      "VCT" = "Saint Vincent and the Grenadines",
      "WSM" = "Samoa",
      "SMR" = "San Marino",
      "STP" = "Sao Tome and Principe",
      "SAU" = "Saudi Arabia",
      "SEN" = "Senegal",
      "SRB" = "Serbia",
      "SYC" = "Seychelles",
      "SLE" = "Sierra Leone",
      "SGP" = "Singapore",
      "SVK" = "Slovakia",
      "SVN" = "Slovenia",
      "SLB" = "Solomon Islands",
      "SOM" = "Somalia",
      "ZAF" = "South Africa",
      "SSD" = "South Sudan",
      "ESP" = "Spain",
      "LKA" = "Sri Lanka",
      "SDN" = "Sudan",
      "SUR" = "Suriname",
      "SWE" = "Sweden",
      "CHE" = "Switzerland",
      "SYR" = "Syrian Arab Republic",
      "TJK" = "Tajikistan",
      "THA" = "Thailand",
      "TLS" = "Timor-Leste",
      "TGO" = "Togo",
      "TKL" = "Tokelau",
      "TON" = "Tonga",
      "TTO" = "Trinidad and Tobago",
      "TUN" = "Tunisia",
      "TKM" = "Turkmenistan",
      "TUV" = "Tuvalu",
      "TUR" = "Türkiye",
      "UGA" = "Uganda",
      "UKR" = "Ukraine",
      "ARE" = "United Arab Emirates",
      "GBR" = "United Kingdom of Great Britain and Northern Ireland",
      "TZA" = "United Republic of Tanzania",
      "USA" = "United States of America",
      "URY" = "Uruguay",
      "UZB" = "Uzbekistan",
      "VUT" = "Vanuatu",
      "VEN" = "Venezuela (Bolivarian Republic of)",
      "VNM" = "Viet Nam",
      "YEM" = "Yemen",
      "ZMB" = "Zambia",
      "ZWE" = "Zimbabwe"
    )

    # Convert ISO3 to country name
    if (country_iso3 %in% names(iso3_to_country)) {
      area_name <- iso3_to_country[[country_iso3]]
    } else {
      stop(paste(
        "Country ISO3 code",
        country_iso3,
        "not found in EMPRES database.",
        "\nAvailable codes:",
        paste(names(iso3_to_country), collapse = ", ")
      ))
    }
  } else {
    area_name <- country_iso3 # Use as-is if not using lookup
  }

  # Comprehensive disease lookup for animals - each animal can get multiple diseases
  if (use_lookup && is.character(animals) && !all(animals == "All")) {
    animal_diseases <- list(
      # Cattle diseases
      "cattle" = c(
        "Foot and mouth disease",
        "Bovine tuberculosis",
        "Brucellosis",
        "Anthrax",
        "Rabies",
        "Rift Valley fever",
        "Bovine spongiform encephalopathy",
        "Contagious bovine pleuropneumonia",
        "Lumpy skin disease",
        "Theileriosis",
        "Trypanosomosis"
      ),

      # Swine diseases
      "pigs" = c(
        "African swine fever",
        "Classical swine fever",
        "Porcine reproductive and respiratory syndrome",
        "Porcine epidemic diarrhoea",
        "Swine vesicular disease",
        "Nipah virus infection",
        "Japanese encephalitis"
      ),

      # Poultry diseases
      "chicken" = c(
        "Influenza - Avian",
        "Newcastle disease",
        "Infectious bursal disease",
        "Marek's disease",
        "Infectious bronchitis",
        "Avian infectious laryngotracheitis",
        "Fowl cholera",
        "Salmonellosis"
      ),

      # Sheep diseases
      "sheep" = c(
        "Peste des petits ruminants",
        "Sheep pox",
        "Contagious caprine pleuropneumonia",
        "Brucellosis",
        "Anthrax",
        "Rabies",
        "Scrapie",
        "Foot and mouth disease"
      ),

      # Goat diseases
      "goats" = c(
        "Peste des petits ruminants",
        "Contagious caprine pleuropneumonia",
        "Brucellosis",
        "Anthrax",
        "Rabies",
        "Foot and mouth disease",
        "Caprine arthritis encephalitis"
      ),

      # Horse diseases
      "horses" = c(
        "African horse sickness",
        "Equine influenza",
        "Equine herpesvirus",
        "Equine infectious anaemia",
        "Glanders",
        "Venezuelan equine encephalomyelitis",
        "West Nile virus infection"
      ),

      # Aquaculture diseases
      "aquaculture" = c(
        "White spot disease",
        "Infectious haematopoietic necrosis",
        "Viral haemorrhagic septicaemia",
        "Koi herpesvirus disease",
        "Epizootic haematopoietic necrosis",
        "Infectious salmon anaemia",
        "Tilapia lake virus disease"
      ),

      # Bee diseases
      "bees" = c(
        "Varroosis of honey bees",
        "American foulbrood",
        "European foulbrood",
        "Nosemosis",
        "Tropilaelaps infestation",
        "Small hive beetle infestation"
      ),

      # Buffalo diseases
      "buffalo" = c(
        "Foot and mouth disease",
        "Bovine tuberculosis",
        "Brucellosis",
        "Anthrax",
        "Rift Valley fever",
        "Theileriosis"
      ),

      # Camel diseases
      "camels" = c(
        "Middle East respiratory syndrome",
        "Rift Valley fever",
        "Camel pox",
        "Trypanosomosis",
        "Brucellosis"
      ),

      # Deer diseases
      "deer" = c(
        "Chronic wasting disease",
        "Bovine tuberculosis",
        "Foot and mouth disease",
        "Brucellosis",
        "Anthrax"
      ),

      # Disease code shortcuts (for backward compatibility)
      "fmd" = "Foot and mouth disease",
      "asf" = "African swine fever",
      "ai" = "Influenza - Avian",
      "ppr" = "Peste des petits ruminants",
      "ahs" = "African horse sickness",
      "newcastle" = "Newcastle disease",
      "rabies" = "Rabies",
      "anthrax" = "Anthrax",
      "brucellosis" = "Brucellosis",
      "tuberculosis" = "Bovine tuberculosis",
      "covid" = "COVID-19 (SARS-COV-2)"
    )

    # Handle multiple animals
    if (length(animals) > 1) {
      # Check for invalid animals
      invalid_animals <- animals[!animals %in% names(animal_diseases)]
      if (length(invalid_animals) > 0) {
        valid_options <- paste(names(animal_diseases), collapse = ", ")
        stop(paste(
          "Invalid animal(s):",
          paste(invalid_animals, collapse = ", "),
          "\nValid options are:",
          valid_options
        ))
      }

      # Collect all diseases for all animals
      all_diseases <- c()
      for (animal in animals) {
        if (animal %in% names(animal_diseases)) {
          diseases <- animal_diseases[[animal]]
          # Handle both vectors (multiple diseases) and single strings
          if (is.vector(diseases) && length(diseases) > 1) {
            all_diseases <- c(all_diseases, diseases)
          } else {
            all_diseases <- c(all_diseases, diseases)
          }
        }
      }

      # Remove duplicates while preserving order
      disease <- unique(all_diseases)
    } else {
      # Single animal
      if (animals %in% names(animal_diseases)) {
        diseases <- animal_diseases[[animals]]
        # Handle both vectors (multiple diseases) and single strings
        if (is.vector(diseases) && length(diseases) > 1) {
          disease <- diseases
        } else {
          disease <- diseases
        }
      } else {
        disease <- animals
      }
    }
  } else {
    disease <- if (all(animals == "All")) "All" else animals
  }

  # Build parameters list
  params <- list(
    empresi_version = empresi_version,
    diagnosis_status = diagnosis_status,
    confidentiality_level = confidentiality_level
  )

  # Add area if specified
  if (!is.null(area_name)) {
    params$area <- area_name
  }

  # Add disease parameters - handle multiple diseases properly
  if (length(disease) > 1) {
    # For multiple diseases, add each as a separate disease parameter
    for (i in seq_along(disease)) {
      params[[paste0("disease", ifelse(i == 1, "", i))]] <- disease[i]
    }
  } else {
    # Single disease
    params$disease <- disease
  }

  # Set date parameters to NULL (as in original)
  params$start_observation_date <- NULL
  params$end_observation_date <- NULL

  # Build query string
  qs <- paste0(
    names(params),
    "=",
    vapply(params, URLencode, "", reserved = TRUE),
    collapse = "&"
  )

  url <- paste0(base, "?", qs)

  if (verbose) {
    message(sprintf("Fetching EMPRES-i data from API..."))
    if (!is.null(country_iso3)) {
      message(sprintf(
        "Country: %s",
        if (use_lookup) area_name else country_iso3
      ))
    }
    if (length(disease) > 1) {
      message(sprintf("Diseases: %s", paste(head(disease, 3), collapse = ", ")))
      if (length(disease) > 3) {
        message(sprintf("  (and %d more)", length(disease) - 3))
      }
    } else if (disease != "All") {
      message(sprintf("Disease: %s", disease))
    }
  }

  # Fetch data with retry logic
  retry_attempt <- 1
  success <- FALSE
  response <- NULL

  while (retry_attempt <= max_retries && !success) {
    tryCatch(
      {
        response <- httr2::request(url) |>
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
              "Failed to fetch EMPRES-i data after %d attempts. Last error: %s",
              max_retries,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      }
    )
  }

  # Parse JSON response
  tryCatch(
    {
      data <- jsonlite::fromJSON(httr2::resp_body_string(response))

      # Check if data is empty or NULL
      if (
        is.null(data) ||
          (is.data.frame(data) && nrow(data) == 0) ||
          (is.list(data) && length(data) == 0)
      ) {
        message("No EMPRES-i data found for the specified filters")
        return(invisible(NULL))
      }

      # Convert to data frame if it's a list
      if (is.list(data) && !is.data.frame(data)) {
        data <- as.data.frame(data)
      }

      message(sprintf(
        "✓ EMPRES-i data retrieved successfully: %s records",
        format(nrow(data), big.mark = ",")
      ))

      return(data)
    },
    error = function(e) {
      message(sprintf(
        "Error parsing EMPRES-i response: %s",
        conditionMessage(e)
      ))
      return(invisible(NULL))
    }
  )
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

  # Get all the fishstat tables
  tables <- list(
    fishstat::production,
    fishstat::area,
    fishstat::country,
    fishstat::measure,
    fishstat::source,
    fishstat::species,
    fishstat::status
  )

  # Join all tables using purrr::reduce and dplyr::left_join
  prod_all <- purrr::reduce(tables, dplyr::left_join)

  # Filter by iso3 if provided
  if (!is.null(iso3)) {
    if (verbose) {
      message(sprintf("Filtering data for country: %s", iso3))
    }
    prod_all <- prod_all %>%
      dplyr::filter(`iso3` == iso3)
  }

  message(sprintf(
    "✓ Fishstat data retrieved successfully: %s rows",
    format(nrow(prod_all), big.mark = ",")
  ))

  return(prod_all)
}
