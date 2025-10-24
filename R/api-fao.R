#' Get FAOSTAT Agriculture Data
#'
#' @description
#' Retrieves agricultural statistics from the Food and Agriculture Organization
#' Statistics (FAOSTAT) API with built-in lookup tables for common items.
#'
#' **Note:** This function is designed for specific datasets and is not comprehensive.
#' It works best with QCL database and common agricultural elements (2111, 2413, 2515, 7209).
#'
#' @importFrom magrittr %>%
#'
#' @param element Character or numeric. Element code specifying the type of
#'   measurement (e.g., "2111" for livestock stocks, "2413" for crop production).
#' @param item Character or numeric. Item code(s) or common names. When use_lookup
#'   is TRUE, accepts friendly names like "cattle", "wheat", etc.
#' @param database Character. FAOSTAT database name (required).
#' @param item_cs Character. Item classification system. Default is NULL.
#' @param use_lookup Logical. Whether to use built-in lookup tables to convert
#'   common names to item codes. Default is TRUE.
#' @param mrv Integer. Most Recent Values - number of years to retrieve. Default is 50.
#' @param iso3 Character. ISO3 country code to filter data. Default is NULL (all countries).
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
#'   \item \strong{Element 2111 (Stocks)} - Livestock population data
#'   \item \strong{Element 2413 (Production)} - Crop production data
#'   \item \strong{Element 2515 (Agricultural Use)} - Fertilizer and pesticide use
#'   \item \strong{Element 7209 (Land Use)} - Land use statistics
#' }
#'
#' **Built-in lookup tables** support:
#' \itemize{
#'   \item \strong{Animals (Element 2111):} cattle, sheep, chicken, goats, pigs, horses, buffalo, camels, rabbits, ducks
#'   \item \strong{Crops (Element 2413):} wheat, rice, maize, barley, oats, rye, millet, sorghum, soybeans, sunflower, rapeseed, cotton, sugarcane, sugar_beet, potatoes, cassava
#'   \item \strong{Fertilizers (Element 2515):} npk, nitrogen, phosphate, potash, urea, ammonium_sulfate, calcium_phosphate, pesticides_total
#'   \item \strong{Land use (Element 7209):} agricultural_land, forest_land
#' }
#'
#' **Limitations:**
#' \itemize{
#'   \item Only works with specific element codes (2111, 2413, 2515, 7209)
#'   \item Primarily designed for QCL database
#'   \item Lookup tables are limited to common agricultural items
#'   \item For other databases/elements, use \code{use_lookup = FALSE} and provide exact codes
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
#' }
get_faostat_data <- function(
  element,
  item,
  database = NULL,
  item_cs = NULL,
  use_lookup = TRUE,
  mrv = 50,
  iso3 = NULL,
  verbose = FALSE,
  max_retries = 3
) {
  if (verbose) {
    message(sprintf("Automatically fetching last %d years of data", mrv))
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- (current_year - mrv + 1):current_year

  # Helper function to try fetching data with specific years
  try_fetch_data <- function(years_to_try) {
    # FAOSTAT Item Code Lookup Tables
    if (use_lookup && is.character(item)) {
      # Define lookup tables for different data types
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
        "npk" = "4021", # NPK fertilizers
        "nitrogen" = "4009", # Nitrogen fertilizers
        "phosphate" = "4019", # Phosphate fertilizers
        "potash" = "4029", # Potash fertilizers
        "urea" = "4014", # Urea
        "ammonium_sulfate" = "4010", # Ammonium sulfate
        "calcium_phosphate" = "4020", # Calcium phosphate
        "pesticides_total" = "1357", # Pesticides (total) - CORRECT CODE
        "total" = "1357"
      )

      land_use_lookup <- list(
        "agricultural_land" = "6610",
        "forest_land" = "6646"
      )

      # Determine which lookup table to use based on element
      lookup_table <- switch(
        as.character(element),
        "2111" = animal_lookup, # Livestock
        "2413" = crop_lookup, # Crop production
        "2515" = fertilizer_and_pesticides_lookup, # Agricultural use (fertilizers and pesticides)
        "7209" = land_use_lookup, # Land use
        NULL
      )

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
    # Items
    #  "866,1057,1016,976" Cattle, sheep, chicken and goats
    # "572,176,661,56,79,92,270,27,71,83,236,156,97,15"  several crops
    # "6610,6646" # agricultural land and forestry
    # "4021" # NPK fertilizers
    base <- paste0("https://faostatservices.fao.org/api/v1/en/data/", database)

    # Set area parameter based on whether iso3 is specified
    area_param <- if (!is.null(iso3)) {
      iso3 # Use specific country
    } else {
      "2,3,4,7,8,9,1,10,11,52,12,13,16,14,57,255,15,23,53,18,19,80,20,21,26,27,233,29,35,115,32,33,37,39,40,351,96,128,214,41,44,45,46,47,48,98,49,50,167,51,107,116,250,54,72,55,56,58,59,60,61,178,63,209,238,62,64,66,67,68,69,70,74,75,73,79,81,84,86,87,89,90,175,91,93,95,97,99,100,101,102,103,104,105,106,109,110,112,108,114,83,118,113,120,119,121,122,123,124,126,256,129,130,131,132,133,134,127,135,136,137,138,145,141,273,143,144,28,147,148,149,150,153,156,157,158,159,160,154,162,221,165,299,166,168,169,170,171,173,174,177,179,117,146,183,185,184,182,188,189,191,244,193,194,195,272,186,196,197,200,199,198,25,201,202,277,203,38,276,206,207,210,211,212,208,216,176,217,218,219,220,222,213,227,223,228,226,230,225,229,215,231,234,235,155,236,237,249,248,251,181" # All countries
    }

    params <- list(
      area = area_param,
      area_cs = "ISO3",
      element = element, #What kind of measurement/statistic you want (e.g., production, area harvested, yield, etc.)
      item = item, #What specific products/crops/commodities you want data for (e.g., wheat, maize, rice, etc.)
      year = paste(years_to_try, collapse = ","),
      show_codes = "true",
      show_unit = "true",
      show_flags = "true",
      show_notes = "true",
      null_values = "false",
      output_type = "csv"
    )

    # Add item_cs only if specified (some databases don't use it)
    if (!is.null(item_cs)) {
      params$item_cs <- item_cs
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

    # Fetch and parse the data with retry logic
    retry_attempt <- 1
    success <- FALSE
    response <- NULL

    while (retry_attempt <= max_retries && !success) {
      tryCatch(
        {
          response <- httr::GET(url)
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
        if (httr::status_code(response) == 200) {
          content <- httr::content(response, "text", encoding = "UTF-8")

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
                ) %>%
                dplyr::filter(year %in% years_to_try) %>%
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
          error_content <- httr::content(response, "text", encoding = "UTF-8")
          if (verbose) {
            message(sprintf("HTTP error %d", httr::status_code(response)))
            message(sprintf("Error: %s", error_content))
          }

          message(sprintf(
            "HTTP error %d from FAOSTAT API. Response: %s",
            httr::status_code(response),
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

    # Try progressively smaller year ranges to find available data
    year_ranges <- list(
      (current_year - 10):current_year, # Last 10 years
      (current_year - 20):current_year, # Last 20 years
      (current_year - 30):current_year, # Last 30 years
      1990:current_year, # Since 1990
      1980:current_year, # Since 1980
      1970:current_year, # Since 1970
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
        response <- httr::GET(query)

        if (httr::status_code(response) == 200) {
          success <- TRUE
        } else {
          stop(sprintf("HTTP error %d", httr::status_code(response)))
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

  result <- jsonlite::fromJSON(rawToChar(response$content))

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
        response <- httr::GET(url)

        if (httr::status_code(response) == 200) {
          success <- TRUE
        } else {
          stop(sprintf("HTTP error %d", httr::status_code(response)))
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
      data <- jsonlite::fromJSON(httr::content(
        response,
        "text",
        encoding = "UTF-8"
      ))

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
