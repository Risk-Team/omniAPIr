#' @keywords internal
"_PACKAGE"

#' omniAPIr: Unified Interface to Multiple International Data APIs
#'
#' @description
#' Provides a unified interface to retrieve data from multiple international APIs
#' including ACLED, ILO, WHO, FAOSTAT, World Bank, UN SDG, UNDP,
#' FAO (FRA, EMPRES-i, Fishstat), IBAT, Giga, Climate Watch, HDX HAPI,
#' GBIF/GRIIS, OpenStreetMap, and Copernicus Marine.
#'
#' All functions feature consistent parameter naming, automatic pagination,
#' smart year discovery, and comprehensive error handling.
#'
#' Use \code{get_api_info()} to see all available APIs, their documentation URLs,
#' authentication requirements, and Python dependencies.
#'
#' Use \code{list_un_indicators()} to discover available indicators for UN data
#' sources (World Bank, UNSDG, UNDP, ILO, WHO).
#'
#' Use \code{list_faostat_metadata()} to discover available FAOSTAT databases,
#' elements, and items.
#'
#' @examples
#' \dontrun{
#' # View all available APIs with metadata
#' get_api_info()
#'
#' # Get specific API information
#' get_api_info("ACLED")
#'
#' # Discover UN indicators
#' who_indicators <- list_un_indicators("WHO")
#' ilo_unemployment <- list_un_indicators("ILO", search = "unemployment")
#'
#' # Discover FAOSTAT metadata
#' databases <- list_faostat_metadata("databases")
#' elements <- list_faostat_metadata("elements", database = "QCL")
#'
#' # Example: Fetch labor statistics
#' ilo_data <- get_ilo_data(
#'   iso3 = "KEN",
#'   indicators = "UNE_DEAP_SEX_AGE_RT_A",
#'   mrv = 10
#' )
#' }
#'
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check notes about NSE (non-standard evaluation) in dplyr
utils::globalVariables(c(
    # Common variables across multiple functions
    ".",
    "value",
    "sector",
    "theme",
    "country",
    "iso_code3",
    # NDC data variables
    "indicator_id",
    "subsector",
    "overview_category",
    "global_category",
    "key",
    "action",
    "target",
    "timeframe",
    "conditional_costs",
    "unconditional_costs",
    "has_action",
    "has_target",
    "has_timeframe",
    "has_cost_info",
    "with_targets",
    "with_timeframes",
    "with_cost_info",
    "n_actions",
    "total_actions",
    "actions",
    # Biodiversity data variables
    "id_no",
    "type",
    "countryCode",
    "isInvasive",
    "decimalLongitude",
    "decimalLatitude",
    # UN API variables
    "Year",
    "year",
    "timePeriodStart",
    "geoAreaCode",
    "geoAreaName",
    "series",
    "seriesDescription",
    "attributes.Units",
    "indicator",
    "api_response",
    "ref_area",
    "obs_value",
    "all_values",
    "extracted_data",
    "isocode",
    "indicator_id",
    "indicator_name",
    "code",
    "description",
    "id",
    "label.en",
    # FAOSTAT metadata and data variables
    "label",
    "unit",
    "year",
    "item_name",
    "element_name",
    "Area.Code..ISO3."
))
