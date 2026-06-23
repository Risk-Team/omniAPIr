#' Get API Information
#'
#' @description
#' Returns metadata about all APIs integrated in the omniAPIr package,
#' including API endpoints, documentation URLs, and external dependencies.
#'
#' For UN data sources (World Bank, UNSDG, UNDP, ILO, WHO), use
#' \code{list_un_indicators()} to discover available indicator codes.
#'
#' @param api_name Optional character string specifying a single API name.
#'   If NULL (default), returns information for all APIs.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{function_name}{Name of the R function}
#'     \item{api_name}{Human-readable API name}
#'     \item{base_url}{Base URL of the API endpoint}
#'     \item{api_docs_url}{Link to official API documentation}
#'     \item{requires_python}{Logical indicating if Python dependencies are required}
#'     \item{python_packages}{Character string of required Python packages (if any)}
#'     \item{r_packages}{Character string of required R packages (if any)}
#'     \item{requires_auth}{Logical indicating if authentication is required}
#'     \item{description}{Brief description of the API}
#'   }
#'
#' @export
#'
#' @examples
#' # Get all API information
#' get_api_info()
#'
#' # Get specific API information
#' get_api_info("ACLED")
get_api_info <- function(api_name = NULL) {
  api_registry <- data.frame(
    function_name = c(
      "get_acled_data",
      "get_ilo_data",
      "get_who_data",
      "get_faostat_data",
      "get_wb_data",
      "get_unsdg_data",
      "get_fao_fra_data",
      "get_undp_data",
      "get_empres_data",
      "get_and_process_ibat_data",
      "get_giga_schools_data",
      "get_ndc_data",
      "get_hdx_hapi",
      "get_invasive_alien_species",
      "get_osm_feature_class",
      "get_fishstat_data",
      "download_and_process_ee_image",
      "download_and_process_ee_vector",
      "get_fishwatch_data"
    ),
    api_name = c(
      "ACLED",
      "ILO",
      "WHO GHO",
      "FAOSTAT",
      "World Bank",
      "UN SDG",
      "FAO FRA",
      "UNDP HDR",
      "FAO EMPRES-i",
      "IBAT",
      "Giga Schools",
      "Climate Watch NDC",
      "HDX HAPI",
      "GBIF/GRIIS",
      "OpenStreetMap",
      "FAO Fishstat",
      "Google Earth Engine",
      "Google Earth Engine",
      "Global Fishing Watch"
    ),
    base_url = c(
      "https://acleddata.com/api/acled/read",
      "https://rplumber.ilo.org/data/indicator/",
      "https://ghoapi.azureedge.net/api/",
      "https://faostatservices.fao.org/api/v1/en/data/",
      "https://api.worldbank.org/v2/",
      "https://unstats.un.org/SDGAPI/v1/sdg/Indicator/Data",
      "https://fra-data.fao.org/api/cycle-data/table/table-data",
      "https://hdrdata.org/api/CompositeIndices/query-detailed",
      "https://fao-empp-data-explorer-be-175434516411.europe-west1.run.app/events",
      "https://app.ibat-alliance.org/api/v2/data-downloads",
      "https://uni-ooi-giga-maps-service.azurewebsites.net/api/v1/schools_location/",
      "https://www.climatewatchdata.org/api/v1/data/ndc_content",
      "https://hapi.humdata.org/api/v2/",
      "https://api.gbif.org/v1/",
      "https://download.geofabrik.de/",
      "N/A (R package data)",
      "https://earthengine.google.com/",
      "https://earthengine.google.com/",
      "N/A (R package data)"
    ),
    api_docs_url = c(
      "https://apidocs.acleddata.com/",
      "https://rplumber.ilo.org/__docs__/",
      "https://www.who.int/data/gho/info/gho-odata-api",
      "https://www.fao.org/faostat/en/#data",
      "https://github.com/tgherzog/wbgapi",
      "https://unstats.un.org/SDGAPI/swagger/",
      "https://fra-data.fao.org/static/assets/fra-api-swagger.json",
      "https://hdr.undp.org/data-center/documentation-and-downloads",
      "https://empres-i.apps.fao.org/",
      "https://www.ibat-alliance.org/ibat-conservation/login",
      "https://gigamaps.org/",
      "https://www.climatewatchdata.org/about/ndc",
      "https://hapi.humdata.org/docs",
      "https://www.gbif.org/developer/summary",
      "https://wiki.openstreetmap.org/wiki/API",
      "https://github.com/socialcopsdev/fishstat",
      "https://developers.google.com/earth-engine",
      "https://developers.google.com/earth-engine",
      "https://github.com/GlobalFishingWatch/gfwr/"
    ),
    requires_python = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE, # World Bank uses wbgapi Python package
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE, # Google Earth Engine requires Python packages
      TRUE, # Google Earth Engine requires Python packages
      FALSE
    ),
    python_packages = c(
      NA,
      NA,
      NA,
      NA,
      "wbgapi",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "ee, geemap",
      "ee, geemap",
      NA
    ),
    r_packages = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "rgbif",
      "osmextract",
      "fishstat",
      NA,
      NA,
      "gfwr"
    ),
    requires_auth = c(
      TRUE, # ACLED requires email/password
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE, # UNDP requires API key
      FALSE,
      TRUE, # IBAT requires API key and token
      TRUE, # Giga requires token
      FALSE,
      TRUE, # HDX HAPI requires an app identifier
      FALSE,
      FALSE,
      FALSE,
      TRUE, # Google Earth Engine requires authentication
      TRUE, # Google Earth Engine requires authentication
      TRUE # Global Fishing Watch requires API key
    ),
    description = c(
      "Armed Conflict Location & Event Data Project - conflict events database",
      "International Labour Organization - labor statistics",
      "World Health Organization Global Health Observatory - health indicators",
      "Food and Agriculture Organization Statistics - agriculture and food data",
      "World Bank Development Indicators - global development statistics",
      "United Nations Sustainable Development Goals - SDG indicators",
      "FAO Forest Resources Assessment - global forest data",
      "UNDP Human Development Reports - human development indices",
      "FAO Emergency Prevention System - animal disease outbreaks",
      "Integrated Biodiversity Assessment Tool - biodiversity data",
      "Giga Initiative - school connectivity mapping",
      "Climate Watch - Nationally Determined Contributions data",
      "HDX Humanitarian API - food security, WFP prices, population, poverty, and availability metadata",
      "Global Register of Introduced and Invasive Species via GBIF",
      "OpenStreetMap - reusable geographic feature classes and custom tag queries",
      "FAO Fisheries Statistics - fishery production data",
      "Google Earth Engine - satellite imagery and raster data processing",
      "Google Earth Engine - vector data and feature collection processing",
      "Global Fishing Watch - apparent fishing effort and vessel tracking data"
    ),
    stringsAsFactors = FALSE
  )
  if (!is.null(api_name)) {
    result <- api_registry[api_registry$api_name == api_name, ]
    if (nrow(result) == 0) {
      stop(
        "API '",
        api_name,
        "' not found. Available APIs: ",
        paste(api_registry$api_name, collapse = ", ")
      )
    }
    return(result)
  }
  return(api_registry)
}
