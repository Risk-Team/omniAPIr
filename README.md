# omniAPIr

> Unified Interface to Multiple International Data APIs

## Overview

`omniAPIr` provides a consistent and user-friendly interface to retrieve data from 17+ major international APIs, including conflict data, health statistics, agricultural information, humanitarian data, development indicators, marine data, and more. All functions feature:

- **Consistent parameter naming** (e.g., `iso3`, `indicators`, `mrv`)
- **Automatic pagination** for large datasets
- **Smart year discovery** with automatic fallback to available data
- **Comprehensive error handling** and informative messages
- **Built-in lookup tables** for common items (crops, animals, diseases)
- **Helper functions** to discover available indicators and metadata

## Installation

```r
# Install from GitHub (includes all optional dependencies)
# install.packages("devtools")
devtools::install_github("Risk-Team/omniAPIr")

# For Google Earth Engine data (download_and_process_ee_image/vector)
# Requires Python packages: pip install earthengine-api geemap

# For Copernicus Marine data
# Requires Python package: pip install copernicusmarine
```

## Python Dependencies

The World Bank functions (`get_wb_data`, `list_un_indicators` with World Bank) require Python and the `wbgapi` package installed in a conda environment:

```bash
# Create conda environment
conda create -n your_env_name python=3.9
conda activate your_env_name
pip install wbgapi
```

Google Earth Engine and Copernicus Marine functions also use Python through
`reticulate`. For Copernicus Marine, use Python >= 3.10 and install the
Copernicus Marine Toolbox in the conda environment passed to `conda_env`.
Install `r-ncdf4` and `r-ncmeta` to enable the R NetCDF fallback used when
GDAL cannot read Copernicus NetCDF files directly:

```bash
conda create -n marine_env python=3.10
conda activate marine_env
pip install copernicusmarine
mamba install -c conda-forge r-ncdf4 r-ncmeta
```

## Quick Start

```r
library(omniAPIr)

# View all available APIs
get_api_info()

# Discover indicators for UN data sources
who_indicators <- list_un_indicators("WHO")
ilo_unemployment <- list_un_indicators("ILO", search = "unemployment")

# Discover FAOSTAT metadata (use get_api_info for general info)
api_info <- get_api_info("FAOSTAT")

# Fetch data
cattle_data <- get_faostat_data(
  element = "2111",
  item = "cattle",
  database = "QCL",
  iso3 = "USA",
  mrv = 10
)
```

## Available APIs

Use `get_api_info()` to view all available APIs and their documentation:

### Discovering Indicators

**For UN data sources** (World Bank, UNSDG, UNDP, ILO, WHO), use `list_un_indicators()`:

```r
# List all WHO health indicators
who_indicators <- list_un_indicators("WHO")

# Search for specific indicators
unemployment_indicators <- list_un_indicators("ILO", search = "unemployment")

# List World Bank indicators (requires conda environment)
wb_indicators <- list_un_indicators("WorldBank", conda_env = "your_env_name")

# Search UN SDG indicators
education_sdg <- list_un_indicators("UNSDG", search = "education")
```

**For FAOSTAT**, use the built-in lookup tables and function documentation:

```r
# Get API information for FAOSTAT
api_info <- get_api_info("FAOSTAT")

# Use built-in lookup tables for common items
# See ?get_faostat_data for supported items and elements
```

### Supported Data Sources

| Function | API | Authentication | Python Required |
|----------|-----|----------------|-----------------|
| `get_acled_data()` | ACLED - Conflict Events | Yes (email/password) | No |
| `get_ilo_data()` | ILO - Labor Statistics | No | No |
| `get_who_data()` | WHO GHO - Health Indicators | No | No |
| `get_faostat_data()` | FAOSTAT - Agriculture Data | No | No |
| `get_wb_data()` | World Bank - Development Indicators | No | **Yes** |
| `get_unsdg_data()` | UN SDG - SDG Indicators | No | No |
| `get_fao_fra_data()` | FAO FRA - Forest Data | No | No |
| `get_undp_data()` | UNDP HDR - Human Development | Yes (API key) | No |
| `get_empres_data()` | FAO EMPRES-i - Animal Diseases | Yes (API key) | No |
| `get_and_process_ibat_data()` | IBAT - Biodiversity | Yes (API key + token) | No |
| `get_giga_schools_data()` | Giga - School Connectivity | Yes (token) | No |
| `get_ndc_data()` | Climate Watch - NDC Data | No | No |
| `get_hdx_hapi_*()` | HDX Humanitarian API - HAPI | Yes (app identifier) | No |
| `get_invasive_alien_species()` | GBIF/GRIIS - Invasive Species | No | No |
| `get_osm_feature_class()`, `get_osm_features()` | OpenStreetMap - Geographic Features | No | No |
| `get_fishstat_data()` | FAO Fishstat - Fishery Statistics | No | No |
| `list_copernicus_marine_catalogue()`, `download_copernicus_marine()`, `download_and_process_copernicus_marine()` | Copernicus Marine | Yes (username/password) | **Yes** |

## Quick Start Examples

### ACLED - Conflict Data

```r
# Get conflict events for Kenya in 2023
acled_data <- get_acled_data(
  email.address = "your.email@example.com",
  password = "your_password",
  country = "Kenya",
  start.date = "2023-01-01",
  end.date = "2023-12-31"
)

# If your ACLED account can log in but the endpoint rejects OAuth,
# force the documented cookie-auth flow:
acled_data <- get_acled_data(
  email.address = "your.email@example.com",
  password = "your_password",
  country = "Kenya",
  auth_method = "cookie"
)
```

### ILO - Labor Statistics

```r
# Get unemployment data for multiple countries
ilo_data <- get_ilo_data(
  iso3 = c("KEN", "UGA", "TZA"),
  indicators = "UNE_DEAP_SEX_AGE_RT_A",
  mrv = 10
)
```

### FAOSTAT - Agriculture Data

```r
# Option 1: Use friendly names with built-in lookup
cattle_data <- get_faostat_data(
  element = "2111",      # Livestock stocks
  item = "cattle",       # Friendly name (auto-converted to code)
  database = "QCL",
  iso3 = "KEN",
  mrv = 20
)

# Get multiple crop production data
crop_data <- get_faostat_data(
  element = "2413",                      # Production
  item = c("wheat", "maize", "rice"),    # Multiple items
  database = "QCL"
)

# Option 2: Use exact codes (see function documentation for supported elements/items)
wheat_production <- get_faostat_data(
  element = "5510",  # Production
  item = "15",       # Wheat
  database = "QCL",
  iso3 = "USA",
  use_lookup = FALSE  # Use exact codes
)
```

### WHO - Health Indicators

```r
# Get life expectancy data
who_data <- get_who_data(
  iso3 = "KEN",
  indicators = c("WHOSIS_000001", "WHOSIS_000015"),
  mrv = 15
)
```

### HDX HAPI - Humanitarian Data

```r
# Generate an HDX HAPI app identifier once, then store it in
# HDX_HAPI_APP_IDENTIFIER for regular use.
app_id <- encode_hapi_app_identifier(
  application = "my-analysis",
  email = "me@example.org"
)
Sys.setenv(HDX_HAPI_APP_IDENTIFIER = app_id)

# Check availability before requesting a heavy endpoint
ken_available <- get_hdx_hapi_availability(
  iso3 = "KEN",
  category = "food-security-nutrition-poverty"
)

# Fetch all Kenya WFP food prices for the latest two years available
ken_prices <- get_hdx_hapi_wfp_prices(
  iso3 = "KEN",
  commodity_name = "Maize",
  mrv = 2
)

# Convert market price records with lon/lat to sf
library(sf)
ken_prices_sf <- get_hdx_hapi_wfp_prices(
  iso3 = "KEN",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  as_sf = TRUE
)
```

### World Bank - Development Indicators

```r
# Requires Python wbgapi package in conda environment
wb_data <- get_wb_data(
  indicators = c("SP.POP.TOTL", "NY.GDP.MKTP.CD"),
  iso3 = "KEN",
  mrv = 10,
  conda_env = "your_env_name"
)
```

### Copernicus Marine - Marine Data

```r
# Discover catalogue metadata
catalogue <- list_copernicus_marine_catalogue(
  contains = c("global", "temperature"),
  conda_env = "marine_env"
)

# Download a NetCDF subset and process it as a terra raster
marine_temp <- download_and_process_copernicus_marine(
  dataset_id = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m",
  output_filename = "marine_temperature.nc",
  variables = "thetao",
  bbox = c(-45, -10, -35, 5),
  start_datetime = "2024-01-01",
  end_datetime = "2024-01-31",
  username = "your_username",
  password = "your_password",
  conda_env = "marine_env"
)
```

### EMPRES-i - Animal Disease Outbreaks

```r
# Set once for the public API
Sys.setenv(EMPRES_API_KEY = "your_api_key")

# Get domestic cattle events for Kenya
empres_data <- get_empres_data(
  country_iso3 = "KEN",
  specie = "Cattle",
  specie_type = "Domestic",
  specie_class = "Mammal"
)

# Get specific disease data
fmd_data <- get_empres_data(
  country_iso3 = "KEN",
  disease = "Foot and mouth disease"
)

# Invalid disease names fail before the API call and list supported values
get_empres_data(disease = "fake disease")

# Invalid species values do the same
get_empres_data(specie = "fake species")
```

### OpenStreetMap - Geographic Features

```r
library(sf)

# Define region of interest
region <- st_read("region.shp")

# Inspect available reusable OSM feature classes
list_osm_feature_classes()

# Fetch food retail locations
food_retail <- get_osm_feature_class(
  region_sf = region,
  feature_classes = "food_retail",
  cache_dir = "osm-cache"
)
food_retail_points <- food_retail$food_retail$pts

# Fetch health facilities and schools together with one combined OSM query
social_services <- get_osm_feature_class(
  region_sf = region,
  feature_classes = c("health_facilities", "schools"),
  cache_dir = "osm-cache"
)
health_facilities <- social_services$health_facilities$pts
schools <- social_services$schools$pts

# Fetch livestock-related infrastructure
livestock_services <- get_osm_feature_class(
  region_sf = region,
  feature_classes = c("slaughterhouses", "veterinary_services"),
  cache_dir = "osm-cache"
)
slaughterhouses <- livestock_services$slaughterhouses$pts
veterinary_services <- livestock_services$veterinary_services$pts

# Low-level custom tag-based queries are still available
osm_data <- get_osm_features(
  region_sf = region,
  tag_sets = list(
    amenity = c("school", "hospital"),
    highway = "primary"
  )
)
roads <- osm_data$lines
```

## Key Features

### 1. Smart Year Discovery

All time-series functions automatically discover available years if requested data is not found:

```r
# Requests last 23 years, but automatically falls back if data unavailable
ilo_data <- get_ilo_data(
  iso3 = "KEN",
  indicators = "UNE_DEAP_SEX_AGE_RT_A",
  mrv = 23  # Will find the most recent available data
)
```

### 2. Built-in Lookup Tables and Validated Filters

FAOSTAT includes friendly name lookups. EMPRES validates public API filter values before requesting data:

```r
# FAOSTAT crops
get_faostat_data(element = "2413", item = "wheat", database = "QCL")

# FAOSTAT animals
get_faostat_data(element = "2111", item = c("cattle", "sheep", "goats"), database = "QCL")

# EMPRES public API species and disease filters
get_empres_data(country_iso3 = "KEN", specie = "Cattle", specie_type = "Domestic")
get_empres_data(country_iso3 = "KEN", disease = "Foot and mouth disease")
```

### 3. Automatic Pagination

Functions handling large datasets automatically paginate:

```r
# ACLED automatically handles pagination (5000 rows per page)
acled_data <- get_acled_data(
  email.address = "your.email@example.com",
  password = "your_password",
  country = c("Kenya", "Ethiopia", "Somalia")
  # Returns ALL matching records across multiple pages
)
```

### 4. Consistent Return Formats

All functions return data.frames or tibbles with standardized column names:
- `isocode` or `iso3` for country codes
- `Year` for temporal data
- `Value` for indicator values

## Parameter Conventions

- **`iso3`**: ISO3 country code(s) (e.g., "KEN", "USA")
- **`indicators`**: Indicator code(s) specific to each API
- **`mrv`**: Most Recent Values - number of years to retrieve
- **`start.date` / `end.date`**: Date filters in "YYYY-MM-DD" format

## Authentication

Some APIs require authentication credentials:

1. **ACLED**: Email and password (register at https://acleddata.com)
2. **UNDP HDR**: API key (request at https://hdr.undp.org)
3. **IBAT**: API key and token (requires subscription at https://ibat-alliance.org)
4. **Giga**: Bearer token (contact Giga Initiative)
5. **HDX HAPI**: App identifier. Generate one with `encode_hapi_app_identifier()` and set `HDX_HAPI_APP_IDENTIFIER`.

## Error Handling

All functions include comprehensive error handling with informative messages:

```r
# Invalid indicator
get_ilo_data(iso3 = "KEN", indicators = "INVALID")
# Error: indicators parameter is required and must be a non-empty vector

# Invalid country code
get_faostat_data(element = "2111", item = "invalid_animal", database = "QCL")
# Error: Invalid item(s) for element 2111: invalid_animal
# Valid options are: cattle, sheep, chicken, goats, ...
```

## Documentation

Each function includes comprehensive documentation accessible via `?`:

```r
?get_acled_data
?get_faostat_data
?get_wb_data
```

## Testing

The package includes comprehensive tests for all API functions:

```r
# Run automated tests
devtools::test()
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

GPL-3

## Citation

```r
citation("omniAPIr")
```

## Support

For issues and feature requests, please use the [GitHub issue tracker](https://github.com/Risk-Team/omniAPIr/issues).

## Acknowledgments

This package provides R interfaces to the following data sources:
- ACLED (Armed Conflict Location & Event Data Project)
- ILO (International Labour Organization)
- WHO (World Health Organization)
- FAO (Food and Agriculture Organization)
- World Bank
- United Nations (SDG, UNDP)
- IBAT Alliance
- Giga Initiative
- Climate Watch
- HDX Humanitarian API
- GBIF (Global Biodiversity Information Facility)
- OpenStreetMap

All data remains the property of their respective providers. Please cite the original data sources when using data retrieved through this package.

