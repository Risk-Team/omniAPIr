# omniAPIr

> Unified Interface to Multiple International Data APIs

## Overview

`omniAPIr` provides a consistent and user-friendly interface to retrieve data from 15+ major international APIs, including conflict data, health statistics, agricultural information, development indicators, and more. All functions feature:

- **Consistent parameter naming** (e.g., `iso3`, `indicators`, `mrv`)
- **Automatic pagination** for large datasets
- **Smart year discovery** with automatic fallback to available data
- **Comprehensive error handling** and informative messages
- **Built-in lookup tables** for common items (crops, animals, diseases)
- **Helper functions** to discover available indicators and metadata

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("Risk-Team/omniAPIr")

```

## Python Dependencies

The World Bank functions (`get_wb_data`, `list_un_indicators` with World Bank) require Python and the `wbgapi` package installed in a conda environment:

```bash
# Create conda environment
conda create -n your_env_name python=3.9
conda activate your_env_name
pip install wbgapi
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
| `get_empres_data()` | FAO EMPRES-i - Animal Diseases | No | No |
| `get_and_process_ibat_data()` | IBAT - Biodiversity | Yes (API key + token) | No |
| `get_giga_schools_data()` | Giga - School Connectivity | Yes (token) | No |
| `get_ndc_data()` | Climate Watch - NDC Data | No | No |
| `get_invasive_alien_species()` | GBIF/GRIIS - Invasive Species | No | No |
| `get_osm_features()` | OpenStreetMap - Geographic Features | No | No |
| `get_fishstat_data()` | FAO Fishstat - Fishery Statistics | No | No |

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

### EMPRES-i - Animal Disease Outbreaks

```r
# Get cattle disease outbreak data for Kenya
empres_data <- get_empres_data(
  country_iso3 = "KEN",
  animals = "cattle"  # Auto-converts to relevant diseases
)

# Multiple animal types
empres_data <- get_empres_data(
  country_iso3 = "ETH",
  animals = c("cattle", "sheep", "goats")
)

# Get specific disease data using shortcuts
fmd_data <- get_empres_data(
  country_iso3 = "KEN",
  animals = "fmd"  # Foot and mouth disease
)
```

### OpenStreetMap - Geographic Features

```r
library(sf)

# Define region of interest
region <- st_read("region.shp")

# Get schools and hospitals
osm_data <- get_osm_features(
  region_sf = region,
  tag_sets = list(
    "amenity" = c("school", "hospital"),
    "highway" = "primary"
  )
)

# Access different geometry types
schools_points <- osm_data$pts
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

### 2. Built-in Lookup Tables

FAOSTAT and EMPRES functions include friendly name lookups:

```r
# FAOSTAT crops
get_faostat_data(element = "2413", item = "wheat", database = "QCL")

# FAOSTAT animals
get_faostat_data(element = "2111", item = c("cattle", "sheep", "goats"), database = "QCL")

# EMPRES disease shortcuts
get_empres_data(country_iso3 = "KEN", animals = "fmd")  # Foot and mouth disease
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
- GBIF (Global Biodiversity Information Facility)
- OpenStreetMap

All data remains the property of their respective providers. Please cite the original data sources when using data retrieved through this package.

