#' Download and Process Google Earth Engine Image Data
#'
#' Downloads raster data from Google Earth Engine (GEE) and processes it for a specific region.
#' Supports both ee.Image and ee.ImageCollection objects, with optional band selection,
#' multiplication factors, and custom processing functions.
#'
#' @param asset_id Either a string path to EE asset OR an ee.Image/ee.ImageCollection object
#' @param output_filename Character string specifying the output file path
#' @param res Numeric resolution in meters for the output raster
#' @param region Earth Engine geometry object defining the region of interest
#' @param region_sf sf object defining the region for cropping and masking
#' @param crs Character string specifying the coordinate reference system (default: "EPSG:4326")
#' @param band Character string or vector specifying which bands to select (optional)
#' @param multiply_factor Numeric factor to multiply pixel values by (optional)
#' @param others Function to apply additional processing to the EE object (optional)
#' @param convert_uint8 Logical, whether to convert to 8-bit unsigned integer (default: TRUE)
#' @param conda_env Character string specifying the conda environment with ee and geemap packages
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#' @param max_retries Numeric, maximum number of retry attempts for API calls (default: 3)
#'
#' @return A terra raster object cropped and masked to the specified region
#'
#' @details
#' This function provides a comprehensive interface for downloading and processing Google Earth Engine
#' raster data. It automatically handles both Image and ImageCollection objects, applies optional
#' transformations, and returns a processed terra raster object ready for analysis.
#'
#' The function requires Python packages 'ee' (earthengine-api) and 'geemap' to be installed in
#' the specified conda environment. Make sure to authenticate with Google Earth Engine before use.
#'
#' @examples
#' \dontrun{
#' # Download Landsat 8 surface reflectance for a region
#' library(sf)
#' library(terra)
#'
#' # Define region
#' region_sf <- st_read("study_area.shp")
#' region_ee <- geemap::sf_as_ee(region_sf)
#'
#' # Download image
#' raster_data <- download_and_process_ee_image(
#'   asset_id = "LANDSAT/LC08/C02/T1_L2",
#'   output_filename = "landsat8.tif",
#'   res = 30,
#'   region = region_ee,
#'   region_sf = region_sf,
#'   band = c("SR_B4", "SR_B3", "SR_B2"),  # RGB bands
#'   conda_env = "gee_env"
#' )
#'
#' # Download with custom processing
#' ndvi_data <- download_and_process_ee_image(
#'   asset_id = "LANDSAT/LC08/C02/T1_L2",
#'   output_filename = "ndvi.tif",
#'   res = 30,
#'   region = region_ee,
#'   region_sf = region_sf,
#'   others = function(img) {
#'     # Calculate NDVI
#'     ndvi <- img$normalizedDifference(c("SR_B5", "SR_B4"))
#'     return(ndvi)
#'   },
#'   conda_env = "gee_env"
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @export
download_and_process_ee_image <- function(
    asset_id, # Either a string path to EE asset OR an ee.Image/ee.ImageCollection object
    output_filename,
    res,
    region,
    region_sf,
    crs = "EPSG:4326",
    band = NULL,
    multiply_factor = NULL,
    others = NULL,
    convert_uint8 = TRUE,
    conda_env = NULL,
    verbose = TRUE,
    max_retries = 3
) {
    # Validate conda_env parameter
    if (is.null(conda_env)) {
        stop(
            "conda_env parameter is required for Google Earth Engine functions"
        )
    }

    # Set up reticulate with conda environment
    reticulate::use_condaenv(conda_env, required = TRUE)

    # Import Python modules with retry logic
    ee <- NULL
    geemap <- NULL

    for (attempt in 1:max_retries) {
        tryCatch(
            {
                ee <- reticulate::import("ee")
                geemap <- reticulate::import("geemap")
                break
            },
            error = function(e) {
                if (attempt == max_retries) {
                    stop(
                        "Failed to import ee and geemap modules after ",
                        max_retries,
                        " attempts. ",
                        "Make sure the packages are installed in the conda environment: ",
                        conda_env
                    )
                }
                if (verbose) {
                    message("Attempt ", attempt, " failed, retrying...")
                }
                Sys.sleep(2^attempt) # Exponential backoff
            }
        )
    }

    if (verbose) {
        message("Processing Google Earth Engine image data...")
    }

    # Check if asset_id is already an EE object (Image or ImageCollection)
    is_ee_object <- tryCatch(
        {
            # Check if it has EE object methods (not a character string)
            !is.character(asset_id) &&
                inherits(asset_id, "python.builtin.object")
        },
        error = function(e) FALSE
    )


    # If already an EE object, use it directly; otherwise load from asset_id string
    if (is_ee_object) {
        # asset_id is already an ee.Image or ee.ImageCollection object
        asset_obj <- asset_id

        # Check if it's an Image or ImageCollection
        is_image <- tryCatch(
            {
                # Try to call getInfo on it as an Image
                asset_obj$getInfo()$type == "Image"
            },
            error = function(e) FALSE
        )

        # Apply band selection and multiplication if it's an Image
        if (is_image) {
            if (!is.null(band)) {
                asset_obj <- asset_obj$select(band)
            }
            if (!is.null(multiply_factor)) {
                asset_obj <- asset_obj$multiply(multiply_factor)
            }
        }
    } else {
        # asset_id is a string - load from asset path
        # Check if asset_id is an Image (TRUE if no error, FALSE if error)
        is_image <- tryCatch(
            {
                ee$Image(asset_id)$getInfo()
                TRUE
            },
            error = function(e) FALSE
        )

        # Assign asset_obj as Image or ImageCollection
        if (is_image) {
            asset_obj <- ee$Image(asset_id)
            if (!is.null(band)) {
                asset_obj <- asset_obj$select(band)
            }
            if (!is.null(multiply_factor)) {
                asset_obj <- asset_obj$multiply(multiply_factor)
            }
        } else {
            asset_obj <- ee$ImageCollection(asset_id)
            # For collections, band selection/multiplication should be handled in 'others'
        }
    }

    # Apply 'others' if provided (should return an ee$Image)
    asset_obj <- if (is.null(others)) asset_obj else others(asset_obj)

    # Only convert to uint8 if requested
    if (convert_uint8) {
        asset_obj <- asset_obj$uint8()
    }

    if (verbose) {
        message("Downloading image data...")
    }

    # Download with retry logic
    for (attempt in 1:max_retries) {
        tryCatch(
            {
                geemap$download_ee_image(
                    image = asset_obj,
                    filename = output_filename,
                    scale = res,
                    region = region,
                    crs = crs
                )
                break
            },
            error = function(e) {
                if (attempt == max_retries) {
                    stop(
                        "Failed to download image after ",
                        max_retries,
                        " attempts: ",
                        e$message
                    )
                }
                if (verbose) {
                    message(
                        "Download attempt ",
                        attempt,
                        " failed, retrying..."
                    )
                }
                Sys.sleep(2^attempt) # Exponential backoff
            }
        )
    }

    if (verbose) {
        message("Processing downloaded raster...")
    }

    raster_object <- terra::rast(output_filename)
    processed_raster <- terra::crop(raster_object, region_sf, mask = TRUE)

    if (verbose) {
        message("Image processing completed successfully")
    }

    return(processed_raster)
}

#' Download and Process Google Earth Engine Vector Data
#'
#' Downloads vector data from Google Earth Engine (GEE) FeatureCollections and processes it
#' for a specific region. Supports both Shapefile and GeoJSON output formats with automatic
#' filtering and intersection operations.
#'
#' @param asset_id Character string specifying the Earth Engine FeatureCollection asset path
#' @param output_filename Character string specifying the output file path
#' @param region Earth Engine geometry object defining the region of interest
#' @param region_sf sf object defining the region for intersection
#' @param column_empty_return Character string specifying column name for empty results
#' @param value_empty_return Value to assign when no features are found in the region
#' @param download_type Character string specifying output format: "shp" or "geojson" (default: "shp")
#' @param conda_env Character string specifying the conda environment with ee and geemap packages
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#' @param max_retries Numeric, maximum number of retry attempts for API calls (default: 3)
#'
#' @return An sf object containing the downloaded and processed vector data, or a modified
#'         version of region_sf with empty result values if no features are found
#'
#' @details
#' This function downloads vector data from Google Earth Engine FeatureCollections, automatically
#' filters features by the specified region, and returns an sf object ready for analysis.
#' If no features are found in the region, it returns a modified version of the input region
#' with specified empty values.
#'
#' The function requires Python packages 'ee' (earthengine-api) and 'geemap' to be installed in
#' the specified conda environment. Make sure to authenticate with Google Earth Engine before use.
#'
#' @examples
#' \dontrun{
#' # Download protected areas for a region
#' library(sf)
#'
#' # Define region
#' region_sf <- st_read("study_area.shp")
#' region_ee <- geemap::sf_as_ee(region_sf)
#'
#' # Download as Shapefile
#' protected_areas <- download_and_process_ee_vector(
#'   asset_id = "WCMC/WDPA/current/polygons",
#'   output_filename = "protected_areas.shp",
#'   region = region_ee,
#'   region_sf = region_sf,
#'   column_empty_return = "has_protected_areas",
#'   value_empty_return = FALSE,
#'   download_type = "shp",
#'   conda_env = "gee_env"
#' )
#'
#' # Download as GeoJSON
#' settlements <- download_and_process_ee_vector(
#'   asset_id = "TIGER/2018/Places",
#'   output_filename = "settlements.geojson",
#'   region = region_ee,
#'   region_sf = region_sf,
#'   column_empty_return = "settlement_count",
#'   value_empty_return = 0,
#'   download_type = "geojson",
#'   conda_env = "gee_env"
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @export
download_and_process_ee_vector <- function(
    asset_id,
    output_filename,
    region,
    region_sf,
    column_empty_return,
    value_empty_return,
    download_type = "shp",
    conda_env = NULL,
    verbose = TRUE,
    max_retries = 3
) {
    # Validate conda_env parameter
    if (is.null(conda_env)) {
        stop(
            "conda_env parameter is required for Google Earth Engine functions"
        )
    }

    # Validate download_type parameter
    if (!download_type %in% c("shp", "geojson")) {
        stop("download_type must be either 'shp' or 'geojson'")
    }

    # Set up reticulate with conda environment
    reticulate::use_condaenv(conda_env, required = TRUE)

    # Import Python modules with retry logic
    ee <- NULL
    geemap <- NULL

    for (attempt in 1:max_retries) {
        tryCatch(
            {
                ee <- reticulate::import("ee")
                geemap <- reticulate::import("geemap")
                break
            },
            error = function(e) {
                if (attempt == max_retries) {
                    stop(
                        "Failed to import ee and geemap modules after ",
                        max_retries,
                        " attempts. ",
                        "Make sure the packages are installed in the conda environment: ",
                        conda_env
                    )
                }
                if (verbose) {
                    message("Attempt ", attempt, " failed, retrying...")
                }
                Sys.sleep(2^attempt) # Exponential backoff
            }
        )
    }

    if (verbose) {
        message("Processing Google Earth Engine vector data...")
    }

    # Filter the asset by the download_region_ee *before* checking size or downloading
    filtered_asset <- ee$FeatureCollection(asset_id)$filterBounds(region)

    # Step 1: Check if the Earth Engine FeatureCollection is empty
    count <- filtered_asset$size()$getInfo()

    if (count == 0) {
        if (verbose) {
            message(
                "No features found in the specified region for asset: ",
                asset_id
            )
        }
        # Return a modified version of region_sf_for_empty_return
        return(dplyr::mutate(
            region_sf,
            !!column_empty_return := value_empty_return
        ))
    } else {
        if (verbose) {
            message("Found ", count, " features, downloading...")
        }

        # Step 2: If the collection is not empty, export and process
        # The region argument for ee_export_vector is implicitly handled by filtering
        # the asset_id with download_region_ee above.

        # Download with retry logic
        for (attempt in 1:max_retries) {
            tryCatch(
                {
                    if (download_type == "shp") {
                        geemap$ee_export_vector(
                            filtered_asset, # Use the filtered asset
                            output_filename
                        )
                        downloaded_sf <- sf::st_read(output_filename)
                    } else if (download_type == "geojson") {
                        geemap$ee_to_geojson(
                            filtered_asset, # Use the filtered asset
                            output_filename
                        )
                        downloaded_sf <- geojsonsf::geojson_sf(output_filename)
                    }
                    break
                },
                error = function(e) {
                    if (attempt == max_retries) {
                        stop(
                            "Failed to download vector data after ",
                            max_retries,
                            " attempts: ",
                            e$message
                        )
                    }
                    if (verbose) {
                        message(
                            "Download attempt ",
                            attempt,
                            " failed, retrying..."
                        )
                    }
                    Sys.sleep(2^attempt) # Exponential backoff
                }
            )
        }

        if (verbose) {
            message("Processing downloaded vector data...")
        }

        # Intersect the sf object
        intersected_sf <- sf::st_intersection(downloaded_sf, region_sf)

        if (verbose) {
            message("Vector processing completed successfully")
        }

        return(intersected_sf)
    }
}
