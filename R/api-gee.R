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
#' @param max_tile_size Numeric, maximum download tile size in MB passed to
#'   geemap/geedim (default: NULL, geedim's default). Lower it when tiles fail
#'   with "User memory limit exceeded", e.g. for images aggregated with
#'   reduceResolution.
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
    max_retries = 3,
    max_tile_size = NULL
) {
    # Validate conda_env parameter
    if (is.null(conda_env)) {
        stop(
            "conda_env parameter is required for Google Earth Engine functions"
        )
    }

    # geedim reopens downloads for writing; GDAL's LIBERTIFF driver is read-only.
    gdal_skip <- Sys.getenv("GDAL_SKIP")
    if (!grepl("(^|,)\\s*LIBERTIFF\\s*(,|$)", gdal_skip)) {
        Sys.setenv(GDAL_SKIP = paste(
            c(gdal_skip[nzchar(gdal_skip)], "LIBERTIFF"),
            collapse = ","
        ))
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
                    crs = crs,
                    max_tile_size = max_tile_size
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
#' @param ee_filter Optional Earth Engine `Filter`, applied after the region filter and
#'   before download. A function is called with the filtered FeatureCollection and must
#'   return a FeatureCollection.
#' @param properties Optional character vector of FeatureCollection properties to download.
#'   Geometry is retained.
#' @param geometry_type Character, `"polygon"` (default) or `"line"`. `"line"` drops
#'   non-line pieces left by the country intersection so the result stays lines.
#' @param max_features_per_request Positive integer no greater than 5000. Feature
#'   collections larger than this are downloaded in pages and combined locally
#'   (default: 4000).
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
#'
#' # Main river reaches only, kept as lines
#' ee <- reticulate::import("ee")
#' rivers <- download_and_process_ee_vector(
#'   asset_id = "WWF/HydroSHEDS/v1/FreeFlowingRivers",
#'   output_filename = "rivers.geojson",
#'   region = region_ee,
#'   region_sf = region_sf,
#'   column_empty_return = "CSI",
#'   value_empty_return = NA_real_,
#'   download_type = "geojson",
#'   conda_env = "gee_env",
#'   ee_filter = ee$Filter$lte("RIV_ORD", 6),
#'   properties = c("DOF", "DOR", "CSI", "CSI_D", "DIS_AV_CMS", "RIV_ORD"),
#'   geometry_type = "line"
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
    max_retries = 3,
    ee_filter = NULL,
    properties = NULL,
    geometry_type = c("polygon", "line"),
    max_features_per_request = 4000
) {
    geometry_type <- match.arg(geometry_type)
    if (
        length(max_features_per_request) != 1 ||
            !is.numeric(max_features_per_request) ||
            is.na(max_features_per_request) ||
            !is.finite(max_features_per_request) ||
            max_features_per_request < 1 ||
            max_features_per_request > 5000 ||
            max_features_per_request != as.integer(max_features_per_request)
    ) {
        stop(
            "`max_features_per_request` must be an integer from 1 to 5000.",
            call. = FALSE
        )
    }
    max_features_per_request <- as.integer(max_features_per_request)
    if (
        !is.null(properties) &&
            (
                !is.character(properties) ||
                    length(properties) == 0 ||
                    anyNA(properties) ||
                    any(!nzchar(properties))
            )
    ) {
        stop(
            "`properties` must be a non-empty character vector of Earth Engine property names.",
            call. = FALSE
        )
    }
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
    filtered_asset <- .prepare_ee_feature_collection(
        ee$FeatureCollection(asset_id)$filterBounds(region),
        ee_filter = ee_filter,
        properties = properties
    )

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

        page_specs <- .ee_vector_page_specs(
            count,
            max_features_per_request
        )
        page_count <- nrow(page_specs)
        if (page_count == 1) {
            page_filenames <- output_filename
        } else {
            page_dir <- tempfile(
                pattern = "ee-vector-pages-",
                tmpdir = dirname(output_filename)
            )
            dir.create(page_dir)
            on.exit(unlink(page_dir, recursive = TRUE, force = TRUE), add = TRUE)
            extension <- if (download_type == "shp") ".shp" else ".geojson"
            page_filenames <- file.path(
                page_dir,
                sprintf("page-%05d%s", seq_len(page_count), extension)
            )
            if (verbose) {
                message(
                    "Splitting download into ",
                    page_count,
                    " requests of at most ",
                    max_features_per_request,
                    " features"
                )
            }
        }

        downloaded_parts <- vector("list", page_count)
        for (page_index in seq_len(page_count)) {
            page_asset <- if (page_count == 1) {
                filtered_asset
            } else {
                ee$FeatureCollection(filtered_asset$toList(
                    page_specs$size[[page_index]],
                    page_specs$offset[[page_index]]
                ))
            }
            if (verbose && page_count > 1) {
                message(
                    "Downloading vector page ",
                    page_index,
                    " of ",
                    page_count
                )
            }

            for (attempt in seq_len(max_retries)) {
                tryCatch(
                    {
                        if (download_type == "shp") {
                            geemap$ee_export_vector(
                                page_asset,
                                page_filenames[[page_index]]
                            )
                            downloaded_parts[[page_index]] <- sf::st_read(
                                page_filenames[[page_index]],
                                quiet = TRUE
                            )
                        } else {
                            geemap$ee_to_geojson(
                                page_asset,
                                page_filenames[[page_index]]
                            )
                            downloaded_parts[[page_index]] <-
                                geojsonsf::geojson_sf(page_filenames[[page_index]])
                        }
                        break
                    },
                    error = function(e) {
                        if (attempt == max_retries) {
                            stop(
                                "Failed to download vector data page ",
                                page_index,
                                " of ",
                                page_count,
                                " after ",
                                max_retries,
                                " attempts: ",
                                e$message
                            )
                        }
                        if (verbose) {
                            message(
                                "Download attempt ",
                                attempt,
                                " for page ",
                                page_index,
                                " failed, retrying..."
                            )
                        }
                        Sys.sleep(2^attempt)
                    }
                )
            }
        }

        downloaded_sf <- dplyr::bind_rows(downloaded_parts)
        if (page_count > 1) {
            sf::st_write(
                downloaded_sf,
                output_filename,
                driver = if (download_type == "shp") {
                    "ESRI Shapefile"
                } else {
                    "GeoJSON"
                },
                delete_layer = TRUE,
                quiet = TRUE
            )
        }

        if (verbose) {
            message("Processing downloaded vector data...")
        }

        # Intersect the sf object
        intersected_sf <- sf::st_intersection(downloaded_sf, region_sf)
        if (geometry_type == "line") {
            intersected_sf <- .keep_linear_features(intersected_sf)
        }

        if (verbose) {
            message("Vector processing completed successfully")
        }

        return(intersected_sf)
    }
}

.ee_vector_page_specs <- function(count, max_features_per_request) {
    offsets <- seq.int(
        from = 0,
        to = count - 1,
        by = max_features_per_request
    )
    data.frame(
        offset = as.integer(offsets),
        size = as.integer(pmin(max_features_per_request, count - offsets))
    )
}

.prepare_ee_feature_collection <- function(
    feature_collection,
    ee_filter = NULL,
    properties = NULL
) {
    if (is.function(ee_filter)) {
        feature_collection <- ee_filter(feature_collection)
    } else if (!is.null(ee_filter)) {
        feature_collection <- feature_collection$filter(ee_filter)
    }

    if (!is.null(properties)) {
        feature_collection <- feature_collection$select(as.list(properties))
    }

    feature_collection
}

.keep_linear_features <- function(features) {
    if (nrow(features) == 0) {
        return(features)
    }

    linear <- suppressWarnings(
        sf::st_collection_extract(features, "LINESTRING")
    )
    linear[!sf::st_is_empty(sf::st_geometry(linear)), , drop = FALSE]
}
