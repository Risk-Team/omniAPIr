#' List Copernicus Marine Catalogue Metadata
#'
#' @description
#' Retrieves catalogue metadata from the Copernicus Marine Toolbox using the
#' Python \code{copernicusmarine} package.
#'
#' @param product_id Character. Optional Copernicus Marine product ID.
#' @param dataset_id Character. Optional Copernicus Marine dataset ID.
#' @param contains Character vector. Optional search tokens used to filter the
#'   catalogue.
#' @param conda_env Character. Conda environment name containing the
#'   \code{copernicusmarine} Python package.
#' @param show_all_versions Logical. If TRUE, include all dataset versions.
#'   Default is FALSE.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#' @param max_retries Integer. Maximum number of retry attempts for Python import
#'   and catalogue requests. Default is 3.
#'
#' @return A Python-backed Copernicus Marine catalogue object. Use
#'   \code{reticulate::py_to_r()} or object fields such as \code{$products} to
#'   inspect it.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' catalogue <- list_copernicus_marine_catalogue(
#'   contains = c("global", "temperature"),
#'   conda_env = "marine_env"
#' )
#' }
list_copernicus_marine_catalogue <- function(
  product_id = NULL,
  dataset_id = NULL,
  contains = NULL,
  conda_env = NULL,
  show_all_versions = FALSE,
  verbose = TRUE,
  max_retries = 3
) {
  cm <- .import_copernicus_marine(conda_env, verbose, max_retries)

  if (verbose) {
    message("Fetching Copernicus Marine catalogue metadata...")
  }

  args <- .drop_nulls(list(
    product_id = product_id,
    dataset_id = dataset_id,
    contains = .python_list_or_null(contains),
    show_all_versions = show_all_versions,
    disable_progress_bar = !isTRUE(verbose)
  ))

  .with_retries(
    expr = do.call(cm$describe, args),
    max_retries = max_retries,
    verbose = verbose,
    action = "fetch Copernicus Marine catalogue metadata"
  )
}

#' Download a Copernicus Marine Dataset Subset
#'
#' @description
#' Downloads a spatial, temporal, depth, and variable subset from Copernicus
#' Marine using the Python \code{copernicusmarine} package.
#'
#' @param dataset_id Character. Copernicus Marine dataset ID.
#' @param output_filename Character. Optional output file name. Extension is
#'   optional; Copernicus Marine uses \code{file_format} when needed.
#' @param output_directory Character. Output directory. Default is the current
#'   working directory.
#' @param variables Character vector. Optional variable names to extract.
#' @param region_sf Optional sf object. If supplied, its EPSG:4326 bounding box
#'   is used as the Copernicus Marine subset extent.
#' @param bbox Optional numeric vector defining the subset extent as
#'   \code{c(xmin, ymin, xmax, ymax)} or a named vector with names
#'   \code{xmin}, \code{ymin}, \code{xmax}, \code{ymax}. Ignored when
#'   \code{region_sf} is supplied.
#' @param start_datetime,end_datetime Character, Date, POSIXct, or NULL.
#'   Temporal subset bounds accepted by the Copernicus Marine Toolbox.
#' @param minimum_depth,maximum_depth Numeric. Optional depth bounds.
#' @param dataset_version Character. Optional dataset version.
#' @param dataset_part Character. Optional dataset part.
#' @param file_format Character. Output format. Common values are
#'   \code{"netcdf"}, \code{"zarr"}, \code{"csv"}, and \code{"parquet"}.
#'   Default is \code{"netcdf"}.
#' @param service Character. Optional Copernicus Marine service name or short
#'   name, for example \code{"geoseries"} or \code{"timeseries"}.
#' @param coordinates_selection_method Character. One of \code{"inside"},
#'   \code{"strict-inside"}, \code{"nearest"}, or \code{"outside"}.
#'   Default is \code{"inside"}.
#' @param username,password Character. Copernicus Marine credentials. If NULL,
#'   the Python toolbox falls back to environment variables or configured
#'   credentials.
#' @param credentials_file Character. Optional path to a Copernicus Marine
#'   credentials file.
#' @param conda_env Character. Conda environment name containing the
#'   \code{copernicusmarine} Python package.
#' @param overwrite Logical. Overwrite existing output files. Default is FALSE.
#' @param skip_existing Logical. Skip files that already exist. Default is FALSE.
#' @param dry_run Logical. If TRUE, validates the request and returns metadata
#'   without downloading. Default is FALSE.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#' @param max_retries Integer. Maximum number of retry attempts. Default is 3.
#' @param netcdf_compression_level Integer. NetCDF compression level from 0 to 9.
#'   Default is 0.
#' @param netcdf3_compatible Logical. Request NetCDF3-compatible output.
#'   Default is FALSE.
#' @param chunk_size_limit Integer. Copernicus Marine chunk size limit. Default
#'   is -1, matching the Python toolbox default.
#'
#' @return A tibble with Copernicus Marine response metadata, including the
#'   downloaded file path when available.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' response <- download_copernicus_marine(
#'   dataset_id = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m",
#'   output_filename = "global_temperature.nc",
#'   variables = "thetao",
#'   bbox = c(-45, -10, -35, 5),
#'   start_datetime = "2024-01-01",
#'   end_datetime = "2024-01-31",
#'   username = "your_username",
#'   password = "your_password",
#'   conda_env = "marine_env"
#' )
#' }
download_copernicus_marine <- function(
  dataset_id,
  output_filename = NULL,
  output_directory = ".",
  variables = NULL,
  region_sf = NULL,
  bbox = NULL,
  start_datetime = NULL,
  end_datetime = NULL,
  minimum_depth = NULL,
  maximum_depth = NULL,
  dataset_version = NULL,
  dataset_part = NULL,
  file_format = "netcdf",
  service = NULL,
  coordinates_selection_method = "inside",
  username = NULL,
  password = NULL,
  credentials_file = NULL,
  conda_env = NULL,
  overwrite = FALSE,
  skip_existing = FALSE,
  dry_run = FALSE,
  verbose = TRUE,
  max_retries = 3,
  netcdf_compression_level = 0,
  netcdf3_compatible = FALSE,
  chunk_size_limit = -1
) {
  .validate_copernicus_request(
    dataset_id = dataset_id,
    output_directory = output_directory,
    region_sf = region_sf,
    bbox = bbox,
    file_format = file_format,
    coordinates_selection_method = coordinates_selection_method,
    overwrite = overwrite,
    skip_existing = skip_existing,
    netcdf_compression_level = netcdf_compression_level
  )

  cm <- .import_copernicus_marine(conda_env, verbose, max_retries)
  extent <- .copernicus_extent(region_sf = region_sf, bbox = bbox)

  if (verbose) {
    message("Downloading Copernicus Marine subset...")
  }

  args <- .drop_nulls(c(
    list(
      dataset_id = dataset_id,
      dataset_version = dataset_version,
      dataset_part = dataset_part,
      username = username,
      password = password,
      variables = .python_list_or_null(variables),
      minimum_longitude = extent$minimum_longitude,
      maximum_longitude = extent$maximum_longitude,
      minimum_latitude = extent$minimum_latitude,
      maximum_latitude = extent$maximum_latitude,
      minimum_depth = minimum_depth,
      maximum_depth = maximum_depth,
      start_datetime = .datetime_or_null(start_datetime),
      end_datetime = .datetime_or_null(end_datetime),
      coordinates_selection_method = coordinates_selection_method,
      output_filename = output_filename,
      file_format = file_format,
      service = service,
      output_directory = output_directory,
      credentials_file = credentials_file,
      overwrite = overwrite,
      skip_existing = skip_existing,
      dry_run = dry_run,
      disable_progress_bar = !isTRUE(verbose),
      netcdf_compression_level = as.integer(netcdf_compression_level),
      netcdf3_compatible = netcdf3_compatible,
      chunk_size_limit = as.integer(chunk_size_limit)
    )
  ))

  response <- .with_retries(
    expr = do.call(cm$subset, args),
    max_retries = max_retries,
    verbose = verbose,
    action = "download Copernicus Marine subset"
  )

  .copernicus_subset_response_to_tibble(response)
}

#' Download and Process Copernicus Marine Raster Data
#'
#' @description
#' Downloads a Copernicus Marine NetCDF subset and loads it as a terra raster.
#' When \code{region_sf} is supplied, the raster can also be cropped and masked
#' to the region.
#'
#' @inheritParams download_copernicus_marine
#' @param crop Logical. If TRUE and \code{region_sf} is supplied, crop the raster
#'   to the region extent. Default is TRUE.
#' @param mask Logical. If TRUE and \code{region_sf} is supplied, mask the raster
#'   to the region geometry. Default is TRUE.
#'
#' @return A terra raster object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' region <- st_read("study_area.shp")
#'
#' temperature <- download_and_process_copernicus_marine(
#'   dataset_id = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m",
#'   output_filename = "temperature.nc",
#'   variables = "thetao",
#'   region_sf = region,
#'   start_datetime = "2024-01-01",
#'   end_datetime = "2024-01-31",
#'   username = "your_username",
#'   password = "your_password",
#'   conda_env = "marine_env"
#' )
#' }
download_and_process_copernicus_marine <- function(
  dataset_id,
  output_filename,
  output_directory = ".",
  variables = NULL,
  region_sf = NULL,
  bbox = NULL,
  start_datetime = NULL,
  end_datetime = NULL,
  minimum_depth = NULL,
  maximum_depth = NULL,
  dataset_version = NULL,
  dataset_part = NULL,
  service = NULL,
  coordinates_selection_method = "inside",
  username = NULL,
  password = NULL,
  credentials_file = NULL,
  conda_env = NULL,
  overwrite = FALSE,
  skip_existing = FALSE,
  verbose = TRUE,
  max_retries = 3,
  netcdf_compression_level = 0,
  netcdf3_compatible = FALSE,
  chunk_size_limit = -1,
  crop = TRUE,
  mask = TRUE
) {
  if (
    missing(output_filename) ||
      is.null(output_filename) ||
      !is.character(output_filename) ||
      length(output_filename) != 1 ||
      !nzchar(output_filename)
  ) {
    stop("output_filename is required for raster processing.", call. = FALSE)
  }

  response <- download_copernicus_marine(
    dataset_id = dataset_id,
    output_filename = output_filename,
    output_directory = output_directory,
    variables = variables,
    region_sf = region_sf,
    bbox = bbox,
    start_datetime = start_datetime,
    end_datetime = end_datetime,
    minimum_depth = minimum_depth,
    maximum_depth = maximum_depth,
    dataset_version = dataset_version,
    dataset_part = dataset_part,
    file_format = "netcdf",
    service = service,
    coordinates_selection_method = coordinates_selection_method,
    username = username,
    password = password,
    credentials_file = credentials_file,
    conda_env = conda_env,
    overwrite = overwrite,
    skip_existing = skip_existing,
    dry_run = FALSE,
    verbose = verbose,
    max_retries = max_retries,
    netcdf_compression_level = netcdf_compression_level,
    netcdf3_compatible = netcdf3_compatible,
    chunk_size_limit = chunk_size_limit
  )

  downloaded_file <- .copernicus_downloaded_file(response)
  if (is.na(downloaded_file) || !file.exists(downloaded_file)) {
    stop(
      "Copernicus Marine download completed but the output file could not be found.",
      call. = FALSE
    )
  }

  if (verbose) {
    message("Processing downloaded Copernicus Marine raster...")
  }

  raster_object <- .read_copernicus_raster(downloaded_file, variables)

  if (!is.null(region_sf) && (isTRUE(crop) || isTRUE(mask))) {
    region_sf <- sf::st_transform(region_sf, 4326)
    raster_object <- terra::crop(
      raster_object,
      terra::vect(region_sf),
      mask = isTRUE(mask)
    )
  }

  if (verbose) {
    message("Copernicus Marine raster processing completed successfully")
  }

  raster_object
}

.import_copernicus_marine <- function(conda_env, verbose, max_retries) {
  if (is.null(conda_env)) {
    stop(
      "conda_env parameter is required for Copernicus Marine functions. ",
      "Specify the conda environment name containing the copernicusmarine Python package.",
      call. = FALSE
    )
  }

  reticulate::use_condaenv(conda_env, required = TRUE)

  .with_retries(
    expr = reticulate::import("copernicusmarine"),
    max_retries = max_retries,
    verbose = verbose,
    action = paste0(
      "import copernicusmarine from conda environment '",
      conda_env,
      "'"
    ),
    final_message = paste0(
      "Failed to import copernicusmarine. Make sure it is installed in ",
      "the conda environment: ",
      conda_env
    )
  )
}

.with_retries <- function(
  expr,
  max_retries,
  verbose,
  action,
  final_message = NULL
) {
  expr <- substitute(expr)
  env <- parent.frame()

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch(
      {
        eval(expr, envir = env)
      },
      error = function(e) {
        if (attempt == max_retries) {
          message <- if (is.null(final_message)) {
            paste0("Failed to ", action, " after ", max_retries, " attempts: ", e$message)
          } else {
            paste0(final_message, ". Last error: ", e$message)
          }
          stop(message, call. = FALSE)
        }
        if (verbose) {
          message("Attempt ", attempt, " failed while trying to ", action, ", retrying...")
        }
        Sys.sleep(2^attempt)
        NULL
      }
    )
    if (!is.null(result)) {
      return(result)
    }
  }
}

.validate_copernicus_request <- function(
  dataset_id,
  output_directory,
  region_sf,
  bbox,
  file_format,
  coordinates_selection_method,
  overwrite,
  skip_existing,
  netcdf_compression_level
) {
  if (missing(dataset_id) || !is.character(dataset_id) || length(dataset_id) != 1 || !nzchar(dataset_id)) {
    stop("dataset_id must be a non-empty character string.", call. = FALSE)
  }

  if (!is.null(region_sf) && !inherits(region_sf, "sf")) {
    stop("region_sf must be an sf object.", call. = FALSE)
  }

  if (is.null(region_sf) && !is.null(bbox)) {
    .validate_bbox(bbox)
  }

  if (!is.character(output_directory) || length(output_directory) != 1 || !nzchar(output_directory)) {
    stop("output_directory must be a non-empty character string.", call. = FALSE)
  }

  valid_formats <- c("netcdf", "zarr", "csv", "parquet")
  if (!is.null(file_format) && !file_format %in% valid_formats) {
    stop(
      "file_format must be one of: ",
      paste(valid_formats, collapse = ", "),
      call. = FALSE
    )
  }

  valid_selection_methods <- c("inside", "strict-inside", "nearest", "outside")
  if (!coordinates_selection_method %in% valid_selection_methods) {
    stop(
      "coordinates_selection_method must be one of: ",
      paste(valid_selection_methods, collapse = ", "),
      call. = FALSE
    )
  }

  if (isTRUE(overwrite) && isTRUE(skip_existing)) {
    stop("overwrite and skip_existing cannot both be TRUE.", call. = FALSE)
  }

  if (!netcdf_compression_level %in% 0:9) {
    stop("netcdf_compression_level must be an integer between 0 and 9.", call. = FALSE)
  }
}

.validate_bbox <- function(bbox) {
  if (!is.numeric(bbox) || length(bbox) != 4 || any(is.na(bbox))) {
    stop("bbox must be a numeric vector of length 4.", call. = FALSE)
  }

  if (!is.null(names(bbox)) && all(c("xmin", "ymin", "xmax", "ymax") %in% names(bbox))) {
    bbox <- bbox[c("xmin", "ymin", "xmax", "ymax")]
  }

  if (bbox[[1]] >= bbox[[3]] || bbox[[2]] >= bbox[[4]]) {
    stop("bbox must be ordered as c(xmin, ymin, xmax, ymax).", call. = FALSE)
  }

  if (bbox[[2]] < -90 || bbox[[4]] > 90) {
    stop("bbox latitude values must be between -90 and 90.", call. = FALSE)
  }

  invisible(TRUE)
}

.copernicus_extent <- function(region_sf = NULL, bbox = NULL) {
  if (!is.null(region_sf)) {
    region_sf <- sf::st_transform(region_sf, 4326)
    bbox <- sf::st_bbox(region_sf)
  }

  if (is.null(bbox)) {
    return(list(
      minimum_longitude = NULL,
      maximum_longitude = NULL,
      minimum_latitude = NULL,
      maximum_latitude = NULL
    ))
  }

  .validate_bbox(bbox)
  if (!is.null(names(bbox)) && all(c("xmin", "ymin", "xmax", "ymax") %in% names(bbox))) {
    bbox <- bbox[c("xmin", "ymin", "xmax", "ymax")]
  }

  list(
    minimum_longitude = unname(as.numeric(bbox[[1]])),
    maximum_longitude = unname(as.numeric(bbox[[3]])),
    minimum_latitude = unname(as.numeric(bbox[[2]])),
    maximum_latitude = unname(as.numeric(bbox[[4]]))
  )
}

.datetime_or_null <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  as.character(x)
}

.python_list_or_null <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  as.list(as.character(x))
}

.drop_nulls <- function(x) {
  Filter(Negate(is.null), x)
}

.copernicus_subset_response_to_tibble <- function(response) {
  tibble::tibble(
    file_path = .py_attr_character(response, "file_path"),
    output_directory = .py_attr_character(response, "output_directory"),
    filename = .py_attr_character(response, "filename"),
    file_size = .py_attr_numeric(response, "file_size"),
    data_transfer_size = .py_attr_numeric(response, "data_transfer_size"),
    variables = list(.py_attr_vector(response, "variables")),
    status = .py_attr_character(response, "status"),
    message = .py_attr_character(response, "message"),
    file_status = .py_attr_character(response, "file_status"),
    file_names = list(.py_attr_vector(response, "file_names")),
    downloaded_file = .copernicus_downloaded_file_from_fields(
      .py_attr_character(response, "output_directory"),
      .py_attr_character(response, "filename"),
      .py_attr_character(response, "file_path")
    )
  )
}

.py_attr <- function(x, attr) {
  tryCatch(reticulate::py_get_attr(x, attr), error = function(e) NULL)
}

.py_attr_character <- function(x, attr) {
  value <- .py_attr(x, attr)
  if (is.null(value)) {
    return(NA_character_)
  }

  enum_value <- tryCatch(value$value, error = function(e) NULL)
  if (!is.null(enum_value)) {
    value <- enum_value
  }

  out <- tryCatch(reticulate::py_to_r(value), error = function(e) value)
  if (length(out) == 0 || is.null(out)) {
    return(NA_character_)
  }

  out <- tryCatch(as.character(out)[[1]], error = function(e) reticulate::py_str(value))
  if (is.null(out) || !nzchar(out)) {
    return(NA_character_)
  }
  out
}

.py_attr_numeric <- function(x, attr) {
  value <- .py_attr(x, attr)
  if (is.null(value)) {
    return(NA_real_)
  }

  out <- tryCatch(reticulate::py_to_r(value), error = function(e) value)
  out <- suppressWarnings(as.numeric(out)[[1]])
  if (is.na(out)) {
    return(NA_real_)
  }
  out
}

.py_attr_vector <- function(x, attr) {
  value <- .py_attr(x, attr)
  if (is.null(value)) {
    return(character())
  }

  out <- tryCatch(reticulate::py_to_r(value), error = function(e) value)
  if (length(out) == 0 || is.null(out)) {
    return(character())
  }
  as.character(out)
}

.copernicus_downloaded_file <- function(response) {
  if (!"downloaded_file" %in% names(response) || nrow(response) == 0) {
    return(NA_character_)
  }
  response$downloaded_file[[1]]
}

.copernicus_downloaded_file_from_fields <- function(output_directory, filename, file_path) {
  if (!is.na(output_directory) && !is.na(filename)) {
    return(normalizePath(file.path(output_directory, filename), mustWork = FALSE))
  }

  if (!is.na(file_path)) {
    is_absolute_path <- grepl("^(/|[A-Za-z]:[/\\\\])", file_path)
    if (!is.na(output_directory) && !is_absolute_path) {
      file_path <- file.path(output_directory, file_path)
    }
    return(normalizePath(file_path, mustWork = FALSE))
  }

  NA_character_
}

.read_copernicus_raster <- function(file, variables = NULL) {
  is_netcdf <- grepl("\\.(nc|netcdf)$", file, ignore.case = TRUE)

  terra_object <- tryCatch(
    terra::rast(file),
    error = function(e) e
  )

  if (inherits(terra_object, "SpatRaster")) {
    return(terra_object)
  }

  if (is_netcdf && requireNamespace("stars", quietly = TRUE)) {
    raster_object <- tryCatch(
      .read_copernicus_netcdf_with_stars(file, variables),
      error = function(e) e
    )

    if (inherits(raster_object, "SpatRaster")) {
      return(raster_object)
    }
  } else {
    raster_object <- NULL
  }

  if (is_netcdf && !requireNamespace("stars", quietly = TRUE)) {
    stop(
      "Failed to read Copernicus Marine NetCDF with terra, and the stars package ",
      "is not installed. Install r-stars, r-ncdf4, and r-ncmeta, or fix the GDAL ",
      "NetCDF driver for terra. terra error: ",
      conditionMessage(terra_object),
      call. = FALSE
    )
  }

  if (is_netcdf) {
    stop(
      "Failed to read Copernicus Marine NetCDF with terra::rast() or ",
      "stars::read_ncdf(). terra error: ",
      conditionMessage(terra_object),
      " stars error: ",
      conditionMessage(raster_object),
      call. = FALSE
    )
  }

  stop(
    "Failed to read Copernicus Marine raster with terra::rast(). terra error: ",
    conditionMessage(terra_object),
    call. = FALSE
  )
}

.read_copernicus_netcdf_with_stars <- function(file, variables = NULL) {
  var_names <- if (is.null(variables)) NULL else as.character(variables)

  if (is.null(var_names) || length(var_names) == 0) {
    stars_object <- stars::read_ncdf(file)
    return(.stars_to_terra(stars_object))
  }

  rasters <- lapply(var_names, function(var_name) {
    stars_object <- stars::read_ncdf(file, var = var_name)
    .stars_to_terra(stars_object, variable_name = var_name)
  })

  do.call(c, rasters)
}

.stars_to_terra <- function(stars_object, variable_name = NULL) {
  dim_names <- names(dim(stars_object))
  lon_candidates <- intersect(c("longitude", "lon", "x"), dim_names)
  lat_candidates <- intersect(c("latitude", "lat", "y"), dim_names)
  lon_name <- if (length(lon_candidates) > 0) lon_candidates[[1]] else NULL
  lat_name <- if (length(lat_candidates) > 0) lat_candidates[[1]] else NULL

  if (is.null(lon_name) || is.null(lat_name)) {
    stop(
      "Could not identify longitude and latitude dimensions in the stars object.",
      call. = FALSE
    )
  }

  if (is.null(variable_name)) {
    variable_name <- names(stars_object)[[1]]
  }

  arr <- as.array(stars_object[[variable_name]])
  arr_dim_names <- dim_names
  other_dim_names <- setdiff(arr_dim_names, c(lon_name, lat_name))
  arr <- aperm(arr, c(match(c(lon_name, lat_name, other_dim_names), arr_dim_names)))

  lon <- as.numeric(stars::st_get_dimension_values(stars_object, lon_name))
  lat <- as.numeric(stars::st_get_dimension_values(stars_object, lat_name))
  other_dims <- lapply(other_dim_names, function(dim_name) {
    stars::st_get_dimension_values(stars_object, dim_name)
  })
  names(other_dims) <- other_dim_names

  dx <- if (length(lon) > 1) stats::median(diff(sort(lon))) else 0
  dy <- if (length(lat) > 1) stats::median(diff(sort(lat))) else 0

  n_layers <- if (length(other_dim_names) == 0) {
    1
  } else {
    prod(vapply(other_dims, length, integer(1)))
  }

  raster_object <- terra::rast(
    nrows = length(lat),
    ncols = length(lon),
    nlyrs = n_layers,
    xmin = min(lon) - dx / 2,
    xmax = max(lon) + dx / 2,
    ymin = min(lat) - dy / 2,
    ymax = max(lat) + dy / 2,
    crs = "EPSG:4326"
  )

  layer_indices <- if (length(other_dim_names) == 0) {
    matrix(integer(), nrow = 1, ncol = 0)
  } else {
    do.call(
      expand.grid,
      c(lapply(other_dims, seq_along), stringsAsFactors = FALSE)
    )
  }

  for (layer in seq_len(n_layers)) {
    slice <- if (length(other_dim_names) == 0) {
      arr
    } else {
      do.call(
        "[",
        c(
          list(arr, TRUE, TRUE),
          as.list(layer_indices[layer, , drop = TRUE]),
          list(drop = TRUE)
        )
      )
    }
    matrix_values <- t(slice)
    if (isTRUE(lat[[1]] < lat[[length(lat)]])) {
      matrix_values <- matrix_values[nrow(matrix_values):1, , drop = FALSE]
    }
    raster_object[[layer]] <- terra::setValues(
      raster_object[[layer]],
      as.vector(t(matrix_values))
    )
  }

  names(raster_object) <- .copernicus_layer_names(
    variable_name,
    other_dim_names,
    other_dims,
    layer_indices,
    n_layers
  )

  raster_object
}

.copernicus_layer_names <- function(
  variable_name,
  other_dim_names,
  other_dims,
  layer_indices,
  n_layers
) {
  if (length(other_dim_names) == 0) {
    return(variable_name)
  }

  vapply(seq_len(n_layers), function(layer) {
    suffix <- vapply(seq_along(other_dim_names), function(i) {
      value <- other_dims[[i]][[layer_indices[layer, i]]]
      if (inherits(value, "POSIXt")) {
        value <- format(value, "%Y%m%d")
      }
      paste0(other_dim_names[[i]], value)
    }, character(1))

    paste(c(variable_name, suffix), collapse = "_")
  }, character(1))
}
