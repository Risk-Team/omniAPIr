test_that("Copernicus Marine functions require conda_env", {
  expect_error(
    list_copernicus_marine_catalogue(),
    "conda_env parameter is required"
  )

  expect_error(
    download_copernicus_marine(
      dataset_id = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m"
    ),
    "conda_env parameter is required"
  )

  expect_error(
    download_and_process_copernicus_marine(
      dataset_id = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m",
      output_filename = "temperature.nc"
    ),
    "conda_env parameter is required"
  )
})

test_that("Copernicus Marine request validation catches invalid inputs", {
  expect_error(
    download_copernicus_marine(dataset_id = ""),
    "dataset_id must be"
  )

  expect_error(
    download_copernicus_marine(
      dataset_id = "dataset",
      bbox = c(1, 2, 0, 3)
    ),
    "bbox must be ordered"
  )

  expect_error(
    download_copernicus_marine(
      dataset_id = "dataset",
      overwrite = TRUE,
      skip_existing = TRUE
    ),
    "overwrite and skip_existing"
  )

  expect_error(
    download_copernicus_marine(
      dataset_id = "dataset",
      file_format = "tif"
    ),
    "file_format must be"
  )
})

test_that("Copernicus Marine bbox is mapped to Python subset arguments", {
  extent <- omniAPIr:::.copernicus_extent(
    bbox = c(xmin = -45, ymin = -10, xmax = -35, ymax = 5)
  )

  expect_equal(extent$minimum_longitude, -45)
  expect_equal(extent$maximum_longitude, -35)
  expect_equal(extent$minimum_latitude, -10)
  expect_equal(extent$maximum_latitude, 5)
})

test_that("Copernicus Marine response paths resolve against output directory", {
  path <- omniAPIr:::.copernicus_downloaded_file_from_fields(
    output_directory = "downloads",
    filename = NA_character_,
    file_path = "subset.nc"
  )

  expect_match(path, "downloads/subset\\.nc$")
})

test_that("Copernicus Marine list arguments remain Python-compatible lists", {
  variables <- omniAPIr:::.python_list_or_null("thetao")

  expect_type(variables, "list")
  expect_equal(variables, list("thetao"))
})

test_that("Copernicus Marine NetCDF rasters prefer terra before stars", {
  calls <- character()
  fake_raster <- terra::rast(nrows = 1, ncols = 1)

  local_mocked_bindings(
    rast = function(x, ...) {
      calls <<- c(calls, "terra")
      expect_equal(x, "subset.nc")
      fake_raster
    },
    .package = "terra"
  )
  local_mocked_bindings(
    .read_copernicus_netcdf_with_stars = function(file, variables = NULL) {
      calls <<- c(calls, "stars")
      expect_equal(file, "subset.nc")
      expect_equal(variables, "thetao")
      fake_raster
    },
    .package = "omniAPIr"
  )

  result <- omniAPIr:::.read_copernicus_raster("subset.nc", "thetao")

  expect_true(inherits(result, "SpatRaster"))
  expect_identical(calls, "terra")
})

test_that("Copernicus Marine NetCDF rasters fall back to stars when terra fails", {
  skip_if_not_installed("stars")

  calls <- character()
  fake_raster <- terra::rast(nrows = 1, ncols = 1)

  local_mocked_bindings(
    rast = function(x, ...) {
      calls <<- c(calls, "terra")
      expect_equal(x, "subset.nc")
      stop("terra failed", call. = FALSE)
    },
    .package = "terra"
  )
  local_mocked_bindings(
    .read_copernicus_netcdf_with_stars = function(file, variables = NULL) {
      calls <<- c(calls, "stars")
      expect_equal(file, "subset.nc")
      expect_equal(variables, "thetao")
      fake_raster
    },
    .package = "omniAPIr"
  )

  result <- omniAPIr:::.read_copernicus_raster("subset.nc", "thetao")

  expect_true(inherits(result, "SpatRaster"))
  expect_identical(calls, c("terra", "stars"))
})

test_that("Copernicus Marine is listed in API registry", {
  result <- get_api_info("Copernicus Marine")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(result$requires_python))
  expect_true(all(result$requires_auth))
  expect_true(all(result$python_packages == "copernicusmarine"))
})
