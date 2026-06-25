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

test_that("Copernicus Marine is listed in API registry", {
  result <- get_api_info("Copernicus Marine")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(result$requires_python))
  expect_true(all(result$requires_auth))
  expect_true(all(result$python_packages == "copernicusmarine"))
})
