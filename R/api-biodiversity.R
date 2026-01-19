#' Get and Process IBAT Biodiversity Data
#'
#' @description
#' Downloads and processes biodiversity data from the Integrated Biodiversity
#' Assessment Tool (IBAT) API. Requires IBAT authentication credentials.
#'
#' @importFrom magrittr %>%
#' @param region_sf An sf object defining the region of interest.
#' @param datasets Character vector. Dataset name(s) to download (e.g., "kba", "redlist").
#' @param ibat_api_key Character. IBAT API key (required).
#' @param ibat_token Character. IBAT authentication token (required).
#' @param path Character. Base path for saving downloaded data.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#' @param remove_archive Logical. If TRUE, remove the downloaded archive after
#'   extracting its contents. Default is TRUE.
#'
#' @return Character vector of paths to processed data files, or a data.frame
#'   for redlist data.
#'
#' @details
#' API Documentation: \url{https://www.ibat-alliance.org/ibat-conservation/login}
#'
#' **Authentication Required:** Obtain API key and token from IBAT Alliance.
#'
#' The function downloads geodatabase archives, extracts them, and for redlist
#' data, filters by the provided region and returns species information.
#' When verbose = TRUE, the presigned download URL is printed before download to
#' facilitate debugging.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' region <- st_read("region.shp")
#' ibat_data <- get_and_process_ibat_data(
#'   region_sf = region,
#'   datasets = c("redlist", "kba"),
#'   ibat_api_key = "your_key",
#'   ibat_token = "your_token",
#'   path = ".",
#'   remove_archive = TRUE
#' )
#' }
get_ibat_data <- function(
  region_sf,
  datasets,
  ibat_api_key,
  ibat_token,
  path,
  verbose = FALSE,
  max_retries = 3,
  remove_archive = TRUE
) {
  if (!length(datasets)) {
    stop(
      "'datasets' must contain at least one name (e.g., 'kba' or 'redlist')."
    )
  }

  .download_one_ibat <- function(
    dataset,
    api_key,
    token,
    dest,
    overwrite,
    retries,
    verbose
  ) {
    stopifnot(nzchar(dataset), nzchar(api_key), nzchar(token))

    os <- tryCatch(Sys.info()[["sysname"]], error = function(...) {
      .Platform$OS.type
    })

    # Get presigned URL with retry logic
    retry_attempt <- 1
    success <- FALSE
    presigned <- NULL

    while (retry_attempt <= retries && !success) {
      tryCatch(
        {
          response <- httr2::request(
            "https://app.ibat-alliance.org/api/v2/data-downloads"
          ) %>%
            httr2::req_url_query(
              dataset_name = dataset,
              auth_key = api_key,
              auth_token = token
            ) %>%
            httr2::req_perform() %>%
            httr2::resp_body_json()
          presigned <- purrr::pluck(response, "download_url")
          success <- TRUE
        },
        error = function(e) {
          if (retry_attempt < retries) {
            wait_time <- 2^retry_attempt
            if (verbose) {
              message(sprintf(
                "Request failed for '%s' (attempt %d/%d): %s. Retrying in %d seconds...",
                dataset,
                retry_attempt,
                retries,
                conditionMessage(e),
                wait_time
              ))
            }
            Sys.sleep(wait_time)
            retry_attempt <<- retry_attempt + 1
          } else {
            stop(
              sprintf(
                "Failed to fetch IBAT data for '%s' after %d attempts. Last error: %s",
                dataset,
                retries,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        }
      )
    }

    if (is.null(presigned) || !nzchar(presigned)) {
      stop(
        "IBAT did not return a download_url for '",
        dataset,
        "'.",
        call. = FALSE
      )
    }

    if (verbose) {
      message(sprintf("Presigned URL for '%s': %s", dataset, presigned))
      message(sprintf("Downloading dataset '%s' archive...", dataset))
    }

    # Output path (name as .tar.gz since the object is compressed)
    if (!dir.exists(dest)) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    }
    out_name <- paste0(dataset, "_ibat.tar.gz")
    final_path <- file.path(dest, out_name)

    if (file.exists(final_path)) {
      if (!overwrite) {
        stop(
          "File exists: ",
          final_path,
          " (set overwrite=TRUE).",
          call. = FALSE
        )
      }
      unlink(final_path)
    }

    # Tuned curl handle; macOS prefers IPv4 + HTTP/1.1
    h <- curl::new_handle()
    if (identical(os, "Darwin")) {
      curl::handle_setopt(h, ipresolve = 1, http_version = 1)
    } else {
      curl::handle_setopt(h, ipresolve = 0, http_version = 0)
    }
    curl::handle_setopt(
      h,
      tcp_nodelay = TRUE,
      tcp_keepalive = TRUE,
      connecttimeout = 30L,
      low_speed_time = 30L,
      low_speed_limit = 1024L,
      buffersize = 256L * 1024L,
      nosignal = TRUE
    )

    # Try libcurl to disk, with optional retry
    ok <- FALSE
    for (i in 0:retries) {
      ok <- try(
        {
          curl::curl_fetch_disk(presigned, final_path, handle = h)
          TRUE
        },
        silent = TRUE
      )
      if (isTRUE(ok)) break
    }

    # Fallback to system transfer tool if needed
    if (!isTRUE(ok)) {
      if (identical(os, "Darwin") || identical(os, "Linux")) {
        extra <- "-L --fail --retry 2 --compressed"
        if (identical(os, "Darwin")) {
          extra <- paste(extra, "--http1.1 -4")
        }
        utils::download.file(
          presigned,
          final_path,
          method = "curl",
          extra = extra,
          mode = "wb",
          quiet = TRUE
        )
      } else {
        utils::download.file(
          presigned,
          final_path,
          method = "libcurl",
          mode = "wb",
          quiet = TRUE
        )
      }
    }

    if (!file.exists(final_path) || isTRUE(file.info(final_path)$size == 0)) {
      stop("Download failed for '", dataset, "'.", call. = FALSE)
    }

    # Extract the tar.gz file - archive contains dataset.gdb folder
    if (verbose) {
      message(sprintf("Extracting dataset '%s'...", dataset))
    }
    # Extract to dest directly - the archive already contains the .gdb folder
    utils::untar(final_path, exdir = dest)
    gdb_path <- file.path(dest, paste0(dataset, ".gdb"))

    # Remove the archive if requested
    if (remove_archive) {
      unlink(final_path, force = TRUE)
      if (verbose) {
        message(sprintf("Archive removed: %s", final_path))
      }
    }

    # Return the .gdb path directly
    if (!dir.exists(gdb_path)) {
      stop(
        "Extraction failed for '",
        dataset,
        "': .gdb folder not found.",
        call. = FALSE
      )
    }
    return(gdb_path)
  }

  # Download all datasets using purrr
  if (verbose) {
    message(sprintf(
      "Starting download of %d IBAT dataset(s)...",
      length(datasets)
    ))
  }

  dataset_paths <- purrr::map_chr(
    datasets,
    function(d) {
      .download_one_ibat(
        dataset = d,
        api_key = ibat_api_key,
        token = ibat_token,
        dest = path,
        overwrite = TRUE,
        retries = max_retries,
        verbose = verbose
      )
    }
  )
  names(dataset_paths) <- datasets
  result <- dataset_paths

  if (verbose) {
    message("IBAT datasets downloaded successfully")
  }
  # Process redlist data if it was downloaded
  if ("redlist" %in% datasets) {
    redlist_gdb_path <- dataset_paths[["redlist"]]
    # The .gdb path is returned directly from .download_one_ibat
    if (dir.exists(redlist_gdb_path)) {
      if (verbose) {
        message("Processing redlist data for the specified region...")
      }

      # Process redlist data directly in this function
      if (verbose) {
        message("Reading species list from geodatabase...")
      }

      species_list <- sf::st_read(
        redlist_gdb_path,
        layer = "IUCN_RL_2025_1_Species_List",
        quiet = TRUE
      ) %>%
        dplyr::select(1:9)

      # Get bounding box of region
      bbox <- sf::st_bbox(region_sf)

      if (verbose) {
        message(sprintf(
          "Filtering species points within bounding box (lat: %.4f to %.4f, lon: %.4f to %.4f)...",
          bbox["ymin"],
          bbox["ymax"],
          bbox["xmin"],
          bbox["xmax"]
        ))
      }

      # Build SQL query to filter by bounding box
      sql <- sprintf(
        "SELECT * FROM IUCN_RL_2025_1_Species_Points
         WHERE dec_lat BETWEEN %f AND %f
           AND dec_long BETWEEN %f AND %f",
        bbox["ymin"],
        bbox["ymax"],
        bbox["xmin"],
        bbox["xmax"]
      )

      # Read and filter redlist data
      redlist_filtered <- sf::st_read(
        redlist_gdb_path,
        query = sql,
        quiet = TRUE
      ) %>%
        sf::st_as_sf(
          coords = c("dec_long", "dec_lat"),
          crs = 4326,
          remove = FALSE
        ) %>%
        sf::st_filter(region_sf, .predicate = sf::st_within) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(id_no) %>%
        dplyr::distinct()

      # Join with species information
      result_df <- dplyr::left_join(
        redlist_filtered,
        species_list,
        by = "id_no"
      )

      if (verbose) {
        message(sprintf("Found %d species in the region", nrow(result_df)))
      }

      # Save processed data
      output_path <- file.path(path, "redlist.rds")
      saveRDS(result_df, output_path)
      result <- output_path

      message(sprintf(
        "✓ IBAT redlist data processed successfully: %d species found",
        nrow(result_df)
      ))
    }
  } else {
    message(sprintf(
      "✓ IBAT data retrieved successfully: %d dataset(s)",
      length(datasets)
    ))
  }

  return(result)
}

#' Get Invasive Alien Species Data
#'
#' @description
#' Retrieves invasive and alien species data from the Global Register of
#' Introduced and Invasive Species (GRIIS) via GBIF API. Optionally fetches
#' species occurrence data with geolocation.
#'
#' @importFrom magrittr %>%
#'
#' @param iso3 Character. ISO3 country code (required).
#' @param max_species Integer. Maximum number of species to retrieve. Default is 200.
#' @param limit_per_species Integer. Maximum occurrences per species when
#'   geolocation is TRUE. Default is 1000.
#' @param output_filename Character. Path for saving the downloaded DwC-A archive.
#' @param geolocation Logical. If TRUE, fetches occurrence data with coordinates.
#'   Default is FALSE.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A data.frame or sf object (if geolocation=TRUE) containing invasive
#'   species data with taxonomy and occurrence information.
#'
#' @details
#' API Documentation: \url{https://www.gbif.org/developer/summary}
#'
#' The function downloads the GRIIS dataset for the specified country, identifies
#' alien and invasive species, and optionally retrieves occurrence data with
#' coordinates from GBIF.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get invasive species list only
#' invasive_spp <- get_invasive_alien_species(
#'   iso3 = "KEN",
#'   output_filename = "griis_kenya.zip"
#' )
#'
#' # Get invasive species with locations
#' invasive_spp_geo <- get_invasive_alien_species(
#'   iso3 = "KEN",
#'   output_filename = "griis_kenya.zip",
#'   geolocation = TRUE
#' )
#' }
get_invasive_alien_species <- function(
  iso3,
  max_species = 200,
  limit_per_species = 1000,
  output_filename,
  geolocation = FALSE,
  verbose = FALSE,
  max_retries = 3
) {
  stopifnot(is.character(iso3), length(iso3) == 1)

  # --- Resolve country name and ISO2 ---
  country_name <- countrycode::countrycode(iso3, "iso3c", "country.name")
  iso2 <- countrycode::countrycode(iso3, "iso3c", "iso2c")
  if (is.na(country_name) || is.na(iso2)) {
    stop("Invalid iso3 code: ", iso3, call. = FALSE)
  }

  if (verbose) {
    message(sprintf(
      "Fetching invasive species for: %s (%s)",
      country_name,
      iso3
    ))
  }

  # --- 1) Find GRIIS dataset on GBIF (country checklist) with retry logic ---
  if (verbose) {
    message("Searching for GRIIS dataset on GBIF...")
  }

  retry_attempt <- 1
  success <- FALSE
  ds <- NULL

  while (retry_attempt <= max_retries && !success) {
    tryCatch(
      {
        ds <- rgbif::dataset_search(
          q = paste(
            "Global Register of Introduced and Invasive Species",
            country_name
          ),
          type = "CHECKLIST",
          limit = 100
        )$data
        success <- TRUE
      },
      error = function(e) {
        if (retry_attempt < max_retries) {
          wait_time <- 2^retry_attempt
          if (verbose) {
            message(sprintf(
              "Dataset search failed (attempt %d/%d): %s. Retrying in %d seconds...",
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
              "Failed to search GBIF datasets after %d attempts. Last error: %s",
              max_retries,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      }
    )
  }

  if (is.null(ds) || !nrow(ds)) {
    stop("No GRIIS dataset found for: ", country_name, call. = FALSE)
  }
  griis_key <- ds$datasetKey[
    stringr::str_detect(
      ds$title,
      stringr::regex(
        "^Global Register of Introduced and Invasive Species",
        ignore_case = TRUE
      )
    ) &
      stringr::str_detect(
        ds$title,
        stringr::regex(country_name, ignore_case = TRUE)
      )
  ][1]

  if (is.na(griis_key)) {
    stop("No GRIIS dataset key found for: ", country_name, call. = FALSE)
  }

  if (verbose) {
    message(sprintf("Found GRIIS dataset key: %s", griis_key))
  }

  # --- 2) Get DwC-A endpoint(s) and pick one (simple & robust) ---
  if (verbose) {
    message("Fetching dataset endpoints...")
  }

  eps <- rgbif::dataset_endpoint(griis_key)
  if (is.null(eps) || !nrow(eps)) {
    stop("No endpoints for dataset: ", griis_key, call. = FALSE)
  }

  dwc_urls <- eps %>%
    dplyr::filter(stringr::str_detect(
      type,
      stringr::regex("^DWC_ARCHIVE$", ignore_case = TRUE)
    )) %>%
    dplyr::pull(url)

  if (!length(dwc_urls)) {
    stop("No DWC_ARCHIVE endpoint for dataset: ", griis_key, call. = FALSE)
  }

  if (verbose) {
    message(sprintf("Downloading GRIIS archive to: %s", output_filename))
  }

  utils::download.file(
    dwc_urls[1],
    output_filename,
    mode = "wb",
    quiet = !verbose
  )

  if (verbose) {
    message("Extracting archive...")
  }

  files <- utils::unzip(
    output_filename,
    exdir = dirname(output_filename),
    overwrite = TRUE
  )
  # Process files
  if (verbose) {
    message("Processing species data...")
  }

  # Alien species
  alien <- readr::read_table(
    files[stringr::str_detect(files, "distribution")],
    show_col_types = FALSE
  ) %>%
    dplyr::filter(countryCode == iso2)

  # Invasive species
  invasive <- readr::read_table(
    files[stringr::str_detect(files, "speciesprofile")],
    show_col_types = FALSE
  ) %>%
    dplyr::filter(isInvasive == "Invasive")

  taxonomy <- readr::read_tsv(
    files[stringr::str_detect(files, "taxon")],
    show_col_types = FALSE
  )

  # Alien and Invasive species
  alien_invasive <- dplyr::left_join(invasive, alien, by = "id") %>%
    dplyr::left_join(., taxonomy, by = "id") %>%
    dplyr::slice_head(n = max_species)

  if (verbose) {
    message(sprintf("Found %d invasive alien species", nrow(alien_invasive)))
  }

  # --- 5) Fetch GBIF occurrences (coords only) for those taxa in that country ---
  if (geolocation) {
    if (verbose) {
      message(sprintf(
        "Fetching occurrence data from GBIF for %d species...",
        nrow(alien_invasive)
      ))
    }

    occ_list <- purrr::map(alien_invasive$scientificName, function(k) {
      if (verbose) {
        message(sprintf("  - Fetching occurrences for: %s", k))
      }

      retry_attempt <- 1
      success <- FALSE
      result <- NULL

      while (retry_attempt <= max_retries && !success) {
        tryCatch(
          {
            result <- rgbif::occ_search(
              scientificName = k,
              country = iso2,
              hasCoordinate = TRUE,
              limit = limit_per_species
            )
            success <- TRUE
          },
          error = function(e) {
            if (retry_attempt < max_retries) {
              wait_time <- 2^retry_attempt
              if (verbose) {
                message(sprintf(
                  "    Occurrence search failed (attempt %d/%d): %s. Retrying in %d seconds...",
                  retry_attempt,
                  max_retries,
                  conditionMessage(e),
                  wait_time
                ))
              }
              Sys.sleep(wait_time)
              retry_attempt <<- retry_attempt + 1
            } else {
              if (verbose) {
                message(sprintf(
                  "    Failed to fetch occurrences for '%s' after %d attempts. Skipping.",
                  k,
                  max_retries
                ))
              }
              retry_attempt <<- retry_attempt + 1 # Exit loop
            }
          }
        )
      }

      # Extract the data component if it exists
      if (!is.null(result) && !is.null(result$data)) {
        return(result$data)
      } else {
        return(NULL)
      }
    })

    # Combine all results and remove NULLs
    occ <- dplyr::bind_rows(occ_list)

    if (nrow(occ) == 0) {
      message("No occurrence data found. Returning species list only.")
      return(alien_invasive)
    }

    if (verbose) {
      message(sprintf("Retrieved %d occurrence records", nrow(occ)))
    }

    # --- 6) Convert to sf layer (WGS84) ---
    occ_sf <- occ %>%
      dplyr::mutate(
        decimalLongitude = as.numeric(decimalLongitude),
        decimalLatitude = as.numeric(decimalLatitude)
      ) %>%
      dplyr::filter(is.finite(decimalLongitude), is.finite(decimalLatitude)) %>%
      sf::st_as_sf(
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = 4326,
        remove = FALSE # Keep the coordinate columns too
      )

    message(sprintf(
      "✓ Invasive species data retrieved successfully: %d species, %d occurrences",
      nrow(alien_invasive),
      nrow(occ_sf)
    ))

    return(occ_sf)
  } else {
    message(sprintf(
      "✓ Invasive species data retrieved successfully: %d species",
      nrow(alien_invasive)
    ))
    return(alien_invasive)
  }
}
