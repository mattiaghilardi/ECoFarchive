library(cli)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(rvest)


#' Retrieve the current online version of the Eschmeyer's Catalog of Fishes
#'
#' @return A string (year.month format)
#'
#' @noRd
get_current_ECoF_version <- function() {
  url <- "https://researcharchive.calacademy.org/research/ichthyology/catalog/SpeciesByFamily.asp"
  text <- url |>
    rvest::read_html() |>
    rvest::html_elements("table") |>
    rvest::html_text()
  date <- text[2] |>
    stringr::str_extract("[0-90-9]+ [A-Za-z]+ [0-90-9]+") |>
    strsplit(" ")
  #day <- date[[1]][1]
  month <- match(date[[1]][2], month.name)
  month <- ifelse(month < 10, paste0(0, month), as.character(month))
  year <- date[[1]][3]
  paste(year, month, sep = ".")
}

#' Download ECoF database
#'
#' @param seed A seed (only for testing)
#' @param n Number of fish families (only for testing)
#'
#' @return A list of two elements:
#' species_family output and search_cas output for all species
#'
#' @noRd
download_ECoF <- function(seed = NULL, n = NULL) {

  # ECoF higher classification
  # Currently based on a personal development version of rFishTaxa
  source("https://raw.githubusercontent.com/mattiaghilardi/rFishTaxa/fix-species_family/R/species_family.R")
  welcome <- capture.output(high_ranks <- species_family())

  # Get all families
  all_fam <- unique(high_ranks[, "family", drop = TRUE])

  if (!is.null(seed) & !is.null(n)) {
    # Check that seed and n are integers
    (!is.integer(seed) | !is.integer(n)) ||
      cli::cli_abort("{.var seed} and {.var n} must be integers")

    # Keep only n families
    cli::cli_inform("Downloading species for {n} families")
    set.seed(seed)
    all_fam <- sample(all_fam, n)

  }

  # Get all valid species
  all_sp <- lapply(cli::cli_progress_along(1:length(all_fam),
                                           "Extracting species"),
                   function(x) {
                     # Currently based on a personal development version of rFishTaxa
                     source("https://raw.githubusercontent.com/mattiaghilardi/rFishTaxa/fix-search_catalog/R/search_catalog.R")
                     search_cas(all_fam[x], type = "species_family")
                   }) |>
    dplyr::bind_rows()

  rm("get_cas", "search_cas", "species_family", envir = sys.frame()) # remove sourced functions

  # Build taxonomy table
  taxonomy <- all_sp |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(species)) |>
    dplyr::mutate(genus = gsub(" .*", "", species)) |>
    tidyr::separate(family,
                    into = c("family", "subfamily"),
                    sep = "_",
                    remove = FALSE,
                    fill = "right") |>
    dplyr::select("species", "genus", "subfamily", "family") |>
    dplyr::distinct() |>
    dplyr::arrange(species) |>
    dplyr::left_join(high_ranks |>
                       dplyr::select("subfamily", "family", "order", "class"))

  # return a list
  list(all_species = all_sp,
       taxonomy = taxonomy)
}

# current version
version <- get_current_ECoF_version()

# existing versions
available_releases <- dir("archive") |>
  stringr::str_extract_all('(?<=ECoF_).+?(?=.rds)') |>
  unlist()

if (! version %in% available_releases) {
  # download latest taxonomy
  cli::cli_inform("Downloading latest version: { version }")
  db <- download_ECoF()
  # save rds in /archve
  readr::write_rds(db,
                   here::here("archive", paste0("ECoF_", version, ".rds")),
                   compress = "gz")
  # # save internal
  # all_db <- dir("archive")
  # all_db <- sort(all_db, decreasing = TRUE) # latest first
  # versions <- gsub("ECoF_", "", gsub(".rds", "", all_db))
  # ECoF_db <- lapply(paste(here::here("archive"), all_db, sep = "/"),
  #                   readr::read_rds)
  # names(ECoF_db) <- versions
  # cli::cli_inform("Saving internal data: ECoF_db")
  # usethis::use_data(ECoF_db, internal = TRUE, overwrite = TRUE)
} else {
  cli::cli_inform("Latest version already available")
}
