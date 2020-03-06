# Need to build a package or connect to a built package
# Post data to the package
# Connect to or create a GitHub repository for the data package
# push updates
# Create readme.md notes
# Connect to a data description repo and provide details about the package
#

#' @title Create an R package
#' @description This function automates the process of building a Github R package with the desired data.
#' @param
#' @param
#' @param
dpr_build_data_package <- function(list_data, package_name, export_folder = "../../temp_data") {

  cwd <- getwd()

  # package path
  ppath <- fs::path(export_folder, package_name)

  usethis::create_package(ppath, open = FALSE, rstudio = FALSE)
  usethis::local_project(ppath)

  usethis::use_data_raw(package_name, open = FALSE)

  raw_data_script_path <- fs::path(ppath, "data-raw", package_name, ext = "R")
  raw_data_folder <- fs::path(ppath, "data-raw")

  temp_dir <- tempdir()
  temp_data_paths <- purrr::map(names(list_data),~(fs::path(temp_dir, .x, ext = "csv")))
  map2(names(list_data), list_data, ~readr::write_csv(.y, fs::path(temp_dir, .x, ext = "csv")))
  temp_data_paths %>% purrr::map(~dpr_export(.x,
                                       export_format = c(".rds",".xlsx",".sav",".dta",".csv", ".json", ".sas7bdat"),
                                       export_folder = raw_data_folder, details = FALSE))
  fs::dir_delete(temp_dir)
  fs::dir_info(raw_data_folder, recurse = TRUE) %>%
    mutate(size_mb = size / 1000)

  # Now push R script text into the data-raw R script
  # Now push R data objects usethis::use_data()


}


#' @title Write and document R data in the package
#' @description This function automates the process of building a Github R package with the desired data.
#' @param
#' @param
#' @param

#' @title Store new data in an R package
#' @description This function automates the process of building a Github R package with the desired data.
#' @param
#' @param
#' @param

#' @title Store new data in an R package
#' @description This function automates the process of building a Github R package with the desired data.
#' @param
#' @param
#' @param
