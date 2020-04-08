#'
#' @title Build data documentation
#' @description  Document data file as a readme or as `data.R` for use when building to `.Rd` file for an R package.
#' @param data_object is an R data frame.
#' @param extension is a character vector of formats to build.  Defaults to "md".  Potential extensions are `md` and `R`.  If both documents are desired then `mdR` or any combination can be used to get both.
#' @param export_folder is the folder where the data document files will be placed. If the extension is `.R` then the `export_folder` will have the subdirectory `R` added.
#' @param object_name is the name of the data object for an R package data set or the name of the file for and md extension.
#' @param title is the title of the data.
#' @param description is a short description of the data. If NULL then not used.
#' @param source is a description of the source of the data.
#' @param format defaults to the text "A data frame with columns:" and is generally the best text to use.
#' @param var_details a list with the column name and the column description.
#' @param append Defaults to TRUE.  If TRUE then the new data text for `data.R` or the `readme.md` will be appended. If FALSE then the files will be overwritten.
#' @param details  If TRUE then information is printed to screen during the process.  Default is TRUE.
#' @examples dd <- read_csv(system.file("extdata", "Draft_vietnam.csv", package = "DataPushR"))
#'	dd_descriptions <- list(day_month = "Day of the month", day_year = "Day of the year (1-365)", month = "1-12 for the numeric order of months", month_name = "The english name of the month", n69 = "1969 draft order. 1-366", n70 = "1970 draft order. 1-366", n71 = "1969 draft order. 1-366", n72 = "1972 draft order. 1-366")
#'	dpr_document(dd, extension = ".R", export_folder = getwd(), object_name = "dat_draft", title = "Vietnam Draft Numbers", description = "This data can be used to teach correlation.", source = "https://www.randomservices.org/random/data/Draft.html", var_details = dd_descriptions)
#'	dpr_document(dd, extension = ".md", export_folder = getwd(), object_name = "dat_draft", title = "Vietnam Draft Numbers", description = "This data can be used to teach correlation.", source = "https://www.randomservices.org/random/data/Draft.html", var_details = dd_descriptions)
#' @format a text object is returned and a file is writtern to the folder specificed named data.
#' @export
dpr_document  <-  function(data_object, extension = "md", export_folder = "R", object_name = "pdat",
                           title = "My title of my data", description = "Short Description",
                           source = "Where the data was found", format = "A data frame with columns:",
                           var_details = list(day_month = "The numbered day of the month",
                                                   day_year = "The numbered day of the year"),
                           append = TRUE, details = TRUE) {

    names_types <- data_object %>%
        dplyr::summarise_all(class) %>%
        tidyr::pivot_longer(cols = 1:ncol(data_object), names_to = "variable", values_to = "class")

  names_description <- dplyr::bind_rows(var_details) %>%
    tidyr::pivot_longer(cols = 1:length(var_details), names_to = "variable", values_to = "description")

  names_info <- dplyr::left_join(names_types, names_description, by = "variable") %>%
    dplyr::mutate(full_description = glue::glue("The variable is {class}. {desc}",
                                         class = class, desc = ifelse(is.na(description), "", description)))

  # return message for variables that don't have a description.


  ### to build output
  if (stringr::str_detect(extension, ".R|R|.r|r")) {
  ### to build .R output

  var_glue <- purrr::map2(names_info$variable, names_info$full_description,
              ~glue::glue("#'  \\item{--var.name--}{--var.desc--}", .open = "--", .close = "--",
                          , var.name = .x, var.desc = .y)) %>%
              unlist() %>%
              stringr::str_c(collapse = "\n")


  out <- glue::glue(
"
#'
#' @title --title--
#' @description --desc--
#' @format --format--
#' \\describe{
--items--
#' }
#' @source \\url{--source--}
#' @examples
#' \\dontrun{
#' --data.name--
#'}
'--data.name--'
\n\n\n
", .open = "--", .close = "--",
             data.name = object_name, items = var_glue,
             title = title, desc = description, format = format, source = source)

  cat(out, file = fs::path(export_folder, "R", "data", ext = "R"), append = append)

  }

  if (stringr::str_detect(extension, ".md|md|.MD|MD")) {
  ### to build .md output
  ###
    var_glue <- dplyr::select(names_info, -full_description ) %>%
      knitr::kable() %>%
      stringr::str_c(collapse = "\n")


    out <- glue::glue(
"
## --title--

The data is called --data.name--.

### Description

--desc--

The source of this data is < --source-- >

### Data format

--format--

--items--
\n\n\n
    ", .open = "--", .close = "--",
                      data.name = object_name, items = var_glue,
                      title = title, desc = description, format = format, source = source)

    cat(out, file = fs::path(export_folder, "data", ext = "md"), append = append)


  }

  if (!stringr::str_detect(extension, ".md|md|.MD|MD|.R|R|.r|r")) {
  ### error

    stop("Only extensions are markdown and r scripts.  Please use one of them.")

  }

  if (details) {
    print(out)
  }
  out
}





