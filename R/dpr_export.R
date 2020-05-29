#'	@title Export csv files to other file formats as defined
#' 	@param object is a character string of the csv file or an R data.frame.
#'	@param export_format is a character vector of formats to convert.  Defaults to ".rds".  Potential file types are .xlsx, .rds, .sav, .dta, .sas7bdat, and .json
#'	@param export_folder is the folder where the exported data will be placed.  In this folder the a subfolder will be created for each csv file name and
#'  selected export_format file types will be created in the folder.
#'	@param details  If TRUE then information is printed to screen during the process.  Default is TRUE.
#'	@param readme is the data summary that describes the data.  Accepts a text string describing the provenance.
#'	@examples dd <- read_csv(system.file("extdata", "Draft_vietnam.csv", package = "DataPushR"))
#'	tempf <- fs::file_temp("Draft_vietnam_-_", ext = "csv")
#'	write_csv(dd, tempf)
#'	dpr_export(tempf, export_format = c(".rds", ".xlsx", ".sav", ".dta", ".csv", ".json", ".sas7bdat"),
#'	export_folder=getwd())
#'	dpr_export(dd, export_format=c(".rds",".xlsx",".sav",".dta",".csv", ".json", ".sas7bdat"), export_folder=getwd())
#'	@export
dpr_export  <-  function(object = NULL, export_format = NULL, export_folder, details = TRUE, readme = NULL) {


  # user needs to specify the export formats
  if (is.null(export_format))  stop("Don't you want to export something....")

  # read in the file
  if (is.character(object)) {

    file_name <- fs::path_file(object) %>% fs::path_ext_remove()
    # To remove tempfile stuff if needed. Could remove
    file_name <- gsub("_-_(\\d|\\w){1,250}.", ".", file_name)

    temp_data <- readr::read_csv(object)
    # change periods to underscores and then move to lowercase in column names
    colnames(temp_data) <- gsub("\\.", "_", tolower(colnames(temp_data)))
  } else {
    file_name <-deparse(substitute(object))
    temp_data <- object
  }

  if (details == TRUE) {
    print(head(temp_data))
  }

 # Create a folder with the same name as the file
 fs::dir_create(fs::path(export_folder,file_name))

  if (!is.null(readme)) {
    cat(readme, file = fs::path(export_folder,file_name, file_name, ext = "readme"))
  }
  if (any(export_format %in% ".rds")) {
    readr::write_rds(list(data = temp_data), path = fs::path(export_folder, file_name, file_name, ext = "rds"))
  }
  if (any(export_format %in% ".xlsx")) {
    # move to readxlsx
    writexl::write_xlsx(temp_data, path = fs::path(export_folder, file_name, file_name, ext = "xlsx"))
  }
  if (any(export_format%in%".sav")) {
    haven::write_sav(temp_data, path = fs::path(export_folder, file_name, file_name, ext = "sav"))
  }
  if (any(export_format %in% ".dta")) {
    haven::write_dta(temp_data, path = fs::path(export_folder, file_name, file_name, ext = "dta"))
  }
  if (any(export_format %in% ".csv")) {
    readr::write_csv(temp_data, path = fs::path(export_folder,file_name,file_name, ext = "csv"))
  }
  if (any(export_format %in% ".json")) {
    jsonlite::toJSON(temp_data) %>%
      readr::write_lines(path = fs::path(export_folder, file_name, file_name, ext = "json"))
  }
  if (any(export_format %in% ".sas7bdat")) {
    haven::write_sas(temp_data, fs::path(export_folder, file_name, file_name, ext = "sas7bdat"))
  }

  if (details == TRUE) return(temp_data)

}
