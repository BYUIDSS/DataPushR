# Need to build a package or connect to a built package
# Post data to the package
# Connect to or create a GitHub repository for the data package
# push updates
# Create readme.md notes
# Connect to a data description repo and provide details about the package
#

#' @title Create an R package for data
#' @description This function automates the process of building a Github R package with the desired data stored in the `raw-data` folder.
#' @param list_data is a list object of named ojbects that can be written to a csv.
#' @param package_name is the name of the created data R package.
#' @param export_folder is the base folder where the package folder will be created.
#' @param git_remote is the `HTTPS` url of the GitHub remote.
#' @examples dd <- read_csv(system.file("extdata", "Draft_vietnam.csv", package = "DataPushR"))
#' dpr_create_package(list_data = list(dat_draft = dd), package_name = "Test3", export_folder = getwd())
#' @export

dpr_create_package <- function(list_data, package_name, export_folder = getwd(), git_remote) {

  # https://www.tidyverse.org/blog/2019/05/itdepends/

  # package path
  ppath <- fs::path(export_folder, package_name)

  # create package details
  usethis::create_package(ppath, open = FALSE, rstudio = FALSE)
  #usethis::local_project(ppath)
  usethis::proj_set(ppath)

  usethis::use_data_raw(package_name, open = FALSE)

  # Now push data into the R project
  raw_data_script_path <- fs::path(ppath, "data-raw", package_name, ext = "R")
  raw_data_folder <- fs::path(ppath, "data-raw")

  temp_dir <- tempdir(check = TRUE)
  temp_data_paths <- purrr::map(names(list_data),~(fs::path(temp_dir, .x, ext = "csv")))
  map2(names(list_data), list_data, ~readr::write_csv(.y, fs::path(temp_dir, .x, ext = "csv")))
  temp_data_paths %>% purrr::map(~dpr_export(.x,
                                       export_format = c(".rds",".xlsx",".sav",".dta",".csv", ".json", ".sas7bdat"),
                                       export_folder = raw_data_folder, details = FALSE))

  # Remove temp files created
  temp_data_paths %>% purrr::map(~fs::file_delete(.x))

  # check for sizes that are too big
  sizes <- fs::dir_info(raw_data_folder, recurse = TRUE) %>%
    mutate(size_big = size > fs::fs_bytes("100M")) %>%
    select(path, size, size_big, birth_time)

  repo <- git2r::init(usethis::proj_get())

  if(any(sizes$size_big)) stop("Some data files are larger than the 100M file limit for GitHub. No connection made.")



  dpr_push(usethis::proj_get(), message = "'first push'", repo_url = git_remote)

  return(usethis::proj_get())
  # use_git_remote(name = "origin", url = git_remote, overwrite = FALSE)
  # git2r::add(repo, "*")
  # git2r::commit(repo, message = "Initial commit")
  # git2r::branch_set_upstream(git2r::repository_head(repo), "origin/master")
  # git2r::push(repo, credentials = git2r::cred_token())

  # Could push R script text into the data-raw R script
  # Could push R data objects usethis::use_data()


}

#' @title Connect to remote, Commit and Push Latest Updates to Repo
#' @param folder_dir is the folder on your local computer where you store your git repository
#' @param message is the commit message to use
#' @param repo_url is the https url from GitHub. If NULL remote connect call not run
#' @export
dpr_push <- function(folder_dir = "/Users/hathawayj/git/temp_data/Test3", message = "'First Push from Hathaway'", repo_url = NULL){

  # connect git local to GitHub
  git_text_remote <- glue::glue("git -C {folder} remote add origin {url}", folder = folder_dir, url = repo_url)


  # The three commands to move files to github
  git_add <- glue::glue("git -C {folder}  add .", folder = folder_dir)
  git_commit <- glue::glue("git -C {folder} commit -m {message}", folder = folder_dir, message = message)
  git_push <- glue::glue("git -C {folder} push -u origin master", folder = folder_dir)

  # Execute commands on system
  if (!is.null(repo_url)) system(git_text_remote)
  system(git_add)
  system(git_commit)
  system(git_push)

}


#' @title Create readme.md in repo
#' @param folder_dir is the folder on your local computer where you store your git repository
#' @param package_name is the name of the created data R package.
#' @param github_user is the Github group or user where the package is stored.
#' @export
dpr_readme <- function(folder_dir, package_name, github_user) {
  install_path <- fs::path(github_user, package_name)

  out <- glue::glue(
    "
## --title--

The data descriptions can be found at [data.md](data.md). The [data-raw](data-raw) folder has varied data formats of the data objects that are loaded when the package is installed.

## Installation

```r
install.packages(pacman)
pacman::p_load_gh('--github--')
```
\n\n\n
    ", .open = "--", .close = "--",
    title = package_name, github = install_path)

  cat(out, file = fs::path(folder_dir, "readme.md"), append = FALSE)



}


#' @title Github Repo Create
#' @param folder_dir is the folder on your local computer where you store your git repository
#' @param package_name is the name of the created data R package.
#' @param github_user is the Github group or user where the package is stored.
#' @export
dpr_create_github <- function(package_name, post_text = c("/orgs/ORGNAME/repos", "/user/repos")[2], public = TRUE) {
  create_gh <- gh::gh(glue::glue("POST {post}",post = post_text), name = package_name,
                      private = !public, has_wiki = FALSE, auto_init = FALSE)
  create_gh
}


#' @title Github Repo Delete
#' @param folder_dir is the folder on your local computer where you store your git repository
#' @param package_name is the name of the created data R package.
#' @param github_user is the Github group or user where the package is stored.
#' @export
dpr_delete_github <- function(owner_name, repo_name) {
  delete_gh <- gh::gh("DELETE /repos/:owner/:repo", owner = owner_name, repo = repo_name)
  delete_gh
}

#' @title Write data R script
#' @param folder_dir is the folder on your local computer where you store your git repository
#' @param r_read either a path to an R script, the clipboard if `NULL`, or an `rlang::expr` object.
#' @param r_folder_write a path to a folder where the R script will be written
#' @param r_write the name of the file to write contents of `r_read`
#' @param append_file Whether to append `TRUE` or overwrite `FALSE` (default) the file `r_write`.
#' @export
dpr_write_script <- function(folder_dir = "../../temp_data/Test2", r_read = "",
                             r_folder_write = "data-raw", r_write = "", append_file = FALSE){

  # build the path for the R script in the package where code will be written.
  if (r_write == "") {
    path_r_write <-  fs::dir_ls(fs::path(folder_dir, r_folder_write), regexp = ".R")

    # stop if there is more than one R file.
    if (length(path_r_write) > 1) {
      stop("specify your R script as there is more than one shown in the folder")
    }
  } else  {
    path_r_write <- fs::path(folder_dir, r_folder_write, r_write)
  }

  # get R code to write into the path_r_write file.
 if (is.null(r_read) ) {
   print("writing clipboard contents")
   write_text <- readr::clipboard()

  } else if (is.character(r_read)) {

    # Writing the saved R script
    write_text <- readr::read_lines(file = r_read)


  } else if (is.expression(r_script)) {
    # writing an expression
    write_text <- rlang::expr_text(r_read)

  } else {
    stop("Expecting a NULL, file path as a character string, or an rlang expression object")
  }
  cat(write_text, file = path_r_write, append = append_file, sep = "\n")

}
