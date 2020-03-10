# Example of use

library(tidyverse)
library(usethis)
devtools::load_all()

base_folder <- "../../temp_data"

dat_draft <- read_csv(system.file("extdata", "Draft_vietnam.csv", package = "DataPushR"))

dd_descriptions <- list(day_month = "Day of the month", day_year = "Day of the year (1-365)", month = "1-12 for the numeric order of months", month_name = "The english name of the month", n69 = "1969 draft order. 1-366", n70 = "1970 draft order. 1-366", n71 = "1969 draft order. 1-366", n72 = "1972 draft order. 1-366")

iris_description <- list(Sepal.Length = "The Length of the Sepal", Sepal.Width = "The Width of the Sepal", Petal.Length = "The Length of the Petal", Petal.Width = "The Width of the Petal", Species = "The Iris Species")

package_path <- dpr_create_package(list_data = list(dat_draft = dat_draft, iris = iris), package_name = "Test2", export_folder = base_folder,
                   git_remote = "https://github.com/hathawayj/Test2.git")

usethis::proj_set(package_path)

usethis::use_data(dat_draft, iris)

dpr_document(dat_draft, extension = ".R", export_folder = fs::path(usethis::proj_get(), "R"),
             object_name = "dat_draft", title = "Vietnam Draft Numbers",
             description = "This data can be used to teach correlation.",
             source = "https://www.randomservices.org/random/data/Draft.html",
             var_details = dd_descriptions)

dpr_document(iris, extension = ".R", export_folder = fs::path(usethis::proj_get(), "R"),
             object_name = "iris", title = "Iris Data",
             description = "This data can be used to teach statistics.",
             source = "Fisher",
             var_details = iris_description)


dpr_document(dat_draft, extension = ".md", export_folder = fs::path(usethis::proj_get(), ""),
             object_name = "dat_draft", title = "Vietnam Draft Numbers",
             description = "This data can be used to teach correlation.",
             source = "https://www.randomservices.org/random/data/Draft.html",
             var_details = dd_descriptions)

dpr_document(iris, extension = ".md", export_folder = fs::path(usethis::proj_get(), ""),
             object_name = "iris", title = "Iris Data",
             description = "This data can be used to teach statistics.",
             source = "Fisher",
             var_details = iris_description)

dpr_readme(usethis::proj_get(), "Test2", "hathawayj")


dpr_push(folder_dir = usethis::proj_get(), message = "'Second Push from Hathaway'", repo_url = NULL)
