suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(nycflights13))
suppressPackageStartupMessages(library(dbplyr))
suppressPackageStartupMessages(library(dtplyr))
if (requireNamespace("arrow", quietly = TRUE)) {
  suppressPackageStartupMessages(library(arrow))
}

base_url <- "https://raw.githubusercontent.com/ianmcook/coursera-datasets/master/"
suppressWarnings(tryCatch({

  card_rank     <<- as_tibble(read.table(file = paste0(base_url, "card_rank.txt"),     header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  card_suit     <<- as_tibble(read.table(file = paste0(base_url, "card_suit.txt"),     header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  crayons       <<- as_tibble(read.table(file = paste0(base_url, "crayons.txt"),       header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  customers     <<- as_tibble(read.table(file = paste0(base_url, "customers.txt"),     header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  employees     <<- as_tibble(read.table(file = paste0(base_url, "employees.txt"),     header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  games         <<- as_tibble(read.table(file = paste0(base_url, "games.txt"),         header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  inventory     <<- as_tibble(read.table(file = paste0(base_url, "inventory.txt"),     header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  makers        <<- as_tibble(read.table(file = paste0(base_url, "makers.txt"),        header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  offices       <<- as_tibble(read.table(file = paste0(base_url, "offices.txt"),       header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  orders        <<- as_tibble(read.table(file = paste0(base_url, "orders.txt"),        header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  salary_grades <<- as_tibble(read.table(file = paste0(base_url, "salary_grades.txt"), header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))
  toys          <<- as_tibble(read.table(file = paste0(base_url, "toys.txt"),          header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE))

  # dbplyr
  db_con        <<- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  copy_to(db_con, iris, name = "iris_db")
  iris_db       <<- tbl(db_con, "iris_db")
  copy_to(db_con, flights, name = "flights_db")
  flights_db    <<- tbl(db_con, "flights_db")
  copy_to(db_con, planes, name = "planes_db")
  planes_db     <<- tbl(db_con, "planes_db")

  # dtplyr
  iris_dt       <<- lazy_dt(iris)
  flights_dt    <<- lazy_dt(flights)
  planes_dt    <<- lazy_dt(planes)

  # arrow
  if ("package:arrow" %in% search()) {
    iris_at       <<- arrow_table(iris)
    iris_ar       <<- record_batch(iris)
    iris_ad       <<- InMemoryDataset$create(iris)
    flights_at    <<- arrow_table(flights)
    flights_ar    <<- record_batch(flights)
    flights_ad    <<- InMemoryDataset$create(flights)
    planes_at    <<- arrow_table(planes)
    planes_ar    <<- record_batch(planes)
    planes_ad    <<- InMemoryDataset$create(planes)
  }

  invisible(NULL)
}, error = function(e) {
  invisible(NULL)
}))
