suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(nycflights13))
suppressPackageStartupMessages(library(dbplyr))
suppressPackageStartupMessages(library(dtplyr))

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

  iris_db       <<- tbl_memdb(iris, name = "iris_db")

  iris_dt       <<- lazy_dt(iris)
  flights_dt    <<- lazy_dt(flights)

  invisible(NULL)
}, error = function(e) {
  invisible(NULL)
}))

