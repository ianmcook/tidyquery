suppressPackageStartupMessages(library(dplyr))

library(nycflights13)

base_url <- "https://raw.githubusercontent.com/ianmcook/coursera-datasets/master/"
suppressWarnings(tryCatch({
  card_rank     <<- as_tibble(read.table(file = paste0(base_url, "card_rank.txt"),     header = TRUE, sep = "\t", quote = ""))
  card_suit     <<- as_tibble(read.table(file = paste0(base_url, "card_suit.txt"),     header = TRUE, sep = "\t", quote = ""))
  crayons       <<- as_tibble(read.table(file = paste0(base_url, "crayons.txt"),       header = TRUE, sep = "\t", quote = ""))
  customers     <<- as_tibble(read.table(file = paste0(base_url, "customers.txt"),     header = TRUE, sep = "\t", quote = ""))
  employees     <<- as_tibble(read.table(file = paste0(base_url, "employees.txt"),     header = TRUE, sep = "\t", quote = ""))
  games         <<- as_tibble(read.table(file = paste0(base_url, "games.txt"),         header = TRUE, sep = "\t", quote = ""))
  inventory     <<- as_tibble(read.table(file = paste0(base_url, "inventory.txt"),     header = TRUE, sep = "\t", quote = ""))
  makers        <<- as_tibble(read.table(file = paste0(base_url, "makers.txt"),        header = TRUE, sep = "\t", quote = ""))
  offices       <<- as_tibble(read.table(file = paste0(base_url, "offices.txt"),       header = TRUE, sep = "\t", quote = ""))
  orders        <<- as_tibble(read.table(file = paste0(base_url, "orders.txt"),        header = TRUE, sep = "\t", quote = ""))
  salary_grades <<- as_tibble(read.table(file = paste0(base_url, "salary_grades.txt"), header = TRUE, sep = "\t", quote = ""))
  toys          <<- as_tibble(read.table(file = paste0(base_url, "toys.txt"),          header = TRUE, sep = "\t", quote = ""))
  invisible(NULL)
}, error = function(e) {
  invisible(NULL)
}))
