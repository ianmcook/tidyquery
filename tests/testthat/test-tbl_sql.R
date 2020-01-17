test_that("Simple SELECT example query #1 returns expected result on tbl_sql", {
  skip_if_not(exists("iris_db"), message = "Test data not loaded")
  expect_equal(
    query("SELECT Species, COUNT(*) AS n FROM iris_db GROUP BY Species") %>%
      collect(),
    iris_db %>%
      group_by(Species) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      collect()
  )
})

test_that("Simple SELECT statement with backticks is inchanged after show_dplyr() then dbplyr::show_query()", {
  skip_if_not(exists("iris_db"), message = "Test data not loaded")
  expect_equal(
    paste(
      capture.output(
        show_query(
          eval(
            str2lang(
              paste0(
                capture.output(
                  show_dplyr("SELECT DISTINCT `Species` FROM `iris_db`")
                ),
                collapse = " "
              )
            )
          )
        )
      )[-1],
      collapse = " "
    ),
    "SELECT DISTINCT `Species` FROM `iris_db`"
  )
})
