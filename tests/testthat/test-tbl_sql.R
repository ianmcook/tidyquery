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
