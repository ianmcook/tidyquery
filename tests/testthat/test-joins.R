test_that("Inner join example query #1 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys JOIN makers ON toys.maker_id = makers.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".toys", ".makers")) %>%
      rename(toys.name = "name.toys", makers.name = "name.makers")
  )
})

test_that("Inner join example query #2 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys t JOIN makers m ON toys.maker_id = makers.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".t", ".m")) %>%
      rename(t.name = "name.t", m.name = "name.m")
  )
})

test_that("Inner join example query #3 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM toys t JOIN makers m ON t.maker_id = m.id"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".t", ".m")) %>%
      rename(t.name = "name.t", m.name = "name.m")
  )
})

test_that("Inner join example query #4 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT toys.id AS id, toys.name AS toy, price, maker_id, makers.name AS maker, city
             FROM toys JOIN makers ON toys.maker_id = makers.id;"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".toys", ".makers")) %>%
      rename(toys.name = "name.toys", makers.name = "name.makers") %>%
      select(id, toy = toys.name, price, maker_id, maker = makers.name, city)
  )
})

test_that("Inner join example query #5 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT t.id AS id, t.name AS toy, price, maker_id, m.name AS maker, city
             FROM toys t JOIN makers m ON t.maker_id = m.id;"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".t", ".m")) %>%
      rename(t.name = "name.t", m.name = "name.m") %>%
      select(id, toy = t.name, price, maker_id, maker = m.name, city)
  )
})

test_that("Inner join example query #6 returns expected result", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_equal(
    query("SELECT m.name AS maker, AVG(price) AS avg_price
             FROM toys t JOIN makers m ON t.maker_id = m.id
             GROUP BY maker
             ORDER BY avg_price;"),
    toys %>%
      inner_join(makers, by = c(maker_id = "id"), suffix = c(".toys", ".makers")) %>%
      rename(toys.name = "name.toys", makers.name = "name.makers") %>%
      rename(maker = makers.name) %>%
      group_by(maker) %>%
      summarise(avg_price = mean(price, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(avg_price)
  )
})

test_that("Inner join example query #7 returns expected result", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat_ws(' ', 'Now boarding', name, 'flight', CAST(flight AS string))
             FROM flights f JOIN airlines a USING (carrier)"),
    flights %>%
      inner_join(airlines, by = "carrier") %>%
      transmute(stringr::str_c("Now boarding", name, "flight", as.character(flight), sep = " "))
  )
})

test_that("Inner join example query #8 returns expected result", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat_ws(' ', 'Now boarding', airlines.name, 'flight', CAST(flight AS string))
             FROM flights f JOIN airlines a USING (carrier)"),
    flights %>%
      inner_join(airlines, by = "carrier") %>%
      transmute(stringr::str_c("Now boarding", name, "flight", as.character(flight), sep = " "))
  )
})

test_that("Inner join example query #9 returns expected result", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat_ws(' ', 'Now boarding', a.name, 'flight', CAST(f.flight AS string))
             FROM flights f JOIN airlines a USING (carrier)"),
    flights %>%
      inner_join(airlines, by = "carrier") %>%
      transmute(stringr::str_c("Now boarding", name, "flight", as.character(flight), sep = " "))
  )
})

test_that("Inner join example query #10 returns expected result", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_equal(
    query("SELECT concat_ws(' ', 'Now boarding', airlines.name, 'flight', CAST(flights.flight AS string))
             FROM flights f JOIN airlines a USING (carrier)"),
    flights %>%
      inner_join(airlines, by = "carrier") %>%
      transmute(stringr::str_c("Now boarding", name, "flight", as.character(flight), sep = " "))
  )
})

test_that("Join fails on misqualified column reference example #1", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_error(
    query("SELECT concat_ws(' ', 'Now boarding', a.name, 'flight', CAST(a.flight AS string)) FROM flights f JOIN airlines a USING (carrier)"),
    "a.flight"
  )
})

test_that("Join fails when column names have suffixes matching table names or aliases", {
  expect_error(
    query("select * FROM iris JOIN iris AS Length ON Species = Species"),
    "Names"
  )
})

test_that("Join fails on misqualified column reference example #2", {
  skip_if_not(exists("flights") && exists("airlines"), message = "Test data not loaded")
  expect_error(
    query("SELECT concat_ws(' ', 'Now boarding', airlines.name, 'flight', CAST(airlines.flight AS string)) FROM flights f JOIN airlines a USING (carrier)"),
    "airlines.flight"
  )
})

test_that("Join fails on ambiguous column reference example #1", {
  skip_if_not(exists("toys") && exists("makers"), message = "Test data not loaded")
  expect_error(
    query("SELECT name FROM toys t JOIN makers m ON toys.maker_id = makers.id"),
    "name"
  )
})

test_that("Join alias/conditions example #1 variant A returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games ON game = name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant B returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON game = name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant C returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games ON name = game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant D returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON name = game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant E returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games ON inventory.game = games.name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})


test_that("Join alias/conditions example #1 variant F returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games ON games.name = inventory.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant G returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON i.game = g.name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant H returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON g.name = i.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant I returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON inventory.game = g.name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant J returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON g.name = inventory.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant K returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON i.game = games.name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant L returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games g ON games.name = i.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant M returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games ON games.name = i.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant N returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games ON name = i.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant O returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i JOIN games ON i.game = name"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant P returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games g ON g.name = inventory.game"),
    inventory %>% inner_join(games, by = c(game = "name"))
  )
})

test_that("Join alias/conditions example #1 variant Q returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory JOIN games_with_col_renamed USING (game)")
    },
    inventory %>% inner_join(games %>% rename(game = name), by = c("game"))
  )
})

test_that("Join alias/conditions example #1 variant R returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g USING (game)")
    },
    inventory %>% inner_join(games %>% rename(game = name), by = c("game"))
  )
})

test_that("Join alias/conditions example #1 variant S returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g ON game = game")
    },
    inventory %>% inner_join(games %>% rename(game = name), by = c("game"))
  )
})

test_that("Join alias/conditions example #1 variant T returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games g ON game = g.name"),
    inventory %>% inner_join(games %>% rename(game = name), by = c("game"))
  )
})

test_that("Join alias/conditions example #1 variant U returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g ON g.game = game")
    },
    inventory %>% inner_join(games %>% rename(game = name), by = c("game"))
  )
})

test_that("Join alias/conditions example #1 variant V returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory JOIN games g ON games.name = game"),
    inventory %>% inner_join(games %>% rename(game = name), by = c("game"))
  )
})

test_that("Bad join conditions example #1 variant A fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON i.foo = i.bar"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant B fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory JOIN games g ON i.game = i.bar"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant C fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory JOIN games g ON i.foo = i.game"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant D fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON name = name"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant E fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g ON name = name")
    },
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant F fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON g.name = name"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant G fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON g.foo = game"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant H fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g ON g.name = game")
    },
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant I fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g ON q.game = game")
    },
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant J fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON foo = bar"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant K fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON foo = g.bar"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant K fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON foo = z.bar"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant L fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON inventory.foo = bar"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant M fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON i.zzz = g.zzz"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant N fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON g.zzz = i.zzz"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant O fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON mmm.name = i.game"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant P fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g ON yyy.game = zzz.game")
    },
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant Q fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON foo = i.game"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant R fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON i.name = g.game"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant S fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON g.game = i.game"),
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant T fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    {
      games_with_col_renamed <<- games %>% rename(game = name)
      query("SELECT * FROM inventory i JOIN games_with_col_renamed g ON i.game = i.game")
    },
    "Invalid"
  )
})

test_that("Bad join conditions example #1 variant U fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM inventory i JOIN games g ON g.game = i.name"),
    "Invalid"
  )
})

test_that("Inner join with two join conditions example query #1 returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory INNER JOIN games ON inventory.game = games.name AND inventory.price = games.list_price"),
    inventory %>% inner_join(games, by = c(game = "name", price = "list_price"))
  )
})

test_that("Inner join with two join conditions example query #2 returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM inventory i INNER JOIN games g ON i.game = g.name AND i.price = g.list_price"),
    inventory %>% inner_join(games, by = c(game = "name", price = "list_price"))
  )
})

test_that("Left semi-join example query #1 returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    # names and list prices of games that are in inventory at at least one of the shops
    query("SELECT name, list_price FROM games g LEFT SEMI JOIN inventory i ON g.name = i.game"),
    games %>% semi_join(inventory, by = c(name = "game")) %>% select(name, list_price)
  )
})

test_that("Left anti-join example query #1 returns expected result", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_equal(
    # names and list prices of games that are not in inventory at any of the shops
    query("SELECT name, list_price FROM games g LEFT ANTI JOIN inventory i ON g.name = i.game"),
    games %>% anti_join(inventory, by = c(name = "game")) %>% select(name, list_price)
  )
})

test_that("Left outer join example query #1 returns expected result", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT empl_id, first_name, e.office_id AS office_id, city
             FROM employees e LEFT OUTER JOIN offices o
             ON e.office_id = o.office_id;"),
    employees %>%
      left_join(offices, by = "office_id") %>%
      select(empl_id, first_name, office_id, city)
  )
})

test_that("Left outer join fails when query includes qualified join key column from the right table", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # which city offices have no employees?
    # with a SQL engine, this query would return one row representing Singapore
    query("SELECT city FROM offices o LEFT OUTER JOIN employees e USING (office_id) WHERE e.office_id IS NULL"),
    "e.office_id"
  )
})

test_that("Full outer join fails when query includes qualified join key column from the right table", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # which city offices have no employees?
    # with a SQL engine, this query would return one row representing Singapore
    query("SELECT city FROM offices o FULL OUTER JOIN employees e USING (office_id) WHERE e.office_id IS NULL"),
    "e.office_id"
  )
})

test_that("Right outer join example query #1 returns expected result", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT empl_id, first_name, o.office_id AS office_id, city
             FROM employees e RIGHT OUTER JOIN offices o
             ON e.office_id = o.office_id;"),
    employees %>%
      right_join(offices, by = "office_id") %>%
      select(empl_id, first_name, office_id, city)
  )
})

test_that("Right outer join fails when query includes qualified join key column from the left table", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # which employees do not work in an office?
    # with a SQL engine, this query would return one row representing Val Snyder
    query("SELECT first_name, last_name FROM offices o RIGHT OUTER JOIN employees e USING (office_id) WHERE o.office_id IS NULL"),
    "o.office_id"
  )
})

test_that("Full outer join example query #1 returns expected result", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT empl_id, first_name, office_id, city
             FROM employees e FULL OUTER JOIN offices o
             ON e.office_id = o.office_id;"),
    employees %>%
      full_join(offices, by = "office_id") %>%
      select(empl_id, first_name, office_id, city)
  )
})

test_that("Full outer join fails when query includes qualified join key column from the left table", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # which employees do not work in an office?
    # with a SQL engine, this query would return one row representing Val Snyder
    query("SELECT first_name, last_name FROM offices o FULL OUTER JOIN employees e USING (office_id) WHERE o.office_id IS NULL"),
    "o.office_id"
  )
})

test_that("Full outer join with USING fails when query includes qualified join key columns from both tables", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # return a result set containing the offices that have no employees and the employees who do not work in an office
    # with a SQL engine, this query would return two rows representing Singapore and Val Snyder
    query("SELECT city, first_name, last_name FROM offices o FULL OUTER JOIN employees e USING (office_id) WHERE e.office_id IS NULL OR o.office_id IS NULL"),
    "\\Qo.office_id, e.office_id\\E|\\Qe.office_id, o.office_id\\E"
  )
})

test_that("Full outer join with ON and table names fails when query includes qualified join key column from the right table", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # which city offices have no employees?
    # with a SQL engine, this query would return one row representing Singapore
    query("SELECT city FROM offices o FULL OUTER JOIN employees e ON o.office_id = e.office_id WHERE e.office_id IS NULL"),
    "e.office_id"
  )
})

test_that("Full outer join with ON and table aliases fails when query includes qualified join key column from the right table", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # which city offices have no employees?
    # with a SQL engine, this query would return one row representing Singapore
    query("SELECT city FROM offices o FULL OUTER JOIN employees e ON offices.office_id = employees.office_id WHERE e.office_id IS NULL"),
    "e.office_id"
  )
})

test_that("Full outer join with ON fails when query includes table-name-qualified join key column from the right table", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # which city offices have no employees?
    # with a SQL engine, this query would return one row representing Singapore
    query("SELECT city FROM offices o FULL OUTER JOIN employees e ON o.office_id = e.office_id WHERE employees.office_id IS NULL"),
    "employees.office_id"
  )
})

test_that("Full outer join with ON and unqualified conditions fails when query includes qualified join key column from the right table", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_error(
    # which city offices have no employees?
    # with a SQL engine, this query would return one row representing Singapore
    query("SELECT city FROM offices o FULL OUTER JOIN employees e ON office_id = office_id WHERE e.office_id IS NULL"),
    "e.office_id"
  )
})

test_that("Left outer join example with all clauses returns expected result", {
  skip_if_not(exists("flights") && exists("planes"), message = "Test data not loaded")
  expect_equal(
    {
      my_query <- "SELECT origin, dest,
          round(AVG(distance)) AS dist,
          round(COUNT(*)/10) AS flights_per_year,
          round(SUM(seats)/10) AS seats_per_year,
          round(AVG(arr_delay)) AS avg_arr_delay
        FROM flights f LEFT JOIN planes p
          ON f.tailnum = p.tailnum
        WHERE distance BETWEEN 300 AND 400
        GROUP BY origin, dest
        HAVING flights_per_year > 100
        ORDER BY seats_per_year DESC
        LIMIT 6;"
      query(my_query)
    },
    flights %>%
      left_join(planes, by = "tailnum", suffix = c(".f", ".p"), na_matches = "never") %>%
      rename(f.year = "year.f", p.year = "year.p") %>%
      filter(between(distance, 300, 400)) %>%
      group_by(origin, dest) %>%
      filter(round(n() / 10) > 100) %>%
      summarise(
        dist = round(mean(distance, na.rm = TRUE)),
        flights_per_year = round(n() / 10),
        seats_per_year = round(sum(seats, na.rm = TRUE) / 10),
        avg_arr_delay = round(mean(arr_delay, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      arrange(desc(seats_per_year)) %>%
      head(6)
  )
})

test_that("Natural inner join example query #1 returns expected result", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM offices NATURAL JOIN employees"),
    offices %>% inner_join(employees)
  )
})

test_that("Natural left outer join example query #1 returns expected result", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM offices NATURAL LEFT OUTER JOIN employees"),
    offices %>% left_join(employees)
  )
})

test_that("Natural right outer join example query #1 returns expected result", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM offices NATURAL RIGHT OUTER JOIN employees"),
    offices %>% right_join(employees)
  )
})

test_that("Natural full outer join example query #1 returns expected result", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_equal(
    query("SELECT * FROM offices NATURAL FULL OUTER JOIN employees"),
    offices %>% full_join(employees)
  )
})

test_that("Natural join example query generates the expected message", {
  skip_if_not(exists("offices") && exists("employees"), message = "Test data not loaded")
  expect_message(
    query("SELECT * FROM offices NATURAL JOIN employees"),
    "office_id"
  )
})

test_that("Right anti-join fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT name, list_price FROM inventory i RIGHT ANTI JOIN games g ON i.game = g.name"),
    "Unsupported"
  )
})

test_that("Right semi-join fails", {
  skip_if_not(exists("inventory") && exists("games"), message = "Test data not loaded")
  expect_error(
    query("SELECT name, list_price FROM inventory i RIGHT SEMI JOIN games g ON i.game = g.name"),
    "Unsupported"
  )
})

test_that("Cross join fails", {
  skip_if_not(exists("card_rank") && exists("card_suit"), message = "Test data not loaded")
  expect_error(
    query("SELECT rank, suit FROM card_rank CROSS JOIN card_suit;"),
    "Unsupported"
  )
})

test_that("Join with three tables fails", {
  skip_if_not(exists("employees") && exists("offices") && exists("orders"), message = "Test data not loaded")
  expect_error(
    query("SELECT city, SUM(total) total FROM orders LEFT JOIN employees USING (empl_id) LEFT JOIN offices USING (office_id) GROUP BY city"),
    "unsupported"
  )
  # if it were supported, the result should be equal to:
  # orders %>%
  #   left_join(employees, by = "empl_id") %>%
  #   left_join(offices, by = "office_id") %>%
  #   group_by(city) %>%
  #   summarise(total = sum(total))
})

test_that("Join fails when data object does not exist", {
  expect_error(
    query("SELECT a FROM a435irawjesz9834are JOIN w3tzldvjsdfkgjwetro USING (b)"),
    "exist"
  )
})

test_that("Join fails when data object has unsupported type", {
  skip_if_not(exists("letters"), message = "Test data not loaded")
  expect_error(
    query("SELECT * FROM letters NATURAL JOIN state.name"),
    "supported"
  )
})

test_that("Inner join does not match NAs", {
  expect_equal(
    {
      join_test_na_match_data_x <<- data.frame(k1 = c(NA, NA, 3, 4, 5), k2 = c(1, NA, NA, 4, 5), data = 1:5)
      join_test_na_match_data_y <<- data.frame(k1 = c(NA, 2, NA, 4, 5), k2 = c(NA, NA, 3, 4, 5), data = 1:5)
      query("select COUNT(*) FROM join_test_na_match_data_x JOIN join_test_na_match_data_y USING (k1)") %>% pull(1)
    },
    2L
  )
})
