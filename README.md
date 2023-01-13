
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyquery <img src="man/figures/logo.png" align="right" width="120" />

**tidyquery** runs SQL queries on R data frames.

It uses [queryparser](https://github.com/ianmcook/queryparser) to
translate SQL queries into R expressions, then it uses
[dplyr](https://dplyr.tidyverse.org) to evaluate these expressions and
return results. **tidyquery** does not load data frames into a database;
it queries them in place.

For an introduction to **tidyquery** and **queryparser**, watch the
recording of the talk [“Bridging the Gap between SQL and
R”](https://www.youtube.com/watch?v=JwP5KdWSgqE) from
rstudio::conf(2020).

## Installation

Install the released version of **tidyquery** from
[CRAN](https://CRAN.R-project.org/package=tidyquery) with:

``` r
install.packages("tidyquery")
```

Or install the development version from
[GitHub](https://github.com/ianmcook/tidyquery) with:

``` r
# install.packages("remotes")
remotes::install_github("ianmcook/tidyquery")
```

## Usage

**tidyquery** exports two functions: `query()` and `show_dplyr()`.

### Using `query()`

To run a SQL query on an R data frame, call the function `query()`,
passing a `SELECT` statement enclosed in quotes as the first argument.
The table names in the `FROM` clause should match the names of data
frames in your current R session:

``` r
library(tidyquery)
library(nycflights13)

query(
" SELECT origin, dest,
    COUNT(flight) AS num_flts,
    round(SUM(seats)) AS num_seats,
    round(AVG(arr_delay)) AS avg_delay
  FROM flights f LEFT OUTER JOIN planes p
    ON f.tailnum = p.tailnum
  WHERE distance BETWEEN 200 AND 300
    AND air_time IS NOT NULL
  GROUP BY origin, dest
  HAVING num_flts > 3000
  ORDER BY num_seats DESC, avg_delay ASC
  LIMIT 2;"
)
#> # A tibble: 2 × 5
#>   origin dest  num_flts num_seats avg_delay
#>   <chr>  <chr>    <int>     <dbl>     <dbl>
#> 1 LGA    DCA       4468    712643         6
#> 2 EWR    BOS       5247    611192         5
```

Alternatively, for single-table queries, you can pass a data frame as
the first argument and a `SELECT` statement as the second argument,
omitting the `FROM` clause. This allows `query()` to function like a
dplyr verb:

``` r
library(dplyr)

airports %>%
  query("SELECT name, lat, lon ORDER BY lat DESC LIMIT 5")
#> # A tibble: 5 × 3
#>   name                                         lat    lon
#>   <chr>                                      <dbl>  <dbl>
#> 1 Dillant Hopkins Airport                     72.3   42.9
#> 2 Wiley Post Will Rogers Mem                  71.3 -157. 
#> 3 Wainwright Airport                          70.6 -160. 
#> 4 Wainwright As                               70.6 -160. 
#> 5 Atqasuk Edward Burnell Sr Memorial Airport  70.5 -157.
```

You can chain dplyr verbs before and after `query()`:

``` r
planes %>%
  filter(engine == "Turbo-fan") %>%
  query("SELECT manufacturer AS maker, COUNT(*) AS num_planes GROUP BY maker") %>%
  arrange(desc(num_planes)) %>%
  head(5)
#> # A tibble: 5 × 2
#>   maker            num_planes
#>   <chr>                 <int>
#> 1 BOEING                 1276
#> 2 BOMBARDIER INC          368
#> 3 AIRBUS                  331
#> 4 EMBRAER                 298
#> 5 AIRBUS INDUSTRIE        270
```

In the `SELECT` statement, the names of data frames and columns are
case-sensitive (like in R) but keywords and function names are
case-insensitive (like in SQL).

In addition to R data frames and tibbles (`tbl_df` objects), `query()`
can be used to query other data frame-like objects, including:

-   `dtplyr_step` objects created with
    [dtplyr](https://dtplyr.tidyverse.org), a
    [data.table](https://r-datatable.com/) backend for dplyr
-   `tbl_sql` objects created with
    [dbplyr](https://dbplyr.tidyverse.org) or a dbplyr backend package,
    enabling you to write SQL which is translated to dplyr then
    translated back to SQL and run in a database (a fun party trick!)
-   Apache Arrow `Table`, `RecordBatch`, `Dataset`, and
    `arrow_dplyr_query` objects created with
    [arrow](https://arrow.apache.org/docs/r/)

### Using `show_dplyr()`

**tidyquery** works by generating dplyr code. To print the dplyr code
instead of running it, use `show_dplyr()`:

``` r
show_dplyr(
" SELECT manufacturer, 
    COUNT(*) AS num_planes
  FROM planes
  WHERE engine = 'Turbo-fan'
  GROUP BY manufacturer
  ORDER BY num_planes DESC;"
)
#> planes %>%
#>   filter(engine == "Turbo-fan") %>%
#>   group_by(manufacturer) %>%
#>   summarise(num_planes = dplyr::n()) %>%
#>   ungroup() %>%
#>   arrange(dplyr::desc(num_planes))
```

## Current Limitations

**tidyquery** is subject to the current limitations of the queryparser
package. Please see the **Current Limitations** section of the
queryparser README on
[CRAN](https://cran.r-project.org/package=queryparser/readme/README.html#current-limitations)
or
[GitHub](https://github.com/ianmcook/queryparser#current-limitations).

**tidyquery** also has the following additional limitations:

-   Joins involving three or more tables are not supported.
-   Because joins in dplyr currently work in a fundamentally different
    way than joins in SQL, some other types of join queries are not
    supported. Examples of unsupported join queries include non-equijoin
    queries and outer join queries with qualified references to the join
    column(s). Planned changes in dplyr will enable future versions of
    tidyquery to support more types of joins.

## Related Work

The **sqldf** package ([CRAN](https://cran.r-project.org/package=sqldf),
[GitHub](https://github.com/ggrothendieck/sqldf)) runs SQL queries on R
data frames by transparently setting up a database, loading data from R
data frames into the database, running SQL queries in the database, and
returning results as R data frames.

The **duckdb** package
([CRAN](https://cran.r-project.org/package=duckdb),
[GitHub](https://github.com/duckdb/duckdb/tree/master/tools/rpkg))
includes the function `duckdb_register()` which registers an R data
frame as a virtual table in a [DuckDB](https://duckdb.org) database,
enabling you to run SQL queries on the data frame with
`DBI::dbGetQuery()`.

The **[dbplyr](https://dbplyr.tidyverse.org)** package
([CRAN](https://cran.r-project.org/package=dbplyr),
[GitHub](https://github.com/tidyverse/dbplyr)) is like tidyquery in
reverse: it converts dplyr code into SQL, allowing you to use dplyr to
work with data in a database.

In **Python**, the
[**dataframe_sql**](https://github.com/zbrookle/dataframe_sql) package
(targeting [**pandas**](https://pandas.pydata.org)) and the
[**sql_to_ibis**](https://github.com/zbrookle/sql_to_ibis) package
(targeting [**Ibis**](https://ibis-project.org)) are analogous to
tidyquery.
