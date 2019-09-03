# Copyright 2019 Cloudera Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Query an R data frame with SQL
#'
#' @description \code{query} takes a SQL \code{SELECT} statement and uses it to
#'   query an R data frame
#'
#' @param data a data frame or data frame-like object (optional)
#' @param sql a character string containing a SQL \code{SELECT} statement
#' @return An object of the same class as \code{data}.
#' @details If the \code{data} argument is not specified, then the \code{FROM}
#'   clause of the SQL statement determines which data frame to query.
#' @examples
#' library(dplyr)
#'
#' iris %>% query("SELECT Species, AVG(Petal.Length) GROUP BY Species")
#'
#' query("SELECT Species, AVG(Petal.Length) FROM iris GROUP BY Species")
#'
#' iris %>%
#'   filter(Petal.Length > 4) %>%
#'   query("SELECT Species, MAX(Sepal.Length) AS max_sep_len
#'            GROUP BY Species") %>%
#'   arrange(desc(max_sep_len))
#'
#' library(nycflights13)
#'
#' query <- "SELECT origin, dest,
#'     COUNT(flight) AS num_flts,
#'     round(AVG(distance)) AS dist,
#'     round(AVG(arr_delay)) AS avg_delay
#'   FROM flights
#'   WHERE distance BETWEEN 200 AND 300
#'     AND air_time IS NOT NULL
#'   GROUP BY origin, dest
#'   HAVING num_flts > 5000
#'   ORDER BY num_flts DESC, avg_delay DESC
#'   LIMIT 100;"
#'
#' query(query)
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate transmute select distinct filter summarise group_by arrange
#' @importFrom dplyr is_grouped_df ungroup
#' @importFrom queryparser parse_query
#' @importFrom utils head
#' @export
query <- function(data = NULL, sql = NULL) {
  if (is.null(data) && is.null(sql)) {
    stop("0 arguments passed to query() which requires 1 or 2 arguments")
  }
  if (!is.data.frame(data) && !inherits(data, "tbl") && is.character(data)) {
    if (is.data.frame(sql) || inherits(sql, "tbl")) {
      stop("When calling query() with two arguments, specify the data first and the SQL statement second")
    }
    sql <- data
  }
  if (!is.character(sql) || length(sql) != 1) {
    stop("The first or second argument to query() must be a character vector of length 1")
  }

  tree <- parse_query(sql, tidyverse = TRUE)

  ### from clause ###
  if (is.null(tree$from)) {
    if (!is.data.frame(data) && !inherits(data, "tbl")) {
      stop("When calling query(), you must specify which data frame to query ",
           "in the FROM clause of the SQL statement ",
           "or by passing a data frame as the first argument")
    }
  } else {
    if (is.data.frame(data) || inherits(data, "tbl")) {
      stop("When calling query(), specify which data frame to query ",
           "using either the first argument or the FROM clause, not both")
    }
    data <- tryCatch({
      eval(tree$from[[1]])
    }, error = function(e) {
      NULL
    })
    if (is.null(data)) {
      stop("No data frame exists with the name specified in the FROM clause")
    }
    if (!is.data.frame(data) && !inherits(data, "tbl")) {
      stop("The object with the name specified in the FROM clause is not a data frame")
    }
  }

  out <- data

  if (inherits(data, "tbl_sql")) { # or "tbl_lazy"?

    # TBD: check to see if the translation
    # defines all the functions that are used

    tree <- unscope_all_expressions(tree)

  } else if (is_grouped_df(data)) {

    stop("query() cannot work with grouped data frames. Use dplyr::ungroup() ",
         "to remove grouping from the data frame before calling query()")

  } else if (!is.data.frame(data)) {

    stop("Unsupported data object")

  }

  ### select clause stage 1 ###
  tree$select <- replace_star_with_cols(tree$select, colnames(data))

  alias_names <- names(tree$select)[names(tree$select) != ""]
  if (any(duplicated(alias_names))) {
    stop("The same alias is assigned to two or more columns in the SELECT list")
  }
  alias_values <- tree$select[alias_names]

  ### where clause ###
  if (!is.null(tree$where)) {
    out <- out %>% filter(!!(tree$where[[1]]))
  }

  ### group by clause ###
  if (!is.null(tree$group_by)) {
    tree$group_by <- replace_aliases_with_values(tree$group_by, alias_names, alias_values)
    out <- out %>% group_by(!!!(tree$group_by))
  }

  ### having clause ###
  if (!is.null(tree$having)) {
    #tree$having <- replace_aliases_with_values(tree$having, alias_names, alias_values)
    out <- out %>% filter(!!(tree$having[[1]]))
  }

  ### select clause stage 2 ###
  if (isTRUE(attr(tree, "aggregate"))) {
    cols_to_include_in_summarise <- unname(tree$select[attr(tree$select, "aggregate")])
    out <- out %>% summarise(!!!(cols_to_include_in_summarise)) %>% ungroup()

    if (!is.null(names(tree$select))) {
      out <- out %>% mutate(!!!(tree$select[names(tree$select) != ""])) # should we quote though?
    }

    cols_to_add_after_grouping <- tree$select[!attr(tree$select, "aggregate") & !tree$select %in% tree$group_by]
    out <- out %>% mutate(!!!cols_to_add_after_grouping)

    cols_in_desired_order <- as.character(tree$select)
    if (!is.null(names(tree$select))) {
      cols_in_desired_order[names(tree$select) != ""] <- names(tree$select)[names(tree$select) != ""]
    }
    out <- out %>% select(cols_in_desired_order)

  } else if (isTRUE(attr(tree$select, "distinct"))) {

    out <- out %>% distinct(!!!(unname(tree$select)))

    if (!is.null(names(tree$select))) {
      out <- out %>% mutate(!!!(tree$select[names(tree$select) != ""])) # should we quote though?
    }

  } else {

    if (any(!as.character(tree$select) %in% colnames(data))) {
      out <- out %>% mutate(!!!(unname(tree$select)))
    }

    if (!is.null(names(tree$select))) {
      out <- out %>% mutate(!!!(tree$select[names(tree$select) != ""])) # should we quote though?
    }

  }

  ### order by clause ###
  if (!is.null(tree$order_by)) {
    #tree$order_by <- replace_values_with_aliases(tree$order_by, as.character(alias_values), lapply(alias_names, as.name))
    if (isTRUE(attr(tree, "aggregate")) || isTRUE(attr(tree$select, "distinct"))) {
      tree$order_by <- quote_columns(tree$order_by, as.character(tree$select))
    }
    out <- out %>% arrange(!!!(tree$order_by))
  }

  ### select clause stage 3 ###
  if (!isTRUE(attr(tree, "aggregate")) && !isTRUE(attr(tree$select, "distinct"))) {
    if (all(as.character(tree$select) %in% colnames(data))) {
      out <- out %>% select(!!!(tree$select))
    } else {
      out <- out %>% transmute(!!!(tree$select))
    }
  }

  ### limit clause ###
  if (!is.null(tree$limit)) {
    out <- out %>% head(n = tree$limit[[1]])
  }

  out
}
