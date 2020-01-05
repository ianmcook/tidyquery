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

#' @include compat.R
NULL

# import something from lubridate and stringr to stop "All declared Imports should be used" NOTE
#' @importFrom lubridate ymd
#' @importFrom stringr str_count
NULL

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
#'
#'   The names of data frames and columns are case-sensitive (like in R).
#'   Keywords and function names are not case-sensitive (like in SQL).
#'
#'   It is possible to use this function together with \pkg{dbplyr} to query
#'   remote database tables (\code{tbl_sql} objects), but this depends on which
#'   database and which backend package (if any) you are using, so results may
#'   vary.
#'
#'   This function is subject to the
#'   \href{https://cran.r-project.org/package=queryparser/readme/README.html#current-limitations}{current
#'   limitations of the \pkg{queryparser} package}.
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
#' @export
query <- function(data, sql) {
  query_(data, sql, TRUE)
}

#' @importFrom dplyr %>%
#' @importFrom dplyr mutate transmute select distinct filter summarise group_by arrange
#' @importFrom dplyr is_grouped_df ungroup
#' @importFrom queryparser parse_query
#' @importFrom utils head
query_ <- function(data, sql, query = TRUE) {
  if (query) {
    fun_name <- "query"
  } else {
    fun_name <- "show_dplyr"
  }

  if (missing("data") && missing("sql")) {
    stop("0 arguments passed to ", fun_name,"() which requires 1 or 2 arguments", call. = FALSE)
  }
  if (missing("sql")) {
    sql <- NULL
  }
  if (missing("data")) {
    data <- NULL
  }
  if (!is_supported_data_object(data) && is.character(data)) {
    if (is_supported_data_object(sql)) {
      stop("When calling ", fun_name, "() with two arguments, ",
           "specify the data first and the SQL statement second", call. = FALSE)
    }
    sql <- data
  }
  if (!is.character(sql) || length(sql) != 1) {
    stop("The first or second argument to ",
         fun_name, "() must be a character vector of length 1", call. = FALSE)
  }

  tree <- parse_query(sql, tidyverse = TRUE)

  ### from clause ###
  if (is.null(tree$from)) {

    if (!is_supported_data_object(data)) {
      stop("When calling ", fun_name, "(), you must specify which data frame to query ",
           "in the FROM clause of the SQL statement ",
           "or by passing a data frame as the first argument", call. = FALSE)
    }
    code <- paste0("<", class(data)[1], ">")

  } else {

    if (is_supported_data_object(data)) {
      stop("When calling ", fun_name, "(), specify which data frame to query ",
           "using either the first argument or the FROM clause, not both", call. = FALSE)
    }
    if (length(tree$from) > 1) {
      stop("Joins are not supported", call. = FALSE)
    }
    data <- tryCatch({
      eval(tree$from[[1]])
    }, error = function(e) {
      NULL
    })
    if (is.null(data)) {
      stop("No data frame exists with the name specified in the FROM clause", call. = FALSE)
    }
    if (!is_supported_data_object(data)) {
      stop("The object with the name specified in the FROM clause is not supported data object", call. = FALSE)
    }
    code <- tree$from[[1]]

  }

  if (!query) {
    data <- data %>% head(0L)
  }

  out <- list(
    code = code,
    data = data
  )

  if (inherits(data, "tbl_sql")) { # or "tbl_lazy"?

    tree <- unscope_all_expressions(tree)

  } else if (is_grouped_df(data)) {

    stop(fun_name, "() cannot work with grouped data frames. Use dplyr::ungroup() ",
         "to remove grouping from the data frame before calling ", fun_name, "()", call. = FALSE)

  } else if (!is_supported_data_object(data)) {

    stop("Unsupported data object", call. = FALSE)

  }

  ### select clause stage 1 ###
  tree$select <- replace_star_with_cols(tree$select, colnames(data))

  final_select_list <- tree$select

  alias_names <- names(tree$select)[names(tree$select) != ""]
  if (any(duplicated(alias_names))) {
    stop("The same alias is assigned to two or more columns in the SELECT list", call. = FALSE)
  }
  alias_values <- tree$select[alias_names]
  aliases <- quote_full_expressions(alias_values)

  if (is.null(names(tree$select))) {
    unaliased_select_exprs <- setdiff(vapply(tree$select, deparse, ""), colnames(data))
  } else {
    unaliased_select_exprs <- setdiff(vapply(tree$select, deparse, "")[names(tree$select) == ""], colnames(data))
  }

  ### where clause ###
  if (!is.null(tree$where)) {

    # SQL engines typically do not allow column aliases in the WHERE clause
    # so replace_aliases_with_values() is not called here
    out <- out %>% verb(filter, !!(tree$where[[1]]))

  }

  ### group by clause ###
  if (!is.null(tree$group_by)) {

    tree$group_by <- replace_aliases_with_values(tree$group_by, alias_names, alias_values)
    out <- out %>% verb(group_by, !!!(tree$group_by))

  }

  ### having clause ###
  if (!is.null(tree$having)) {

    tree$having <- replace_aliases_with_values(tree$having, alias_names, alias_values)
    out <- out %>% verb(filter, !!(tree$having[[1]]))

  }

  ### select clause stage 2 ###
  if (isTRUE(attr(tree, "aggregate"))) {

    cols_to_include_in_summarise <- unique(append(
      unname(tree$select[attr(tree$select, "aggregate")]),
      remove_desc_from_expressions(tree$order_by[attr(tree$order_by, "aggregate")])
    ))
    out <- out %>% verb(summarise, !!!(cols_to_include_in_summarise)) %>% verb(ungroup)

    if (length(aliases) > 0) {
      out <- out %>% verb(mutate, !!!aliases)
    }

    cols_to_add_after_grouping <- tree$select[!attr(tree$select, "aggregate") & !tree$select %in% tree$group_by]
    if (length(cols_to_add_after_grouping) > 0) {
      out <- out %>% verb(mutate, !!!cols_to_add_after_grouping)
    }

  } else if (isTRUE(attr(tree$select, "distinct"))) {

    out <- out %>% verb(distinct, !!!(unname(tree$select)))

    if (length(aliases) > 0) {
      out <- out %>% verb(mutate, !!!aliases)
    }

  } else {

    if (any(!vapply(tree$select, deparse, "") %in% colnames(data))) {
      cols_before <- colnames(out$data)
      out <- out %>% verb(mutate, !!!(unname(tree$select)))
      cols_after <- colnames(out$data)

      new_select_exprs <- setdiff(cols_after, c(cols_before, alias_names, alias_values))
      if (!identical(new_select_exprs, unaliased_select_exprs)) {

        if (length(new_select_exprs) < length(unaliased_select_exprs)) {

          stop("The SELECT list includes two or more long expressions with no aliases assigned ",
               "to them. You must assign aliases to these expressions", call. = FALSE)

        } else if (length(new_select_exprs) == length(unaliased_select_exprs)) {

          final_select_list[as.character(final_select_list) %in% unaliased_select_exprs] <-
            lapply(new_select_exprs, as.name)

        }
      }
    }

    # when dplyr shortens the expressions in the column names, use the aliases instead
    missing_exprs <- tree$select[which(!vapply(tree$select, deparse, "") %in% colnames(out$data))]
    if (length(missing_exprs) > 0) {
      out <- out %>% verb(mutate, !!!missing_exprs)
      aliases[names(missing_exprs)] <- NULL
      final_select_list[names(missing_exprs)] <-
        lapply(names(final_select_list[names(missing_exprs)]), as.name)
    }

    if (length(aliases) > 0) {
      out <- out %>% verb(mutate, !!!aliases)
    }

  }

  ### order by clause ###
  if (!is.null(tree$order_by)) {

    if (isTRUE(attr(tree, "aggregate")) || isTRUE(attr(tree$select, "distinct"))) {
      tree$order_by <- quote_columns_in_expressions(
        tree$order_by,
        unique(c(
          vapply(tree$select, deparse, ""),
          vapply(remove_desc_from_expressions(tree$order_by[attr(tree$order_by, "aggregate")]), deparse, "")
        ))
      )
    }
    out <- out %>% verb(arrange, !!!(tree$order_by))

  }

  ### select clause stage 3 ###
  if (isTRUE(attr(tree, "aggregate"))) {

    cols_to_return <- as.character(replace_values_with_aliases(tree$select, alias_values, alias_names))
    cols_to_return <- lapply(cols_to_return, as.name)
    out <- out %>% verb(select, !!!cols_to_return)

  } else {

    if (all(vapply(tree$select, deparse, "") %in% colnames(data))) {
      out <- out %>% verb(select, !!!(tree$select))
    } else {
      out <- out %>% verb(transmute, !!!(quote_full_expressions(final_select_list)))
    }

  }

  ### limit clause ###
  if (!is.null(tree$limit)) {

    out <- list(
      code = paste0(out$code, " %>%\n  head(", tree$limit[[1]], ")"),
      data = out$data %>% head(tree$limit[[1]])
    )

  }

  if (query) {
    out$data
  } else {
    cat(out$code)
    invisible(NULL)
  }
}

verb <- function(input, fun, ...) {
  list(
    code = paste(
      input$code,
      paste0(deparse(substitute(fun)), "(", deparse_dots(...), ")"),
      sep = " %>%\n  "
    ),
    data = input$data %>% fun(...)
  )
}

#' @importFrom rlang exprs expr_deparse
deparse_dots <- function(...) {
  output <- paste0(trimws(expr_deparse(exprs(...))), collapse = " ")
  output <- substr(output, 8, nchar(output) - 1)
  output
}
