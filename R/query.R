# Copyright 2021 Cloudera Inc.
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
#'   In addition to R data frames and tibbles (\code{tbl_df} objects), this
#'   function can query \code{dtplyr_step} objects created by \pkg{dtplyr}, a
#'   \pkg{data.table} backend for \pkg{dbplyr}. It is also possible to use this
#'   function together with \pkg{dbplyr} to query remote database tables
#'   (\code{tbl_sql} objects), but this depends on which database and which
#'   backend package (if any) you are using, so results may vary.
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
#' @importFrom dplyr ungroup
#' @importFrom queryparser parse_query unqualify_query
#' @importFrom utils head
query_ <- function(data, sql, query = TRUE) {
  if (query) {
    fun_name <- "query"
  } else {
    fun_name <- "show_dplyr"
  }

  if (missing("data") && missing("sql")) {
    stop("0 arguments passed to ", fun_name, "() which requires 1 or 2 arguments", call. = FALSE)
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
        "specify the data first and the SQL statement second",
        call. = FALSE
      )
    } else if (!is.null(sql)) {
      stop("Unexpected input to ", fun_name, "()", call. = FALSE)
    }
    sql <- data
  }
  if ((!is.character(sql) || length(sql) != 1) && (!is.character(data) || length(data) != 1)) {
    stop("The first or second argument to ",
      fun_name, "() must be a character vector of length 1",
      call. = FALSE
    )
  }

  tree <- parse_query(sql, tidyverse = TRUE)


  ### from clause ###
  if (is.null(tree$from)) {
    if (!is_supported_data_object(data)) {
      stop("When calling ", fun_name, "(), you must specify which data frame to query ",
        "in the FROM clause of the SQL statement ",
        "or by passing a data frame as the first argument",
        call. = FALSE
      )
    }
    out <- list()
    out$data <- data
    rm(data)
    out$code <- paste0("<", class(data)[1], ">")

  } else {

    if (is_supported_data_object(data)) {
      stop("When calling ", fun_name, "(), specify which data frame to query ",
        "using either the first argument or the FROM clause, not both",
        call. = FALSE
      )
    }

    out <- join(tree)

    if (length(tree$from) > 1) {

      # unqualify column references that do not need qualificaiton
      table_names <- as.character(tree$from)
      if (!is.null(names(tree$from))) {
        table_aliases <- names(tree$from)
      } else {
        table_aliases <- character(0)
      }
      table_prefixes <- c(table_names, table_aliases)
      tree <- unqualify_query(tree, prefixes = table_prefixes, except = column_names(out$data))

    }

  }

  if (is_grouped_data_object(out$data)) {
    stop(fun_name, "() cannot work with grouped data frames. Use dplyr::ungroup() ",
      "to remove grouping from the data frame before calling ", fun_name, "()",
      call. = FALSE
    )
  }

  if (data_object_uses_function_translations(out$data)) {
    tree <- unscope_all_expressions(tree)
  }

  if (!query) {
    out$data <- out$data %>% head(0L)
  }

  ### select clause stage 1 ###
  tree$select <- replace_star_with_cols(tree$select, column_names(out$data))

  final_select_list <- tree$select

  alias_names <- names(tree$select)[names(tree$select) != ""]
  if (any(duplicated(alias_names))) {
    stop("The same alias is assigned to two or more columns in the SELECT list", call. = FALSE)
  }
  alias_values <- tree$select[alias_names]
  aliases <- quote_full_expressions(alias_values)

  if (is.null(names(tree$select))) {
    unaliased_exprs <- vapply(tree$select, deparse, "")
    aliases_and_unaliased_exprs <- unaliased_exprs
  } else {
    unaliased_exprs <- vapply(tree$select, deparse, "")[names(tree$select) == ""]
    aliases_and_unaliased_exprs <- vapply(replace_empty_names_with_values(names(tree$select), unname(tree$select)), deparse, "")
  }
  if (any(duplicated(unaliased_exprs))) {
    stop("The SELECT list includes two or more identical expressions with no aliases. Use aliases ",
      "to give these expressions unique column names",
      call. = FALSE
    )
  }
  if (any(duplicated(aliases_and_unaliased_exprs))) {
    stop("The SELECT list would result in two or more columns with identical names. Use aliases ",
      "to assign unique column names to the expressions in the SELECT list",
      call. = FALSE
    )
  }

  if (is.null(names(tree$select))) {
    unaliased_select_exprs <- setdiff(vapply(tree$select, deparse, ""), column_names(out$data))
  } else {
    unaliased_select_exprs <- setdiff(vapply(tree$select, deparse, "")[names(tree$select) == ""], column_names(out$data))
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
  cols_before <- column_names(out$data)

  # if the ORDER BY or GROUP BY clause refers to one or more aliased expressions defined in the SELECT list
  # not by its alias but using the same expression itself, then set use_quoted_deparsed_expressions to TRUE,
  # indicating that additional steps are necessary to process this query
  if (is.null(names(tree$select))) {
    aliased_exprs <- setdiff(tree$select, column_names(out$data))
  } else {
    aliased_exprs <- setdiff(unname(tree$select[names(tree$select) != ""]), column_names(out$data))
  }
  use_quoted_deparsed_expressions <-
    any(remove_desc_from_expressions(tree$order_by) %in% aliased_exprs) || any(tree$group_by %in% aliased_exprs)

  if (isTRUE(attr(tree, "aggregate"))) {

    if (any(!c(tree$group_by, remove_desc_from_expressions(tree$order_by)) %in%
      replace_empty_names_with_values(names(tree$select), unname(tree$select)))) {
      use_quoted_deparsed_expressions <- TRUE
    }

    if (use_quoted_deparsed_expressions) {
      cols_to_include_in_summarise <- unique(c(
        unname(tree$select[attr(tree$select, "aggregate")]),
        remove_desc_from_expressions(tree$order_by[attr(tree$order_by, "aggregate")])
      ))
      out <- out %>%
        verb(summarise, !!!(cols_to_include_in_summarise)) %>%
        verb(ungroup)
    } else {
      out <- out %>%
        verb(summarise, !!!(tree$select[attr(tree$select, "aggregate")])) %>%
        verb(ungroup)
    }

    cols_to_add_after_grouping <- tree$select[!attr(tree$select, "aggregate") & !tree$select %in% tree$group_by]
    if (length(cols_to_add_after_grouping) > 0) {
      out <- out %>% verb(mutate, !!!cols_to_add_after_grouping)
    }

    transmute_early <- FALSE

  } else if (isTRUE(attr(tree$select, "distinct"))) {

    if (any(!remove_desc_from_expressions(tree$order_by) %in%
      replace_empty_names_with_values(names(tree$select), unname(tree$select)))) {
      use_quoted_deparsed_expressions <- TRUE
    }

    if (use_quoted_deparsed_expressions) {
      out <- out %>% verb(distinct, !!!(unname(tree$select)))
    } else {
      out <- out %>% verb(distinct, !!!(tree$select))
    }

    transmute_early <- FALSE

  } else {

    # if the ORDER BY clause refers only to aliases and unalised expressions that exist in the SELECT list,
    # then set transmute_early to TRUE, indicating that we can use transmute() instead of mutate() at this
    # stage, and omit the select() in a later stage
    if (is.null(names(tree$select))) {
      transmute_early <-
        !use_quoted_deparsed_expressions && all(remove_desc_from_expressions(tree$order_by) %in% tree$select)
    } else {
      transmute_early <- !use_quoted_deparsed_expressions && all(
        remove_desc_from_expressions(tree$order_by) %in% replace_empty_names_with_values(names(tree$select), unname(tree$select))
      )
    }

    if (transmute_early && all(vapply(tree$select, deparse, "") %in% column_names(out$data))) {
      out <- out %>% verb(select, !!!(tree$select))
    } else if (transmute_early) {
      out <- out %>% verb(transmute, !!!(tree$select))
    } else if (use_quoted_deparsed_expressions) {
      out <- out %>% verb(mutate, !!!(unname(tree$select)))
    } else {
      out <- out %>% verb(mutate, !!!(tree$select))
    }

  }

  cols_after <- column_names(out$data)

  new_select_exprs <- setdiff(
    c(cols_after),
    setdiff(c(cols_before, alias_names, alias_values), lapply(tree$group_by, deparse))
  )

  if (!identical(new_select_exprs, unaliased_select_exprs)) {

    if (length(new_select_exprs) < length(unaliased_select_exprs)) {

      stop("The SELECT list includes two or more long expressions with no aliases assigned ",
        "to them. You must assign aliases to these expressions",
        call. = FALSE
      )

    } else if (length(new_select_exprs) == length(unaliased_select_exprs)) {

      final_select_list[as.character(final_select_list) %in% unaliased_select_exprs] <-
        lapply(new_select_exprs, as.name)

    }

  }

  if (use_quoted_deparsed_expressions) {

    # when dplyr shortens the expressions in the column names, use the aliases instead
    missing_exprs <- tree$select[which(!vapply(tree$select, deparse, "") %in% column_names(out$data))]
    if (length(missing_exprs) > 0) {
      if (!isTRUE(attr(tree, "aggregate")) || length(setdiff(missing_exprs, cols_to_add_after_grouping)) > 0) {
        out <- out %>% verb(mutate, !!!missing_exprs)
      }
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

    if (is.null(names(final_select_list))) {
      cols_to_return <- quote_full_expressions(final_select_list)
    } else {
      cols_to_return <- replace_empty_names_with_values(names(final_select_list), unname(final_select_list))
      cols_to_return <- quote_full_expressions(cols_to_return)
    }

    if (length(cols_to_return) > 0 && length(cols_to_return) < ncol(out$data)) {
      out <- out %>% verb(select, !!!cols_to_return)
    }

  } else {

    if (use_quoted_deparsed_expressions) {

      out <- out %>% verb(transmute, !!!(quote_full_expressions(final_select_list)))

    } else if (!transmute_early) {

      if (is.null(names(final_select_list))) {
        cols_to_return <- quote_full_expressions(final_select_list)
      } else {
        cols_to_return <- replace_empty_names_with_values(names(final_select_list), unname(final_select_list))
        cols_to_return <- quote_full_expressions(cols_to_return)
        if (!all(cols_to_return %in% final_select_list)) {
          cols_to_return <- final_select_list
        }
      }

      if (length(cols_to_return) > 0 && length(cols_to_return) < ncol(out$data)) {
        out <- out %>% verb(select, !!!cols_to_return)
      }

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
  dots <- exprs(...)
  if (length(dots) < 1) {
    ""
  } else {
    output <- expr_deparse(dots)
    output <- paste0(trimws(output), collapse = " ")
    output <- substr(output, 8, nchar(output) - 1)
    output
  }
}
