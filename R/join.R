# Copyright 2020 Cloudera Inc.
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

#' @include query.R
NULL

#' @importFrom dplyr inner_join left_join right_join full_join semi_join anti_join
join <- function(from) {

  out <- list()

  join_types <- attr(from, "join_types")
  join_conditions <- attr(from, "join_conditions")

  for (i in seq_along(from)) {

    data <- tryCatch({
      eval(from[[i]])
    }, error = function(e) {
      NULL
    })
    code <- from[[i]]

    if (is.null(data)) {
      stop("No data frame exists with the name ", from[[i]], call. = FALSE)
    }
    if (!is_supported_data_object(data)) {
      stop("The object with the name ", from[[i]], " is not a supported data object", call. = FALSE)
    }

    if (i == 1) {

      out$data <- data
      out$code <- code

    } else {

      join_type <- join_types[i - 1]
      join_condition <- unlist(translate_join_condition(
        join_conditions[[i - 1]],
        from[i - 1],
        from[i],
        column_names(out$data),
        column_names(data)
      ))

      if (join_type %in% inner_join_types) {

        out$data <- out$data %>% inner_join(
          data,
          by = join_condition,
          suffix = c(".x", ".y"), # TBD: specify suffixes that match table names/aliases
          na_matches = "never"
        )
        out$code <- paste0(
          out$code, " %>%\n  ",
          "inner_join(", from[[i]],
          ", by = ", deparse(join_condition),
          ", suffix = c(\".x\", \".y\")", # TBD: specify suffixes that match table names/aliases
          ", na_matches = \"never\")"
        )

      } else if (join_type %in% left_outer_join_types) {

        # in a left outer join, check whether the select list contains any references to the columns in the "by" list
        # that are qualified with the name or alias of the right table; if so, throw an error saying:
        # In left outer joins, dplyr returns the join key column(s) from the left table. Qualified references to join key column(s) from the right table are unsupported

      } else if (join_type %in% right_outer_join_types) {

        # in a right outer join, check whether the select list contains any references to the columns in the "by" list
        # that are qualified with the name or alias of the left table; if so, throw an error saying:
        # In right outer joins, dplyr returns the join key column(s) from the right table. Qualified references to join key column(s) from the left table are unsupported

      } else if (join_type %in% full_outer_join_types) {

        # in a full outer join, check whether the select list contains any references to the columns in the "by" list
        # that are qualified with the name or alias of the left or right tables; if so, throw an error saying:
        # In full outer joins, dplyr coalesces the join key column(s) from the right and left tables. Qualified references to join key column(s) are unsupported

      } else if (join_type %in% left_semi_join_types) {



      } else if (join_type %in% right_semi_join_types) {



      } else if (join_type %in% left_anti_join_types) {



      } else if (join_type %in% right_anti_join_types) {



      } else {

        stop("Unsupported join type", call. = FALSE)

      }


    }



  }
  out
}

translate_join_condition <- function(condition, left_table_name, right_table_name, left_table_columns, right_table_columns) {
  if (is.logical(condition) && isTRUE(is.na(condition))) {
    return(NULL)
  }
  join_by <- get_join_by(condition, left_table_name, right_table_name, left_table_columns, right_table_columns)

  # traverse the AST
  # whenver you find a `==`, take its args
  # check which represents a column in the right table and which represents a column in the left table
  # remember that the names of left_table_name and right_table_name can have names giving the aliases or can be NULL or ""!





}

get_join_by <- function(expr, left_table_name, right_table_name, left_table_columns, right_table_columns) {
  if (identical(typeof(expr), "language")) {
    if (identical(expr[[1]], quote(`==`))) {
      table_1_ref <- get_prefix(expr[[2]])
      table_2_ref <- get_prefix(expr[[3]])
      column_1_ref <- remove_prefix(expr[[2]])
      column_2_ref <- remove_prefix(expr[[3]])
      if (identical(column_1_ref, column_2_ref)) {
        if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
          if ((is.null(table_1_ref) || isTRUE(table_1_ref %in% c(names(left_table_name), as.character(left_table_name)))) &&
              (is.null(table_2_ref) || isTRUE(table_2_ref %in% c(names(right_table_name), as.character(right_table_name))))) {
            return(as.character(column_1_ref))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else if (column_1_ref %in% right_table_columns && column_2_ref %in% left_table_columns) {
          if ((is.null(table_1_ref) || isTRUE(table_1_ref %in% c(names(right_table_name), as.character(right_table_name)))) &&
              (is.null(table_2_ref) || isTRUE(table_2_ref %in% c(names(left_table_name), as.character(left_table_name))))) {
            return(as.character(column_1_ref))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else {
          stop("Invalid join conditions", call. = FALSE)
        }
      } else if (is.null(table_1_ref) && is.null(table_2_ref)) {
        if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
          return(structure(as.character(column_2_ref), .Names = as.character(column_1_ref)))
        } else if (column_1_ref %in% right_table_columns && column_2_ref %in% left_table_columns) {
          return(structure(as.character(column_1_ref), .Names = as.character(column_2_ref)))
        } else {
          stop("Invalid join conditions", call. = FALSE)
        }
      } else if (is.null(table_1_ref)) {
        if (table_2_ref %in% c(names(left_table_name), as.character(left_table_name))) {
          if (column_1_ref %in% right_table_columns && column_2_ref %in% left_table_columns) {
            return(structure(as.character(column_1_ref), .Names = as.character(column_2_ref)))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else if (table_2_ref %in% c(names(right_table_name), as.character(right_table_name))) {
          if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
            return(structure(as.character(column_2_ref), .Names = as.character(column_1_ref)))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else {
          stop("Invalid join conditions", call. = FALSE)
        }
      } else if (is.null(table_2_ref)) {
        if (table_1_ref %in% c(names(left_table_name), as.character(left_table_name))) {
          if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
            return(structure(as.character(column_2_ref), .Names = as.character(column_1_ref)))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else if (table_1_ref %in% c(names(right_table_name), as.character(right_table_name))) {
          if (column_1_ref %in% right_table_columns && column_2_ref %in% left_table_columns) {
            return(structure(as.character(column_1_ref), .Names = as.character(column_2_ref)))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else {
          stop("Invalid join conditions", call. = FALSE)
        }
      } else {
        if (table_1_ref %in% c(names(left_table_name), as.character(left_table_name))) {
          if (table_2_ref %in% c(names(right_table_name), as.character(right_table_name))) {
            if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
              return(structure(as.character(column_2_ref), .Names = as.character(column_1_ref)))
            } else {
              stop("Invalid join conditions", call. = FALSE)
            }
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else if (table_2_ref %in% c(names(left_table_name), as.character(left_table_name))) {
          if (table_1_ref %in% c(names(right_table_name), as.character(right_table_name))) {
            if (column_1_ref %in% right_table_columns && column_2_ref %in% left_table_columns) {
              return(structure(as.character(column_1_ref), .Names = as.character(column_2_ref)))
            } else {
              stop("Invalid join conditions", call. = FALSE)
            }
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else {
          stop("Invalid join conditions", call. = FALSE)
        }
      }
    } else {
      lapply(expr, get_join_by, left_table_name, right_table_name, left_table_columns, right_table_columns)
    }
  } else {
    NULL
  }
}

get_prefix <- function(colname) {
  colname <- as.character(colname)
  if (!grepl(".", colname, fixed = TRUE)) {
    NULL
  } else {
    sub("^([^.]+?)\\..+$", "\\1", colname)
  }
}

remove_prefix <- function(colname) {
  colname <- as.character(colname)
  if (!grepl(".", colname, fixed = TRUE)) {
    colname
  } else {
    sub("^[^.]+?\\.(.+)$", "\\1", colname)
  }
}

inner_join_types <- c(
  "inner join",
  "natural inner join"
)
left_outer_join_types <- c(
  "left outer join",
  "natural left outer join"
)
right_outer_join_types <- c(
  "right outer join",
  "natural right outer join"
)
full_outer_join_types <- c(
  "full outer join",
  "natural full outer join"
)
left_semi_join_types <- c(
  "left semi join",
  "natural left semi join"
)
right_semi_join_types <- c(
  "right semi join",
  "natural right semi join"
)
left_anti_join_types <- c(
  "left anti join",
  "natural left anti join"
)
right_anti_join_types <- c(
  "right anti join",
  "natural right anti join"
)

# TBD: implement cross joins after implemented in dplyr
# (see https://github.com/tidyverse/dplyr/issues/4206)
