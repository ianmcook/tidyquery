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
#' @importFrom dplyr rename
join <- function(tree) {

  out <- list()

  join_types <- attr(tree$from, "join_types")
  join_conditions <- attr(tree$from, "join_conditions")

  for (i in seq_along(tree$from)) {

    data <- tryCatch({
      eval(tree$from[[i]])
    }, error = function(e) {
      NULL
    })
    code <- tree$from[[i]]

    if (is.null(data)) {
      stop("No data frame exists with the name ", tree$from[[i]], call. = FALSE)
    }
    if (!is_supported_data_object(data)) {
      stop("The object with the name ", tree$from[[i]], " is not a supported data object", call. = FALSE)
    }

    if (i == 1) {

      out$data <- data
      out$code <- code

    } else {

      left_table_ref <- tree$from[i - 1]
      right_table_ref <- tree$from[i]
      left_table_columns <- column_names(out$data)
      right_table_columns <- column_names(data)

      join_type <- join_types[i - 1]
      join_condition <- unlist(
        translate_join_condition(
          condition = join_conditions[[i - 1]],
          left_table_ref = left_table_ref,
          right_table_ref = right_table_ref,
          left_table_columns = left_table_columns,
          right_table_columns = right_table_columns
        )
      )

      left_table_suffix = replace_empty_name_with_value(names(left_table_ref), as.character(left_table_ref))
      right_table_suffix = replace_empty_name_with_value(names(right_table_ref), as.character(right_table_ref))

      # check for column names that would make it impossible to identify the non-joined duplicate variables
      # based on their suffixes after the join
      if (any(ends_with_suffix(c(left_table_columns, right_table_columns), left_table_suffix)) ||
          any(ends_with_suffix(c(left_table_columns, right_table_columns), right_table_suffix))) {
        stop("Names of columns in data must not end with .", left_table_suffix,
             " or .", right_table_suffix, call. = FALSE)
      }

      if (join_type %in% inner_join_types) {

        out$data <- out$data %>% inner_join(
          data,
          by = join_condition,
          suffix = c(paste0(".", left_table_suffix), paste0(".", right_table_suffix)),
          na_matches = "never"
        )
        out$code <- paste0(
          out$code, " %>%\n  ",
          "inner_join(", tree$from[[i]],
          ", by = ", deparse(join_condition),
          ", suffix = c(\".", left_table_suffix, "\", \".", right_table_suffix, "\")",
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

      # check for suffixes, and if found, change them to prefixes
      columns_to_rename <- list()
      has_left_table_suffix <- ends_with_suffix(column_names(out$data), left_table_suffix)
      has_right_table_suffix <- ends_with_suffix(column_names(out$data), right_table_suffix)
      if (any(has_left_table_suffix)) {
        columns_to_rename <- c(columns_to_rename, structure(
          column_names(out$data)[has_left_table_suffix],
          .Names = suffix_to_prefix(column_names(out$data)[has_left_table_suffix], left_table_suffix)
        ))
      }
      if (any(has_right_table_suffix)) {
        columns_to_rename <- c(columns_to_rename, structure(
          column_names(out$data)[has_right_table_suffix],
          .Names = suffix_to_prefix(column_names(out$data)[has_right_table_suffix], right_table_suffix)
        ))
      }
      if (length(columns_to_rename) > 0) {
        out <- out %>% verb(rename, !!!columns_to_rename)
      }



    }
  }
  out
}

translate_join_condition <- function(condition, left_table_ref, right_table_ref, left_table_columns, right_table_columns) {
  if (is.logical(condition) && isTRUE(is.na(condition))) {
    return(NULL)
  }
  get_join_by(condition, left_table_ref, right_table_ref, left_table_columns, right_table_columns)
}

get_join_by <- function(expr, left_table_ref, right_table_ref, left_table_columns, right_table_columns) {
  if (identical(typeof(expr), "language")) {
    if (identical(expr[[1]], quote(`==`))) {
      table_1_ref <- get_prefix(expr[[2]])
      table_2_ref <- get_prefix(expr[[3]])
      column_1_ref <- remove_prefix(expr[[2]])
      column_2_ref <- remove_prefix(expr[[3]])
      if (identical(column_1_ref, column_2_ref)) {
        if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
          if ((is.null(table_1_ref) || isTRUE(table_1_ref %in% c(names(left_table_ref), as.character(left_table_ref)))) &&
              (is.null(table_2_ref) || isTRUE(table_2_ref %in% c(names(right_table_ref), as.character(right_table_ref))))) {
            return(as.character(column_1_ref))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else if (column_1_ref %in% right_table_columns && column_2_ref %in% left_table_columns) {
          if ((is.null(table_1_ref) || isTRUE(table_1_ref %in% c(names(right_table_ref), as.character(right_table_ref)))) &&
              (is.null(table_2_ref) || isTRUE(table_2_ref %in% c(names(left_table_ref), as.character(left_table_ref))))) {
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
        if (table_2_ref %in% c(names(left_table_ref), as.character(left_table_ref))) {
          if (column_1_ref %in% right_table_columns && column_2_ref %in% left_table_columns) {
            return(structure(as.character(column_1_ref), .Names = as.character(column_2_ref)))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else if (table_2_ref %in% c(names(right_table_ref), as.character(right_table_ref))) {
          if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
            return(structure(as.character(column_2_ref), .Names = as.character(column_1_ref)))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else {
          stop("Invalid join conditions", call. = FALSE)
        }
      } else if (is.null(table_2_ref)) {
        if (table_1_ref %in% c(names(left_table_ref), as.character(left_table_ref))) {
          if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
            return(structure(as.character(column_2_ref), .Names = as.character(column_1_ref)))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else if (table_1_ref %in% c(names(right_table_ref), as.character(right_table_ref))) {
          if (column_1_ref %in% right_table_columns && column_2_ref %in% left_table_columns) {
            return(structure(as.character(column_1_ref), .Names = as.character(column_2_ref)))
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else {
          stop("Invalid join conditions", call. = FALSE)
        }
      } else {
        if (table_1_ref %in% c(names(left_table_ref), as.character(left_table_ref))) {
          if (table_2_ref %in% c(names(right_table_ref), as.character(right_table_ref))) {
            if (column_1_ref %in% left_table_columns && column_2_ref %in% right_table_columns) {
              return(structure(as.character(column_2_ref), .Names = as.character(column_1_ref)))
            } else {
              stop("Invalid join conditions", call. = FALSE)
            }
          } else {
            stop("Invalid join conditions", call. = FALSE)
          }
        } else if (table_2_ref %in% c(names(left_table_ref), as.character(left_table_ref))) {
          if (table_1_ref %in% c(names(right_table_ref), as.character(right_table_ref))) {
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
      lapply(expr, get_join_by, left_table_ref, right_table_ref, left_table_columns, right_table_columns)
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

suffix_to_prefix <- function(name, suffix) {
  sub(paste0("(.+?)\\.", suffix, "$"), paste0(suffix, ".\\1"), name)
}

ends_with_suffix <- function(x, suffix) {
  grepl(paste0("\\.", suffix, "$"), x)
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
