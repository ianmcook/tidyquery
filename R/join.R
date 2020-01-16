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
#' @importFrom queryparser column_references
join <- function(tree) {

  out <- list()

  join_types <- attr(tree$from, "join_types")
  join_conditions <- attr(tree$from, "join_conditions")

  if (length(tree$from) > 2) {
    stop("Joins of three or more tables are unsupported", call. = FALSE)
  }

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

    column_refs <- column_references(tree, from = FALSE)

    # throw error if there are misqualified column references
    table_name <- as.character(tree$from[[i]])
    if (!is.null(names(tree$from[i])) && names(tree$from[i]) != "") {
      table_alias <- names(tree$from[i])
    } else {
      table_alias <- character(0)
    }
    table_prefixes <- c(table_name, table_alias)
    qualified_column_refs <-
      column_refs[grepl(paste0("^(\\Q", paste0(table_prefixes, collapse = "\\E|\\Q"), "\\E)\\."), column_refs)]
    qualified_column_names <-
      sub(paste0("^(\\Q", paste0(table_prefixes, collapse = "\\E|\\Q"), "\\E)\\."), "", qualified_column_refs)
    misqualified_column_refs <- !(qualified_column_names %in% column_names(data))
    if (any(misqualified_column_refs)) {
      stop("Query contains misqualified column reference(s): ",
           paste0(qualified_column_refs[misqualified_column_refs], collapse = ", "), call. = FALSE)
    }

    if (i == 1) {

      out$data <- data
      out$code <- code

    } else {

      left_table_ref <- tree$from[i - 1]
      right_table_ref <- tree$from[i]
      all_table_refs <- c(
        if (!is.null(names(left_table_ref)) && names(left_table_ref) != "") names(left_table_ref),
        as.character(left_table_ref),
        if (!is.null(names(right_table_ref)) && names(right_table_ref) != "") names(right_table_ref),
        as.character(right_table_ref)
      )

      left_table_columns <- column_names(out$data)
      right_table_columns <- column_names(data)

      join_type <- join_types[i - 1]
      join_condition <- unlist(
        translate_join_condition(
          condition = join_conditions[[i - 1]],
          all_table_refs = all_table_refs,
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

      # check for qualified references to join key column(s) from the left or right table that are not returned
      # by joins in dplyr
      if (join_type %in% c(left_outer_join_types, full_outer_join_types)) {
        right_table_refs <- c(
          as.character(right_table_ref),
          if (!is.null(names(right_table_ref)) && names(right_table_ref) != "") names(right_table_ref)
        )
        right_table_join_columns <- paste(right_table_refs, unname(join_condition), sep = ".")
        bad_right_table_columns <- right_table_join_columns %in% column_refs
      }
      if (join_type %in% c(right_outer_join_types, full_outer_join_types)) {
        left_table_refs <- c(
          as.character(left_table_ref),
          if (!is.null(names(left_table_ref)) && names(left_table_ref) != "") names(left_table_ref)
        )
        left_table_join_columns <- paste(
          left_table_refs,
          c(
            if (is.null(names(join_condition))) join_condition else names(join_condition),
            unname(join_condition)[names(join_condition) == ""]
          ),
          sep = "."
        )
        bad_left_table_columns <- left_table_join_columns %in% column_refs
      }
      if (join_type %in% left_outer_join_types) {
        if (any(bad_right_table_columns)) {
          stop("In left outer joins, dplyr returns only the join key column(s) from the left table. ",
               "The following qualified references to join key column(s) from the right table are unsupported: ",
               paste0(right_table_join_columns[bad_right_table_columns], collapse = ", "), call. = FALSE)
        }
      } else if (join_type %in% right_outer_join_types) {
        if (any(bad_left_table_columns)) {
          stop("In right outer joins, dplyr returns only the join key column(s) from the right table. ",
               "The following qualified references to join key column(s) from the left table are unsupported: ",
               paste0(left_table_join_columns[bad_left_table_columns], collapse = ", "), call. = FALSE)
        }
      } else if (join_type %in% full_outer_join_types) {
        if (any(bad_left_table_columns) || any(bad_right_table_columns)) {
          stop("In full outer joins, dplyr coalesces the join key column(s) from the right and left tables. ",
               "The following qualified references to join key column(s) are unsupported: ",
               paste0(c(left_table_join_columns[bad_left_table_columns],
                 right_table_join_columns[bad_right_table_columns]), collapse = ", "), call. = FALSE)
        }
      }

      # determine which dplyr join function to use
      if (join_type %in% inner_join_types) {
        join_function <- inner_join
        join_function_name <- "inner_join"
      } else if (join_type %in% left_outer_join_types) {
        join_function <- left_join
        join_function_name <- "left_join"
      } else if (join_type %in% right_outer_join_types) {
        join_function <- right_join
        join_function_name <- "right_join"
      } else if (join_type %in% full_outer_join_types) {
        join_function <- full_join
        join_function_name <- "full_join"
      } else if (join_type %in% left_semi_join_types) {
        join_function <- semi_join
        join_function_name <- "semi_join"
      } else if (join_type %in% left_anti_join_types) {
        join_function <- anti_join
        join_function_name <- "anti_join"
      } else {
        stop("Unsupported join type", call. = FALSE)
      }

      # perfom the join
      out$data <- out$data %>% join_function(
        data,
        by = join_condition,
        suffix = c(paste0(".", left_table_suffix), paste0(".", right_table_suffix)),
        na_matches = "never"
      )
      out$code <- paste0(
        out$code, " %>%\n  ",
        join_function_name, "(", as.character(right_table_ref),
        ", by = ", deparse(join_condition),
        ", suffix = c(\".", left_table_suffix, "\", \".", right_table_suffix, "\")",
        ", na_matches = \"never\")"
      )

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

      # throw error if there are ambiguous column references
      ambiguous_column_refs <-
        (column_refs %in% column_names(data)) & !(column_refs %in% column_names(out$data))
      if (any(ambiguous_column_refs)) {
        stop("Query contains ambiguous column reference(s): ",
             paste0(column_refs[ambiguous_column_refs], collapse = ", "), call. = FALSE)
      }

    }

  }
  out
}

translate_join_condition <- function(condition, all_table_refs, left_table_ref,
                                     right_table_ref, left_table_columns, right_table_columns) {
  if (is.logical(condition) && isTRUE(is.na(condition))) {
    return(NULL)
  }
  get_join_by(condition, all_table_refs, left_table_ref, right_table_ref, left_table_columns, right_table_columns)
}

get_join_by <- function(expr, all_table_refs, left_table_ref, right_table_ref, left_table_columns, right_table_columns) {
  if (identical(typeof(expr), "language")) {
    if (identical(expr[[1]], quote(`==`))) {
      table_1_ref <- get_prefix(expr[[2]], all_table_refs)
      table_2_ref <- get_prefix(expr[[3]], all_table_refs)
      column_1_ref <- remove_prefix(expr[[2]], table_1_ref)
      column_2_ref <- remove_prefix(expr[[3]], table_2_ref)
      if (identical(column_1_ref, column_2_ref)) {
        if (column_1_ref %in% left_table_columns && column_1_ref %in% right_table_columns) {
          if ((is.null(table_1_ref) || isTRUE(table_1_ref %in% c(names(left_table_ref), as.character(left_table_ref)))) &&
              (is.null(table_2_ref) || isTRUE(table_2_ref %in% c(names(right_table_ref), as.character(right_table_ref))))) {
            return(as.character(column_1_ref))
          } else if ((is.null(table_1_ref) || isTRUE(table_1_ref %in% c(names(right_table_ref), as.character(right_table_ref)))) &&
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
      lapply(expr, get_join_by, all_table_refs, left_table_ref, right_table_ref, left_table_columns, right_table_columns)
    }
  } else {
    NULL
  }
}

get_prefix <- function(col_name, possible_prefixes) {
  colname <- as.character(col_name)
  if (!grepl(".", col_name, fixed = TRUE)) {
    NULL
  } else {
    sub(paste0("^(\\Q", paste0(possible_prefixes, collapse = "\\E|\\Q"), "\\E)\\..+$"), "\\1", col_name)
  }
}

remove_prefix <- function(col_names, prefix) {
  col_names <- as.character(col_names)
  if (is.null(prefix) || !any(grepl(".", col_names, fixed = TRUE))) {
    col_names
  } else {
    sub(paste0("^\\Q", prefix, "\\E\\.(.+)$"), "\\1", col_names)
  }
}

suffix_to_prefix <- function(name, suffix) {
  sub(paste0("^(.+?)\\.\\Q", suffix, "\\E$"), paste0(suffix, ".\\1"), name)
}

ends_with_suffix <- function(x, suffix) {
  grepl(paste0("\\.\\Q", suffix, "\\E$"), x)
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
left_anti_join_types <- c(
  "left anti join",
  "natural left anti join"
)

# TBD: implement cross joins after implemented in dplyr
# (see https://github.com/tidyverse/dplyr/issues/4206)
