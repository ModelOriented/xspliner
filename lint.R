#!/usr/bin/env Rscript
library("optparse")

# This file lints the whole project and contains our linter configuration.
#
# To see errors in RStudio, load linters_config and run: lintr::lint("path_to_file.R", linters_config)
# This is useful in fixing errors.
# To omit files, add them to .lintignore file.

option_list <- list(
  make_option(
    c("-v", "--verbose"),
    action = "store_true",
    default = FALSE,
    help = "Print lints output [default %default]"
  ),
  make_option(
    c("--accepted-errors"),
    type = "integer",
    default = 0,
    help = "Number of accepted errors [default %default]",
    metavar = "number"
  ),
  make_option(
    c("--accepted-warnings"),
    type = "integer",
    default = 0,
    help = "Number of accepted warnings [default %default]",
    metavar = "number"
  ),
  make_option(
    c("--accepted-style-errors"),
    type = "integer",
    default = 0,
    help = "Number of accepted style errors [default %default]",
    metavar = "number"
  ),
  make_option(
    c("--type-errors"),
    type = "character",
    default = NULL,
    help = "Type of errors to print in verbose mode: [\"error\",\"warning\",\"style\"] ",
    metavar = "character"
  ),
  make_option(
    c("--path"),
    type = "character",
    default = ".",
    help = "Path to directory to lint: [default %default]",
    metavar = "character"
  )
)

opt <- parse_args(OptionParser(option_list = option_list))

linters_config <- lintr::with_defaults(
  # Purposefully disabled linters:
  object_usage_linter = NULL, # Conflicts with standard usage of dplyr.
  camel_case_linter = NULL, # Conflicts with Shiny functions which are camelCase
  # Linters temporarily disabled - we should enable them and fix errors:
  infix_spaces_linter = NULL,
  single_quotes_linter = NULL,
  spaces_left_parentheses_linter = NULL,
  # Enabled linters with custom arguments:
  open_curly_linter = lintr::open_curly_linter(allow_single_line = TRUE),
  closed_curly_linter = lintr::closed_curly_linter(allow_single_line = TRUE),
  line_length_linter = lintr::line_length_linter(140),
  object_length_linter = lintr::object_length_linter(40)
  # Enabled linters with defaults (we leave them here for future reference):
  # object_name_linter = NULL,
  # absolute_paths_linter = NULL,
  # assignment_linter = NULL,
  # commas_linter = NULL,
  # trailing_blank_lines_linter = NULL,
  # trailing_whitespace_linter = NULL,
  # spaces_inside_linter = NULL,
  # no_tab_linter = NULL,
)

extract_error_type <- function(lint_error) {
  lint_error$type
}

count_error_types <- function(lint_errors) {
  list(
    styles = length(lint_errors[lapply(lint_errors, extract_error_type) == "style"]),
    warnings = length(lint_errors[lapply(lint_errors, extract_error_type) == "warning"]),
    errors = length(lint_errors[lapply(lint_errors, extract_error_type) == "error"])
  )
}

display_results <- function(lint_errors, error_type_counts) {
  if (opt$verbose) {
    if (is.null(opt$"type-errors")) {
      print(lint_errors)
    } else {
      print(lint_errors[lapply(lint_errors, extract_error_type) == opt$"type-errors"])
    }
  }
  print(error_type_counts)
}

run_linter <- function(linters_config, ignore = TRUE, path = ".") {
  files_to_lint <- list.files(path = path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  if (ignore == TRUE) {
    read_safely <- purrr::safely(read.table)
    lintignored <- unlist(read_safely(file = ".lintignore", as.is = T))$result
    if (!is.null(lintignored)) {
      files_to_lint <- files_to_lint[!(files_to_lint %in% lintignored)]
    }
  }
  lint_function <- function(filename) {
    cat(paste0(Sys.time(), " ", filename, "\n"))
    lint_out <- lintr::lint(linters = linters_config, filename = filename)
    lint_error_count <- count_error_types(lint_out)
    if (lint_error_count$errors > opt$"accepted-errors" |
      lint_error_count$warnings > opt$"accepted-warnings" |
      lint_error_count$styles > opt$"accepted-style-errors") {
      display_results(lint_out, lint_error_count)
    }
    lint_out
  }
  unlist(lapply(files_to_lint, lint_function), recursive = F)
}

lint_errors <- run_linter(linters_config, ignore = FALSE, path = opt$path)
error_type_counts <- count_error_types(lint_errors)

cat("\nTotal errors:\n")
display_results(lint_errors, error_type_counts)

stopifnot(
  error_type_counts$errors <= opt$"accepted-errors" &
    error_type_counts$warnings <= opt$"accepted-warnings" &
    error_type_counts$styles <= opt$"accepted-style-errors"
)

