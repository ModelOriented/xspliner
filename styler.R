#!/usr/bin/env Rscript
library(optparse)
library(styler)

option_list <- list(
  make_option(
    c("-s", "--scope"),
    type = "character",
    default = "tokens",
    help = "Scope of styler: [\"none\",\"spaces\",\"indention\",\"line_breaks\",\"tokens\"]\n\t[default \"%default\"]"
  ),
  make_option(
    c("--strict"),
    type = "logical",
    default = TRUE,
    help = "Should intentation be strict?\n\t[default \"%default\"]"
  ),
  make_option(
    c("-r", "--recursive"),
    type = "logical",
    default = TRUE,
    help = "Should be recursive?\n\t[default \"%default\"]"
  ),
  make_option(
    c("--path"),
    type = "character",
    default = ".",
    help = "Path to directory to style:\n\t[default \"%default\"]"
  ),
  make_option(
    c("-t", "--type"),
    type = "character",
    default = "r",
    help = "Type of file to style.\n\t[default \"%default\"]"
  )
)

opt <- parse_args(OptionParser(option_list = option_list))

# Styler Config
# Scope Levels(1-5) summary:
# “none”: Performs no transformation at all.
# “spaces”: Manipulates spacing between tokens on the same line. |
#            Strict = TRUE  -> spaces are set to either zero or one,
#                     FALSE -> set spacing to at least one around "="
# “indention”: In addition to “spaces”, this option also manipulates the indention level.
# “line_breaks”: In addition to “indention”, this option also manipulates line breaks.
# “tokens”: In addition to “line_breaks”, this option also manipulates tokens.

# Set required parameters here, higher numbers = more invasive styler:
style_dir(path = opt$path, scope = opt$scope, strict = opt$strict, filetype = opt$type, recursive = opt$recursive)

