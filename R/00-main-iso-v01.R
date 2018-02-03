
# Need to instatiate the package options?
# devtools::install_github("aelhabr/teproj")
require("teproj")

# source("R/config-iso-v01.R")

rgx_input <- "analyze"
dir_input <- "R"
dir_output <- "output"

out <-
  teproj::render_proj_io(
    # render = FALSE,
    rgx_input = rgx_input,
    dir_input = dir_input,
    dir_output = dir_output
    )
out

