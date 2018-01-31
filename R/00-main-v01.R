
# Need to instatiate the package options?
# devtools::install_github("aelhabr/teproj")
library("teproj")

out <-
  teproj::render_proj_io(
    # render = FALSE,
    rgx_input = rgx_input,
    dir_input = dir_input,
    dir_output = dir_output
    )
out

