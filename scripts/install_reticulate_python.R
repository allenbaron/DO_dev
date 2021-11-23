# setup python virtualenv

library(reticulate)
library(tidyverse)

# specify setup
py_v <- "3.9.9"
venv_path <- "./pyenv"
py_pkg <- c("datetime", "GitPython", "rdflib", "pandas", "numpy")

# unset RETICULATE_PYTHON variable if set
Sys.unsetenv("RETICULATE_PYTHON")

py <- reticulate::install_python(version = py_v, force = TRUE)

py_venv <- reticulate::virtualenv_create(
    envname = venv_path,
    python = py,
    version = py_v,
    packages = py_pkg
)

# set RETICULATE_PYTHON variable because
py_path <- reticulate::use_virtualenv("./pyenv", required = TRUE) # doesn't work

renviron <- try(readLines(".Renviron"), silent = TRUE)

if (class(renviron) == "try-error") {
    writeLines(
        text = paste0("RETICULATE_PYTHON = ", py_path),
        con = ".Renviron"
    )
} else {
    append(x = renviron, values = paste0("RETICULATE_PYTHON = ", py_path)) %>%
        writeLines(con = ".Renviron")
}
readRenviron(".Renviron")

# verify installation
reticulate::py_config()
