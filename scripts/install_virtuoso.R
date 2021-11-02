# setup virtuoso for local rdf file access
# J. Allen Baron
# 2021-10-28

# Procedure
#   1. Load virtuoso R package (install if necessary)
#   2. Install virtuoso
if (!"virtuoso" %in% installed.packages()[, 1]) {
    install.packages("virtuoso")
}

library(virtuoso)
virtuoso::vos_install()
