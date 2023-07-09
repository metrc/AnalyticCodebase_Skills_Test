
# TODO make sure to set your working directory to this files location

Sys.setenv(GITHUB_PAT='TODO ADD GITHUB PAT')
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)

devtools::install_github("metrc/AnalyticSystem@*release", upgrade="never", dependencies="Depends")

devtools::build(dirname(getwd()))
devtools::install(dirname(getwd()), upgrade="never", dependencies="Depends")

library(AnalyticCodebase)
# Set up for Weight Bearing

set_data_custom(read_feather("weight bearing-cached_data/weight bearing_data.feather"), "weight bearing", "weight bearing-cached_data", server=TRUE)

analytic <- build_analytic_dataset()
cb <- get_codebook()
