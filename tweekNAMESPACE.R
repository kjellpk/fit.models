wd <- commandArgs(trailingOnly = TRUE)[[1]]
cat("\nexport(plot.glmfm)\n", file = file.path(wd, "NAMESPACE"), append = TRUE)


