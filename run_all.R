# Runs everything, from the project folder: Rscript run_all.R
# each script runs in its own R session so nothing carries over between them

scripts <- c("R/01_prepare_data.R",
             "R/02_table1.R",
             "R/03_table2.R",
             "R/04_table3.R",
             "R/05_table7.R",
             "R/06_figures.R",
             "R/07_extension_a.R",
             "R/08_extension_b.R")

for (s in scripts) {
  cat("\n----", s, "----\n")
  status <- system(paste("Rscript", s))
  if (status != 0) stop("Error in ", s)
}

# save package versions
py <- system("python3 -c \"import sys, pandas, sklearn; print('Python', sys.version.split()[0], 'pandas', pandas.__version__, 'scikit-learn', sklearn.__version__)\"",
             intern = TRUE)
source("R/00_setup.R")
writeLines(c(capture.output(sessionInfo()), "", py), "output/session_info.txt")
