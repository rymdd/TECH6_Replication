# Table III: polynomial order 0-4 and different bandwidths, all with controls

source("R/00_setup.R")
dat <- readRDS("data/derived/analysis.rds")

bands <- c(1, 0.5, 0.25, 0.10, 0.05)
poly_names <- c("None", "Linear", "Quadratic", "Cubic", "Quartic")

# one row per sex and polynomial, one column per bandwidth, each cell is estimate (SE)
grid <- matrix("", nrow = 10, ncol = 5)
n_obs <- numeric(5)
for (s in 1:2) {
  y <- c("hischshr1520f", "hischshr1520m")[s]
  for (p in 0:4) {
    for (k in 1:5) {
      fit <- rd_reg(dat, y, bands[k], p, covs = TRUE)
      ct <- coeftable(fit)["i94", ]
      grid[5 * (s - 1) + p + 1, k] <- paste0(fmt(ct[1]), stars(ct[4]), " (", fmt(ct[2]), ")")
      n_obs[k] <- nobs(fit)
    }
  }
}

table3 <- data.frame(sex = c("Women", rep("", 4), "Men", rep("", 4)),
                     polynomial = rep(poly_names, 2),
                     grid)
names(table3) <- c("", "Polynomial", paste("h =", fmt(bands, 2)))
print(table3)
cat("Observations in each window:", n_obs, "\n")

kable(table3, format = "latex", booktabs = TRUE, align = "llccccc", linesep = c(rep("", 4), "\\addlinespace")) |>
  writeLines("output/tables/table3.tex")
