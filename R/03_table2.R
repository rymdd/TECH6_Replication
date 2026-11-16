# Table II: effect of an Islamic mayor on high school education in 2000

source("R/00_setup.R")
dat <- readRDS("data/derived/analysis.rds")

# columns of the table. col 1-2 global OLS, 3-8 RD with different polynomials and bandwidths, col 9 is enrollment instead of high school
poly <- c(0, 0, 1, 1, 1, 1, 2, 3, 1)
bw_mult <- c(Inf, Inf, 1, 1, 0.5, 2, 1, 1, 1)
covs <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
# col 9 has its own bandwidths, as printed in the paper
h_enrol_women <- 0.205
h_enrol_men <- 0.230

# Panel C: test of equal effect for women and men. stack the two samples and interact everything with a men dummy, then men:i94 is the difference
equal_pval <- function(yw, ym, hw, hm, p, covs) {
  women <- rd_window(dat, yw, hw) |> mutate(men = 0, y = .data[[yw]])
  men <- rd_window(dat, ym, hm) |> mutate(men = 1, y = .data[[ym]])
  fit <- feols(as.formula(paste0("y ~ men * (", rd_rhs(p, covs), ")")),
               data = bind_rows(women, men), cluster = "pcode", notes = FALSE, warn = FALSE)
  coeftable(fit)["men:i94", "Pr(>|t|)"]
}

women <- list()
men <- list()
h_w <- numeric(9)
h_m <- numeric(9)
p_equal <- numeric(9)
for (j in 1:9) {
  if (j == 9) {
    yw <- "studshr1530f"
    ym <- "studshr1530m"
    h_w[j] <- h_enrol_women
    h_m[j] <- h_enrol_men
  } else {
    yw <- "hischshr1520f"
    ym <- "hischshr1520m"
    h_w[j] <- h_women * bw_mult[j]
    h_m[j] <- h_men * bw_mult[j]
  }
  women[[j]] <- rd_reg(dat, yw, h_w[j], poly[j], covs[j])
  men[[j]] <- rd_reg(dat, ym, h_m[j], poly[j], covs[j])
  p_equal[j] <- equal_pval(yw, ym, h_w[j], h_m[j], poly[j], covs[j])
}

polynomial <- c("None", "Linear", "Quadratic", "Cubic")[poly + 1]
bandwidth <- function(h) ifelse(is.finite(h), fmt(h), "All")
lines_women <- list("Polynomial in the margin" = polynomial, "Bandwidth" = bandwidth(h_w))
lines_men <- list("Polynomial in the margin" = polynomial, "Bandwidth" = bandwidth(h_m),
                  "p-value, same effect as for women" = fmt(p_equal))

etable(women, keep = "%^i94$", group = ctrl_line, extralines = lines_women)
etable(men, keep = "%^i94$", group = ctrl_line, extralines = lines_men)

etable(women, keep = "%^i94$", group = ctrl_line, extralines = lines_women,
       file = "output/tables/table2_women.tex", replace = TRUE)
etable(men, keep = "%^i94$", group = ctrl_line, extralines = lines_men,
       file = "output/tables/table2_men.tex", replace = TRUE)
