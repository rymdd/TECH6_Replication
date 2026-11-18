library(tidyverse)
library(haven)
library(fixest)
library(patchwork)
library(knitr)

options(scipen = 999)

dir.create("data/derived", showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# the 11 controls from the paper. need shhs and all four municipality types to get the published numbers
controls <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
              "ageshr60", "sexr", "shhs", "merkezi", "merkezp", "subbuyuk", "buyuk")

# Imbens-Kalyanaraman bandwidths as printed in the paper's Table II (three decimals)
h_women <- 0.240
h_men <- 0.323


rd_window <- function(data, y, h) {
  data |>
    filter(!is.na(.data[[y]]), abs(iwm94) < h) |>
    # margin divided by h, otherwise the high order terms get dropped as collinear in the small windows (estimate of i94 is the same)
    mutate(prov81 = droplevels(prov81), xs = iwm94 / ifelse(is.finite(h), h, 1))
}

rd_rhs <- function(p, covs) {
  rhs <- "i94"
  for (k in seq_len(p)) {
    rhs <- paste0(rhs, " + I(xs^", k, ") + I(xs^", k, "):i94")
  }
  # province dummies as a factor and not as fixed effects (| prov81), so the clustered SEs get the same df correction as in Stata
  if (covs) rhs <- paste(rhs, "+", paste(controls, collapse = " + "), "+ prov81")
  rhs
}

# RD regression from the paper: OLS inside |iwm94| < h, polynomial of order p on each side of the cutoff, optional controls + province dummies.
# h = Inf and p = 0 gives the global OLS
rd_reg <- function(data, y, h = Inf, p = 1, covs = FALSE) {
  d <- rd_window(data, y, h)
  feols(as.formula(paste(y, "~", rd_rhs(p, covs))), data = d,
        cluster = "pcode", notes = FALSE, warn = FALSE)
}


# labels and defaults for every etable() in the project
setFixest_dict(c(
  i94 = "Islamic mayor in 1994",
  hischshr1520f = "High school, women 15-20",
  hischshr1520m = "High school, men 15-20",
  studshr1530f = "Enrolled, women 15-30",
  studshr1530m = "Enrolled, men 15-30",
  pcode = "province"))
# wide tables are shrunk to the text width, and "No" is written out in the yes/no lines
setFixest_etable(digits = "r3", digits.stats = "r3", fitstat = ~ n + r2 + my,
                 signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
                 style.tex = style.tex(yesNo = c("Yes", "No"), adjustbox = "max width=\\textwidth"))
# the controls and province dummies are shown as one yes/no line, the slopes are not shown
ctrl_line <- list("Controls and province dummies" = c(controls, "prov81"))


# formatting for the Table III grid and the extra lines of the etables
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
stars <- function(p) {
  case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}

save_fig <- function(plot, name, width, height) {
  ggsave(paste0("output/figures/", name, ".pdf"), plot, width = width, height = height)
  ggsave(paste0("output/figures/", name, ".png"), plot, width = width, height = height, dpi = 200)
}

theme_paper <- function() {
  theme_classic(base_size = 9) +
    theme(plot.title = element_text(size = 9, hjust = 0.5),
          axis.title = element_text(size = 8),
          strip.background = element_blank(),
          legend.position = "none")
}
