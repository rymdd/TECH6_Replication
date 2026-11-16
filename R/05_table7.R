# Table VII: outcomes in 2011. one table per outcome, women and men side by side

source("R/00_setup.R")
dat <- readRDS("data/derived/analysis_2011.rds")

# column headers: the outcome is named in the caption of each table
sex_labels <- c(ahischshr15_19f = "Women", ahischshr15_19m = "Men",
                ahischshr15_29f = "Women", ahischshr15_29m = "Men",
                nevm15_19f = "Women", nevm15_19m = "Men")

outcomes <- c("hs1519", "hs1529", "nevm1519")
y_women <- c("ahischshr15_19f", "ahischshr15_29f", "nevm15_19f")
y_men <- c("ahischshr15_19m", "ahischshr15_29m", "nevm15_19m")
# bandwidths as printed in the paper's Table VII
h_women_2011 <- c(0.239, 0.263, 0.280)
h_men_2011 <- c(0.199, 0.227, 0.271)

for (i in 1:3) {
  fits <- list()
  bandwidth <- character()
  for (s in 1:2) {
    y <- c(y_women[i], y_men[i])[s]
    h <- c(h_women_2011[i], h_men_2011[i])[s]
    fits <- c(fits, list(rd_reg(dat, y, Inf, 0, FALSE),
                         rd_reg(dat, y, Inf, 0, TRUE),
                         rd_reg(dat, y, h / 2, 1, TRUE),
                         rd_reg(dat, y, h, 1, TRUE),
                         rd_reg(dat, y, 2 * h, 1, TRUE)))
    bandwidth <- c(bandwidth, "All", "All", fmt(h / 2), fmt(h), fmt(2 * h))
  }
  lines <- list("Bandwidth" = bandwidth)
  print(etable(fits, keep = "%^i94$", group = ctrl_line, extralines = lines, dict = sex_labels))
  etable(fits, keep = "%^i94$", group = ctrl_line, extralines = lines, dict = sex_labels,
         file = paste0("output/tables/table7_", outcomes[i], ".tex"), replace = TRUE)
}
