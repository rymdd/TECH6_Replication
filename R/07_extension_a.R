# Extension A: was the discontinuity there already before 1994?
# 1) placebo: the paper's RD on the 1990 census outcome, + minimum detectable effect
# 2) RD on the high school share of older cohorts in 2000. people 31+ were done with school before the 1994 election
# 3) checks: placebo at h/2 and 2h, and selection into the 1990 sample
# all with the paper's specification for women (linear, controls, h = 0.240)

source("R/00_setup.R")
dat <- readRDS("data/derived/analysis.rds")

# only 1609 municipalities have the 1990 census outcomes
d90 <- dat |> filter(!is.na(c90hischshr1520f))
dat$in_1990_sample <- as.numeric(!is.na(dat$c90hischshr1520f))
cat("1990 sample:", nrow(d90), "municipalities,", sum(d90$i94), "with an Islamic mayor\n")

fits <- list(rd_reg(d90, "hischshr1520f", h_women, 1, TRUE),
             rd_reg(d90, "c90hischshr1520f", h_women, 1, TRUE),
             rd_reg(d90, "c90hischshr1520f", h_women / 2, 1, TRUE),
             rd_reg(d90, "c90hischshr1520f", 2 * h_women, 1, TRUE),
             rd_reg(dat, "in_1990_sample", h_women),
             rd_reg(dat, "hischshr2125f", h_women, 1, TRUE),
             rd_reg(dat, "hischshr2630f", h_women, 1, TRUE),
             rd_reg(dat, "hischshr3164f", h_women, 1, TRUE))

# minimum detectable effect, 5% level and 80% power (from the experiments lecture)
mde <- sapply(fits, function(f) (qnorm(0.975) + qnorm(0.80)) * se(f)["i94"])
lines <- list("Sample" = c(rep("1990 sample", 4), rep("All", 4)),
              "Bandwidth" = fmt(c(1, 1, 0.5, 2, 1, 1, 1, 1) * h_women),
              "Minimum detectable effect" = fmt(mde))
# short column headers: the table caption says the outcome is the share of women with high school
labels <- c(hischshr1520f = "15-20 in 2000",
            c90hischshr1520f = "15-20 in 1990",
            in_1990_sample = "In 1990 sample",
            hischshr2125f = "21-25 in 2000",
            hischshr2630f = "26-30 in 2000",
            hischshr3164f = "31-64 in 2000")

etable(fits, keep = "%^i94$", group = ctrl_line, extralines = lines, dict = labels)
etable(fits, keep = "%^i94$", group = ctrl_line, extralines = lines, dict = labels,
       file = "output/tables/ext_a.tex", replace = TRUE)


# figure: RD estimate for each cohort in 2000, women and men
cohorts <- c("15-20", "21-25", "26-30", "31-64", "65+")
stems <- c("hischshr1520", "hischshr2125", "hischshr2630", "hischshr3164", "hischshr65")

coh <- tibble()
for (sex in c("women", "men")) {
  suffix <- ifelse(sex == "women", "f", "m")
  h <- ifelse(sex == "women", h_women, h_men)
  for (i in 1:5) {
    ct <- coeftable(rd_reg(dat, paste0(stems[i], suffix), h, 1, TRUE))["i94", ]
    coh <- bind_rows(coh, tibble(sex = sex, cohort = cohorts[i], est = ct[1], se = ct[2]))
  }
}
print(coh)

fig_coh <- coh |>
  mutate(cohort = factor(cohort, levels = cohorts),
         sex = factor(sex, levels = c("women", "men"), labels = c("Women", "Men"))) |>
  ggplot(aes(cohort, est, shape = sex, colour = sex)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                  size = 0.3, position = position_dodge(width = 0.45)) +
  scale_colour_manual(values = c(Women = "black", Men = "grey55")) +
  labs(x = "Age in 2000", y = "RD estimate (0.01 = 1 percentage point)", shape = NULL, colour = NULL) +
  theme_paper() +
  theme(legend.position = "bottom")
save_fig(fig_coh, "ext_a_cohorts", 4.5, 3.2)
