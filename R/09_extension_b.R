# Extension B: for whom does the effect hold?
# the paper's Table V splits by literacy in 2000 (poor), but that is measured
# after six years with the mayor. here I split on things fixed before 1994:
#   poor  = below median predicted schooling of young women (lasso in py/ext_b_lasso.py)
#   pious = above median Islamic vote share in 1994 (paper's Panel C)
# medians are taken inside the women's window. the splits were decided before looking at the results
# note: the SEs treat the groups as given, even though the lasso index is estimated

source("R/00_setup.R")
dat <- readRDS("data/derived/analysis.rds") |> mutate(muni_id = row_number())

# the lasso is done in python. it reads this csv (outcome, treatment and the pre-1994 predictors)
# and writes back the predicted schooling for each muni_id
dat |>
  select(muni_id, hischshr1520f, i94, lpop1994, merkezi, merkezp, subbuyuk, buyuk,
         hischshr3164f, hischshr3164m, hischshr65f, hischshr65m) |>
  write_csv("data/derived/ext_b_lasso_input.csv")
if (system("python3 py/ext_b_lasso.py") != 0) stop("py/ext_b_lasso.py failed")

pred <- read_csv("data/derived/ext_b_predicted_schooling.csv", show_col_types = FALSE)
dat <- left_join(dat, pred, by = "muni_id")
stopifnot(!anyNA(dat$pred_schooling))

window <- filter(dat, abs(iwm94) < h_women)
dat$poor <- dat$pred_schooling < median(window$pred_schooling)
dat$pious <- dat$vshr_islam1994 > median(window$vshr_islam1994)


# RD effect in each group + the difference. the first regression gives the
# effect in each group, the second one the same model written with the
# difference (gyes:i94) so I get its standard error and p-value
rd_by_group <- function(y, h, group) {
  d <- rd_window(dat, y, h)
  d$g <- ifelse(d[[group]], "yes", "no")
  ctrl <- paste("+", paste(c(controls, "prov81"), collapse = " + "))
  each <- feols(as.formula(paste(y, "~ g + g:(i94 + xs + xs:i94)", ctrl)),
                data = d, cluster = "pcode", notes = FALSE, warn = FALSE)
  gap <- feols(as.formula(paste(y, "~ g * (i94 + xs + xs:i94)", ctrl)),
               data = d, cluster = "pcode", notes = FALSE, warn = FALSE)
  list(each = each, gap = coeftable(gap)["gyes:i94", ],
       treated = paste(sum(d$i94[d$g == "yes"]), "/", sum(d$i94[d$g == "no"])))
}

splits <- list(rd_by_group("hischshr1520f", h_women, "poor"),
               rd_by_group("hischshr1520f", h_women, "pious"),
               rd_by_group("hischshr1520m", h_men, "poor"),
               rd_by_group("hischshr1520m", h_men, "pious"))

# check: predicted schooling uses only pre-1994 data, so it should not jump at the cutoff
balance <- rd_reg(dat, "pred_schooling", h_women)

fits <- c(lapply(splits, function(x) x$each), list(balance))
lines <- list(
  "Split" = c("Poor", "Pious", "Poor", "Pious", ""),
  "Difference, group minus rest" = c(sapply(splits, function(x) paste0(fmt(x$gap[1]), " (", fmt(x$gap[2]), ")")), ""),
  "p-value of the difference" = c(sapply(splits, function(x) fmt(x$gap[4])), ""),
  "Treated municipalities, group / rest" = c(sapply(splits, function(x) x$treated), ""),
  "Bandwidth" = fmt(c(h_women, h_women, h_men, h_men, h_women)))
labels <- c(gyes = "Group", gno = "Rest", pred_schooling = "Predicted schooling, women 15-20")

rows <- c("%^gyes:i94$", "%^gno:i94$", "%^i94$")
etable(fits, keep = rows, order = rows, group = ctrl_line, extralines = lines, dict = labels)
etable(fits, keep = rows, order = rows, group = ctrl_line, extralines = lines, dict = labels,
       file = "output/tables/ext_b.tex", replace = TRUE)
