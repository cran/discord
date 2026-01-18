## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

# if local
# source("../data-raw/pretty_regression_tables.R")

## ----discord-setup, message = FALSE-------------------------------------------
# For easy data manipulation
library(dplyr)
library(tidyr)

# For kinship linkages
library(NlsyLinks)
# For discordant-kinship regression
library(discord)
# To clean data frames
library(janitor)
library(broom)
# For pipe
library(magrittr)
# For pedigree data manipulation
library(BGmisc)
# For pedigree plotting
library(ggpedigree)
library(ggplot2)

## -----------------------------------------------------------------------------
df_wide <- data.frame(
  pid = 1:5,
  id_s1 = c(101, 201, 301, 401, 501),
  id_s2 = c(102, 202, 302, 402, 502),
  age_s1 = c(30, 27, 40, 36, 30),
  age_s2 = c(28, 25, 38, 35, 28),
  height_s1 = c(175, 160, 180, 170, 165),
  height_s2 = c(170, 162, 178, 172, 168),
  weight_s1 = c(70, 60, 80, 75, 65),
  weight_s2 = c(68, 62, 78, 74, 66)
)

df_wide %>%
  slice(1:5) %>%
  knitr::kable()

## -----------------------------------------------------------------------------
df_long <- df_wide %>%
  tidyr::pivot_longer(
    cols = -pid, # keep the dyad identifier intact
    names_to = c(".value", "sibling"), # split base names and the sibling marker
    names_sep = "_s" # original suffix delimiter in column names
  )

df_long %>%
  slice(1:10) %>%
  knitr::kable()

## -----------------------------------------------------------------------------
df_long2wide <- df_long %>%
  tidyr::pivot_wider(
    names_from = sibling, # the column that indicates the sibling number
    values_from = c(id, age, height, weight), # variables to spread into paired columns
    names_sep = "_s" # ensures id_s1, id_s2,etc
  )

df_long2wide %>%
  slice(1:5) %>%
  knitr::kable()

## -----------------------------------------------------------------------------
library(NlsyLinks)
data(Links79PairExpanded)

Links79PairExpanded %>%
  arrange(ExtendedID) %>%
  filter(RelationshipPath == "Gen1Housemates" & RFull == 0.5) %>%
  select(
    ExtendedID,
    SubjectTag_S1, SubjectTag_S2,
    RelationshipPath, RFull, IsMz,
    EverSharedHouse
  ) %>% # used to make the table
  slice_head(n = 5) %>%
  knitr::kable()

## ----echo=TRUE, fig.height=3, fig.width=5, message=FALSE, warning=FALSE-------
data(potter)
ggpedigree(potter, config = list(
  label_method = "geom_text",
  label_nudge_y = .25,
  focal_fill_personID = 7,
  focal_fill_include = TRUE,
  focal_fill_force_zero = TRUE,
  focal_fill_na_value = "grey50",
  focal_fill_low_color = "darkred",
  focal_fill_high_color = "gold",
  focal_fill_mid_color = "orange",
  focal_fill_scale_midpoint = .65,
  focal_fill_component = "additive",
  focal_fill_method = "steps", #
  # focal_fill_method = "viridis_c",
  focal_fill_use_log = FALSE,
  focal_fill_n_breaks = 10,
  sex_color_include = F,
  focal_fill_legend_title = "Genetic Relatives \nof Harry Potter"
)) +
  labs(title = "Potter Pedigree Plot") +
  theme(legend.position = "right")

## -----------------------------------------------------------------------------
data(potter)
df_ped <- potter %>%
  as.data.frame() %>%
  select(
    personID, sex, famID, momID, dadID, spouseID,
    twinID, zygosity
  ) %>%
  mutate(x_var = round(rnorm(nrow(.), mean = 0, sd = 1), digits = 2))

df_ped %>%
  slice(1:5) %>%
  knitr::kable(digits = 2)

## -----------------------------------------------------------------------------
add <- ped2add(df_ped)
cn <- ped2cn(df_ped)

## -----------------------------------------------------------------------------
df_links <- com2links(
  writetodisk = FALSE,
  ad_ped_matrix = add,
  cn_ped_matrix = cn,
  drop_upper_triangular = TRUE
) %>%
  filter(ID1 != ID2)

df_links %>%
  slice(1:5) %>%
  knitr::kable(digits = 3)

## -----------------------------------------------------------------------------
df_links %>%
  group_by(addRel, cnuRel) %>%
  tally() %>%
  knitr::kable()

## -----------------------------------------------------------------------------
df_cousin <- df_links %>%
  filter(addRel == .125) %>% # only cousins %>%
  filter(cnuRel == 0) # only kin raised in separate homes

## -----------------------------------------------------------------------------
set.seed(2024)

df_synthetic <- discord::kinsim(
  mu_all = c(1, 1), # means
  cov_a = .5,
  cov_c = .1, #
  cov_e = .3,
  c_vector = rep(df_cousin$cnuRel, 3),
  r_vector = rep(df_cousin$addRel, 3)
)

## -----------------------------------------------------------------------------
df_synthetic <- df_synthetic %>%
  select(
    pid = id,
    r,
    weight_s1 = y1_1,
    weight_s2 = y1_2,
    height_s1 = y2_1,
    height_s2 = y2_2
  ) %>%
  mutate( # simulates age such that the 2nd sibling is between 1 and 5 years younger.
    age_s1 = round(rnorm(nrow(.), mean = 30, sd = 5), digits = 0),
    age_s2 = age_s1 - sample(1:5, nrow(.), replace = TRUE)
  )

df_synthetic %>%
  slice(1:5) %>%
  knitr::kable(digits = 2)

## -----------------------------------------------------------------------------
# CHOOSE ONE based on your path
# source_wide <- df_wide
# source_wide <- df_long2wide
source_wide <- df_synthetic # if you followed the pedigree path

## -----------------------------------------------------------------------------
df_discord_weight <- discord::discord_data(
  data = source_wide,
  outcome = "weight",
  predictors = c("height", "age"),
  demographics = "none",
  pair_identifiers = c("_s1", "_s2"),
  id = "pid" # or "famID"
)

df_discord_weight %>%
  slice(1:5) %>%
  knitr::kable(digits = 2, caption = "Transformed data ready for discordant-kinship regression")

## ----examine-transformation---------------------------------------------------
# Show original data for first 3 pairs
source_wide %>%
  slice(1:3) %>%
  select(pid, weight_s1, weight_s2, height_s1, height_s2) %>%
  knitr::kable(
    digits = 2,
    caption = "Original data: siblings not yet ordered by outcome"
  )

df_discord_weight %>%
  select(
    id,
    weight_1, weight_2, weight_mean, weight_diff,
    height_1, height_2, height_mean, height_diff
  ) %>%
  slice(1:3) %>%
  knitr::kable(
    digits = 2,
    caption = "After discord_data(): siblings ordered so weight_1 >= weight_2"
  )

## ----select-for-ols-----------------------------------------------------------
df_for_ols <- df_synthetic %>%
  dplyr::select(
    id = pid,
    weight = weight_s1,
    height = height_s1,
    age = age_s1
  )


df_discord_age <- discord::discord_data(
  data = source_wide,
  outcome = "age",
  predictors = c("height", "weight"),
  demographics = "none",
  pair_identifiers = c("_s1", "_s2"),
  id = "pid" # or "famID" if you followed the pedigree path
)

## ----ols-sample-output--------------------------------------------------------
df_discord_age %>%
  select(
    id, age_1, age_2,
    age_mean, age_diff, weight_1, height_1
  ) %>%
  slice(1:5) %>%
  knitr::kable(caption = "One sibling per pair for OLS regression, selecting by age", digits = 2)

df_for_ols %>%
  select(id, age, weight, height) %>%
  slice(1:5) %>%
  knitr::kable(caption = "One sibling per pair for OLS regression using original wide data", digits = 2)

## ----ols-regression-----------------------------------------------------------
ols_model <- lm(weight ~ height + age, data = df_for_ols)

## ----ols-output, message=FALSE, warning=FALSE, eval = knitr::is_html_output(),results='asis'----
stargazer::stargazer(ols_model,
  type = "html", ci = TRUE,
  digits = 3, single.row = TRUE, title = "Standard OLS Regression Results"
)

## ----ols-output-latex, message=FALSE, warning=FALSE,  eval = knitr::is_latex_output(), error=FALSE----
# stargazer::stargazer(ols_model,
#   type = "latex", ci = TRUE,
#   digits = 3, single.row = TRUE, title = "Standard OLS Regression Results"
# )

## ----between-family-----------------------------------------------------------
between_model <- lm(
  weight_mean ~ height_mean + age_mean,
  data = df_discord_weight
)

## ----between-output, results='asis', message=FALSE, warning=FALSE, eval = knitr::is_html_output()----
stargazer::stargazer(between_model,
  type = "html", ci = TRUE,
  digits = 3, single.row = TRUE, title = "Between-Family Regression Results"
)

## ----between-output-latex, echo = FALSE, eval = knitr::is_latex_output(), error=FALSE----
# stargazer::stargazer(between_model,
#   type = "latex", ci = TRUE,
#   digits = 3, single.row = TRUE, title = "Between-Family Regression Results"
# )

## ----discord-manual-----------------------------------------------------------
discord_model_manual <- lm(
  weight_diff ~ weight_mean + height_mean + height_diff + age_mean + age_diff,
  data = df_discord_weight
)

tidy(discord_model_manual, conf.int = TRUE) %>%
  knitr::kable(digits = 3, caption = "Discordant Regression (Manual)")

## ----discord-manual-output, message=FALSE, warning=FALSE, eval = knitr::is_html_output(),results='asis'----
stargazer::stargazer(between_model, discord_model_manual,
  type = "html", ci = TRUE,
  digits = 3, single.row = TRUE, title = "Between-Family and Discordant Regression Results"
)

## ----discord-manual-output-latex, echo = FALSE, eval = knitr::is_latex_output(), error=FALSE----
# stargazer::stargazer(between_model, discord_model_manual,
#   type = "latex", ci = TRUE,
#   digits = 3, single.row = TRUE, title = "Between-Family and Discordant Regression Results"
# )

## ----discord-function---------------------------------------------------------
discord_model <- discord_regression(
  data = source_wide,
  outcome = "weight",
  predictors = c("height", "age"),
  demographics = "none",
  sex = NULL,
  race = NULL,
  pair_identifiers = c("_s1", "_s2"),
  id = "pid"
)

tidy(discord_model, conf.int = TRUE) %>%
  knitr::kable(digits = 3, caption = "Discordant Regression Results")

glance(discord_model) %>%
  select(r.squared, adj.r.squared, sigma, p.value, nobs) %>%
  knitr::kable(digits = 3)

## ----discord-compare, message=FALSE, warning=FALSE, eval = knitr::is_html_output(),results='asis'----
stargazer::stargazer(discord_model,
  discord_model_manual,
  type = "html", ci = TRUE,
  digits = 3, single.row = TRUE, title = "Discordant Regression Results Comparison"
)

## ----discord-compare-latex, echo = FALSE, eval = knitr::is_latex_output(), error=FALSE----
# stargazer::stargazer(discord_model,
#   discord_model_manual,
#   type = "latex",
#   digits = 3, single.row = TRUE, title = "Discordant Regression Results Comparison"
# )

## ----all-models-comparison, message=FALSE, warning=FALSE, eval = knitr::is_html_output(),results='asis'----
stargazer::stargazer(
  ols_model,
  between_model,
  discord_model_manual,
  type = "html", ci = TRUE,
  digits = 3,
  single.row = TRUE,
  title = "Comparison of OLS, Between-Family, and Discordant Regression Models",
  column.labels = c("Standard OLS", "Between-Family", "Discordant"),
  model.names = FALSE
)

## ----all-models-comparison-latex, echo = FALSE, eval = knitr::is_latex_output(), error=FALSE----
# stargazer::stargazer(
#   ols_model,
#   between_model,
#   discord_model_manual,
#   type = "latex", ci = TRUE,
#   digits = 3,
#   single.row = TRUE,
#   title = "Comparison of OLS, Between-Family, and Discordant Regression Models",
#   column.labels = c("Standard OLS", "Between-Family", "Discordant"),
#   model.names = FALSE
# )

## -----------------------------------------------------------------------------
sessioninfo::session_info()

