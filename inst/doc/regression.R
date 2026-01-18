## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup-discord-data, include = FALSE, cache = FALSE, eval=FALSE-----------
# NA

## ----discord-setup, message = FALSE-------------------------------------------
# For easy data manipulation
library(dplyr)
# For kinship linkages
library(NlsyLinks)
# For discordant-kinship regression
library(discord)
# To clean data frame names
library(janitor)
# tidy up output
library(broom)
# pipe
library(magrittr)

data(data_flu_ses)

## ----preview-pre-processed-data, echo = FALSE, eval = knitr::is_html_output(),error=FALSE----
data_flu_ses %>%
  select(CASEID, RACE, SEX, FLU_total, S00_H40) %>%
  filter(!is.na(S00_H40)) %>%
  slice(1:500) %>%
  slice_sample(n = 6) %>%
  kableExtra::kbl("html", align = "c") %>%
  kableExtra::kable_styling(full_width = FALSE) # %>%
#    kableExtra::column_spec(1:11, extra_css = "text-align: center;")

## ----preview-pre-processed-data-latex, echo = FALSE, eval = knitr::is_latex_output()----
# data_flu_ses %>%
#   select(CASEID, RACE, SEX, FLU_total, S00_H40) %>%
#   filter(!is.na(S00_H40)) %>%
#   slice(1:500) %>%
#   slice_sample(n = 6) %>%
#   kableExtra::kbl(format = "latex", booktabs = TRUE, align = "c") %>%
#   kableExtra::kable_styling(latex_options = c("striped", "hold_position"), position = "center")

## ----set-kinship-link-vars----------------------------------------------------
# Get kinship links for individuals with the following variables:
link_vars <- c(
  "FLU_total", "FLU_2008", "FLU_2010",
  "FLU_2012", "FLU_2014", "FLU_2016",
  "S00_H40", "RACE", "SEX"
)

## ----create-linked-data-------------------------------------------------------
# Specify NLSY database and kin relatedness
link_pairs <- Links79PairExpanded %>%
  filter(RelationshipPath == "Gen1Housemates" & RFull == 0.5)

df_link <- CreatePairLinksSingleEntered(
  outcomeDataset = data_flu_ses,
  linksPairDataset = link_pairs,
  outcomeNames = link_vars
)

## ----preview-linked-dat, echo = FALSE, eval = knitr::is_html_output(),error=FALSE----
df_link %>%
  select(
    ExtendedID,
    SubjectTag_S1, SubjectTag_S2,
    FLU_total_S1, FLU_total_S2,
    S00_H40_S1, S00_H40_S2
  ) %>%
  filter(!is.na(S00_H40_S1) & !is.na(S00_H40_S2)) %>%
  slice(1:500) %>%
  slice_sample(n = 6) %>%
  kableExtra::kbl("html", align = "c") %>%
  kableExtra::kable_styling(full_width = FALSE) # %>%
#  kableExtra::column_spec(1:11, extra_css = "text-align: center;")

## ----preview-linked-dat-latex, echo = FALSE, eval = knitr::is_latex_output()----
# df_link %>%
#   select(
#     ExtendedID,
#     SubjectTag_S1, SubjectTag_S2,
#     FLU_total_S1, FLU_total_S2,
#     S00_H40_S1, S00_H40_S2
#   ) %>%
#   filter(!is.na(S00_H40_S1) & !is.na(S00_H40_S2)) %>%
#   slice(1:500) %>%
#   slice_sample(n = 6) %>%
#   kableExtra::kbl(format = "latex", booktabs = TRUE, align = "c") %>%
#   kableExtra::kable_styling(latex_options = c("striped", "hold_position", "scale_down"))

## ----consistent-kin-data------------------------------------------------------
# Take the linked data, group by the sibling pairs and
# count the number of responses for flu each year. If there is an NA,
# then data is missing for one of the years, and we omit it.
consistent_kin <- df_link %>%
  group_by(SubjectTag_S1, SubjectTag_S2) %>%
  count(
    FLU_2008_S1, FLU_2010_S1,
    FLU_2012_S1, FLU_2014_S1,
    FLU_2016_S1, FLU_2008_S2,
    FLU_2010_S2, FLU_2012_S2,
    FLU_2014_S2, FLU_2016_S2
  ) %>%
  na.omit()

# Create the flu_modeling_data object with only consistent responders.
# Clean the column names with the {janitor} package.
flu_modeling_data <- semi_join(df_link,
  consistent_kin,
  by = c(
    "SubjectTag_S1",
    "SubjectTag_S2"
  )
) %>%
  clean_names()

## ----finalize-flu-modeling-data, cache = FALSE--------------------------------
flu_modeling_data <- flu_modeling_data %>%
  group_by(extended_id) %>%
  slice_sample() %>%
  ungroup()

## ----preview-flu-modeling-data, echo = FALSE, eval = knitr::is_html_output()----
flu_modeling_data %>%
  select(contains(c("extended_id", "subject_tag", "flu_total", "race", "sex", "s00_h40"))) %>%
  rename(
    ses_age_40_s1 = s00_h40_s1,
    ses_age_40_s2 = s00_h40_s2
  ) %>%
  slice(1:10) %>%
  kableExtra::kbl("html", align = "c") %>%
  kableExtra::kable_styling() %>%
  kableExtra::column_spec(1:11, extra_css = "text-align: center;")

## ----preview-flu-modeling-data-latex, eval = knitr::is_latex_output(), echo = FALSE----
# flu_modeling_data %>%
#   select(contains(c("extended_id", "subject_tag", "flu_total", "race", "sex", "s00_h40"))) %>%
#   rename(
#     ses_age_40_s1 = s00_h40_s1,
#     ses_age_40_s2 = s00_h40_s2
#   ) %>%
#   slice(1:10) %>%
#   kableExtra::kbl(format = "latex", booktabs = TRUE, align = "c") %>%
#   kableExtra::kable_styling(latex_options = c("striped", "hold_position", "scale_down"))

## ----run-regression, cache = FALSE--------------------------------------------
# Setting a seed for reproducibility
set.seed(18)
flu_model_output <- discord_regression(
  data = flu_modeling_data,
  outcome = "flu_total",
  predictors = "s00_h40",
  id = "extended_id",
  sex = "sex",
  race = "race",
  pair_identifiers = c("_s1", "_s2")
)

## ----broom-reg, echo = FALSE--------------------------------------------------
flu_model_output %<>%
  broom::tidy()

## ----summarize-model-html, echo = FALSE, eval = knitr::is_html_output(), error=FALSE----
flu_model_output %>%
  mutate(
    p.value = scales::pvalue(p.value, add_p = TRUE),
    across(.cols = where(is.numeric), ~ round(.x, 3))
  ) %>%
  rename(
    "Standard Error" = std.error,
    "T Statistic" = statistic
  ) %>%
  rename_with(~ snakecase::to_title_case(.x)) %>%
  kableExtra::kbl("html", align = "c") %>%
  kableExtra::kable_styling() %>%
  kableExtra::column_spec(1:5, extra_css = "text-align: center;")

## ----summarize-model-latex, echo = FALSE, eval = knitr::is_latex_output(), error=FALSE----
# flu_model_output %>%
#   mutate(
#     p.value = scales::pvalue(p.value, add_p = TRUE),
#     across(.cols = where(is.numeric), ~ round(.x, 3))
#   ) %>%
#   rename(
#     "Standard Error" = std.error,
#     "T Statistic" = statistic
#   ) %>%
#   rename_with(~ snakecase::to_title_case(.x)) %>%
#   kableExtra::kbl(format = "latex", booktabs = TRUE, align = "c") %>%
#   kableExtra::kable_styling(latex_options = c("striped", "hold_position"), position = "center")

## ----session-info, echo = FALSE-----------------------------------------------
sessioninfo::session_info()

