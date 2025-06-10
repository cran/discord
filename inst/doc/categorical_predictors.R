## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, message = FALSE---------------------------------------------------
# Loading necessary packages and data
# For easy data manipulation
library(dplyr)
# For kinship linkages
library(NlsyLinks)
# For discordant-kinship regression
library(discord)
# pipe
library(magrittr)

# data
data(data_flu_ses)

## ----set-df-link--------------------------------------------------------------
# for reproducibility
set.seed(2023)

link_vars <- c("S00_H40", "RACE", "SEX")

# Specify NLSY database and kin relatedness

link_pairs <- Links79PairExpanded %>%
  filter(RelationshipPath == "Gen1Housemates" & RFull == 0.5)

## -----------------------------------------------------------------------------
df_link <- CreatePairLinksSingleEntered(
  outcomeDataset = data_flu_ses,
  linksPairDataset = link_pairs,
  outcomeNames = link_vars
)

## -----------------------------------------------------------------------------
# We removed the pair when the Dependent Variable is missing.
df_link <- df_link %>%
  filter(!is.na(S00_H40_S1) & !is.na(S00_H40_S2)) %>%
  mutate(
    SEX_S1 = case_when(
      SEX_S1 == 0 ~ "MALE",
      SEX_S1 == 1 ~ "FEMALE"
    ),
    SEX_S2 = case_when(
      SEX_S2 == 0 ~ "MALE",
      SEX_S2 == 1 ~ "FEMALE"
    ),
    RACE_S1 = case_when(
      RACE_S1 == 0 ~ "NONMINORITY",
      RACE_S1 == 1 ~ "MINORITY"
    ),
    RACE_S2 = case_when(
      RACE_S2 == 0 ~ "NONMINORITY",
      RACE_S2 == 1 ~ "MINORITY"
    )
  )

## -----------------------------------------------------------------------------
df_link <- df_link %>%
  dplyr::filter(RACE_S1 == RACE_S2)

## -----------------------------------------------------------------------------
df_link <- df_link %>%
  group_by(ExtendedID) %>%
  slice_sample() %>%
  ungroup()

## -----------------------------------------------------------------------------
cat_sex <- discord_data(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  demographics = "sex",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "both"
)

## ----sex, echo = FALSE, eval = knitr::is_html_output(),error=FALSE------------
cat_sex <- cat_sex %>%
  dplyr::mutate(SEX_binarymatch = case_when(
    SEX_binarymatch == 0 ~ "mixed-sex",
    SEX_binarymatch == 1 ~ "same-sex"
  ))

cat_sex %>%
  slice(1:500) %>%
  slice_sample(n = 6) %>%
  kableExtra::kbl("html", align = "c") %>%
  kableExtra::kable_styling(full_width = FALSE)

## ----preview-sex, echo = FALSE, eval = knitr::is_html_output()----------------
cat_sex %>%
  group_by(SEX_1, SEX_2) %>%
  summarize(n(), .groups = "drop") %>%
  kableExtra::kbl("html", align = "c", col.names = c("SEX_1", "SEX_2", "sample_size")) %>%
  kableExtra::kable_styling(full_width = FALSE)

## ----sex-compositions, echo = FALSE, eval = knitr::is_html_output()-----------
cat_sex %>%
  group_by(SEX_binarymatch, SEX_multimatch, SEX_1, SEX_2) %>%
  summarize(n(), .groups = "drop") %>%
  kableExtra::kbl("html", align = "c", col.names = c("binary", "multi", "SEX_1", "SEX_2", "sample_size")) %>%
  kableExtra::kable_styling(full_width = FALSE)

## -----------------------------------------------------------------------------
set.seed(2023) # for reproducibility

# Prepare data with race as demographic variable
cat_race <- discord_data(
  data = df_link,
  outcome = "S00_H40",
  predictors = NULL,
  sex = "SEX",
  race = "RACE",
  demographics = "race",
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "both"
)

## ----preview-race, echo = FALSE, eval = knitr::is_html_output()---------------
cat_race <- cat_race %>%
  dplyr::mutate(RACE_binarymatch = case_when(
    RACE_binarymatch == 0 ~ "mixed-race",
    RACE_binarymatch == 1 ~ "same-race"
  ))

cat_race %>%
  group_by(RACE_binarymatch, RACE_multimatch, RACE_1, RACE_2) %>%
  summarize(n(), .groups = "drop") %>%
  kableExtra::kbl("html",
    align = "c",
    col.names = c("RACE_binarymatch", "RACE_multimatch", "RACE_1", "RACE_2", "sample_size")
  ) %>%
  kableExtra::kable_styling(full_width = FALSE)

## -----------------------------------------------------------------------------
# for reproducibility

set.seed(2023)

cat_both <- discord_data(
  data = df_link,
  outcome = "S00_H40",
  predictors = NULL,
  sex = "SEX",
  race = "RACE",
  demographics = "both",
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "both"
)

## ----processboth--------------------------------------------------------------
cat_both <- cat_both %>%
  dplyr::mutate(
    RACE_binarymatch = case_when(
      RACE_binarymatch == 0 ~ "mixed-race",
      RACE_binarymatch == 1 ~ "same-race"
    ),
    SEX_binarymatch = case_when(
      SEX_binarymatch == 0 ~ "mixed-sex",
      SEX_binarymatch == 1 ~ "same-sex"
    )
  )

## ----preview-both, echo = FALSE, eval = knitr::is_html_output()---------------
cat_both %>%
  group_by(RACE_multimatch, RACE_1, RACE_2, SEX_binarymatch, SEX_multimatch, SEX_1, SEX_2) %>%
  summarize(n(), .groups = "drop") %>%
  kableExtra::kbl("html",
    align = "c",
    col.names = c(
      "RACE_multi", "RACE_1", "RACE_2", "SEX_binary", "SEX_multi", "SEX_1", "SEX_2",
      "sample_size"
    )
  ) %>%
  kableExtra::kable_styling(full_width = FALSE)

## -----------------------------------------------------------------------------
discord_sex_binary <- discord_regression(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  demographics = "sex",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "binary"
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
discord_sex_binary %>%
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
discord_sex_multi <- discord_regression(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "multi"
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
discord_sex_multi %>%
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
discord_cat_mean <- lm(S00_H40_mean ~ SEX_binarymatch,
  data = cat_sex
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
discord_cat_mean %>%
  # for nicer regression output
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
discord_cat_mean2 <- lm(S00_H40_mean ~ SEX_multimatch,
  data = cat_sex
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
discord_cat_mean2 %>%
  # for nicer regression output
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
# perform kinship regressions
cat_race_reg <- discord_regression(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  demographics = "race",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "multi"
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
cat_race_reg %>%
  # for nicer regression output
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
discord_cat_mean <- lm(S00_H40_mean ~ RACE_multimatch,
  data = cat_race
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
discord_cat_mean %>%
  # for nicer regression output
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
both_multi <- discord_regression(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  demographics = "both",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "multi"
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
both_multi %>%
  # for nicer regression output
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
discord_cat_diff <- lm(
  S00_H40_diff ~ S00_H40_mean +
    RACE_multimatch + SEX_binarymatch,
  data = cat_both
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
# for nicer regression output

discord_cat_diff %>%
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
discord_cat_mean <- lm(
  S00_H40_mean ~ RACE_multimatch +
    SEX_multimatch,
  data = cat_both
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
discord_cat_mean %>%
  broom::tidy() %>%
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

## -----------------------------------------------------------------------------
discord_cat_mean2 <- lm(S00_H40_mean ~ RACE_multimatch + SEX_binarymatch,
  data = cat_both
)

## ----echo = FALSE, eval = knitr::is_html_output()-----------------------------
discord_cat_mean2 %>%
  broom::tidy() %>%
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

