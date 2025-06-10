## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
library(BGmisc)
library(ggpedigree)
library(tidyverse)
library(discord)

data(potter)

## -----------------------------------------------------------------------------
df_potter <- potter

names(df_potter)[names(df_potter) == "famID"] <- "oldfam"

df_potter <- ped2fam(df_potter,
  famID = "famID",
  personID = "personID"
)

## -----------------------------------------------------------------------------
df_potter <- checkSex(df_potter,
  code_male = 1,
  code_female = 0,
  verbose = FALSE, repair = TRUE
)

## ----echo=TRUE, fig.cap="Pedigree plot of the Potter dataset", fig.height=3, fig.width=4, message=FALSE, warning=FALSE----
ggpedigree(potter)

## -----------------------------------------------------------------------------
add <- ped2add(df_potter)
cn <- ped2cn(df_potter)

## -----------------------------------------------------------------------------
df_links <- com2links(
  writetodisk = FALSE,
  ad_ped_matrix = add, cn_ped_matrix = cn,
  drop_upper_triangular = TRUE
) %>%
  filter(ID1 != ID2)

df_links %>%
  slice(1:10) %>%
  knitr::kable()

## -----------------------------------------------------------------------------
df_sim <- df_links %>%
  filter(addRel == .5) %>% # only full siblings %>%
  filter(cnuRel == 1) # only kin raised in the same home

df_cousin <- df_links %>%
  filter(addRel == .125) %>% # only cousins %>%
  filter(cnuRel == 0) # only kin raised in separate homes

## -----------------------------------------------------------------------------
set.seed(1234)
syn_df <- discord::kinsim(
  mu_all = c(1, 1),
  cov_a = .4,
  cov_e = .4,
  c_all = 0,
  r_vector = df_cousin$addRel
) %>%
  select(-c(
    A1_1, A1_2, A2_1, A2_2,
    C1_1, C1_2, C2_1, C2_2,
    E1_1, E1_2, E2_1, E2_2,
    r
  ))

## -----------------------------------------------------------------------------
data_demo <- cbind(df_cousin, syn_df)


summary(data_demo)

## -----------------------------------------------------------------------------
model_output <- discord_regression(
  data = data_demo,
  outcome = "y1",
  predictors = "y2",
  id = "id",
  sex = NULL,
  race = NULL,
  pair_identifiers = c("_1", "_2")
)
summary(model_output)

