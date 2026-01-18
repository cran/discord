## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Setup: Use discord_data
# Visualizing the Results

library(discord)
library(NlsyLinks)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggExtra)
library(janitor)

# Load the data
data(data_flu_ses)

# Get kinship links for individuals with the following variables:
list_vars <- c(
  "FLU_total", "FLU_2008", "FLU_2010",
  "FLU_2012", "FLU_2014", "FLU_2016",
  "S00_H40", "RACE", "SEX"
)

df_link_pairs <- Links79PairExpanded %>%
  filter(RelationshipPath == "Gen1Housemates", RFull == 0.5)


df_link <- CreatePairLinksSingleEntered(
  outcomeDataset = data_flu_ses,
  linksPairDataset = df_link_pairs,
  outcomeNames = list_vars
)

df_consistent_kin <- df_link %>%
  group_by(SubjectTag_S1, SubjectTag_S2) %>%
  count(
    FLU_2008_S1, FLU_2010_S1,
    FLU_2012_S1, FLU_2014_S1,
    FLU_2016_S1, FLU_2008_S2,
    FLU_2010_S2, FLU_2012_S2,
    FLU_2014_S2, FLU_2016_S2
  ) %>%
  na.omit()

# Create the df_flu_modeling object with only consistent responders.
# Clean the column names with the {janitor} package.
df_flu_modeling <- semi_join(df_link,
  df_consistent_kin,
  by = c(
    "SubjectTag_S1",
    "SubjectTag_S2"
  )
) %>%
  clean_names()

## ----warning=FALSE------------------------------------------------------------
df_discord_flu <- discord_data(
  data = df_flu_modeling,
  outcome = "flu_total",
  predictors = "s00_h40",
  id = "extended_id",
  sex = "sex",
  race = "race",
  pair_identifiers = c("_s1", "_s2"),
  demographics = "both"
) %>%
  filter(!is.na(s00_h40_mean), !is.na(flu_total_mean))

## -----------------------------------------------------------------------------
df_discord_flu <- df_discord_flu %>%
  mutate(
    # # Classify Difference Grouping
    ses_diff_group = factor(
      case_when(
        as.numeric(scale(s00_h40_diff)) > 0.33 ~ "More Advantaged",
        as.numeric(scale(s00_h40_diff)) < -0.33 ~ "Less Advantaged",
        abs(as.numeric(scale(s00_h40_diff))) <= 0.33 ~ "Equally Advantaged"
      ),
      levels = c(
        "Less Advantaged",
        "Equally Advantaged",
        "More Advantaged"
      )
    )
  )

## -----------------------------------------------------------------------------
# Create a color palette for the shading
color_shading_4 <- c("firebrick4", "firebrick1", "dodgerblue1", "dodgerblue4")
color_na <- "#AD78B6" # purple for missing values

color_shading_3 <- c(color_shading_4[2], color_na, color_shading_4[3])

# Determine the range of SES differences for color scaling
max_val <- max(abs(df_discord_flu$s00_h40_diff), na.rm = TRUE)

# values <- seq(-max_val, max_val, length = length(color_shading_4))

## ----individual, echo=TRUE, message=FALSE-------------------------------------
# Individual level plot
plot_indiv <- plot_indiv_sib1 <- ggplot(
  df_flu_modeling,
  aes(
    x = s00_h40_s1,
    y = flu_total_s1,
    color = s00_h40_s1 - s00_h40_s2
  )
) +
  geom_point(
    size = 0.8, alpha = 0.8, na.rm = TRUE,
    position = position_jitter(width = 0.2, height = 0.2)
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black")

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
plot_indiv_sib1 +
  scale_colour_gradientn(
    name = "Sibling\nDifferences\nin SES",
    colours = color_shading_4,
    na.value = color_na,
    values = scales::rescale(c(-max_val, max_val))
  ) +
  labs(
    x = "SES at Age 40",
    y = "Flu Vaccination Count"
  ) +
  theme_minimal() +
  ggtitle("Individual Level Plot: Sibling 1 Only") +
  theme(plot.title = element_text(hjust = 0.5))

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
plot_indiv <- plot_indiv +
  # added sibling 2 to the plot
  geom_point(
    size = 0.8, alpha = 0.8, na.rm = TRUE,
    position = position_jitter(width = 0.2, height = 0.2),
    aes(
      x = s00_h40_s2,
      y = flu_total_s2,
      color = s00_h40_s2 - s00_h40_s1 # this reverses the color difference so sibling 2 points use the opposite color gradient compared to sibling 1, making it visually clear which sibling is being represented and how their SES difference is encoded
    )
  ) +
  scale_colour_gradientn(
    name = "Sibling\nDifferences\nin SES",
    colours = color_shading_4,
    na.value = color_na,
    values = scales::rescale(c(-max_val, max_val))
  ) +
  labs(
    x = "SES at Age 40",
    y = "Flu Vaccination Count"
  ) +
  theme_minimal() #+
#  theme(legend.position = "none")

plot_indiv +
  ggtitle("Individual Level Plot") +
  theme(plot.title = element_text(hjust = 0.5))

## -----------------------------------------------------------------------------
plot_indiv_s00 <- ggplot(
  df_flu_modeling,
  aes(
    x = s00_h40_s1,
    y = s00_h40_s2,
    color = s00_h40_s1 - s00_h40_s2
  )
) +
  geom_point(
    size = 0.8, alpha = 0.8, na.rm = TRUE,
    position = position_jitter(width = 0.2, height = 0.2)
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black")


plot_indiv_s00 +
  scale_colour_gradientn(
    name = "Sibling\nDifferences\nin SES",
    colours = color_shading_4,
    na.value = color_na,
    values = scales::rescale(c(-max_val, max_val))
  ) +
  labs(
    x = "SES at Age 40 (Sibling 1)",
    y = "SES at Age 40 (Sibling 2)"
  ) +
  theme_minimal() +
  ggtitle("Individual Level Plot: SES Comparison Between Siblings") +
  theme(plot.title = element_text(hjust = 0.5))

## -----------------------------------------------------------------------------
plot_indiv_flu <- ggplot(
  df_flu_modeling,
  aes(
    x = flu_total_s1,
    y = flu_total_s2,
    color = s00_h40_s1 - s00_h40_s2
  )
) +
  geom_point(
    size = 0.8, alpha = 0.8, na.rm = TRUE,
    position = position_jitter(width = 0.2, height = 0.2)
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black")


plot_indiv_flu +
  scale_colour_gradientn(
    name = "Sibling\nDifferences\nin SES",
    colours = color_shading_4,
    na.value = color_na,
    values = scales::rescale(c(-max_val, max_val))
  ) +
  labs(
    x = "Flu Vaccinations (Sibling 1)",
    y = "Flu Vaccinations (Sibling 2)"
  ) +
  theme_minimal() +
  ggtitle("Individual Level Plot: FLU Comparison Between Siblings") +
  theme(plot.title = element_text(hjust = 0.5))

## ----scatter, message=FALSE, include=FALSE, echo=TRUE-------------------------
# Main scatter plot
plot_btwn <- ggplot(df_discord_flu, aes(
  x = s00_h40_mean,
  y = flu_total_mean,
  color = ses_diff_group,
)) +
  geom_point( # this layer creates invisible points to all the marginal plots to align correctly
    size = 1.8, alpha = 0.0, na.rm = TRUE,
    shape = 21,
    position = position_jitter(width = 0.2, height = 0.2, seed = 1234)
  ) +
  geom_point(
    size = 1.8, alpha = 0.8, na.rm = TRUE,
    shape = 21,
    aes(
      fill = s00_h40_diff,
      colour = ses_diff_group
    ),
    group = 1,
    position = position_jitter(width = 0.2, height = 0.2, seed = 1234)
  ) +
  geom_smooth(
    method = "lm", se = FALSE, color = "black",
    fill = "black",
    group = 1
  ) +
  scale_fill_gradientn(
    name = "Sibling\nDifferences\nin SES",
    colours = color_shading_4,
    na.value = color_na,
    values = scales::rescale(c(
      -max_val,
      0,
      max_val
    ))
  ) +
  scale_color_manual(
    values = color_shading_3,
  ) +
  theme_minimal() +
  theme(legend.position = "left") +
  labs(x = "Mean SES at Age 40", y = "Mean Flu Vaccinations (2006–2016)")

## ----echo=TRUE, message=FALSE-------------------------------------------------
plot_btwn

## ----echo=TRUE, message=FALSE-------------------------------------------------
# Set relative size of marginal plots (main plot 10x bigger than marginals)
ggMarginal(plot_btwn,
  type = "histogram", size = 10, # groupColour = TRUE,
  groupFill = T,
  fill = color_shading_3
)


ggMarginal(plot_btwn, type = "density", size = 10, groupColour = F, groupFill = T, aes(
  color = ses_diff_group,
  fill = ses_diff_group,
  alpha = 0.95
))
ggMarginal(plot_btwn, type = "boxplot", size = 10, groupColour = F, groupFill = T)

## ----plot-raw-data, message=FALSE---------------------------------------------
# Marginal X density (SES mean)
plot_xdensity <- ggplot(df_discord_flu, aes(
  x = s00_h40_mean,
  group = ses_diff_group,
  color = ses_diff_group
)) +
  geom_density(adjust = 2, linewidth = 1, fill = NA) +
  scale_colour_manual(
    name = "Sibling\nDifferences\nin SES",
    values = color_shading_3
  ) +
  theme_minimal() +
  theme(
    legend.position = "left",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
  ) +
  labs(x = NULL, y = NULL)

## -----------------------------------------------------------------------------
# Marginal Y density (Flu mean)
plot_ydensity <- ggplot(df_discord_flu, aes(
  x = flu_total_mean,
  group = ses_diff_group,
  color = ses_diff_group
)) +
  geom_density(
    adjust = 2,
    linewidth = 1,
    fill = NA
  ) +
  scale_colour_manual(
    values = color_shading_3
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(x = NULL, y = NULL)

## ----echo=FALSE, message=FALSE------------------------------------------------
# add titles
plot_xdensity + theme_bw() + theme(
  plot.title = element_text(hjust = 0.5, size = 12),
  axis.title.x = element_text(size = 12)
) + labs(title = "Mean SES at Age 40")

plot_ydensity + theme_bw() + theme(
  plot.title = element_text(hjust = 0.5, size = 12),
  axis.title.x = element_text(size = 12)
) + labs(title = "Mean Flu Vaccinations (2006–2016)")

## -----------------------------------------------------------------------------
# Blank placeholder plot
plot_blank <- ggplot() +
  theme_void()

# Final layout
grid.arrange(
  arrangeGrob(plot_xdensity,
    plot_blank,
    ncol = 2,
    widths = c(4, 1)
  ),
  arrangeGrob(plot_btwn,
    plot_ydensity,
    ncol = 2,
    widths = c(4, 1)
  ),
  heights = c(1.5, 4),
  top = textGrob("Sibling Differences in SES and Flu Vaccinations",
    gp = gpar(
      fontsize = 20,
      font = 3
    )
  )
)

## ----echo=FALSE, message=FALSE------------------------------------------------
plot_btwn + facet_wrap(~ses_diff_group,
  ncol = 1
) +
  theme(legend.position = "none") +
  labs(title = "Between Family Differences in SES and Flu Vaccinations by SES Difference Group")

## ----plot-within-family, echo=TRUE, message=FALSE-----------------------------
# Within Family
#

# Main scatter plot

# Setup: Use discord_data

# Main scatter plot
plot_within <- ggplot(df_discord_flu, aes(
  x = s00_h40_diff,
  y = flu_total_diff,
  color = ses_diff_group
)) +
  geom_point( # this layer creates invisible points to all the marginal plots to align correctly
    size = 1.8, alpha = 0.0, na.rm = TRUE,
    shape = 21,
    position = position_jitter(width = 0.2, height = 0.2, seed = 1234)
  ) +
  geom_point(
    size = 1.8, alpha = 0.9, na.rm = TRUE,
    shape = 21,
    aes(
      fill = s00_h40_diff,
      colour = ses_diff_group
    ),
    group = 1,
    position = position_jitter(width = 0.2, height = 0.2, seed = 1234)
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  scale_fill_gradientn(
    name = "Sibling\nDifferences\nin SES",
    colours = color_shading_4,
    na.value = color_na,
    values = scales::rescale(c(
      -max_val,
      0,
      max_val
    ))
  ) +
  scale_color_manual(
    values = color_shading_3,
  ) +
  theme_minimal() +
  theme(legend.position = "left") +
  labs(
    x = "Diff SES at Age 40",
    y = "Diff Flu Vaccinations (2006–2016)"
  )

plot_within

## ----echo=TRUE, message=FALSE-------------------------------------------------
# Set relative size of marginal plots (main plot 10x bigger than marginals)
library(ggExtra)
ggMarginal(plot_within,
  type = "histogram", size = 10, # groupColour = TRUE,
  groupFill = T # ,
  #  color = color_shading_3
)


ggMarginal(plot_within, type = "density", size = 10, groupColour = F, groupFill = T, aes(
  color = ses_diff_group,
  fill = ses_diff_group,
  alpha = 0.95
))
ggMarginal(plot_within, type = "boxplot", size = 10, groupColour = F, groupFill = T)

## ----echo=TRUE, message=FALSE-------------------------------------------------
plot_within + facet_wrap(~ses_diff_group,
  ncol = 1
) +
  theme(legend.position = "bottom") +
  labs(title = "Within Family Differences in SES and Flu Vaccinations")

