library(dplyr)
library(tidyselect)
library(tidyr)
library(magrittr)
library(similR)
library(igraph)
library(ggplot2)

# List of statistics that will be used
statistics <- c(
  `Hamman (S)`           = "shamann",
  `Hamming (D)`          = "dhamming",
  `Mean Manhattan (D)`   = "dmh",
  `Michael (S)`          = "smichael",
  `Sized Difference (D)` = "dsd"
)

# Reading covariate data -------------------------------------------------------
dat_group <- haven::read_spss("data-raw/MURI_AllSurveys - FINAL - Group level data_1.sav")
dat_individual <- 
  haven::read_spss("data-raw/MURI_AllSurveys - FINAL_073018.sav")
dat_individual <- dat_individual %>%
  mutate(Group = as.integer(Group))

networks_las        <- readRDS("data/networks_advice_las.rds")
networks_truth      <- readRDS("data/networks_truth.rds")[["3"]]$advice
networks_advice_css <- readRDS("data/networks_advice_css_jen.rds")
networks_sizes      <- readr::read_csv("data-raw/Study1_Group sizes.csv")

# Comparing LAS vs CSS ---------------------------------------------------------

# LAS
accuracy_lasL <- lapply(names(networks_advice_css), function(n) {
  
  # Computing similalrity/distance
  ans <- similR::similarity(
    c(list(networks_las[[n]]), networks_advice_css[[n]]),
    statistic  = statistics,
    normalized = FALSE,
    firstonly  = TRUE,
    exclude_j  = TRUE
  )
  
  cbind(
    data.frame(
      Group = as.integer(n),
      ID    = rownames(networks_las[[n]]),
      PID   = sprintf("%02i%s", as.integer(n), rownames(networks_las[[n]])),
      stringsAsFactors = FALSE
      ),
    ans
    )
  
})

accuracy_las <- accuracy_lasL %>% bind_rows %>%
  as_tibble %>%
  select(-i, -j)
  

# Computing range
accuracy_las_range <- lapply(accuracy_lasL, function(d) {
  
  d[, statistics, drop=FALSE] %>%
    as.data.frame %>%
    gather("statistic", "value") %>%
    group_by(statistic) %>%
    summarize(range = diff(range(value))) %>% 
    ungroup %>%
    spread(statistic, range)
  
}) %>%
  bind_rows %>%
  bind_cols(networks_sizes) %>%
  gather("statistic", "value", -Group, -groupSize) %>%
  group_by(statistic) %>%
  mutate(
    groupSize = as.factor(groupSize),
    value_01  = (value - min(value))/diff(range(value))
  ) %>% ungroup

# Comparing TRUTH vs CSS ---------------------------------------------------------

accuracy_truthL <- lapply(names(networks_advice_css), function(n) {
  
  # Computing similalrity/distance
  ans <- similR::similarity(
    c(list(networks_truth[[n]]), networks_advice_css[[n]]),
    statistic  = statistics,
    normalized = FALSE,
    firstonly  = TRUE,
    exclude_j  = TRUE
  )
  
  cbind(
    data.frame(
      Group = as.integer(n),
      ID    = rownames(networks_las[[n]]),
      PID   = sprintf("%02i%s", as.integer(n), rownames(networks_las[[n]])),
      stringsAsFactors = FALSE
    ),
    ans
  )
  
})

accuracy_truth <- accuracy_truthL %>% bind_rows %>%
  as_tibble %>%
  select(-i, -j)


# Computing range
accuracy_truth_range <- lapply(accuracy_truthL, function(d) {
  
  d[, statistics, drop=FALSE] %>%
    as.data.frame %>%
    gather("statistic", "value") %>%
    group_by(statistic) %>%
    summarize(range = diff(range(value))) %>% 
    ungroup %>%
    spread(statistic, range)
  
}) %>%
  bind_rows %>%
  bind_cols(networks_sizes) %>%
  gather("statistic", "value", -Group, -groupSize) %>%
  group_by(statistic) %>%
  mutate(
    groupSize = as.factor(groupSize),
    value_01  = (value - min(value))/diff(range(value))
  ) %>% ungroup

# Plotting ---------------------------------------------------------------------

# Setting seed for reproducibility
set.seed(12)

# Generating the violin plots
accuracy_truth_range %>%
  
  # Adding nicer labels
  mutate(statistic = names(statistics)[match(statistic, statistics)]) %>%
  
  # Plot
  ggplot(aes(x = statistic, y = value_01)) +
  geom_violin() +
  geom_jitter(height = 0, width=.1, aes(colour=groupSize, shape = groupSize), size=4) +
  scale_colour_viridis_d(alpha = .7)  +
  
  # Plot labels and titles
  labs(
    y = "Range in Similarity/Distance (normalized to be in [0,1])",
    x = "", shape = "Group Size", colour="Group Size") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  labs(
    title    = "Distribution of within team ranges of different distance/similarity metrics.",
    subtitle = "Truth vs CSS (comparisons exclude the j-th row+colum since these are not reported in the CSS)."
    ) +
  ggsave("data/accuracy_truth.png", width = 7, height = 7)

# Saving the original data -----------------------------------------------------
readr::write_csv(accuracy_las, "data/accuracy_las.csv")
readr::write_csv(accuracy_truth, "data/accuracy_truth.csv")

