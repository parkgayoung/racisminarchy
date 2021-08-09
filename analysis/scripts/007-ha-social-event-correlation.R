
ha_social_events_stats_fn <- function(){

library(rvest)
library(tidyverse)

# read in our local copy of the Wikipedia table that we made in 005
event_all_tbl <-
  read_csv(here::here("analysis/data/Timeline_of_African-American_history_social_events.csv"))

# events per year
event_all_tbl_tally <-
  event_all_tbl %>%
  group_by(year) %>%
  tally(sort = TRUE)

event_all_tbl_tally <-
  event_all_tbl_tally %>%
  full_join(
    tibble(year = seq(min(event_all_tbl_tally$year),
                      max(event_all_tbl_tally$year),
                      1))
  )

n_events = sum(event_all_tbl_tally$n, na.rm = TRUE)

# visualise
min_year <- min(event_all_tbl$year)
max_year <- max(event_all_tbl$year)

# compare with words and years in SAA abstracts

library(tidyverse)
library(quanteda)

# read in all txt files of HA abstracts
ha_abstracts <- read_csv("analysis/data/ha_titles_abstracts_years_df.csv")

# count all words for each year
all_text_c_ha <- corpus(ha_abstracts$abstract)
docnames(all_text_c_ha) <-  ha_abstracts$year

# make a dfm
all_text_c_ha_dtm <-
  all_text_c_ha %>%
  tokens(
    verbose = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    split_hyphens = TRUE,
    remove_punct = TRUE) %>%
  dfm()

# Explore key words over time
keywords <-
  c("race",
    "racism",
    "racial",
    "racist")

dfm_keywords <-
  dfm_select(all_text_c_ha_dtm,
             pattern = keywords,
             selection = "keep")

ha_words_per_year <-
  tibble(year = as.numeric(names(rowSums(dfm_keywords))),
         ha_wordcount = rowSums(dfm_keywords)) %>%
  mutate(year = parse_number(str_sub(year, 1, 4))) %>%
  group_by(year) %>%
  summarise(ha_wordcount = sum(ha_wordcount, na.rm = TRUE))

n_words <- sum(ha_words_per_year$ha_wordcount, na.rm = TRUE )
n_abstracts <- length(rowSums(dfm_keywords))

# visualise
gg1 <-
  ggplot(ha_words_per_year) +
  aes(x = year,
      y = ha_wordcount) +
  geom_col() +
  theme_bw(base_size = 8) +
  labs(x = "Year",
       y = paste0("HA abstracts (n = ", n_words, ")")
       # title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
       # subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history"
  )


# join HA and history data
ha_and_history_tbl <-
  event_all_tbl_tally %>%
  right_join(ha_words_per_year) %>%
  arrange(year) %>%
  replace_na(list(n = 0, ha_wordcount = 0)) %>%
  mutate(
    `Same year` = ha_wordcount,
    `1 year lag` = lag(ha_wordcount),
    `2 year lag` = lag(ha_wordcount, 2),
    `3 year lag` = lag(ha_wordcount, 3),
    `4 year lag` = lag(ha_wordcount, 4),
    `5 year lag` = lag(ha_wordcount, 5),
    `6 year lag` = lag(ha_wordcount, 6)
  ) %>%
  dplyr::select(-ha_wordcount,
         -`Same year`)  %>%
  pivot_longer(-c(year, n)) %>%
  replace_na(list(value = 0)) %>%
  mutate(name = factor(name,
                       levels = c(
                         "1 year lag",
                         "2 year lag",
                         "3 year lag",
                         "4 year lag",
                         "5 year lag",
                         "6 year lag")))

# African-American events on the scatterplot
# which are events that also happened in a year or
# lag years when race appeared in an HA abstract
n_events_subset <-
  event_all_tbl_tally %>%
  right_join(ha_words_per_year) %>%
  arrange(year) %>%
  filter(ha_wordcount != 0) %>%
  pull(n) %>%
  sum(na.rm = TRUE)

# how many events per plot?
ha_and_history_tbl_per_lag_plot <-
  ha_and_history_tbl %>%
  filter(value != 0) %>%
  group_by(name) %>%
  summarise(n_events = sum(n, na.rm = TRUE),
            n_words = sum(value, na.rm = TRUE))

ha_and_history_tbl <-
  ha_and_history_tbl %>%
  left_join(ha_and_history_tbl_per_lag_plot) %>%
  mutate(facet_label = paste0(name, "\n(", n_events, " events, ", n_words, " words)"))

# use best fitting model and compute linear models
library(MASS)
ha_fit_out <-
  ha_and_history_tbl %>%
  nest(-name) %>%
  mutate(i_model = map(data, ~(glm.nb(value ~ n,
                                      data = .x)))) %>%
  mutate(i_summary = map(i_model, ~summary(.x))) %>%
  mutate(coefs = map(i_summary, ~bind_rows(coef(.x)["n", c("Estimate",
                                                           "Std. Error",
                                                           "Pr(>|z|)")]))) %>%
  unnest(coefs) %>%
  mutate(name_and_p = paste0(name, "\n(p = ", round(`Pr(>|z|)`, 3), ")" ))

# compute confidence intervals on the estimates
ha_fit_out_confint <-
  ha_fit_out %>%
  nest(-c(, name, i_model)) %>%
  mutate(confints = map(i_model,  ~confint(.x) %>% as_tibble %>% slice(2) )) %>%
  unnest(confints)

# combine models and confints
ha_fit_out_model_and_confint <-
  ha_fit_out_confint %>%
  dplyr::select(name, `2.5 %`, `97.5 %`) %>%
  left_join(ha_fit_out)

# get pseudo-r-squared
library(rsq)
ha_fit_out_model_and_confint <-
  ha_fit_out_model_and_confint %>%
  mutate(rsqs = map_dbl(i_model, rsq))

# plot coefficients of models, none include zero
coef_plot <-
  ggplot(ha_fit_out_model_and_confint) +
  aes(y =Estimate,
      x = fct_rev(name_and_p)) +
  geom_pointrange(aes(ymin = `2.5 %`,
                      ymax = `97.5 %`)) +
  scale_y_continuous(
    name = "Estimate and 95% CI") +
  geom_hline(yintercept = 0,
             colour = "red") +
  xlab("") +
  coord_flip() +
  theme_bw(base_size = 8)

# scatter plot
library(ggpubr)
library(ggrepel)

sp <-
  ggplot(ha_and_history_tbl) +
  aes(n,
      value) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = year),
                  size = 4) +
  geom_smooth(method = 'glm.nb') +
  geom_text(data = tibble( facet_label = unique(ha_and_history_tbl$facet_label),
                           n = 5,
                           value = 30,
                           label = paste0("pseudo-R-squared =\n", round(ha_fit_out_model_and_confint$rsqs, 3))),
            aes(label = label),
            size = 3) +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency"),
       y = paste0("Mentions of 'race', etc. (total of ", n_words, ")\nin HA abstracts (n = ", n_abstracts, ")")) +
  facet_wrap( ~ facet_label) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# --------------------------------------------------------------
# what about looking only at protest events that might stimulate
# expressions of solidarity

protest_event_all_tbl <-
  event_all_tbl %>%
  filter(str_detect(event_all, "protest"))

# events per year
protest_event_all_tbl_tally <-
  protest_event_all_tbl %>%
  group_by(year) %>%
  tally(sort = TRUE)

protest_event_all_tbl_tally <-
  protest_event_all_tbl_tally %>%
  full_join(
    tibble(year = seq(min(protest_event_all_tbl_tally$year),
                      max(protest_event_all_tbl_tally$year),
                      1))
  )

n_protest_events = sum(protest_event_all_tbl_tally$n, na.rm = TRUE)

# visualise
min_year <- min(protest_event_all_tbl$year)
max_year <- max(protest_event_all_tbl$year)

gg_protest <-
  ggplot(protest_event_all_tbl) +
  aes(year) +
  geom_histogram(binwidth = 1) +
  theme_bw(base_size = 8) +
  scale_x_continuous(
    breaks = seq(1550, max_year,
                 by = 50)) +
  labs(x = "Year",
       y = paste0("African-American\nhistorical protest event annual\nfrequency (n = ", n_protest_events, ")")
       # title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
       # subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history"
  )

# join SAA and protest history data
ha_and_protest_history_tbl <-
  protest_event_all_tbl_tally %>%
  right_join(ha_words_per_year) %>%
  arrange(year) %>%
  replace_na(list(n = 0, ha_wordcount = 0)) %>%
  mutate(
    `Same year` = ha_wordcount,
    `1 year lag` = lag(ha_wordcount),
    `2 year lag` = lag(ha_wordcount, 2),
    `3 year lag` = lag(ha_wordcount, 3),
    `4 year lag` = lag(ha_wordcount, 4),
    `5 year lag` = lag(ha_wordcount, 5),
    `6 year lag` = lag(ha_wordcount, 6)
  ) %>%
  dplyr::select(-ha_wordcount,
         -`Same year`)  %>%
  pivot_longer(-c(year, n)) %>%
  replace_na(list(value = 0)) %>%
  mutate(name = factor(name,
                       levels = c(
                         "1 year lag",
                         "2 year lag",
                         "3 year lag",
                         "4 year lag",
                         "5 year lag",
                         "6 year lag")))

# African-American events on the scatterplot
# which are events that also happened in a year or
# lag years when race appeared in an SAA abstract
n_protest_events_subset <-
  protest_event_all_tbl_tally %>%
  right_join(ha_words_per_year) %>%
  arrange(year) %>%
  filter(ha_wordcount != 0) %>%
  pull(n) %>%
  sum(na.rm = TRUE)

# how many events per plot?
ha_and_protest_history_tbl_per_lag_plot <-
  ha_and_protest_history_tbl %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  summarise(n_events = sum(n, na.rm = TRUE),
            n_words = sum(value, na.rm = TRUE))

ha_and_protest_history_tbl <-
  ha_and_protest_history_tbl %>%
  left_join(ha_and_protest_history_tbl_per_lag_plot) %>%
  mutate(facet_label = paste0(name, "\n(", n_events, " events, ", n_words, " words)"))

# inspect models for protest data
ha_p_model_out <-
  ha_and_protest_history_tbl %>%
  nest(-name) %>%
  mutate(zi_model = map(data, ~(glm.nb(value ~ n,
                                       data = .x)))) %>%
  mutate(zi_summary = map(zi_model, ~summary(.x))) %>%
  mutate(coefs = map(zi_summary, ~bind_rows(coef(.x)["n", c("Estimate",
                                                            "Std. Error",
                                                            "Pr(>|z|)")]))) %>%
  unnest(coefs) %>%
  mutate(name_and_p = paste0(name, "\n(p = ", round(`Pr(>|z|)`, 3), ")" ))

# compute confidence intervals on the estimates
ha_p_model_out_confint <-
 ha_p_model_out %>%
  nest(-c(, name, zi_model)) %>%
  mutate(confints = map(zi_model,  ~confint(.x) %>% as_tibble %>% slice(2) )) %>%
  unnest(confints)

# combine models and confints
ha_p_model_out_model_and_confint <-
  ha_p_model_out_confint %>%
  dplyr::select(name, `2.5 %`, `97.5 %`) %>%
  left_join(ha_p_model_out)

# get pseudo-r-squared
library(rsq)
ha_p_model_out_model_and_confint_rsq <-
 ha_p_model_out_model_and_confint %>%
  mutate(rsqs = map_dbl(zi_model, rsq))

# plot coefficients of models, none include zero
coef_plot <-
  ggplot(ha_p_model_out_model_and_confint) +
  aes(y =Estimate,
      x = fct_rev(name_and_p)) +
  geom_pointrange(aes(ymin = `2.5 %`,
                      ymax = `97.5 %`)) +
  scale_y_continuous(
    name = "Estimate and 95% CI") +
  geom_hline(yintercept = 0,
             colour = "red") +
  xlab("") +
  coord_flip( ylim = c(-3, 3)) +
  theme_bw(base_size = 8)

# protest events scatter plot
library(ggpubr)
library(ggrepel)

protest_sp <-
  ggplot(ha_and_protest_history_tbl) +
  aes(n,
      value) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = year),
                  size = 4) +
  geom_smooth(method = 'glm.nb') +
  geom_text(data = tibble( facet_label = unique(ha_and_protest_history_tbl$facet_label),
                           n = 1,
                           value = 100,
                           label = paste0("pseudo-R-squared =\n", round(ha_p_model_out_model_and_confint_rsq$rsqs, 3))),
            aes(label = label),
            size = 3) +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency (protests only)"),
       y = paste0("Mentions of 'race', etc. (total of ", n_words, ")\nin HA abstracts (n = ", n_abstracts, ")")) +
  facet_wrap( ~ facet_label, scales = "free_x") +
  scale_y_continuous(breaks = scales::pretty_breaks())

# put both plots together
# put all plots together
library(cowplot)
p_ha <-
  plot_grid(
    plot_grid(gg_protest,
              coef_plot,
              rel_widths = c(1, 0.6)),
    protest_sp,
    axis = "lr",
    rel_heights = c(1,2),
    ncol = 1
  )

# diagnostic plots for the regression models

# write PNG file with desired size and resolution
library(ragg)
agg_png(here::here("analysis/figures/999-model-diagnostic-ha-protest-2-yr.png"),
        width = 13,
        height = 10,
        units = "cm",
        res = 1000,
        scaling = 0.5)

library(ggfortify)

# HA 2 year lag with protest events
print(autoplot(ha_p_model_out_model_and_confint_rsq$zi_model[[2]]))

invisible(dev.off())

# write PNG file with desired size and resolution
agg_png(here::here("analysis/figures/999-model-diagnostic-ha-protest-3-yr.png"),
        width = 13,
        height = 10,
        units = "cm",
        res = 1000,
        scaling = 0.5)

# SAA 3 year lag with protest events
print(autoplot(ha_p_model_out_model_and_confint_rsq$zi_model[[3]]))

invisible(dev.off())


}




