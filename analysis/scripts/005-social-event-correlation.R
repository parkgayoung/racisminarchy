
social_event_correlation_plots_fn <- function(){

library(rvest)
library(tidyverse)

# get data from Wikipedia of social events about African-Americans
url <- "https://en.wikipedia.org/wiki/Timeline_of_African-American_history"

event_desc_year <- ".tright+ ul li , p+ ul li , p b"

event_all <-
  url %>%
  read_html() %>%
  html_nodes(event_desc_year) %>%
  html_text()

# tidy it into a data frame
event_all_tbl <-
  tibble(event_all = event_all,
         year = 0) %>%
  mutate(year = ifelse(nchar(event_all) == 4, event_all, NA))  %>%
  mutate(year = as.numeric(year)) %>%
  fill(year) %>%
  filter(nchar(event_all) != 4)

# save a copy of the Wikipedia table locally for future reference
write_csv(event_all_tbl,
          here::here("analysis/data/Timeline_of_African-American_history_social_events.csv"))

#------------------------------------------------------------------
# or read in our local copy of the Wikipedia table that we made above
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

gg <-
ggplot(event_all_tbl) +
  aes(year) +
  geom_histogram(binwidth = 1) +
  theme_bw(base_size = 8) +
  scale_x_continuous(
                     breaks = seq(1550, max_year,
                     by = 50)) +
  labs(x = "Year",
       y = paste0("African-American\nhistorical event annual\nfrequency (n = ", n_events, ")")
      # title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
      # subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history"
       )

# gg

# plotly::ggplotly(gg)

# compare with words and years in SAA abstracts

library(tidyverse)
library(quanteda)

# read in all txt files of SAA abstracts from 1962 to 2020. Data is from SAA website and scanned images of text is converted to PDF using OCR
# the code for OCR is here: https://github.com/benmarwick/saa-meeting-abstracts/blob/master/code/001-PDF-page-images-to-txt.R
# this is a character vector, one abstract per element
if(!exists("all_text_clean")){
  all_text_clean <- readRDS(here::here("analysis","data", "all_text_clean.rds"))
}

# count all words for each year
if(!exists("all_text_c")){
  all_text_c <- corpus(all_text_clean)
}

# make a dfm
if(!exists("all_text_c_dtm")){
    all_text_c_dtm <- readRDS(here::here("analysis/data/all_text_c_dtm.rds"))
}

# Explore key words over time
keywords <-
  c("race",
    "racism",
    "racial",
    "racist")

dfm_keywords <-
  dfm_select(all_text_c_dtm,
             pattern = keywords,
             selection = "keep")

saa_words_per_year <-
  tibble(year = as.numeric(names(rowSums(dfm_keywords))),
         saa_wordcount = rowSums(dfm_keywords))

n_words <- sum(saa_words_per_year$saa_wordcount, na.rm = TRUE )
n_years <- length(rowSums(dfm_keywords))

# visualise
gg1 <-
  ggplot(saa_words_per_year) +
  aes(x = year,
      y = saa_wordcount) +
  geom_col() +
  theme_bw(base_size = 8) +
  labs(x = "Year",
       y = paste0("SAA annual\nfrequency (n = ", n_words, ")")
       # title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
       # subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history"
  )

# join SAA and history data
saa_and_history_tbl <-
  event_all_tbl_tally %>%
  right_join(saa_words_per_year) %>%
  arrange(year) %>%
  replace_na(list(n = 0, saa_wordcount = 0)) %>%
  mutate(
    `Same year` = saa_wordcount,
    `1 year lag` = lag(saa_wordcount),
    `2 year lag` = lag(saa_wordcount, 2),
    `3 year lag` = lag(saa_wordcount, 3),
    `4 year lag` = lag(saa_wordcount, 4),
    `5 year lag` = lag(saa_wordcount, 5),
    `6 year lag` = lag(saa_wordcount, 6)
    ) %>%
  dplyr::select(-saa_wordcount,
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

## --------------------------------------------------


# African-American events on the scatterplot
# which are events that also happened in a year or
# lag years when race appeared in an SAA abstract
n_events_subset <-
event_all_tbl_tally %>%
  right_join(saa_words_per_year) %>%
  arrange(year) %>%
  dplyr::filter(saa_wordcount != 0) %>%
  pull(n) %>%
  sum(na.rm = TRUE)

# how many events per plot?
saa_and_history_tbl_per_lag_plot <-
saa_and_history_tbl %>%
  dplyr::filter(value != 0) %>%
  group_by(name) %>%
  summarise(n_events_ = sum(n, na.rm = TRUE),
            n_words_ = sum(value, na.rm = TRUE))

saa_and_history_tbl <-
  saa_and_history_tbl %>%
  left_join(saa_and_history_tbl_per_lag_plot) %>%
  mutate(facet_label = paste0(name, "\n(", n_events_, " events, ", n_words_, " words)"))

# scatter plot
library(ggpubr)
library(ggrepel)

library(broom)
require(ggplot2)
require(pscl)

# is it zero-inflated?
saa_and_history_tbl %>%
  filter(name == "1 year lag") %>%
  ggplot() +
  aes(value) +
  geom_histogram()

# devtools::install_github("r-forge/countreg/pkg")
library(MASS)

# here I explored several different models (lowest BIC is best)
# glm( family = "poisson" )    min BIC 247.27 no
# glm.nb                       min BIC 173.93 ok - use this one
# zeroinfl( dist = "poisson" ) min BIC 198.24 no
# zeroinfl( dist = "negbin" )  min BIC 177.64 ok

# compute models
compare_models_out <-
  saa_and_history_tbl %>%
  nest(-name) %>%
  mutate(glm.nb = map(data, ~(glm.nb(value ~ n,
                                        data = .x)))) %>%
  mutate(glm.p = map(data, ~(glm(value ~ n,
                                 family = "poisson",
                                     data = .x)))) %>%
  mutate(zeroinfl.p = map(data, ~(zeroinfl(value ~ n,
                                           dist = "poisson",
                                 data = .x)))) %>%

  mutate(zeroinfl.nb = map(data, ~(zeroinfl(value ~ n,
                                           dist = "negbin",
                                           data = .x))))
# inspect fits
png(here::here("analysis/figures/999-model-selection-glm.nb.png"),
    width = 7,
    height = 5,
    units = "in",
    res = 300)
par(mfrow = c(2,3))
map2(compare_models_out$glm.nb,
     compare_models_out$name,
    ~countreg::rootogram(.x, main = as.character(.y)))
dev.off()

png(here::here("analysis/figures/999-model-selection-glm.p.png"),
    width = 7,
    height = 5,
    units = "in",
    res = 300)
par(mfrow = c(2,3))
map2(compare_models_out$glm.p,
     compare_models_out$name,
     ~countreg::rootogram(.x, main = as.character(.y)))
dev.off()

png(here::here("analysis/figures/999-model-selection-zeroinfl.p.png"),
    width = 7,
    height = 5,
    units = "in",
    res = 300)
par(mfrow = c(2,3))
map2(compare_models_out$zeroinfl.p,
     compare_models_out$name,
     ~countreg::rootogram(.x, main = as.character(.y)))
dev.off()

png(here::here("analysis/figures/999-model-selection-zeroinfl.nb.png"),
    width = 7,
    height = 5,
    units = "in",
    res = 300)
par(mfrow = c(2,3))
map2(compare_models_out$zeroinfl.nb,
     compare_models_out$name,
     ~countreg::rootogram(.x, main = as.character(.y)))
dev.off()

# choose glm.nb based on these plots

# check BIC values to compare models
models <- c("glm.nb", "glm.p", "zeroinfl.p", "zeroinfl.nb")

model_summaries_list <-
  map(models, ~map_df(compare_models_out[[.x]], vcdExtra::LRstats))

names(model_summaries_list) <- models

 model_summaries_list %>%
  bind_rows(.id = "model") %>%
   group_by(model) %>%
   summarise(mean_bic = mean(BIC)) %>%
   arrange((mean_bic))

# smallest mean BIC comes from glm.nb, confirming that as a good choice

# compute models
model_out <-
  saa_and_history_tbl %>%
  nest(-name) %>%
  mutate(the_model = map(data, ~(glm.nb(value ~ n,
                                     data = .x))))



# use best fitting model and compute linear models
best_fit_out <-
  saa_and_history_tbl %>%
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
best_fit_out_confint <-
  best_fit_out %>%
  nest(-c(, name, i_model)) %>%
  mutate(confints = map(i_model,  ~confint(.x) %>% as_tibble %>% slice(2) )) %>%
  unnest(confints)

# combine models and confints
best_fit_out_model_and_confint <-
  best_fit_out_confint %>%
  dplyr::select(name, `2.5 %`, `97.5 %`) %>%
  left_join(best_fit_out)

# get pseudo-r-squared
library(rsq)
best_fit_out_model_and_confint <-
best_fit_out_model_and_confint %>%
  mutate(rsqs = map_dbl(i_model, rsq))

# plot coefficients of models, none include zero
coef_plot <-
ggplot(best_fit_out_model_and_confint) +
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

# plot linear models
sp <-
ggplot(saa_and_history_tbl) +
  aes(n,
      value) +
  geom_point(alpha = 0.3) +
  facet_wrap( ~ facet_label) +
  geom_text(data = tibble( facet_label = unique(saa_and_history_tbl$facet_label),
                           n = 14,
                           value = 30,
                           label = paste0("pseudo-R-squared =\n", round(best_fit_out_model_and_confint$rsqs, 3))),
            aes(label = label),
            size = 3) +
  geom_text_repel(aes(label = year),
            size = 4) +
  geom_smooth(method = 'glm.nb') +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency"),
       y = paste0("Mentions of 'race', etc. (total of ", n_words, ")\nin SAA abstracts")) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# put all plots together
library(cowplot)
p_5 <-
plot_grid(
  plot_grid(gg,
            coef_plot,
            rel_widths = c(1, 0.6)),
  sp,
  axis = "lr",
  rel_heights = c(1,2),
  ncol = 1
          )

pngfile_5 <- here::here("analysis/figures/005-keyword-and-event-relationships.png")
jpgfile_5 <- here::here("analysis/figures/005-keyword-and-event-relationships.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile_5,
        width = 13,
        height = 10,
        units = "cm",
        res = 1000,
        scaling = 0.5)

print(p_5)

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_5 <- image_read(pngfile_5)
png_5_jpg_5 <- image_convert(img_in_5, "jpg")
image_write(png_5_jpg_5, jpgfile_5, density = 1000, quality = 100)

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
saa_and_protest_history_tbl <-
  protest_event_all_tbl_tally %>%
  right_join(saa_words_per_year) %>%
  arrange(year) %>%
  replace_na(list(n = 0, saa_wordcount = 0)) %>%
  mutate(
    `Same year` = saa_wordcount,
    `1 year lag` = lag(saa_wordcount),
    `2 year lag` = lag(saa_wordcount, 2),
    `3 year lag` = lag(saa_wordcount, 3),
    `4 year lag` = lag(saa_wordcount, 4),
    `5 year lag` = lag(saa_wordcount, 5),
    `6 year lag` = lag(saa_wordcount, 6)
  ) %>%
  dplyr::select(-saa_wordcount,
         -`Same year`)  %>%
  pivot_longer(-c(year, n)) %>%
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
  right_join(saa_words_per_year) %>%
  arrange(year) %>%
  filter(saa_wordcount != 0) %>%
  pull(n) %>%
  sum(na.rm = TRUE)

# how many events per plot?
saa_and_protest_history_tbl_per_lag_plot <-
  saa_and_protest_history_tbl %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  summarise(n_events = sum(n, na.rm = TRUE),
            n_words = sum(value, na.rm = TRUE))

saa_and_protest_history_tbl <-
  saa_and_protest_history_tbl %>%
  left_join(saa_and_protest_history_tbl_per_lag_plot) %>%
  mutate(facet_label = paste0(name, "\n(", n_events, " events, ", n_words, " words)"))

# inspect models for protest data
p_model_out <-
  saa_and_protest_history_tbl %>%
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
p_model_out_confint <-
  p_model_out %>%
  nest(-c(, name, zi_model)) %>%
  mutate(confints = map(zi_model,  ~confint(.x) %>% as_tibble %>% slice(2) )) %>%
  unnest(confints)

# combine models and confints
p_model_out_model_and_confint <-
  p_model_out_confint %>%
  dplyr::select(name, `2.5 %`, `97.5 %`) %>%
  left_join(p_model_out)

# get pseudo-r-squared
library(rsq)
p_model_out_model_and_confint_rsq <-
  p_model_out_model_and_confint %>%
  mutate(rsqs = map_dbl(zi_model, rsq))

# plot coefficients of models, none include zero
coef_plot <-
  ggplot(p_model_out_model_and_confint) +
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
  ggplot(saa_and_protest_history_tbl) +
  aes(n,
      value) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = year),
                  size = 4) +
  geom_smooth(method = 'glm.nb') +
  geom_text(data = tibble( facet_label = unique(saa_and_protest_history_tbl$facet_label),
                           n = 2,
                           value = 100,
                           label = paste0("pseudo-R-squared =\n", round(p_model_out_model_and_confint_rsq$rsqs, 3))),
            aes(label = label),
            size = 3) +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency (protests only)"),
       y = paste0("Mentions of 'race', etc. (total of ", n_words, ")\nin SAA abstracts")) +
  facet_wrap( ~ facet_label, scales = "free_x")


# put all plots together
library(cowplot)
p_6 <-
  plot_grid(
    plot_grid(gg_protest,
              coef_plot,
              rel_widths = c(1, 0.6)),
    protest_sp,
    axis = "lr",
    rel_heights = c(1,2),
    ncol = 1
  )

# ------------------------------------------------------
# plot diagnostics for model with significant relationship

library(ggfortify)

# write PNG file with desired size and resolution
agg_png(here::here("analysis/figures/999-model-diagnostic-saa-soc-1-yr.png"),
        width = 13,
        height = 10,
        units = "cm",
        res = 1000,
        scaling = 0.5)

# SAA 1 year lag with all social events
print(autoplot(best_fit_out_model_and_confint$i_model[[1]]))
invisible(dev.off())

# write PNG file with desired size and resolution
agg_png(here::here("analysis/figures/999-model-diagnostic-saa-protest-5-yr.png"),
        width = 13,
        height = 10,
        units = "cm",
        res = 1000,
        scaling = 0.5)

# SAA 5 year lag with protest events
print(autoplot(p_model_out_model_and_confint_rsq$zi_model[[5]]))
invisible(dev.off())





}




