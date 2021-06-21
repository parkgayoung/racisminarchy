
library(quanteda)
library(stm)
library(rsvd)

race_words <- c(
  "race", # because "race to stay ahead" etc.
  "racism",
  "racial",
  "racist")

if(!exists("ha_abstracts")){
  ha_abstracts <- read_csv(here::here("analysis","data", "ha_titles_abstracts_years_df.csv"))
}

names_ha_text <- names(ha_abstracts)
all_text_c_ha <- corpus(ha_abstracts$abstract)

# make a dfm
custom_stopwords <-
  c("archaeological", "data", "sites",
    "analysis", "site", "sites",
    "university", "early", "late",
    "see", "research", "use", "culture",
    "paper", "region", "pattern", "area",
    "archaeology", "cultural", "new",
    "change", "studies", "study", "uses",
    "abstract", "excavation", "present",
    "discuss", "presentation", "settlement",
    "excavations", "saa", "meeting",
    "abstracts", "prehistoric", "patterns",
    "state", "results", "can", "discussed",
    "used", "also", "within", "chair",
    "social", "period", "evidence",
    "annual", "seventh", "ninth",
    "papers", "dataset", "datasets",
    "human", "valley", "maya", "two",
    "arizona", "one", "pro", "ing",
    "tion", "a.d", "well", "time", "using",
    "understanding", "material",
    "ancient", "manuscripts", "central",
    "recent", "75th", "79th", "80th", "81st", "mas",
    "com", "archae", "may", "information", "discussant",
    "american", "resources", "remains",
    "archacological", "subsistence", "ceramic",
    "archaeologists", "survey", "changes")

all_text_c_ha_dtm <-
  all_text_c_ha %>%
  tokens(
    verbose = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    split_hyphens = TRUE,
    remove_punct = TRUE) %>%
  tokens_select(min_nchar = 3) %>%
  tokens_remove(c(stopwords("english"),
                  custom_stopwords)) %>%
  dfm()

# check freq of race words
featfreq_out <- featfreq(all_text_c_ha_dtm)
featfreq_out[names(featfreq_out) %in% race_words ]

# keep only words occurring >= 10 times and in >= 2 documents
all_text_c_ha_dtm_trim <-
  dfm_trim(all_text_c_ha_dtm,
           min_termfreq = 30,
           min_docfreq = 2)

# check freq of race words
featfreq_out2 <- featfreq(all_text_c_ha_dtm_trim)
featfreq_out2[names(featfreq_out2) %in% race_words]

rm(featfreq_out2, featfreq_out)
rm(all_text_c_ha_dtm)
rm(all_text_c_ha)
rm(all_text)

# set random number (100) for setting seed  to get same result for every run
set.seed(100)

saaFit <- stm(all_text_c_ha_dtm_trim,
              K = 0,
              max.em.its = 50,
              init.type = "Spectral",
              seed = 100)

# Find topics that contain user specified words.
sl <- sageLabels(saaFit, n = 200)
race_topics <- findTopic(sl, race_words) #

labelTopics(saaFit, race_topics)

# https://juliasilge.com/blog/evaluating-stm/
library(tidyverse)
library(tidytext)

# beta matrix for our topic model and look at the
# probabilities that each word is generated from each topic.
td_beta <- tidy(saaFit)

# topics that feature race
race_topics

#  probabilities that each document is
# generated from each topic, that gamma matrix.
td_gamma <- tidy(saaFit,
                 matrix = "gamma",
                 document_names = names_ha_text)

# get top words for those topics

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>%
  unnest()

n <- 1000
top_terms_n <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(n, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>%
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

top_topics_plot <-
  gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 5.6) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = scales::percent_format()) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  theme_bw(base_size = 30) +
  scale_fill_viridis_d() +
  labs(x = NULL, y = expression(gamma))

# same plot for the race topics
race_topics_plot <-
  gamma_terms %>%
  filter(parse_number(as.character(topic)) %in% race_topics) %>%
  ggplot(aes(topic, gamma,
             label = str_wrap(terms, 40),
             fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0,
            nudge_y = 0.0005,
            size = 9) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = scales::percent_format()) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  theme_bw(base_size = 30) +
  scale_fill_viridis_d() +
  labs(x = NULL, y = expression(gamma))

library(patchwork)
top_topics_plot / race_topics_plot + plot_layout(ncol = 1, heights = c(1, 0.5))

# which years have high proportions of these topics?

# see topics with race
r <-
  top_terms_n %>%
  filter(topic %in% race_topics )

topics_time_series <-
  ggplot() +
  # all topic proportions
  geom_smooth(data = td_gamma %>%
                mutate(document = as.numeric(document)) %>%
                mutate(topic = as.factor(topic)),
              aes(document,
                  gamma,
                  group = topic),
              se = FALSE,
              span = 0.45,
              colour = "grey90",
              alpha = 0.2) +
  geom_smooth(data = td_gamma %>%
                filter(topic %in% race_topics ) %>%
                mutate(document = as.numeric(document)) %>%
                left_join(r) %>%
                mutate(topics = word(terms, 1, 5)),
              aes(document,
                  gamma,
                  colour = topics,
                  group = topics),
              se = FALSE,
              span = 0.45) +
  geom_point(data = td_gamma %>%
               filter(topic %in% race_topics ) %>%
               mutate(document = as.numeric(document)) %>%
               left_join(r) %>%
               mutate(topics = word(terms, 1, 5)),
             aes(document,
                 gamma,
                 colour = topics,
                 group = topics)) +
  scale_y_log10() +
  theme_bw(base_size = 30) +
  guides(colour = guide_legend(nrow = 2,
                               byrow = TRUE,
                               title = "Topics including race/racial/racism/racist"
  )) +
  theme(legend.position = c(0.45, 0.85),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20,
                                   margin = margin(
                                     t = 10,
                                     r = 6,
                                     unit = "pt")),
        legend.spacing.y = unit(0.3, 'pt'),
        legend.spacing.x = unit(0.3, 'pt')) +
  labs(x = "Year",
       y = "Probability that a document is generated from a topic")


# (top_topics_plot + race_topics_plot) / topics_time_series +
#  plot_layout(ncol = 1,
# heights = c(1, 0.5, 1)
#              )

# ggsave(here::here("analysis/figures/002-topic-model.jpg"),
#        h = 8.3,
#        w = 10,
#        scale = 3.3,
#        units = "cm",
#        dpi = "retina")



p_2 <- (top_topics_plot + race_topics_plot) / topics_time_series +
  plot_layout(ncol = 1,
              # heights = c(1, 0.5, 1)
  )


pngfile_2 <- here::here("analysis/figures/002-topic-model.png")
jpgfile_2 <- here::here("analysis/figures/002-topic-model.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile_2,
        width = 13,
        height = 12,
        units = "cm",
        res = 1000,
        scaling = 0.2)
p_2

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_2 <- image_read(pngfile_2)
png_2_jpg_2 <- image_convert(img_in_2, "jpg")
image_write(png_2_jpg_2, jpgfile_2, density = 1000, quality = 100)



