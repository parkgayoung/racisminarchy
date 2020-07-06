
dfm_keywords <- readRDS(here::here("analysis","data","dfm_keywords.rds"))

# distances for specific documents for keyword
dist_keywords <- textstat_dist(dfm_keywords,
                               dfm_keywords[,"discrimination"],
                               margin = "documents")

dist_keywords_tbl <-
  as.data.frame(dist_keywords, to = "data.frame")

# compute similarities between features using relative frequency
simi_all <-
  dfm_weight(all_text_c_dtm, scheme = "prop") %>%
  textstat_simil(selection = keywords,
                 method = "correlation",
                 margin = "features")

head(as.matrix(simi_all), 10)
as.list(simi_all, n = 10)

# compute similarities between features
simi_keywords <- textstat_simil(dfm_keywords,
                                all_text_c_dtm[, c("black",
                                                   "people",
                                                   "african",
                                                   "asian")],
                                method = "correlation",
                                margin = "features")
head(as.matrix(simi_keywords), 10)
