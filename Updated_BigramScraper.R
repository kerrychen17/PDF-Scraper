library(pdftools)
library(tm)
library(textdata)
library(tidytext)
library(syuzhet)
library(dplyr)
library(stringr)
library(tidyr)
library(igraph)
library(tidygraph)
library(ggraph)
library(sentimentr)
x = 8 
pdf_dir <- "~/Desktop/BankingComments/Public_comments_to_SEC/New_Scanned_Other Finance-related"
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

list_bigram_counts <- list()

for (i in seq_along(pdf_files)) {
  fpath <- pdf_files[i]
  txt <- pdf_text(fpath) %>% paste(collapse = " ")
  txt <- gsub("\r", "", txt)
  
  docs <- Corpus(VectorSource(txt)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english"))
  
  dtm    <- TermDocumentMatrix(docs)
  mat    <- as.matrix(dtm)
  freqs  <- sort(rowSums(mat), decreasing = TRUE)
  df     <- data.frame(word = names(freqs), freq = unname(freqs))
  
  clean_bigrams <- tibble(text = txt) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  
  bigram_counts <- clean_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, sort = TRUE) %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           is.na(as.numeric(word1)), is.na(as.numeric(word2))) %>%
    unite("bigram", c(word1, word2), sep = " ")
  
  list_bigram_counts[[i]] <- bigram_counts
  
  rm(df)
}

names(list_bigram_counts) <- tools::file_path_sans_ext(basename(pdf_files))

# --- AFINN by PDF ---
afinn <- get_sentiments("afinn")
afinn_bigram_per_pdf <- data.frame()

for (i in seq_along(list_bigram_counts)) {
  file_name <- names(list_bigram_counts)[i]
  
  word_pairs <- list_bigram_counts[[i]] %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    pivot_longer(cols = c("word1", "word2"), names_to = "position", values_to = "word") %>%
    filter(!is.na(word))
  
  sentiment_counts <- word_pairs %>%
    inner_join(afinn, by = "word") %>%
    mutate(sentiment = case_when(
      value > 0 ~ "positive",
      value < 0 ~ "negative",
      TRUE ~ "neutral"
    )) %>%
    count(sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
  
  sentiment_counts$file <- file_name
  afinn_bigram_per_pdf <- bind_rows(afinn_bigram_per_pdf, sentiment_counts)
}

afinn_bigram_per_pdf <- afinn_bigram_per_pdf %>%
  relocate(file)

write.csv(afinn_bigram_per_pdf, "~/Downloads/afinn_bigram_sentiments_by_filNew_Scanned_Other Finance-related_New.csv", row.names = FALSE)

# --- BING by PDF ---
bing <- get_sentiments("bing")
bing_bigram_per_pdf <- data.frame()

for (i in seq_along(list_bigram_counts)) {
  file_name <- names(list_bigram_counts)[i]
  
  word_pairs <- list_bigram_counts[[i]] %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    pivot_longer(cols = c("word1", "word2"), names_to = "position", values_to = "word") %>%
    filter(!is.na(word))
  
  sentiment_counts <- word_pairs %>%
    inner_join(bing, by = "word") %>%
    count(sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
  
  sentiment_counts$file <- file_name
  bing_bigram_per_pdf <- bind_rows(bing_bigram_per_pdf, sentiment_counts)
}

bing_bigram_per_pdf <- bing_bigram_per_pdf %>%
  relocate(file) %>%
  rename(pos_words = positive, neg_words = negative)

write.csv(bing_bigram_per_pdf, "~/Downloads/bing_bigram_sentiments_by_fileNew_Scanned_Other Finance-related_New.csv", row.names = FALSE)

# --- NRC by PDF ---
nrc <- get_sentiments("nrc")
nrc_bigram_per_pdf <- data.frame()

for (i in seq_along(list_bigram_counts)) {
  file_name <- names(list_bigram_counts)[i]
  
  word_pairs <- list_bigram_counts[[i]] %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    pivot_longer(cols = c("word1", "word2"), names_to = "position", values_to = "word") %>%
    filter(!is.na(word))
  
  sentiment_counts <- word_pairs %>%
    inner_join(nrc, by = "word") %>%
    count(sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
  
  sentiment_counts$file <- file_name
  nrc_bigram_per_pdf <- bind_rows(nrc_bigram_per_pdf, sentiment_counts)
}

nrc_bigram_per_pdf <- nrc_bigram_per_pdf %>%
  relocate(file)

write.csv(nrc_bigram_per_pdf, "~/Downloads/nrc_bigram_sentiments_by_fileNew_Scanned_Other Finance-related_New.csv", row.names = FALSE)