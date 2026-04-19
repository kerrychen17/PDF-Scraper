library(pdftools)
library(tm)
library(textdata)
library(tidytext)
library(syuzhet)
library(dplyr)


# setwd("~/Desktop/BankingComments/Public consultation on the first set of Draft ESRS/Other Banks")
# directory containing your PDFs
pdf_dir <- "~/Desktop/BankingComments/OtherUSRegulations"


# get all pdf file paths
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)


# prepare empty lists
list_temp1 <- list()
list_temp2 <- list()
list_temp3 <- list()


for (i in seq_along(pdf_files)) {
  fpath <- pdf_files[i]
  
  #---- 1) read & collapse pages ----
  txt <- pdf_text(fpath) %>% paste(collapse = " ")
  txt <- gsub("\r", "", txt)
  
  #---- 2) build tm Corpus & clean ----
  docs <- Corpus(VectorSource(txt)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english"))
  
  #---- 3) term‐document matrix & frequency df ----
  dtm    <- TermDocumentMatrix(docs)
  mat    <- as.matrix(dtm)
  freqs  <- sort(rowSums(mat), decreasing = TRUE)
  df     <- data.frame(word = names(freqs), freq = unname(freqs))
  
  #---- 4) AFINN merge → temp1 ----
  afinn       <- get_sentiments("afinn")
  temp       <- merge(df, afinn, by = "word", all.x = FALSE)
  temp1 <- data.frame(pos_words = NA, neg_words = NA, neutral_words = NA)
  temp1$pos_words <- nrow(temp[temp$value > 0,])
  temp1$neg_words <- nrow(temp[temp$value < 0,])
  temp1$neutral_words <- nrow(temp[temp$value == 0,])
  
  #---- 5) BING merge → temp2 ----
  bing        <- get_sentiments("bing")
  temp       <- merge(df, bing, by = "word", all.x = FALSE)
  temp2 <- data.frame(pos_words = NA, neg_words = NA)
  temp2$pos_words <- nrow(temp[temp$sentiment == "positive",])
  temp2$neg_words <- nrow(temp[temp$sentiment == "negative",])
  
  #---- 6) NRC on full text → temp3 ----
  nrc_scores  <- get_nrc_sentiment(txt)
  temp3       <- data.frame(Type = colnames(nrc_scores),
                           Value = colSums(nrc_scores))
  
  #---- 7) store in lists ----
  list_temp1[[i]] <- temp1
  list_temp2[[i]] <- temp2
  list_temp3[[i]] <- temp3
  
  rm(df)
}


# Optionally name each list element by the PDF filename:
names(list_temp1) <- tools::file_path_sans_ext(basename(pdf_files))
names(list_temp2) <- names(list_temp1)
names(list_temp3) <- names(list_temp1)


# Now:
# - list_temp1[[j]] is the AFINN‐merged df for pdf_files[j]
# - list_temp2[[j]] is the BING‐merged df for pdf_files[j]
# - list_temp3[[j]] is the NRC sentiment summary for pdf_files[j]


afinn_files <- do.call("rbind",list_temp1)
bing_files <- do.call("rbind",list_temp2)
nrc_files <- do.call("rbind",list_temp3)


write.csv(afinn_files, file = "~/Downloads/afinn_sentiments_OtherUSRegulations_new.csv",
          row.names = TRUE)
write.csv(bing_files, file = "~/Downloads/bing_sentiments_OtherUSRegulations_new.csv",
          row.names = TRUE)
write.csv(nrc_files, file = "~/Downloads/nrc_sentiments_OtherUSRegulations_new.csv",
          row.names = TRUE)
