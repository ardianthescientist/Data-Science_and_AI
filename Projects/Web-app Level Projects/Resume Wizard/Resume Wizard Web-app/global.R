library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyCopy2clipboard)

library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(purrr)
library(tm)
library(NLP)
library(proxy)
library(NLP)
library(pdftools)
library(data.table)
library(tools)
library(tidytext)
library(glue)
library(textclean)
library(tokenizers)
library(stringr)
library(wordcloud)
library(RColorBrewer)

PDFs_folder_to_table <- function(folder_path) {
  # Function to convert PDF to text
  convert_pdf_to_text <- function(pdf_path) {
    pdf_text_content <- pdf_text(pdf_path)
    
    extracted_text <- list()
    
    for (page in seq_along(pdf_text_content)) {
      text <- pdf_text_content[[page]]
      extracted_text[[page]] <- text
    }
    
    all_text <- paste(extracted_text, collapse = "\n")
  }
  
  # Function to get file name
  get_file_name <- function(file_path) {
    file_path_sans_ext(basename(file_path))
  }
  
  # Getting PDF files from the specified folder
  pdf_files <- list.files(folder_path, pattern = ".pdf", full.names = TRUE)
  
  # Converting PDFs to text
  pdf_texts <- lapply(pdf_files, convert_pdf_to_text)
  
  # Creating a data frame for the extracted texts
  table_data <- data.frame(
    PDF_Text = unlist(pdf_texts)
  )
  
  # Renaming the data frame rows with the file names
  rownames(table_data) <- get_file_name(pdf_files)
  
  return(table_data)
}

clean_text <- function(text, as.corpus = T, lower = T, rm.number = T, rm.stopwords_english = T, rm.stopwords_bahasa = T, rm.punctuation = T, stem = T, rm.whitespace = T){
  text_corpus <- text %>% VectorSource() %>% VCorpus()
  
  # Lowercasing
  if (lower){
    text_corpus <- tm_map(x = text_corpus,
                          FUN = content_transformer(tolower))
  }
  # Removing numbers
  if (rm.number){
    text_corpus <- tm_map(x = text_corpus,
                          FUN = removeNumbers)
  }
  # Removing english stop words
  if (rm.stopwords_english){
    list_stop_words_english <- readLines("stop-words_english.txt", warn = FALSE, encoding = "UTF-8")
    
    text_corpus <- tm_map(x = text_corpus,
                          FUN = removeWords,
                          list_stop_words_english)
  }
  # Removing bahasa stop words
  if (rm.stopwords_bahasa){
    list_stop_words_bahasa <- readLines("stop-words_bahasa.txt", warn = FALSE, encoding = "UTF-8")
    
    text_corpus <- tm_map(x = text_corpus,
                          FUN = removeWords,
                          list_stop_words_bahasa)
  }
  # Removing punctuation
  if (rm.punctuation){
    text_corpus <- tm_map(x = text_corpus,
                          FUN = removePunctuation)
  }
  # Reducing words to their base form
  if (stem){
    text_corpus <- tm_map(x = text_corpus,
                          FUN = stemDocument)
  }
  # Removing white/blank spaces
  if (rm.whitespace){
    text_corpus <- tm_map(x = text_corpus,
                          FUN = stripWhitespace)
  }
  
  # Returning the text as or not as corpus
  if (as.corpus){
    return(text_corpus)
  }
  else(
    return(sapply(text_corpus, as.character))
  )
}

get_wordcloud <- function(tokens, scale = c(2, 0), normalize_higher_ngrams = F) {
  
  # Creating a data frame of tokens and count their occurrences.
  words <- data.frame(token = tokens) %>%
    count(token, sort = TRUE) %>% 
    na.omit()
  
  # Calculate the tf-idf weight of each token
  if (normalize_higher_ngrams){
    words <- words %>% rowwise() %>% mutate(n = n * length(unlist(str_split(token, " "))) + 1)
  }
  
  # Generating a word cloud with specified settings, scaling word size by frequency
  words %>%
    with(
      wordcloud(
        words = token,
        random.order = FALSE,
        color = colorRampPalette(c("#B8E2FF", "#00416F"))(length(unique(words$token))),
        min.freq = 1,
        scale = scale,
        rot.per = 0,
        freq = n
      )
    )
}

rank_resumes <- function(resumes_df, desired_skills, is_descriptive_skills = F) {
  # Function to tokenize the text by n gram range
  tokenize_ngrams_by_range <- function(text, upper_range) {
    tokens <- c()
    for (i in upper_range:1) {
      tokens <- c(tokens, unlist(tokenize_ngrams(text, n = i)))
    }
    
    return(tokens)
  }
  
  # Tokenizing the desired skills
  if (is_descriptive_skills){
    desired_skills <- desired_skills %>% clean_text(as.corpus = F,
                                                    lower = T,
                                                    rm.number = F,
                                                    rm.stopwords_english = T,
                                                    rm.stopwords_bahasa = T,
                                                    rm.punctuation = T,
                                                    stem = T,
                                                    rm.whitespace = T)
    tokenized_desired_skills <- desired_skills %>% tokenize_ngrams_by_range(2)
    resumes_df$PDF_Text <- resumes_df$PDF_Text %>% clean_text(as.corpus = F,
                                                              lower = T,
                                                              rm.number = F,
                                                              rm.stopwords_english = T,
                                                              rm.stopwords_bahasa = T,
                                                              rm.punctuation = T,
                                                              stem = T,
                                                              rm.whitespace = T)
    max_n_gram <- 2
  } else{
    tokenized_desired_skills <- unlist(str_split(desired_skills %>% tolower() %>% trimws(), ", ")) %>% unique()
    max_n_gram <- strsplit(desired_skills, ", ") %>% sapply(length) %>% max()
  }
  
  # Creating the ranking data frame
  ranking_df <- resumes_df %>% 
    rowwise() %>% 
    # Extracting the matching skills
    mutate(Matching_Skills = intersect(tokenize_ngrams_by_range(PDF_Text, max_n_gram), tokenized_desired_skills) %>% paste(collapse = ", "),
           # Calculating the matching percentage
           Matching_Percentage = ifelse(Matching_Skills == "", 0, round(length(unlist(str_split(Matching_Skills, ", "))) / length(tokenized_desired_skills), 2)),
           Matching_Percentage_Formatted = paste0(Matching_Percentage * 100, "%")) %>% 
    as.data.frame() %>% 
    mutate(Resume = rownames(resumes_df)) %>% 
    arrange(desc(Matching_Percentage)) %>% 
    select(Resume, everything())
  
  return(ranking_df)
}

cluster_resumes <- function(ranking_df, k){
  
  # Creating document term matrix of ranking_df
  dtm <- DocumentTermMatrix((ranking_df %>% mutate(Matching_Skills = removePunctuation(Matching_Skills)))$Matching_Skills)
  
  # Converting the document term matrix into a data frame
  df <- dtm %>%
    as.matrix() %>% 
    as.data.frame()
  
  # Replacing the row names with the resume file names
  rownames(df) <- rownames(ranking_df)
  
  # Applying K-means Clustering algorithm 
  clusters <- kmeans(x = df,
                     centers = k)
  
  clustered_ranking_df <- ranking_df %>% mutate(Cluster = clusters$cluster)
  
  return(clustered_ranking_df)
}

delete_files_in_www <- function() {
  files_to_remove <- list.files("www", full.names = TRUE)
  
  # Keep "foto-ian.jpg" and remove other files
  files_to_remove <- setdiff(files_to_remove, c("www/foto-ian.jpg", "www/paper.html", "www/tester-resumes.jpg"))
  
  if (length(files_to_remove) > 0) {
    file.remove(files_to_remove)
  }
}