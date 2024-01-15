library(shiny)
library(shinydashboard)
library(shinyjs)

library(dplyr)
library(tm)
library(NLP)
library(proxy)
library(pdftools)
library(tools)
library(scales)
library(stringr)
library(tokenizers)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(plotly)

folder_to_table <- function(folder_path) {
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
  
  # Creating a data frame with file names and extracted text
  table_data <- data.frame(
    file_name = paste(sapply(pdf_files, get_file_name), ".pdf", sep = ""),
    text = unlist(pdf_texts)
  )
  
  # Renaming the data frame rows with the file names
  rownames(table_data) <- table_data$file_name
  
  return(table_data %>% select(text))
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

get_wordcloud <- function(words, palette, n_gram = 1){
  # Tokenizing the input string into individual words
  words_list <- tokenize_ngrams(words, n = n_gram) %>% unlist()
  upper_scale <- ifelse(n_gram > 1, 2, 3)
  
  # Creating a data frame of matching words and count their occurrences.
  words <- data.frame(word = words_list) %>%
    count(word, sort = TRUE)
  
  # Generating a word cloud with specified settings, scaling word size by frequency
  words %>%
    with(
      wordcloud(
        words = word,
        random.order = FALSE,
        color = brewer.pal(9, palette),
        min.freq = 1,
        scale = c(upper_scale, 1),
        rot.per = 0,
        freq = n,
      )
    )
}

get_cosine_similarity_matrix <- function(df) {
  # Converting texts to vector corpus
  corpus <- VCorpus(VectorSource(df$text))
  
  # Creating a Document-Term Matrix with TF-IDF weighting
  dtm <- DocumentTermMatrix(corpus)
  rownames(dtm) <- rownames(df)
  dtm_tfidf <- weightTfIdf(dtm)
  
  # Converting the DTM to a regular matrix
  dtm_matrix <- as.matrix(dtm_tfidf)
  
  # Calculating the cosine similarity matrix
  similarities <- simil(dtm_matrix, method = "cosine")
  
  # Converting to a regular matrix
  similarity_matrix <- as.matrix(similarities)
  
  # Replacing NA values with 1
  similarity_matrix[is.na(similarity_matrix)] <- 1
  
  return(similarity_matrix)
}

get_pair_similarity_ranking <- function(similarity_matrix, red_flag_threshold){
  # Converting the similarity matrix into a long format data frame
  similarity_long <- as.data.frame(as.table(similarity_matrix))
  
  # Sorting the data frame by similarity values in descending order
  similarity_sorted <- similarity_long %>% arrange(desc(Freq))
  
  # Completing the ranking data frame
  ranking <- similarity_sorted %>%
    # Removing the rows of two same document and removing duplicated rows
    filter(Var1 != Var2,
           row_number() %% 2 != 0) %>% 
    # Creating column 'Documents' by combining both documents
    mutate(Documents = paste(Var1, "&", Var2)) %>% 
    # Renaming the columns
    rename(Document_1 = Var1,
           Document_2 = Var2,
           Similarity = Freq) %>% 
    # Formatting the similarity values, and adding flag to the documents
    mutate(Similarity_Formatted = round(Similarity * 100) %>% paste0("%"),
           Flag = ifelse(Similarity >= red_flag_threshold, "RED", "GREEN")) %>% 
    # Rearranging the columns
    select(Documents, everything())
  
  return(ranking)
}

get_homework_similarity_ranking <- function(similarity_matrix, red_flag_threshold){
  # Converting similarity matrix to a long format data frame
  similarity_long <- as.data.frame(as.table(similarity_matrix))
  
  # Completing the ranking data frame
  ranking <- similarity_long %>% 
    # Removing the rows of two same document
    filter(Var1 != Var2) %>% 
    # Renaming the columns
    rename(Highest_Similarity = Freq,
           Document = Var2,
           Other_Document = Var1) %>% 
    
    # Grouping by Document and select the pair with the highest similarity
    group_by(Document) %>% 
    top_n(1, wt = Highest_Similarity) %>%
    
    # Adding flags and formatting the similarity values
    mutate(Flag = ifelse(Highest_Similarity >= red_flag_threshold, "RED", "GREEN"),
           Highest_Similarity_Formatted = paste0(round(Highest_Similarity * 100), "%")) %>% 
    # Arranging the rows by similarity values in descending order
    arrange(desc(Highest_Similarity)) %>% 
    
    # Selecting relevant columns
    select(Document, Other_Document, Highest_Similarity, Highest_Similarity_Formatted, Flag)
  
  return(ranking)
}

get_similar_works <- function(pair_similarity_ranking, document) {
  
  similar_works <- pair_similarity_ranking %>% 
    # Filter pair_similarity_ranking where it's input_homework and where it's red flag
    filter(
      document == Document_1 | document == Document_2,
      Flag == "RED"
    ) %>% 
    
    # Modifying the Document column by removing input_homework and "&" from the Documents column
    mutate(
      Document = gsub(paste(c(document, "&"), collapse = "|"), "", Documents),
      Document = trimws(Document)
    ) %>% 
    
    # Selecting and reformatting the similarity value
    select(Document, Similarity_Formatted) %>% 
    rename(Similarity = Similarity_Formatted)
  
  # Return the resulting data frame
  return(similar_works)
}

get_intersect_and_distinct_words <- function(text_1, text_2) {
  
  # Tokenizing both texts
  text_1_tokens <- unlist(strsplit(tolower(text_1), "\\s+"))
  text_2_tokens <- unlist(strsplit(tolower(text_2), "\\s+"))
  
  # Combining tokens
  combined_tokens <- c(text_1_tokens, text_2_tokens)
  
  # Finding intersect words between both texts
  intersect_tokens <- intersect(text_1_tokens, text_2_tokens)
  
  # Creating a list containing distinct words for document_1, intersecting words, and distinct words for document_2
  words <- list(
    "document_1_distinct" = text_1_tokens[!text_1_tokens %in% intersect_tokens] %>% paste(collapse = " ") %>% as.character(),
    "intersect" = combined_tokens[combined_tokens %in% intersect_tokens] %>% paste(collapse = " ") %>% as.character(),
    "document_2_distinct" = text_2_tokens[!text_2_tokens %in% intersect_tokens] %>% paste(collapse = " ") %>% as.character()
  )
  
  return(words)
}

delete_files_in_www <- function() {
  files_to_remove <- list.files("www", full.names = TRUE)
  
  if (length(files_to_remove) > 0) {
    file.remove(files_to_remove)
  }
}

