function(input, output, session) {
  
  observeEvent(
    eventExpr = input$how_to_use,
    handlerExpr = {
      showModal(
        modalDialog(
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/p03UfD3NI14?si=1yHxcbh-UJm5bGQ1&autoplay=1" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'),
          footer = modalButton(icon("right-from-bracket")),
          easyClose = F,
          size = "m"
        )
      )
    }
  )
  
  output$download_tester_documents <- downloadHandler(
    filename = "Homework Truthweaver - tester documents.zip",
    content = function(file) {
      zip(file, files = list.files("Homework Truthweaver - tester documents", full.names = TRUE))
    }
  )
  
  observeEvent(
    eventExpr = input$sample_documents,
    handlerExpr = {
      showModal(
        modalDialog(
          title = "Try with these documents!",
          tags$img(
            src = "tester-documents.jpg",
            width = "100%"
          ),
          easyClose = T,
          fade = T,
          footer = downloadButton("download_tester_documents", label = "Download")
        )
      )
    }
  )

  observeEvent(
    eventExpr = input$conjure,
    handlerExpr = {

      delete_files_in_www()

      if (!dir.exists("www")) {
        dir.create("www")
      }
      
      file.copy(input$homeworks$datapath, file.path("www", input$homeworks$name), overwrite = TRUE)
    }
  )
  
  observeEvent(
    eventExpr = input$conjure,
    handlerExpr = {
      if (input$homeworks %>% is.null()){
        return(
          showModal(
            modalDialog(
              title = h3("⚠️ No documents were submitted ⚠️") %>% column(width = 12, align = "center"),
              easyClose = T,
              footer = NULL,
              p("Come on 😏") %>% column(width = 12, align = "center"),
              tags$br()
            )
          )
        )
      }
    }
  )
  
  df <- eventReactive(
    eventExpr = input$conjure,
    valueExpr = {
      if (input$homeworks %>% is.null()){
        return(NULL)
      }
      
      df <- folder_to_table("www") %>% 
        mutate(text = clean_text(text, F))
      
      return(df)
    }
  )
  
  cosine_similarity_matrix <- eventReactive(
    eventExpr = input$conjure,
    valueExpr = {
      if (df() %>% is.null()) {
        return(NULL)
      }
      
      return(get_cosine_similarity_matrix(df()))
    }
  )
  
  pair_similarity_ranking <- eventReactive(
    eventExpr = c(input$conjure, input$red_flag_threshold),
    valueExpr = {
      if (cosine_similarity_matrix() %>% is.null()) {
        return(NULL)
      }
      
      return(get_pair_similarity_ranking(cosine_similarity_matrix(), input$red_flag_threshold/100))
    }
  )
  
  homework_similarity_ranking <- eventReactive(
    eventExpr = c(input$conjure, input$red_flag_threshold),
    valueExpr = {
      if (cosine_similarity_matrix() %>% is.null()){
        return(NULL)
      }
      
      return(get_homework_similarity_ranking(cosine_similarity_matrix(), input$red_flag_threshold/100))
    }
  )
  
  output$pair_similarity_density_chart <- renderPlot({
    color <- ifelse(pair_similarity_ranking()$Similarity %>% median() >= input$red_flag_threshold/100, "red", "green")
    
    ggplot(pair_similarity_ranking(), aes(x = Similarity * 100)) +
      geom_density(fill = color, color = color, alpha = 0.4) +
      labs(x = "Similarity (%)",
           y = NULL,
           title = "Pair Similarity Density") +
      theme_minimal() +
      scale_x_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
      geom_vline(xintercept = input$red_flag_threshold, color = "black")
  })
  
  output$homework_similarity_density_chart <- renderPlot({
    color <- ifelse(homework_similarity_ranking()$Highest_Similarity %>% median() >= input$red_flag_threshold/100, "red", "green")
    
    ggplot(homework_similarity_ranking(), aes(x = Highest_Similarity * 100)) +
      geom_density(fill = color, color = color, alpha = 0.4) +
      labs(x = "Similarity (%)",
           y = NULL,
           title = "Homework Similarity Density") +
      theme_minimal() +
      scale_x_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
      geom_vline(xintercept = input$red_flag_threshold, color = "black")
  })
  
  output$top_12_pairs_of_homeworks_bar_chart <- renderPlotly({
    top_12_pairs_bar_chart <- ggplot(pair_similarity_ranking()[1:12, ], aes(y = reorder(Documents, Similarity), x = Similarity * 100, fill = Similarity, text = paste0(Documents, "\n", Similarity_Formatted, " similar"))) +
      geom_bar(stat = "identity", show.legend = F) +
      labs(title = "Top 12 Similar Pairs of Homeworks", y = NULL, x = "Similarity (%)") +
      scale_x_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
      scale_fill_gradient(low = "#790000", high = "#FF0D0D") +
      theme_minimal() +
      theme(text = element_text(size = 8))
    
    top_12_pairs_bar_chart %>% ggplotly(tooltip = "text")
  })
  
  output$top_12_homeworks_bar_chart <- renderPlotly({
    top_12_homeworks_bar_chart <- ggplot(homework_similarity_ranking()[1:12, ], aes(y = reorder(Document, Highest_Similarity), x = Highest_Similarity * 100, fill = Highest_Similarity, text = paste(Highest_Similarity_Formatted, "similar to", Other_Document))) +
      geom_bar(stat = "identity", show.legend = F) +
      labs(title = "Top 12 High Similarity Homeworks", y = NULL, x = "Similarity (%)") +
      scale_x_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
      scale_fill_gradient(low = "#790000", high = "#FF0D0D") +
      theme_minimal() +
      theme(text = element_text(size = 9))
    
    top_12_homeworks_bar_chart %>% ggplotly(tooltip = "text")
  })
  
  output$pair_similarity_ranking_table <- renderDataTable(
    expr = datatable(
      data = pair_similarity_ranking() %>% 
        mutate(Similarity = Similarity_Formatted) %>% 
        select(Documents, Similarity, Flag),
      filter = "none",
      selection = "none",
      options = list(scrollX = T)
    ),
    options = list(pageLength = 12)
  )
  
  output$homework_similarity_ranking_table <- renderDataTable(
    expr = datatable(
      data = homework_similarity_ranking() %>% 
        mutate(Highest_Similarity_Detected = Highest_Similarity_Formatted) %>% 
        select(Document, Highest_Similarity_Detected, Flag, Other_Document),
      filter = "none",
      selection = "none",
      options = list(scrollX = T)
    ),
    options = list(pageLength = 12)
  )
  
  info_boxes <- eventReactive(
    eventExpr = c(input$conjure, input$red_flag_threshold),
    valueExpr = {
      if (df() %>% is.null()) {
        return(NULL)
      }
      
      return(
        column(
          width = 12,
          infoBox(
            title = "Median of Pair Similarities",
            value = (round(median(pair_similarity_ranking()$Similarity) * 100)) %>% paste0("%"),
            width = 3,
            color = ifelse(pair_similarity_ranking()$Similarity %>% median() >= input$red_flag_threshold/100, "red", "green"),
            icon = icon("compress"),
            fill = T
          ),
          infoBox(
            title = "Total Red Flag Pairs",
            value = pair_similarity_ranking() %>% filter(Flag == "RED") %>% nrow(),
            width = 3,
            color = "black",
            icon = icon("flag-checkered")
          ),
          infoBox(
            title = "Total Red Flag Homework",
            value = homework_similarity_ranking() %>% filter(Flag == "RED") %>% nrow(),
            width = 3,
            color = "black",
            icon = icon("flag", class = "fa-solid fa-flag")
          ),
          infoBox(
            title = "Median of Homework Similarities",
            value = (round(median(homework_similarity_ranking()$Highest_Similarity) * 100)) %>% paste0("%"),
            width = 3,
            color = ifelse(homework_similarity_ranking()$Highest_Similarity %>% median() >= input$red_flag_threshold/100, "red", "green"),
            icon = icon("compress"),
            fill = T
          )
        )
      )
    }
  )
  
  output$info_boxes <- renderUI({
    info_boxes()
  })
  
  analytics <- eventReactive(
    eventExpr = input$conjure,
    valueExpr = {
      if (input$homeworks %>% is.null()){
        return(NULL)
      }
      
      return(
        box(
          width = 12,
          h2("Analytics") %>% column(width = 12, align = "center", tags$br()),
          column(
            width = 6,
            plotOutput("pair_similarity_density_chart")
          ),
          column(
            width = 6,
            plotOutput("homework_similarity_density_chart")
          ),
          column(
            width = 6,
            plotlyOutput("top_12_pairs_of_homeworks_bar_chart")
          ),
          column(
            width = 6,
            plotlyOutput("top_12_homeworks_bar_chart")
          ),
          column(
            width = 12,
            align = "center",
            checkboxInput("show_ranking_table", "Show table", value = T)
          ),
          renderUI({
            if (input$show_ranking_table){
              list(
                column(
                  width = 6,
                  dataTableOutput("pair_similarity_ranking_table")
                ),
                column(
                  width = 6,
                  dataTableOutput("homework_similarity_ranking_table")
                )
              )
            }
          })
        )
      )
    }
  )
  
  output$analytics <- renderUI({
    analytics()
  })
  
  pair_word_clouds <- eventReactive(
    eventExpr = c(input$document_1, input$document_2),
    valueExpr = {
      if (!input$document_1 %>% trimws() %in% rownames(df()) | !input$document_2 %>% trimws() %in% rownames(df())){
        return(NULL)
      }
      
      document_1_distinct_words <- get_intersect_and_distinct_words(df()[input$document_1 %>% trimws(), "text"], df()[input$document_2 %>% trimws(), "text"])$document_1_distinct
      intersect <- get_intersect_and_distinct_words(df()[input$document_1 %>% trimws(), "text"], df()[input$document_2 %>% trimws(), "text"])$intersect
      document_2_distinct_words <- get_intersect_and_distinct_words(df()[input$document_1 %>% trimws(), "text"], df()[input$document_2 %>% trimws(), "text"])$document_2_distinct
      
    }
  )
  
  output$document_1_distinct_word_clouds <- renderPlot({
    document_1_distinct_words <- get_intersect_and_distinct_words(df()[input$document_1 %>% trimws(), "text"], df()[input$document_2 %>% trimws(), "text"])$document_1_distinct
    
    return(get_wordcloud(document_1_distinct_words, "Greens", ifelse(input$n_gram %>% is.null(), 1, input$n_gram %>% as.integer())))
  })
  
  output$documents_intersect_word_clouds <- renderPlot({
    documents_intersect_words <- get_intersect_and_distinct_words(df()[input$document_1 %>% trimws(), "text"], df()[input$document_2 %>% trimws(), "text"])$intersect
    
    return(get_wordcloud(documents_intersect_words, "Reds", ifelse(input$n_gram %>% is.null(), 1, input$n_gram %>% as.integer())))
  })
  
  output$document_2_distinct_word_clouds <- renderPlot({
    document_2_distinct_words <- get_intersect_and_distinct_words(df()[input$document_1 %>% trimws(), "text"], df()[input$document_2 %>% trimws(), "text"])$document_2_distinct
    
    return(get_wordcloud(document_2_distinct_words, "Greens", ifelse(input$n_gram %>% is.null(), 1, input$n_gram %>% as.integer())))
  })
  
  pair_analysis <- eventReactive(
    eventExpr = input$conjure,
    valueExpr = {
      if (df() %>% is.null()) {
        return(NULL)
      }
      
      return(
        box(
          width = 12,
          h2("Pair Analysis") %>% column(width = 12, align = "center", tags$br()),
          textInput("document_1", "Enter 1st document:", placeholder = "TI100_Ardian_1232145654") %>% column(width = 6, align = "center"),
          textInput("document_2", "Enter 2nd document:", placeholder = "TI100_Asyraf_0989076567") %>% column(width = 6, align = "center"),
          renderUI({
            if (input$document_1 %>% trimws() %in% rownames(df()) & input$document_2 %>% trimws() %in% rownames(df()) & input$document_1 %>% trimws() != input$document_2 %>% trimws()) {
              pair_data <- pair_similarity_ranking() %>% 
                filter(Documents == paste(input$document_1 %>% trimws(), "&", input$document_2 %>% trimws()) | Documents == paste(input$document_2 %>% trimws(), "&", input$document_1 %>% trimws()))
              
              return(
                list(
                  valueBox(
                    width = 12,
                    value = pair_data$Similarity_Formatted %>% paste("similar"),
                    subtitle = paste(input$document_1 %>% trimws(), "&", input$document_2 %>% trimws()),
                    color = pair_data$Flag %>% tolower(),
                    icon = icon("flag")
                  ),
                  paste0(input$document_1 %>% trimws(), "'s distinct words:") %>% column(width = 4, align = "center", plotOutput("document_1_distinct_word_clouds") %>% fluidRow()),
                  "Intersect words:" %>% column(width = 4, align = "center", plotOutput("documents_intersect_word_clouds") %>% fluidRow()),
                  paste0(input$document_2 %>% trimws(), "'s distinct words:") %>% column(width = 4, align = "center", plotOutput("document_2_distinct_word_clouds") %>% fluidRow()),
                  radioButtons("n_gram", "", choiceNames = c("Unigrams", "Bigrams", "Trigrams"), choiceValues = c(1, 2, 3), inline = T, selected = 1) %>% div(width = 12, align = "center", tags$br())
                )
              )
            }
          }),
          column(
            width = 6,
            renderUI({
              if (input$document_1 %>% trimws() %in% rownames(df())){
                document_data <- homework_similarity_ranking() %>% 
                  filter(Document == input$document_1 %>% trimws())
                
                similar_works <- get_similar_works(pair_similarity_ranking(), input$document_1 %>% trimws())
                
                return(
                  fluidRow(
                    tags$iframe(
                      style = "height:900px; width:100%",
                      src = input$document_1 %>% trimws() %>% paste0(".pdf")
                    ) %>% column(width = 12),
                    
                    valueBox(
                      width = 12,
                      value = document_data$Flag %>% paste("FLAG WORK"),
                      subtitle = nrow(similar_works) %>% paste("similar works detected"),
                      color = document_data$Flag %>% tolower(),
                      icon = icon("flag")
                    ),
                    
                    renderDataTable(
                      expr = datatable(
                        data = similar_works,
                        filter = "none",
                        selection = "none",
                        options = list(scrollX = T)
                      ),
                      options = list(pageLength = 5)
                    ) %>% column(width = 12)
                  )
                )
              }
            })
          ),
          column(
            width = 6,
            renderUI({
              if (input$document_2 %>% trimws() %in% rownames(df())){
                document_data <- homework_similarity_ranking() %>% 
                  filter(Document == input$document_2 %>% trimws())
                
                similar_works <- get_similar_works(pair_similarity_ranking(), input$document_2 %>% trimws())
                
                return(
                  fluidRow(
                    width = 6,
                    tags$iframe(
                      style = "height:900px; width:100%",
                      src = input$document_2 %>% trimws() %>% paste0(".pdf")
                    ) %>% column(width = 12),
                    
                    valueBox(
                      width = 12,
                      value = document_data$Flag %>% paste("FLAG WORK"),
                      subtitle = nrow(similar_works) %>% paste("similar works detected"),
                      color = document_data$Flag %>% tolower(),
                      icon = icon("flag")
                    ),
                    
                    renderDataTable(
                      expr = datatable(
                        data = similar_works,
                        filter = "none",
                        selection = "none",
                        options = list(scrollX = T)
                      ),
                      options = list(pageLength = 5)
                    ) %>% column(width = 12)
                  )
                )
              }
            })
          ),
        )
      )
    }
  )
  
  output$pair_analysis <- renderUI({
    pair_analysis()
  })
  
  observeEvent(
    eventExpr = c(input$document_1, input$document_2),
    handlerExpr = {
      if (input$document_1 %>% trimws() %in% rownames(df()) & input$document_2 %>% trimws() %in% rownames(df()) & input$document_1 %>% trimws() == input$document_2 %>% trimws()){
        return(
          showModal(
            modalDialog(
              title = h3("⚠️ Comparing the same document ⚠️") %>% column(width = 12, align = "center"),
              easyClose = T,
              footer = NULL,
              p("Try comparing two different documents 😏") %>% column(width = 12, align = "center"),
              tags$br()
            )
          )
        )
      }
    }
  )
  
  onStop(delete_files_in_www)
}
