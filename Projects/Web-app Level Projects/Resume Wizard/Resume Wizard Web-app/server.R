function(input, output, session) {
  observeEvent(
    eventExpr = input$how_to_use,
    handlerExpr = {
      return(
        showModal(
          modalDialog(
            style = "display: flex; justify-content: center;",
            HTML('<iframe width="840" height="472.5" src="https://www.youtube.com/embed/FC1P1lZXyUw?si=u8pLvLIevTTli2Iw&autoplay=1" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'),
            footer = modalButton(icon("right-from-bracket")),
            easyClose = F,
            size = "l"
          )
        )
      )
    }
  )
  
  output$download_tester_resumes <- downloadHandler(
    filename = "Resume Wizard - tester resumes.zip",
    content = function(file) {
      zip(file, files = list.files("Resume Wizard - tester resumes", full.names = TRUE))
    }
  )
  
  observeEvent(
    eventExpr = input$sample_documents,
    handlerExpr = {
      return(
        showModal(
          modalDialog(
            title = "Try with these designer resumes! 😏",
            tags$img(
              src = "tester-resumes.jpg",
              width = "100%"
            ),
            easyClose = T,
            fade = T,
            footer = downloadButton("download_tester_resumes", label = "Download")
          )
        )
      )
    }
  )
  
  observeEvent(
    eventExpr = input$example_skills,
    handlerExpr = {
      if (!input$is_descriptive_skills) {
        modal_title <- "Try with these technical skills! 😏"
        skills_to_be_copied <- "AutoCAD, Photoshop, Illustrator, SketchUp, Lumion, InDesign, CorelDRAW, SolidWorks, Blender, Revit, Rhinoceros, Cinema 4D, Premiere Pro, Lightroom, Maya, ZBrush, Dreamweaver, XD, Figma, QuarkXPress, Spark, Procreate, Inkscape, Unity3D"
      } else {
        modal_title <- "Try with these descriptive skills! 😏"
        skills_to_be_copied <- "Expertise in Image Manipulation: Skilled in working with light, transparencies, color density, shadowing, and understanding image resolution and sizing.
Retouching and Selection: Strategic approach to retouching, manipulating selections, and using advanced selection tools like Magnetic Lasso.
Layer Management: Creating and managing layers, applying gradients, layer styles, borders, and adjustment layers for enhancing images.
Proficient in Adobe Photoshop: Extensive experience in utilizing Photoshop tools such as masking, layers, silos, and camera raw adjustments.
Specialized Techniques: Proficient in advanced techniques like creating panoramas, correcting image distortion, and extending depth of field.
Content Manipulation: Expertise in content-aware tools for moving and manipulating objects seamlessly within images.
Prototyping: Building interactive prototypes to demonstrate the functionality and flow of designs."
      }
      
      return(
        showModal(
          modalDialog(
            use_copy(),
            title = modal_title,
            skills_to_be_copied,
            easyClose = TRUE,
            fade = TRUE,
            footer = CopyButton(
              "copy_skills",
              label = "Click here to copy",
              icon = icon("copy"),
              text = skills_to_be_copied
            ) %>% column(width = 12, align = "center")
          )
        )
      )
    }
  )
  
  observeEvent(
    eventExpr = input$bewitch,
    handlerExpr = {
      
      delete_files_in_www()
      
      if (!dir.exists("www")) {
        dir.create("www")
      }
      
      file.copy(input$resumes$datapath, file.path("www", input$resumes$name), overwrite = TRUE)
    }
  )
  
  observeEvent(
    eventExpr = input$bewitch,
    handlerExpr = {
      if (!is_input_valid()){
        return(
          showModal(
            modalDialog(
              title = h3(ifelse(input$resumes %>% is.null(), "⚠️ No resumes were submitted ⚠️", "⚠️ No skills were entered ⚠️")) %>% column(width = 12, align = "center"),
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
  
  is_input_valid <- eventReactive(
    eventExpr = input$bewitch,
    valueExpr = {
      if (!input$resumes %>% is.null() & input$desired_skills != ""){
        return(T)
      }
      
      return(F)
    }
  )
  
  output$desired_skills_input <- renderUI({
    if (!input$is_descriptive_skills){
      label <- "Enter desired skills (separate each skill with a comma):"
      placeholder <- "AutoCAD, Photoshop, Illustrator, SketchUp, Lumion, InDesign, CorelDRAW, SolidWorks, Blender, Revit, Rhinoceros, Cinema 4D, Premiere Pro, Lightroom, Maya, ZBrush, Dreamweaver, XD, Figma, QuarkXPress, Spark, Procreate, Inkscape, Unity3D"
    } else {
      label <- "Enter desired skills:"
      placeholder <- "Expertise in Image Manipulation: Skilled in working with light, transparencies, color density, shadowing, and understanding image resolution and sizing.
Retouching and Selection: Strategic approach to retouching, manipulating selections, and using advanced selection tools like Magnetic Lasso."
    }
    
    return(textInput("desired_skills", label = label, placeholder = placeholder))
  })
  
  output$n_segment_input <- renderUI({
    if (input$bewitch > 0 & is_input_valid()){
      return(sliderInput("n_segment", "Set number of clusters/segments:", min = 0, max = 4, value = 0, width = 200) %>% column(width = 12, align = "center"))
    }
  })
  
  resumes_df <- eventReactive(
    eventExpr = input$bewitch,
    valueExpr = {
      if (is_input_valid()){
        resumes_df <- PDFs_folder_to_table("www")
        resumes_df$PDF_Text <- clean_text(resumes_df$PDF_Text, as.corpus = F, rm.number = F, rm.stopwords_english = F, rm.stopwords_bahasa = F, stem = F)
        
        return(resumes_df)
      }
    }
  )
  
  resumes_ranking <- eventReactive(
    eventExpr = input$bewitch,
    valueExpr = {
      if (is_input_valid()){
        
        return(rank_resumes(resumes_df(), input$desired_skills, input$is_descriptive_skills))
      }
    }
  )
  
  clustered_resumes_ranking <- eventReactive(
    eventExpr = input$n_segment,
    valueExpr = {
      if(is_input_valid()){
        if (input$n_segment > 0){
          return(cluster_resumes(resumes_ranking(), input$n_segment))
        } else{
          return(resumes_ranking())
        }
      }
    }
  )
  
  output$top_10_resumes_bar_chart <- renderPlotly({
    if(input$n_segment > 1){
      text <- glue("{clustered_resumes_ranking()$Resume}
                   {clustered_resumes_ranking()$Matching_Percentage_Formatted} Matched
                   from cluster {clustered_resumes_ranking()$Cluster}")
    } else{
      text <- glue("{clustered_resumes_ranking()$Resume}
                   {clustered_resumes_ranking()$Matching_Percentage_Formatted} Matched")
    }
    ggplot <- clustered_resumes_ranking() %>% 
      slice(1:15) %>% 
      ggplot(aes(y = reorder(Resume, Matching_Percentage),
                 x = Matching_Percentage,
                 fill = Matching_Percentage,
                 text = paste0(Resume, "\n", Matching_Percentage_Formatted, " matched", ifelse(input$n_segment > 1, paste("\nfrom cluster", Cluster), "")))) +
      geom_col(show.legend = F) +
      labs(title = "Top 15 Resumes",
           y = NULL,
           x = NULL) +
      scale_x_continuous(labels = percent_format(scale = 100)) +
      scale_fill_gradient(brewer.pal(9, "Blues")) +
      theme_minimal()
    
    return(ggplotly(ggplot, tooltip = "text"))
  })
  
  output$ranking_table <- renderDataTable(
    {
      displayed_data = if (input$n_segment < 2){
        resumes_ranking()
      } else {clustered_resumes_ranking()}
      
      return(
        datatable(
          data = displayed_data %>%
            mutate(Matching_Percentage = Matching_Percentage_Formatted,
                   Rank = row_number()) %>% 
            select(Rank, everything(), -PDF_Text, -Matching_Percentage_Formatted),
          options = list(scrollX = T, pageLength = 15),
          selection = "none",
          filter = "none",
          rownames = F,
        )
      )
    },
    
  )
  
  output$resume_wordcloud <- renderPlot({
    n_grams <- input$n_grams %>% as.integer()
    scale <- if (n_grams == 1) {
      c(3, 1)
    } else {
      c(2, 0)
    }
    
    
    if (input$n_grams %>% is.null()){
      n_grams <- 1
      scale <- c(3, 1)
    }
    
    tokens <- resumes_ranking()[clustered_resumes_ranking()$Resume == input$resume_to_view %>% trimws(), "PDF_Text"] %>%
      clean_text(as.corpus = F, rm.number = F, rm.stopwords_english = T, rm.stopwords_bahasa = T, rm.punctuation = F, stem = F, rm.whitespace = T) %>% 
      tokenize_ngrams(n = n_grams) %>% 
      unlist()
    
    
    return(
      get_wordcloud(tokens, scale = c(3, 1))
    )
  })
  
  output$resume_pdf <- renderUI({
    if (input$resume_to_view %>% trimws() %in% rownames(resumes_df())){
      return(
        tags$iframe(
          src = input$resume_to_view %>% trimws() %>% paste0(".pdf"),
          style = "height:900px; width:100%"
        )
      )
    }
  })
  
  clusters_wordclouds <- eventReactive(
    eventExpr = c(input$n_segment, input$bewitch),
    valueExpr = {
      if (is_input_valid() & input$n_segment > 0) {
        unique_clusters <- unique(clustered_resumes_ranking()$Cluster)
        scale <- if(input$n_segment == 4){
          c(2, 0)
        } else {c(3, 1)}
        wordclouds <- lapply(unique_clusters[order(unique_clusters)], function(cluster) {
          tokens <- clustered_resumes_ranking()[clustered_resumes_ranking()$Cluster == cluster, "Matching_Skills"] %>%
            str_split(", ") %>%
            unlist()
          
          wordclouds <- column(
            width = 12 / input$n_segment,
            paste0("Cluster ", cluster) %>% column(width = 12, align = "center"),
            renderPlot({
              get_wordcloud(tokens, scale, input$is_descriptive_skills)
            })
          )
          
          return(wordclouds)
        })
      }
    }
  )
  
  clusters_tables <- eventReactive(
    eventExpr = c(input$n_segment, input$bewitch),
    valueExpr = {
      if (is_input_valid() & input$n_segment > 0) {
        data <- clustered_resumes_ranking() %>% mutate(Rank = row_number())
        
        clusters <- unique(clustered_resumes_ranking()$Cluster)
        tables <- lapply(clusters[order(clusters)], function(cluster) {
          
          df <- data %>% 
            filter(Cluster == cluster) %>% 
            mutate(Matching_Percentage = Matching_Percentage_Formatted) %>% 
            select(Rank, everything(), -PDF_Text, -Matching_Percentage_Formatted)
          
          tables <- column(
            width = 12 / input$n_segment,
            renderDataTable({
              datatable(
                data = df,
                options = list(scrollX = TRUE, pageLength = 5),
                rownames = FALSE
              )
            })
            
          )
          
          return(tables)
        })
      }
    }
  )
  
  clusters_and_ranking_and_resume_viewer <- eventReactive(
    eventExpr = c(input$bewitch),
    valueExpr = {
      if (is_input_valid()) {
        return(
          box(
            width = 12,
            renderUI({
              if (input$n_segment > 0){
                column(
                  width = 12,
                  h2("Clusters:"),
                  renderUI({
                    clusters_wordclouds()
                  }),
                  " ",
                  renderUI({
                    if (input$n_segment > 1) {
                      list(
                        checkboxInput("show_clusters_tables", "Show clusters' resumes", value = isolate(input$show_clusters_tables)) %>% column(width = 12, align = "center"),
                        tags$br(),
                        renderUI({
                          if(input$show_clusters_tables){
                            clusters_tables()
                          }
                        })
                      )
                    }
                  })
                )
              }
            }),
            column(
              width = 6,
              h2("Ranking:"),
              plotlyOutput("top_10_resumes_bar_chart"),
              tags$br(),
              dataTableOutput("ranking_table")
            ),
            column(
              width = 6,
              h2("Resume Viewer:"),
              textInput("resume_to_view", "Enter a resume name:", placeholder = "12345678"),
              renderUI({
                if (trimws(input$resume_to_view) %in% rownames(resumes_df())){
                  resume_data <- clustered_resumes_ranking() %>% filter(Resume == input$resume_to_view %>% trimws())
                  
                  list(
                    valueBox(
                      width = 12,
                      value = paste(resume_data$Matching_Percentage_Formatted, "MATCHING"),
                      subtitle = resume_data$Matching_Skills,
                      color = "blue",
                      icon = icon("percent")
                    ) %>% fluidRow(),
                    plotOutput("resume_wordcloud"),
                    radioButtons("n_grams", "", choiceNames = c("Unigram", "Bigram", "Trigram"), choiceValues = c(1, 2, 3), inline = T) %>% 
                      column(width = 12, align = "center")
                  )
                }
              }),
              uiOutput("resume_pdf")
            )
          )
        )
      }
    }
  )
  
  output$clusters_and_ranking_and_resume_viewer <- renderUI({
    return(clusters_and_ranking_and_resume_viewer())
  })
  
  output$quick_talk <- renderUI({
    if (input$conjure > 0 && !df() %>% is.null()){
      return(NULL)
    } 
    
    return(
      column(
        width = 12,
        style = "display: flex; justify-content: center;",
        box(
          width = 7,
          style = "display: flex; justify-content: center;",
          HTML('<iframe width="840" height="472" src="https://www.youtube.com/embed/p03UfD3NI14?si=1yHxcbh-UJm5bGQ1&autoplay=1" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>')
        )
      )
      
    )
  })
  
  output$home_video <- renderUI({
    if (input$bewitch > 0 && is_input_valid()){
      return(NULL)
    } 
    
    return(
      column(
        width = 12,
        style = "display: flex; justify-content: center;",
        box(
          width = 7,
          style = "display: flex; justify-content: center;",
          HTML('<iframe width="840" height="472.5" src="https://www.youtube.com/embed/o08VfBJEUz0?si=5W4eGZu4P1OCsYvB" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>')
        )
      )
      
    )
  })
  
  onStop(delete_files_in_www)
}

