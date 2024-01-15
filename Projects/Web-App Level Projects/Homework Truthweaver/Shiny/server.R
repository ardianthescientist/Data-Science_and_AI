
function(input, output, session) {

  observeEvent(
    eventExpr = input$conjure,
    handlerExpr = {
      cleanup_files()
      
      # Create "www" folder if it doesn't exist
      if (!dir.exists("www")) {
        dir.create("www")
      }
      
      # Move the uploaded file to the "www" folder
      file.rename(input$homeworks$datapath, file.path("www", input$homeworks$name))
    }
  )
}
