# source helper functions ---
source("helpers.R")

# Define server logic ----
server <- function(input, output) {
  # touch file upload
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file"))
  })
  
  
  #output: table
  output$head <- renderTable({
    data_mean <- aol_mean(df = data, measures = input$select)
    head(data_mean, input$n)#now the data_mean is already the dataframe.
  })
  
  
  ## file download
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$select, ".csv")
    },
    content = function(file1) {
      write.csv(aol_mean(df = data, measures = input$select), file1) #see here, only this "data"(from reactive()) can be delivered by render* and downloadHandler
    }
  )
  
  output$selected_var <- renderText({ 
    
    paste("You have selected", input$select)
  })
  
}