
# source helper functions ---
source("helpers.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("Measures scoring system in Dr.Ebner lab"),
  
  sidebarLayout(
    sidebarPanel( 
      helpText("Introduction: This app is used for the measure scores calculation.", 
               tags$a(href = "https://ebnerlab.psych.ufl.edu/",
                    "Dr.Ebner Lab")),
      
      
      
      h2("Input panel"),
      helpText("Please upload your csv file and choose the measurement you want to score"),
      
      fileInput("file", label = h3("File input(*.csv)"), accept = c(".csv")),
      numericInput("n", "Rows you want to display", value = 5, min = 1, step = 1),
      
      hr(),
      fluidRow(column(4, verbatimTextOutput("value"))),
      
      selectInput("select", label = h3("Measurement selection"), 
                  choices = c("PANAS" = "Q37", 
                              "CES-D" = "Q20",
                              "AES" = "Q19",
                              "BFI" = "Q64",
                              "EQ" = "Q67",
                              "ERQ" = "Q35",
                              "ITW" = "Q36",
                              "WBS" = "Q53",
                              "STAI" = "Q54",
                              "SWLS" = "Q56",
                              "TMMS" = "Q58",
                              "UCLA_loneliness" = "Q59"), 
                  selected = "PANAS")
    ),
    
    mainPanel(
      h1("Output panel"),
      tableOutput("head")
    )
  ),
  
  fluidRow(
    column(width = 3, downloadButton("download", label = "Download", class = "btn-block"))
  )
  
)



