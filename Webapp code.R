# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("Webapp.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Predicting Financial Strength of Companies using Machine Learning'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(
      h3('Input parameters'),
      div("All input parameters are mandatory*", style = "color:red")),
    numericInput("Net.Income", 
                 label = "Net Income", 
                 value = ""),
    numericInput("ROA", 
                 label = "Return on Assets", 
                 value = ""),
    numericInput("operating.cash.flow.2020", 
                 label = "Operating cash flow 2020", 
                 value = ""),
    numericInput("Long.term.debt.2019", 
                 label = "Long term debt 2019", 
                 value = ""),
    numericInput("Long.term.debt.2020", 
                 label = "Long term debt 2020", 
                 value = ""),
    numericInput("Current.Ratio.2019", 
                 label = "Current Ratio 2019", 
                 value = ""),
    numericInput("Current.Ratio.2020", 
                 label = "Current Ratio 2020", 
                 value = ""),
    numericInput("Outstanding.Shares.in.2019", 
                 label = "Outstanding Shares in 2019", 
                 value = ""),
    numericInput("Outstanding.Shares.in.2020", 
                 label = "Outstanding Shares in 2020", 
                 value = ""),
    numericInput("Gross.margin.2019", 
                 label = "Gross margin 2019", 
                 value = ""),
    numericInput("Gross.margin.2020", 
                 label = "Gross margin 2020", 
                 value = ""),
    numericInput("Asset.Turnover.Ratio.2019", 
                 label = "Asset Turnover Ratio 2019", 
                 value = ""),
    numericInput("Asset.Turnover.Ratio.2020", 
                 label = "Asset Turnover Ratio 2020", 
                 value = ""),
    
    actionButton("submitbutton", "Predict", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    h3(tableOutput('tabledata')), # Prediction results table
    br(),
    br(),
    br(),
    br(),
    h3("Description:"),
    h4("This Web Application predicts Financial strength of companies using machine learning. The financial status of the company can be acquired by inputting the data in input parameters panel and click on predict to obtain the results. "),
    h4("By,"),
    h4('Siddeshwar'),
    h4('MBA Student | Dayananda Sagar University | Bengaluru'),
    h4('Data Analyst Intern '),
    h4("",
      a("AmberTAG Analytics", 
        href = "https://www.ambertag.com/"))
      )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Net Income",
               "ROA",
               "operating cash.flow 2020",
               "Long term debt 2019",
               "Long term debt 2020",
               "Current Ratio 2019",
               "Current Ratio 2020",
               "Outstanding Shares in 2019",
               "Outstanding Shares in 2020",
               "Gross margin 2019",
               "Gross margin 2020",
               "Asset Turnover Ratio 2019",
               "Asset Turnover Ratio 2020"),
      Value = as.character(c(input$Net.Income,
                             input$ROA,
                             input$operating.cash.flow.2020,
                             input$Long.term.debt.2019,
                             input$Long.term.debt.2020,
                             input$Current.Ratio.2019,
                             input$Current.Ratio.2020,
                             input$Outstanding.Shares.in.2019,
                             input$Outstanding.Shares.in.2020,
                             input$Gross.margin.2019,
                             input$Gross.margin.2020,
                             input$Asset.Turnover.Ratio.2019,
                             input$Asset.Turnover.Ratio.2020)),
      stringsAsFactors = FALSE)
    
    y <- 0
    df <- rbind(df, y)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Prediction complete.") 
    } else {
      return("Server is ready for Prediction.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)

