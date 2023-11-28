run_app <- function(options = list())
{
  shiny:shinyApp(ui = app_ui,
                 server = app_server,
                 options = options)
}

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Primer Dilution Calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      textInput(inputId = "app_name",
                   label = "Primer Name:",
                   value = ""),
    
      numericInput(inputId = "app_length",
                   label = "Primer Length:",
                   value = 22),
    
      textInput(inputId = "app_conc",
                   label = "Concentration of Primer in ng/µL:",
                   value = ""),
      
      actionButton(inputId = "update",
                    label = "Add"),
      
      actionButton(inputId = "undo",
                   label = "Delete Last Row"),
      
      actionButton(inputId = "clear",
                   label = "Clear"),
      
      downloadButton(outputId = "downloadData",
                   label = "Download")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: HTML table with requested number of observations ----
      tableOutput("reactive_view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  library("xlsx")
  
  conversion_chart <- data.frame(oligonucleotidelength=c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50),nmololigo=c(3.30,3.63,3.96,4.29,4.62,4.95,5.28,5.61,5.94,6.27,6.60,6.93,7.26,7.59,7.92,8.25,8.58,8.91,9.24,9.57,9.90,10.23,10.56,10.89,11.22,11.55,11.88,12.21,12.54,12.87,13.20,13.53,13.86,14.19,14.52,14.85,15.18,15.51,15.84,16.17,16.50))
  
  values <- reactiveValues()
  
  temp_df <- data.frame(Primer_Name = numeric(0), Primer_Length = numeric(0), Concentration = numeric(0), Water_Needed = numeric(0))
  
  names(temp_df)[1]="Primer Name"
  names(temp_df)[2]="Primer Length (nt)"
  names(temp_df)[3]="Concentration (ng/µL)"
  names(temp_df)[4]="Water Needed (µL)"
  
  values$df <- temp_df
  
  date <- function(){
    temp_date <- Sys.Date()
    return(format(temp_date, format = "%m-%d-%Y"))
    } 
  
  observeEvent(input$update, 
    {
        
        result <- which(conversion_chart==as.numeric(input$app_length),arr.ind=TRUE)
        
        row_select <- as.numeric(result[1])
        
        divisor <- as.numeric(conversion_chart[result[1],2])
        
        calculation <- round(as.numeric(input$app_conc)/as.numeric(divisor), 2)
        
        newLine <- isolate(c(input$app_name, input$app_length, input$app_conc))
        isolate(values$df[nrow(values$df) + 1,] <- c(input$app_name, input$app_length, input$app_conc, calculation))
        
        updateTextInput(session, "app_name", value = "")
        updateNumericInput(session, "app_length", value = 22)
        updateTextInput(session, "app_conc", value = "")
      })
  
  observeEvent(input$undo, 
               {
                 isolate(values$df <- values$df[-nrow(values$df), ])
                 #isolate(values$df[nrow(values$df),] <- c(NA, NA, NA, NA))
               })
  
  observeEvent(input$clear, 
               {
                 isolate(values$df <- values$df[c(), ])
                 #isolate(values$df[nrow(values$df),] <- c(NA, NA, NA, NA))
               })
  
    output$reactive_view <- renderTable({values$df})
    
  output$downloadData <- downloadHandler(
    filename = function(){paste("PrimerDilutionCalculations ", date(), ".xlsx", sep = "")},
    content = function(file){
      write.xlsx(values$df, file, sheetName = "Sheet1", row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)