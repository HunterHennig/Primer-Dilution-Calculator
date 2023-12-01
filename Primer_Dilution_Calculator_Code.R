# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("1µM Primer Dilution Calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Create user input options
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
  
  # Install packages as needed
  list_of_packages = c("shiny","xlsx")
  
  lapply(list_of_packages, 
         function(x) if(!require(x,character.only = TRUE)) install.packages(x))
  
  # Load packages
  library(shiny)
  library(xlsx)
  
  # Assign values to be referenced when calculating the amount of water needed
  conversion_chart <- data.frame(oligonucleotidelength=c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50),nmololigo=c(3.30,3.63,3.96,4.29,4.62,4.95,5.28,5.61,5.94,6.27,6.60,6.93,7.26,7.59,7.92,8.25,8.58,8.91,9.24,9.57,9.90,10.23,10.56,10.89,11.22,11.55,11.88,12.21,12.54,12.87,13.20,13.53,13.86,14.19,14.52,14.85,15.18,15.51,15.84,16.17,16.50))
  
  values <- reactiveValues()
  
  # Temporarily store data in a data frame
  temp_df <- data.frame(Primer_Name = numeric(0), Primer_Length = numeric(0), Concentration = numeric(0), Water_Needed = numeric(0))
  
  # Rename table columns
  names(temp_df)[1]="Primer Name"
  names(temp_df)[2]="Primer Length (nt)"
  names(temp_df)[3]="Concentration (ng/µL)"
  names(temp_df)[4]="Water Needed (µL)"
  
  # Convert temporary dataframe with updated column names into the rendered table
  values$df <- temp_df
  
  # Format the date to be written when downloading the table
  date <- function(){
    temp_date <- Sys.Date()
    return(format(temp_date, format = "%m-%d-%Y"))
    } 
  
  # Update the table with the data provided
  observeEvent(input$update, 
    {
        # Save divisor value by cross-referencing the input length and the stored conversion chart
        result <- which(conversion_chart==as.numeric(input$app_length),arr.ind=TRUE)
        
        row_select <- as.numeric(result[1])
        
        divisor <- as.numeric(conversion_chart[result[1],2])
        
        # Calculate amount of water needed for 1µM dilution
        calculation <- round(as.numeric(input$app_conc)/as.numeric(divisor), 2)
        
        # Assign values to be displayed in the output table
        newLine <- isolate(c(input$app_name, input$app_length, input$app_conc))
        isolate(values$df[nrow(values$df) + 1,] <- c(input$app_name, input$app_length, input$app_conc, calculation))
        
        # Reset al input values
        updateTextInput(session, "app_name", value = "")
        updateNumericInput(session, "app_length", value = 22)
        updateTextInput(session, "app_conc", value = "")
      })
  
  # Remove most recently added row from the table when the "Delete Last Row" button is pressed
  observeEvent(input$undo, 
               {
                 isolate(values$df <- values$df[-nrow(values$df), ])
               })
  
  # Clear all inputs from the table when the "Clear" button is pressed
  observeEvent(input$clear, 
               {
                 isolate(values$df <- values$df[c(), ])
               })
  
  # Render the created reactive table
  output$reactive_view <- renderTable({values$df})
    
  # Download the currently displayed table as a .xlsx file when the "Download" button is pressed  
  output$downloadData <- downloadHandler(
    filename = function(){paste("PrimerDilutionCalculations ", date(), ".xlsx", sep = "")},
    content = function(file){
      write.xlsx(values$df, file, sheetName = "Sheet1", row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)