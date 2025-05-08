# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(title = "1µM Primer Dilution Calculator"),
  tags$h5('Add 1µl of primer stock to the volume calculated in the "Water Needed" column.'),
  
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
  list_of_packages = c("shiny","xlsx","plyr")
  
  lapply(list_of_packages, 
         function(x) if(!require(x,character.only = TRUE)) install.packages(x))
  
  # Load packages
  library(shiny)
  library(xlsx)
  library(plyr)
  
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
        # Save divisor value
        
        divisor <- as.numeric(input$app_length * 0.33)
        
        # Calculate amount of water needed for 1µM dilution
        calculation <- round(as.numeric(input$app_conc)/as.numeric(divisor), 2)
        
        # Assign values to be displayed in the output table
        isolate(values$df[nrow(values$df) + 1,] <- c(input$app_name, input$app_length, input$app_conc, calculation))
        
        # Reset all input values
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
    filename = function(){paste("Primer Dilution Calculations ", date(), ".xlsx", sep = "")},
    content = function(file){
      write.xlsx(values$df, file, sheetName = "Sheet1", row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
