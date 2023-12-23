
library(shiny)
library(gtrendsR)
library(lubridate)
library(rlist)

ui <- fluidPage(
        titlePanel("Google Trends Data Updater"),
        sidebarLayout(
                sidebarPanel(
                        dateInput("electionDate", "Enter Election Date", value = Sys.Date(), 
                                  min = as.Date("2004-01-01"), max = Sys.Date()),
                        actionButton("setDate", "Set Date Range"),
                        uiOutput("dateRangeUI"),
                        textInput("candidateName", "Enter Candidate Name"),
                        textInput("partyName", "Enter Party Name"),
                        selectInput("country", "Select Country",
                                    choices = c("Argentina" = "AR", "Bolivia" = "BO", "Brazil" = "BR", 
                                                "Chile" = "CL", "Colombia" = "CO", "Costa Rica" = "CR", 
                                                "Republica Dominicana" = "DO", "Ecuador" = "EC", "El Salvador" = "SV",
                                                "Guatemala" = "GT", "Honduras" = "HN", "Mexico" = "MX", 
                                                "Nicaragua" = "NI", "Panama" = "PA", "Paraguay" = "PY", 
                                                "Peru" = "PE", "Uruguay" = "UY", "Venezuela" = "VE")),
                        numericInput("extractionNumber", "Enter Extraction Number", value = 1),
                        radioButtons("fileOption", "File Options",
                                     choices = c("Create New File" = "new", "Update Existing File" = "update")),
                        conditionalPanel(
                                condition = "input.fileOption == 'update'",
                                textInput("datasetPath", "Enter Dataset Path (for updating)")
                        ),
                        actionButton("update", "Get Data"),
                        tags$hr(),
                        tags$p("Select 'Create New File' to generate a new dataset file. 
                               Select 'Update Existing File' to update an existing dataset. 
                               If updating, please provide the path to the existing file.")
                ),
                mainPanel(
                        tableOutput("trendData")
                )
        )
)


server <- function(input, output, session) {
        output$dateRangeUI <- renderUI({
                req(input$electionDate)
                start_date <- as.Date(input$electionDate) - 1
                end_date <- start_date - 30
                
                dateRangeInput("dateRange", "Select Date Range",
                               start  = start_date, 
                               end    = end_date,
                               min    = as.Date("2004-01-01"),
                               max    = Sys.Date())
        })
        
        observeEvent(input$update, {
                # Input validation
                req(input$candidateName, input$partyName, input$country, input$dateRange)
                
                # Prepare separate start and end dates
                
                start_date <- as.Date(input$dateRange[2],format="%d/%m/%Y")
                end_date <- as.Date(input$dateRange[1],format="%d/%m/%Y")
                
                time <-  paste(start_date, end_date)
                
                
                # Try to extract new trends data with a tryCatch block
                tryCatch({
                        trends <- gtrends(keyword = c(input$candidateName, input$partyName),
                                          time = time, category = 396, onlyInterest = T, gprop = "web",
                                          geo = input$country)
                        
                        output$trendData <- renderTable({
                                trends
                        })
                }, error = function(e) {
                        print(paste("Error in gtrends:", e$message))
                })

                
                # Process the extracted data to get a dataframe
                
                trends.ds <- rlist::list.rbind(trends)
                trends.ds$hits <- as.numeric(trends.ds$hits)
                
                # Calculate the Personalism_Index
                
                Sum <- doBy::summary_by(trends.ds, hits~keyword, FUN= sum, na.rm=T, keep.names = T)
                
              # Calculate the indicator
                Personalism_score <-  sum(Sum[Sum$keyword== input$candidateName,]$hits) / sum(Sum$hits)
                print(Personalism_score)
         
                 # Create the dataset entry
                processed_data <- data.frame(
                        Country = input$country,  # Use the name of the country
                        Year = format(as.Date(input$dateRange[1]), "%Y"),
                        Personalism_Index = Personalism_score  
                )
                
                
                # Add extraction number to the data
                processed_data$Extraction_Number <- input$extractionNumber
                
                processed_data$Year <- as.integer(processed_data$Year)
                processed_data$Personalism_Index <- as.numeric(processed_data$Personalism_Index)
                
                print(processed_data)
                
                # Determine file path based on user selection
                filePath <- if (input$fileOption == "new") {
                        paste0("Gtrends_personalism", "_", input$candidateName,"_" ,input$extractionNumber , ".csv")
                } else {
                        req(input$datasetPath)  # Required only for updating existing file
                        input$datasetPath
                }
                
                
                
                # Perform actions based on file option
                if (input$fileOption == "update") {
                        # Update existing file logic
                        if (!file.exists(filePath)) {
                                print("The specified file does not exist for updating.")
                                return()
                        }
                        existing_data <- read.csv(filePath)
                        
                
                # Handling updating existing file
                        # Check and coerce column classes
                        if ("Year" %in% names(existing_data)) {
                                existing_data$Year <- as.integer(existing_data$Year)
                        } else {
                                print("Year column missing in the existing dataset.")
                                return()
                        }
                        
                        if ("Personalism_Index" %in% names(existing_data)) {
                                existing_data$Personalism_Index <- as.numeric(existing_data$Personalism_Index)
                        } else {
                                print("Personalism_Index column missing in the existing dataset.")
                                return()
                        }
                        
                        # Check if all required columns are present
                        required_cols <- c("Country", "Year", "Personalism_Index")
                        if (!all(required_cols %in% names(existing_data))) {
                                print("Missing required columns in the existing dataset. Required columns are: Country, Year, Personalism_Index")
                                return()
                        }
                        
                        # Merge the existing data with new data
                        updated_data <- dplyr::full_join(existing_data, processed_data)
                        updated_data <- select(updated_data, c("Country", "Year", "Personalism_Index"))
                        write.csv(updated_data, file = filePath)
                        
                } else {
                        
                        # Handling creating a new file
                        processed_data <- select(processed_data, c("Country", "Year", "Personalism_Index"))
                        write.csv(processed_data, file = filePath)
                }
                
        })
        
        # After completing the necessary operations
        #stopApp()
}


shinyApp(ui = ui, server = server)
