# Load Libraries
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(shiny)

# Cleaning and Formatting Data ------------------------------------------------------------------------

## Function to update and order stream summaries by date in a new directory "Ordered Stream Summaries"

update_stream_summaries <- function(source_folder, destination_folder) {
  # Get a list of all CSV files in the specified source folder
  csv_files <- list.files(path = source_folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Function to extract and format dates from file names *AND* add date columns
  extract_date <- function(file) {
    
    ## Extract date from file name
    date_str <- gsub(".*from ([0-9]+_[0-9]+_[0-9]+) to [0-9]+_[0-9]+_[0-9]+\\.csv", "\\1", file)
    date_parts <- unlist(strsplit(date_str, "_"))
    date <- paste0(date_parts[3], "_", date_parts[1], "_", date_parts[2])
    
    ## Read and append new "Date" column to the CSV file
    data <- read.csv(file)
    data$Date <- date
    
    ## Append new "Time Live" (in minutes) column to the CSV files
    data$"Time.Live" <- seq_len(nrow(data))
    
    # Reorder columns so that "Date" and "Time Live" come first
    data <- data[, c("Date", "Time.Live", setdiff(names(data), c("Date", "Time.Live")))]  
    
    ## Update the CSV file accordingly
    write.csv(data, file, row.names=FALSE)
    
    # Return the formatted date
    return(date)
    
  }
  
  # Extract formatted dates from file names
  formatted_dates <- sapply(csv_files, extract_date)
  
  # Sort file names based on formatted start dates
  sorted_files <- csv_files[order(as.Date(formatted_dates, format = "%Y_%m_%d"))]
  
  # Create destination folder if it doesn't exist
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder, recursive = TRUE)
  }
  
  # Copy and rename files to the destination folder
  for (i in seq_along(sorted_files)) {
    new_name <- file.path(destination_folder, paste0("Session_", i, ".csv"))
    file.copy(sorted_files[i], new_name)
  }
  
}

## Creating new folder with updated and ordered Stream Summaries
source_folder <- "Data/Stream Summaries"
destination_folder <- "Data/Ordered Stream Summaries"
update_stream_summaries(source_folder, destination_folder)

## Function to compile all CSV files into a single CSV file in new directory "Compiled CSV"
compile_csv_files <- function(source_folder, destination_folder, output_file_name) {
  # Get a list of all CSV files in the specified source folder
  csv_files <- list.files(path = source_folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty data frame to store the compiled data
  compiled_data <- data.frame()
  
  # Loop through each CSV file and append its data to the compiled data frame
  for (file in csv_files) {
    data <- read.csv(file)
    compiled_data <- rbind(compiled_data, data)
  }
  
  # Sort the compiled data frame by the "Date" column
  compiled_data <- compiled_data[order(as.Date(compiled_data$Date, format = "%Y_%m_%d")), ]
  
  # Create the destination folder if it doesn't exist
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder, recursive = TRUE)
  }
  
  # Write the sorted compiled data frame to a CSV file in the destination folder
  output_file <- file.path(destination_folder, paste0(output_file_name, ".csv"))
  write.csv(compiled_data, output_file, row.names = FALSE)
}

## Creating compiled filed in a new directory
source_folder <- "Data/Ordered Stream Summaries"  # Assuming the ordered files are in this folder
destination_folder <- "Data/Compiled CSV"  # New folder for the compiled CSV file
output_file_name <- "Compiled_Data"  # Name of the compiled CSV file

compile_csv_files(source_folder, destination_folder, output_file_name)

# Creating Visualizations  ------------------------------------------------------------------------

## Creating dataframe from compiled csv
dataframe <- read.csv("Data/Compiled CSV/Compiled_Data.csv")
str(dataframe)

## Center titles for all ggplots
theme_update(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))

##  ------------------- Correlation Analysis -------------------

### Select only numeric columns for correlation analysis
numeric_data <- dataframe[, sapply(dataframe, is.numeric)]

### Calculate correlation matrix
correlation_matrix <- cor(numeric_data)

### Melt the correlation matrix for visualization
melt_cor <- melt(correlation_matrix)

### Plot the correlation heatmap
ggplot(melt_cor, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(high = "purple4", mid="white", low = "green4", name = "Pearson Correlation", limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  coord_fixed() + xlab("") + ylab("") + ggtitle("Correlation of Variables \n in Stream Metrics")

##  ------------------- Shiny Web App --------------------------

### Shiny Web App


#### Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel( h1("Stream Summary Metrics", align = "center"), ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #Set Parameters for Scatterplot
      strong('Scatter Plot Parameters'), p(''),
      selectInput('scatterX', 'Independent Variable (X-Axis)', 
                  names(numeric_data), selected = 'crim'),
      selectInput('scatterCol', 'Factor (to Color Points By)', 
                  names(numeric_data), selected = 'age'),
      
      
      #Brief Variables Description
      strong("Variable Description"),
      tags$ul(
        tags$li('Time.Live: The total time broadcasted in minutes.'),
        tags$li('Timestamp: Local time of stream instance.'),
        tags$li('Average.Viewers: The average number of concurrent viewers in your stream..'),
        tags$li('Live.Views: The total number of views of your live streams. (This does not include VOD or clip views.)'),
        tags$li('New.Followers: The number of follows your channel received.'),
        tags$li('Chatters: The number of unique viewers who chatted.'),
        tags$li('Chat.Messages: The number of chat messages sent.'),
        tags$li('Clips.Created: The number of clips created during your stream.'),
        tags$li('Clip.Views: The amount of new views on stream clips.')
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p('This app explores a personally compiled dataset of Stream Summary metrics provided by Twitch upon ending a stream. The data was 
        collected includes that from late 2023 (November) to early 2024 (March),'),
      p('This app allows the exploration of the relationship between varying variables against average viewership via scatter plots'),
      
      plotOutput('ScatThreeVar')
    )
  )
)

### Define server logic required to create scatter plot
server <- function(input, output) {
  
  #SCATTER PLOT
  
  # Extract the columns of intersect from the Boston dataset and save the data
  scatX <- reactive({
    numeric_data[, input$scatterX]
  })
  
  scatY <- reactive({
    numeric_data[, "Average.Viewers"]
  })
  
  scatCol <- reactive({
    numeric_data[, input$scatterCol]
  })
  
  # Make the Scatter Plot
  
  output$ScatThreeVar <- renderPlot({
    ggplot(data = numeric_data, aes(x = scatX(), y = scatY(), color = scatCol())) +
      geom_point() + 
      scale_color_gradient(high="purple3", low = "thistle") +
      stat_smooth(method = "lm", color="purple3")+
      xlab(input$scatterX) + ylab("Average Viewership") +
      labs(colour = input$scatterCol) +
      ggtitle(paste('Scatter Plot of', input$scatterX, 'vs Average Viewership')) +
      theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"), axis.title=element_text(size=15), axis.text=element_text(size=10))
  })
  
  
}



### Run the application 
shinyApp(ui = ui, server = server)


##  ------------------- Plotting Variables by R Squared Values -------------------------

### Rearranging numeric_data df (Average.Viewers column will be the last column)

cols_except_second <- setdiff(names(numeric_data), names(numeric_data)[2])
numeric_data <- numeric_data[, c(cols_except_second, names(numeric_data)[2])]

### Creating a df with r squared values
r_squared <-vector("numeric", length=7) 

for (i in 1:7) {
  colname <- colnames(numeric_data)[i]
  x<- numeric_data[,colname]
  y<- numeric_data$"Average.Viewers"
  m <- lm(Average.Viewers ~ x, data=numeric_data)
  s <- summary(m)            # get the summary of the model
  # extract every thing you need from the summary object
  r_squared[i] <- c(r.squared = s$r.squared) 
} 

### Creating df with factor names and respective r-squared values
colnames(numeric_data[1:7])
factor <- (colnames(numeric_data[1:7]))

r2_df <- data.frame(factor,r_squared)
r2_df <- r2_df[order(r2_df$r_squared, decreasing=TRUE),]

rownames(r2_df) <- NULL
r2_df


### Creating barplot with r-squared values
ggplot(data= r2_df, aes(x=reorder(factor, -r_squared), y= r_squared)) +
  geom_col(fill="purple4") + labs(title="R-Squared Values by Factor", subtitle="Linear Regression in Relation to Average Viewership") +
  xlab("Factor") +  ylab("R-squared") + ylim(0, .5) +
  theme(
    panel.grid.major = element_blank(),
    legend.position="none", 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1)
    
    )
    


