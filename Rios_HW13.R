# Loading libraries
library(animation) # For creating plots
library(ggplot2) # For creating animations
library(RColorBrewer) # For creating a color palette

# Loading in files ----------------------------------------------

## Saving name of folder containing stream summary metrics to variable
data_folder <- "Stream Summaries"

## Creating list of CSV files in the folder
csv_files <- list.files(path = data_folder, pattern="*.csv", full.names=TRUE)
csv_files

# Renaming and ordering files by date ---------------------------

## Creating functions to extract and format the dates within file names

### For file names/file ordering
extract_date <- function(file) {
  date_str <- gsub(".*from ([0-9]+_[0-9]+_[0-9]+) to [0-9]+_[0-9]+_[0-9]+\\.csv", "\\1", file)
  date_parts <- unlist(strsplit(date_str, "_"))
  paste0(date_parts[3], "_", date_parts[1], "_", date_parts[2])
}

### For graph subtitle- in YYYY/MM/DD format
extract_date_modified <- function(index) {
  date_str <- extract_date(sorted_files[index])
  styled_stream_date <- gsub("_", "/", date_str)
}

## Extracting the formatted dates from files into vector formatted_dates
formatted_dates <- sapply(csv_files, extract_date)

formatted_dates


## Sorting file names based on formatted start dates 
sorted_files <- csv_files[order(as.Date(formatted_dates, format = "%Y_%m_%d"))]
sorted_files

## Creating new folder"Ordered Stream Summaries"- if it doesn't already exist!
destination_folder <- "Ordered Stream Summaries/"

if(!dir.exists(destination_folder)) {
  dir.create(destination_folder, recursive = TRUE)
}

## Renaming files in format "Session i" in order to "Ordered Stream Summaries" 
for (i in seq_along(sorted_files)) {
  new_name <- paste0(destination_folder, "Session_", i, ".csv") # New name
  file.copy(sorted_files[i], new_name)
}


# Creating functions to calculate max values for plot limits ---------

## Time/minutes streamed; equal to row #
find_max_time <- function() {
  
  max_row <- 0 # Initializing var
  
  for (file in csv_files){ # Creating loop to locate/update highest row
    df <-read.csv(file)
    max_row_file <-nrow(df)
    
    # If the value is greater- update the max time
    if (max_row_file > max_row){
      max_row <- max_row_file
    }
    

  }
  return(max_row) # Return the max value
}

## Dependent variable of choice- must be fed as a string! Ex.: "Chatters"
find_max_y <- function(dependent) { 
  
  max_val <- 0 # Initializing variable
  
  for (file in csv_files){
    df <-read.csv(file)
    max_val_file <- max(df[[dependent]])

    if (max_val_file > max_val){
      max_val <- max_val_file
    }
    
  }
  
  return(max_val)
}


# Creating Scatter Plot GIFs -------------------------------------

# Checking max values for limits for variables
max_time <- find_max_time()  # X-axis: time
max_dep <- find_max_y("Chatters") # Y-axis: chosen variable (ex.: "Chatters")

saveGIF({
  
  ## Setting interval between frames
  ani.options(interval = 0.5)
  
  ## Creating graphs/frames
  for(i in 1:10){
    ### Accessing File
    file_path <- paste0(destination_folder, "Session_", i, ".csv") 
    
    ### Converting File to DF for use with ggplot 
    file_df <- read.csv(file_path) 
    
    ### Making scatterplot from file DF
    plot <- ggplot(file_df, aes(as.numeric(rownames(file_df)), Chatters)) + 
      geom_point(aes(color=Chatters)) + # Added points

      ### Theme (and legend breaks!)
      scale_color_gradient("low" = "thistle3", "high" = "purple4", limits= c(0, max_dep), breaks= seq(0, max_dep, by = 2)) +
        theme(
          plot.title = element_text(hjust = 0.5),  # Center the main title horizontally
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major = element_line(color = "grey"),  # Set major gridlines color to black
          panel.grid.minor = element_line(color = "lightgrey")  # Set major gridlines color to black
        ) +
      
      ### Axes limits and tick marks
      scale_x_continuous(limits = c(0, max_time), breaks=seq(0, max_time, by=20)) +
      scale_y_continuous(limits = c(0, max_dep), breaks=seq(0, max_dep, by=1)) +
    
      
    
      ### Labels
      labs(title="Chat Engagement over Time", subtitle=paste0("Stream Date: ", extract_date_modified(i))) +
      xlab("Minutes Streamed") + ylab("Active Chatters")
        
      
      
      
    print(plot)
    }
}, 

## Set name of GIF
movie.name = "stream_scatterplots.gif")


