# Load libraries after ensuring they're installed
library("CarletonStats")
library("devtools")
library("epanetReader")
library("fmsb")
library("ggplot2")
library("ggthemes")
library("ggExtra")
library("latticeExtra")
library("MASS")
library("PerformanceAnalytics")
library("psych")
library("plyr")
library("prettyR")
library("plotrix")
library("proto")
library("RCurl")
library("reshape")
library("reshape2")
library("readxl")
library("RColorBrewer")

# Loading in data
stream_DF <- read.csv("Stream Session 3_22_2024.csv")

# Making tick marks for graph breaks
gg_breaks<- seq(0,80, by=5)

# Creating Marginal Histogram of time streamed vs chatters
scatter_plot<- ggplot(stream_DF, mapping=aes(as.numeric(rownames(stream_DF)), Chatters)) + 
  scale_x_continuous(breaks=gg_breaks) +
  geom_point() +
  labs(title="Chat Engagement over Time", subtitle="3/22/24 Stream") +
  xlab("Minutes Streamed") + ylab("Active Chatters") + 
  geom_point(aes(color=Chatters)) +
  scale_color_gradient("low" = "thistle3", "high" = "purple4") +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the main title horizontally
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey"),  # Set major gridlines color to black
    panel.grid.minor = element_line(color = "lightgrey")  # Set major gridlines color to black
  ) 
ggMarginal(scatter_plot, type = "histogram", fill="thistle1")

