## Libraries ##
library(shiny)
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(clifro)
library(scales)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NOAA Buoy Data Visualizations"),
   
   # Show a plot of the generated distributio
   
   mainPanel(
     plotOutput("waveheight"),
     plotOutput("wavedir"),
     plotOutput("waveper")
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Get raw data  ##
  cflat <- fread("https://www.ndbc.noaa.gov/data/5day2/46087_5day.txt")
  cflat <- cflat[2:nrow(cflat),]
  cflat$buoy <- "Cape Flattery"
  colnames(cflat)[1] <- "YY"
  
  angpt <- fread("https://www.ndbc.noaa.gov/data/5day2/46267_5day.txt")
  angpt <- angpt[2:nrow(angpt),]
  angpt$buoy <- "Angeles Point"
  colnames(angpt)[1] <- "YY"

  data <- as.data.frame(rbind(cflat, angpt))
  
  ## Concatenate time ##
  names <- data$buoy
  data <- data[,1:19] %>% mutate_if(is.character, as.numeric)
  data$buoy <- names
  
  time.stamp <- data %>% 
    select(YY, MM, DD, hh, mm) %>% 
    mutate(time = make_datetime(YY, MM, DD, hh, mm))
  
  data <- cbind(data, time.stamp$time)
  colnames(data)[21] <- "time"
  
  ### Define time ###
  data$time <- as.POSIXct(data$time,  format = "%y%m%d %H:%M:%S", tz = "UTC")
  
  ### Convert time ###
  data$time <- with_tz(data$time, tzone = "America/Los_Angeles")
   
  ### Convert to Feet ###
  data$WVHT <- data$WVHT * 3.281

     ## Make Plots ##
   output$waveheight <- renderPlot({
     ggplot(data) +
       geom_line(aes(x = time, y = WVHT, color = buoy), size = 1) +
       scale_x_datetime(labels = date_format("%m%d %H:%M"), date_breaks = "1 day") +
       scale_y_continuous(limits = c(0, max(data$WVHT)), breaks = seq(0, max(data$WVHT), by = max(data$WVHT) / 10)) +
       ylab("Wave Height (ft)") +
       xlab("Date") +
       theme(text = element_text(size = 20)) +
       theme_linedraw() +
       theme_light()
   })
   
   output$wavedir <- renderPlot({
     ggplot(data) +
       geom_line(aes(x = time, y = MWD, color = buoy), size = 1) +
       scale_x_datetime(labels = date_format("%m%d %H:%M"), date_breaks = "1 day") +
       scale_y_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 45),
                          labels = paste0(c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"))) +
       ylab("Wave Direction") +
       xlab("Date") +
       scale_x_datetime(labels = date_format("%m%d %H:%M"), date_breaks = "1 day") +
       theme_linedraw() +
       theme_light()
   })
   
   output$waveper <- renderPlot({
     ggplot(data) +
       geom_line(aes(x = time, y = DPD, color = buoy), size = 1) +
       scale_x_datetime(labels = date_format("%m%d %H:%M"), date_breaks = "1 day") +
       scale_y_continuous(limits = c(0, max(data$DPD)), breaks = seq(0, max(data$DPD), by = max(data$DPD) / 10)) +
       ylab("Dominant Wave Period (sec)") +
       xlab("Date") +
       theme(text = element_text(size = 20)) +
       theme_linedraw() +
       theme_light()
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

