## Libraries ##
library(shiny)
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(clifro)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NOAA Buoy Data Visualizations"),
   
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("waveheight"),
         plotOutput("wavedir")
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ## Data ##
   data <- fread("https://www.ndbc.noaa.gov/data/5day2/46087_5day.txt")
   data <- data[2:nrow(data),]
   colnames(data)[1] <- "YY"
   
   ### Concatenate time ###
   data <- data %>% mutate_if(is.character, as.numeric)
   
   time.stamp <- data %>% 
     select(YY, MM, DD, hh, mm) %>% 
     mutate(time = make_datetime(YY, MM, DD, hh, mm))
   
   data <- cbind(data, time.stamp$time)
   colnames(data)[20] <- "time"
   
   ### Convert to Feet ###
   data$WVHT <- data$WVHT * 3.281
   
   ## Make Plots ##
   output$waveheight <- renderPlot({
     ggplot(data) +
       geom_line(aes(x = time, y = WVHT), size = 1) +
       scale_y_continuous(limits = c(0, max(data$WVHT)), 
                          breaks = seq(0, max(data$WVHT), by = max(data$WVHT) / 10)) +
       ylab("Wave Height (ft)") +
       xlab("Date") +
       theme_linedraw() +
       theme_light()
   })
   
   output$wavedir <- renderPlot({
     ggplot(data) +
       geom_line(aes(x = time, y = MWD), size = 1) +
       scale_y_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 45),
                          labels = paste0(c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"))) +
       ylab("Wave Direction") +
       xlab("Date") +
       theme_linedraw() +
       theme_light()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

