# make sure current working directory contains shiny r script
#setwd("C:/Users/tommy/Google Drive/Coursework/times_series/shiny")

LoadPackages <- function(packages) {
  # Install and or load packages if not already done so
  #
  # Args:
  #   packages: a vector of package names
  #
  for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
      if (!package %in% installed.packages()) install.packages(package)
      library(package, character.only=T)
    }
  }
}

# LoadPackages(c("dplyr", 'ggplot2', 'DataCombine',
#                'zoo', 'shiny', 'plotly', "shinyWidgets",
#                'shinythemes', 'tidyr', 'rsconnect'))

library('dplyr')
library('ggplot2')
library('shiny')
library('plotly') # interactive plots
library('shinyWidgets') # pretty sidebar elements
library('shinythemes')
library('tidyr')
library('rsconnect')

# read in data
all_wells <- readRDS("./Data/all.rds")

################################################################################
# ui 
ui2 <- shinyUI(navbarPage(
  theme = shinytheme("superhero"),
  strong("Well Water Level App"),
  
  # first tab: all wells
  tabPanel("All Wells",
           sidebarLayout(
             sidebarPanel(
               # radio but for predicted or actual well water levels
               prettyRadioButtons("radio", label = h2("Select Type:"),
                                  choices = list("Actual" = "ACTUAL",
                                                 "Predicted" = "FORECAST"),
                                  selected = "ACTUAL",
                                  animation = "pulse",
                                  inline = T),
               # date range input
               dateRangeInput("dates",
                              label = h2("Select Dates:"),
                              start = "2007-10-01", end = "2018-06-01"),
               # well slection input
               prettyCheckboxGroup("checkGroup", label = h2("Select Wells:"), 
                                   choices = list("G-5610" = "G-561", 
                                                  "G-5800" = "G-580", 
                                                  "G-8600" = "G-860", 
                                                  "G-2147" = "G-2147", 
                                                  "G-2866" = "G-2866", 
                                                  "G-8520" = "G-852", 
                                                  "G-1260" = "G-1260", 
                                                  "G-3549" = "G-3549", 
                                                  "F-4500" = "F-45", 
                                                  "F-1790" = "F-179", 
                                                  "F-3190" = "F-319",
                                                  "All" = "all_wells"),
                                   selected = "G-561",
                                   animation = "pulse",
                                   inline = TRUE),
               # powered by image
               h3("Powered By:"),
               tags$img(src = 'Alien.jpg', height = 80, height = 80)
             ),
             mainPanel(plotlyOutput("lineplot"))  # plotly for interactive plots
           )
  ),
  
  # 2nd tab: individual well
  tabPanel("Individual Well",
           sidebarLayout(
             sidebarPanel(
               # check boxes for actual, forecast and lag              
               prettyCheckboxGroup("checkGroup2", label = h2("Select Type:"), 
                                   choices = list("Actual" = "ACTUAL", 
                                                  "Predicted" = "FORECAST", 
                                                  "Interval" = "Interval", 
                                                  "Lag 1" = "LAG_USED"),
                                   selected = "ACTUAL",
                                   animation = "pulse",
                                   inline = T),               
               # date range input
               dateRangeInput("dates2",
                              label = h2("Select Dates:"),
                              start = "2007-10-01", end = "2018-06-01"),
               # radio buttons for wells
               prettyRadioButtons('radio2', label = h2('Select Well:'),
                                  choices = list("G-5610" = "G-561", 
                                                 "G-5800" = "G-580", 
                                                 "G-8600" = "G-860", 
                                                 "G-2147" = "G-2147", 
                                                 "G-2866" = "G-2866", 
                                                 "G-8520" = "G-852", 
                                                 "G-1260" = "G-1260", 
                                                 "G-3549" = "G-3549", 
                                                 "F-4500" = "F-45", 
                                                 "F-1790" = "F-179", 
                                                 "F-3190" = "F-319"),
                                  selected = "G-561",
                                  animation = "pulse",
                                  inline = TRUE),
               # powered by image
               h3("Powered By:"),
               tags$img(src = 'Alien.jpg', height = 80, height = 80)
             ),
             mainPanel(
               plotlyOutput("well_select"),
               h4(strong('| ')), 
               plotlyOutput("residual")
             )
           )
  )
)
)



###############################################################################
# server

server2 <- function(input, output) {
  
  # all wells line plot
  output$lineplot <- renderPlotly({   
    #filter for dates and wells
    all_wells <- all_wells %>%
      filter(DATE >= input$dates[1] &
               DATE <= input$dates[2])# %>%
      #filter(Well %in% input$checkGroup)
    
    # if 'all wells' is not checked then filter wells to only include
    # the ones selected
    if(!("all_wells" %in% input$checkGroup)){
      all_wells <- all_wells %>% filter(Well %in% input$checkGroup)
    }
    
    # line plot for all wells
    p11 <-  ggplot(all_wells, aes_string(x = 'DATE', y = input$radio, 
                                         colour = 'Well')) +
      geom_line(size = .7) +
      labs(title = "Well Water Level",
           x = 'Date', y = 'Water Level (Ft)') +
      theme_bw()  +
      theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
            axis.text = element_text(size =14),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 12),
            legend.title=element_blank())
    # create interactive plot
    ggplotly(p11) 
    
  }
  )
  
  # individual well line plot
  output$well_select <- renderPlotly({
    # filter for well selected and dates
    well <- all_wells %>% filter(Well %in% input$radio2) %>%
      filter(DATE >= input$dates2[1] &
               DATE <= input$dates2[2])
    
    # create plot
    p <- ggplot(well, aes(x= DATE)) 
    
    # add layers based on conditions 
    if("ACTUAL" %in% input$checkGroup2){
      p <- p + geom_line(aes(y = ACTUAL, colour = "Actual"), size = .8)
    }
    
    if("FORECAST" %in% input$checkGroup2){
      p <- p + geom_line(aes(y = FORECAST, colour = "Predicted"), size = .8)
    }
    
    if("Interval" %in% input$checkGroup2){
      p <- p + geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.15)
    }
    
    if("LAG_USED" %in% input$checkGroup2){
      p <- p + geom_line(aes(y = LAG_USED, colour = "Lag 1"), size = .6)
    }
    
    # plot and add stuff
    p <- p + labs(title = "Well Water Level",
                  x = 'Date', y = 'Water Level (Ft)', color = "Type") +
      theme_bw()  +
      theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
            axis.text = element_text(size =14),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 14),
            legend.title=element_blank())
    # make ggplot interactive
    ggplotly(p)
  }
  )
  
  # Residual plot
  output$residual <- renderPlotly({
    # filter for only well selected and dates
    well <- all_wells %>% filter(Well %in% input$radio2) %>%
      filter(DATE >= input$dates2[1] &
               DATE <= input$dates2[2])
    # add mean abs error and mean error columns
    well$`Abs Error` <- mean(abs(well$RESIDUAL), na.rm = T)
    well$`Avg Error` <- mean(well$RESIDUAL, na.rm = T)
    # transform from wide to long
    well2 <- well %>% 
      rename("Error" = RESIDUAL) %>%
      select(DATE, Error, "Abs Error", "Avg Error") %>% 
      gather(key = Type, value = Value, -DATE)
    
    # create residual plot
    p2 <- ggplot(well2, aes(x = DATE, y = Value, color = Type)) + 
      geom_line() +
      labs(title = "Model Error",
           x = 'Date', y = 'Error') +
      theme_bw()  +
      theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
            axis.text = element_text(size =14),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 14),
            legend.title=element_blank())
    # plot the residual plot if they are not all NA otherwise 
    if(!(all(is.na(well$RESIDUAL)))){
      ggplotly(p2) 
    } else {
      ggplot(data = well, aes(x= DATE)) + 
        ggtitle("Future Model Error Not Available :(") + 
        theme_bw() +
        theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"))
    }
    
  })
  
}

# Create the Shiny app object
shinyApp(ui = ui2, server = server2)