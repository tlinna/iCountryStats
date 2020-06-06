
# iCountryStats app by Tapani Linnaluoto

library(shiny)
library(tidyverse)
library(lubridate)
library(countrycode)
library(fs)

# Prepare data after downloading and restarting, or just load prepared data

source("source/download_prepare.R")

# Define UI for application

ui <- navbarPage("iCountryStats",
                 
                 # Tab bar with different pages
                 
                 
                 
                 tabPanel("iRating distribution",
                          
                          tags$head(
                            tags$style(HTML("body { overflow-y: scroll; }"))
                          ),
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              uiOutput("iRSidebarDetails")
                              
                            ),  
                            
                            mainPanel(
                              
                              plotOutput("iRatingDistribution", height = "500px")
                              
                            )
                          )
                          
                 ),
                 tabPanel("Safety rating distribution",
                          sidebarLayout(
                            sidebarPanel(
                              
                              uiOutput("SRSidebarDetails")
                              
                            ),
                            
                            mainPanel(
                              
                              plotOutput("SRDistribution", height = "500px")
                              
                            )
                          )
                 ),
                 tabPanel("Win rate / iRating",
                          sidebarLayout(
                            sidebarPanel(
                              
                              uiOutput("uiWR")
                              
                            ),
                            mainPanel(
                              
                              plotOutput("winRate", height = "600px", click = "plotClickWR"),
                              tableOutput("clickOutputWR")
                              
                            )
                          )
                          
                          
                 ),
                 tabPanel("Combined Road & Oval",
                          sidebarLayout(
                            sidebarPanel(
                              
                              uiOutput("uiCombined")
                              
                            ),
                            mainPanel(
                              
                              plotOutput("combinedPlot", height = "300px", click = "plotClickCombined"),
                              tableOutput("clickOutputCombined")
                              
                            )
                          )
                 )
)

# Define server logic required

server <- function(input, output) {
  
  combinedClickCoord <- reactiveValues(x = NULL, y = NULL)
  
  output$uiCombined <- renderUI({
    
    req(listOfCountries)
    
    return(
      list(
        selectInput("countryCombined", label = "Select country", listOfCountries, selected = "All countries"),
        selectInput("driverCutCombined", label = "Drivers with at least this many starts",
                    choices = c(0, 25, 50, 100, 250, 500, 1000), selected = 50)
      )
    )
    
    
  })
  
  output$iRSidebarDetails <- renderUI({
    
    req(listOfCountries)
    return(
      list(
        selectInput("countryIR", label = "Select country", listOfCountries, selected = "All countries"),
        selectInput("binWidthIR", label = "Bin width", choices = c(10, 25, 50, 100, 250, 500), selected = 100),
        selectInput("driverCutIR", label = "Drivers with at least this many starts", choices = c(0, 25, 50, 100, 250, 500, 1000), selected = 50),
        radioButtons("roadOrOvalIR", label = NULL, choices = c("Road", "Oval"), selected = "Road"),
        p("Includes all non-rookie drivers, including drivers with inactive subscription."),
        p("Database updated ", paste0(fileTime))
      )
    )
  })
  
  combinedDriversFiltered <- reactive({
    
    req(input$driverCutCombined, input$countryCombined)
    
    if(input$countryCombined == "All countries") {
      drivers <- combinedDrivers
    } else {
      drivers <- filter(combinedDrivers, Country == input$countryCombined)
    }
    
    drivers <- filter(drivers, Road.Starts >= as.numeric(input$driverCutCombined),
                      Oval.Starts >= as.numeric(input$driverCutCombined))
    
    return(drivers)
    
  })
  
  clickHead <- reactive({
    
    input$plotClickCombined
    
    isolate({
      combinedClickCoord <- input$plotClickCombined
      
      clickDrivers <- combinedDriversFiltered()
      clickDrivers <- filter(clickDrivers,
                             Road.iRating >= combinedClickCoord$x - 500,
                             Road.iRating <= combinedClickCoord$x + 500,
                             Oval.iRating >= combinedClickCoord$y - 500,
                             Oval.iRating <= combinedClickCoord$y + 500)
      
      clickDrivers$distanceRoad <- abs(clickDrivers$Road.iRating - combinedClickCoord$x)
      clickDrivers$DistanceOval <- abs(clickDrivers$Oval.iRating - combinedClickCoord$y)
      clickDrivers$DistanceAvg <- sqrt(clickDrivers$distanceRoad^2 + clickDrivers$DistanceOval^2)
      clickDrivers <- clickDrivers[order(clickDrivers$DistanceAvg),]
    })
    
    return(head(clickDrivers, 5))
    
    
    
    
  })
  
  output$combinedPlot <- renderPlot({
    
    drivers <- combinedDriversFiltered()
    
    plot <- ggplot(drivers, aes(x = Road.iRating, y = Oval.iRating)) +
      theme_minimal(base_size = 14) +
      geom_point(alpha = 0.4) +
      scale_x_continuous(breaks = seq(0, 11000, 1000),
                         limits = c(0, 11000)) +
      scale_y_continuous(breaks = seq(0, 11000, 1000),
                         limits = c(0, 11000))
    
    # clickD <- clickHead()
    # targetDriver <- head(clickD, 1)
    # plot <- plot +
    #   geom_segment(aes(x = 0, y = 0, xend = targetDriver$Road.iRating, yend = targetDriver$Oval.iRating),
    #                color = "red")
    # 
    return(plot)
    
  })
  
  output$clickOutputCombined <- renderTable({
    
    req(input$plotClickCombined)
    
    clickD <- clickHead()
    
    clickD <- clickD[c("Driver", "Country", "Road.iRating", "Oval.iRating", "Road.Starts", "Oval.Starts")]
    colnames(clickD) <- c("Driver", "Country", "Road iRating", "Oval iRating", "Road starts", "Oval starts")
    
    return(clickD)
    
  })
  
  output$SRSidebarDetails <- renderUI({
    
    req(listOfCountries)
    return(
      list(
        selectInput("countrySR", label = "Select country", listOfCountries, selected = "All countries"),
        selectInput("binWidthSR", label = "Bin width", choices = c(0.01, 0.05, 0.1, 0.125, 0.25, 0.5, 1), selected = 0.1),
        selectInput("driverCutSR", label = "Drivers with at least this many starts", choices = c(0, 25, 50, 100, 250, 500, 1000), selected = 50),
        radioButtons("roadOrOvalSR", label = NULL, choices = c("Road", "Oval"), selected = "Road"),
        p("Includes all non-rookie drivers, including drivers with inactive subscription."),
        p("Database updated ", paste0(fileTime))
      )
    )
  })
  
  output$uiWR <- renderUI({
    
    return(
      list(
        radioButtons("roadOrOvalWR", label = NULL, choices = c("Road", "Oval"), selected = "Road", inline = T),
        selectInput("countryWR", label = "Select country", listOfCountries, selected = "All countries"),
        selectInput("driverCutWR", label = "Drivers with at least this many starts", choices = c(0, 25, 50, 100, 250, 500, 1000), selected = 50),
        textInput("searchDriverWR", label = "Search drivers", placeholder = "Greger Huttu"),
        actionButton("searchButtonWR", label = "Search"),
        p(),
        p("You can only search individuals with their exact iRacing name."),
        p("Click on the plot to see drivers closest to your mouse cursor.")
      )
    )
    
  })
  
  output$iRatingDistribution <- renderPlot({
    
    req(roadDrivers, ovalDrivers, input$roadOrOvalIR, input$countryIR, input$binWidthIR, input$driverCutIR)
    
    withProgress(message = "Plotting...", {
      
      if(input$roadOrOvalIR == "Road") {
        if(input$countryIR == "All countries") {
          drivers <- filter(roadDrivers, Starts >= as.numeric(input$driverCutIR))
        } else {
          drivers <- filter(roadDrivers, Starts >= as.numeric(input$driverCutIR), Country == input$countryIR)
        }
      }
      if(input$roadOrOvalIR == "Oval") {
        if(input$countryIR == "All countries") {
          drivers <- filter(ovalDrivers, Starts >= as.numeric(input$driverCutIR))
        } else {
          drivers <- filter(ovalDrivers, Starts >= as.numeric(input$driverCutIR), Country == input$countryIR)
        }
      }
      incProgress(0.5, message = "Plotting...")
      
      plot <- ggplot(drivers, aes(x = iRating)) +
        theme_minimal(base_size = 14) +
        geom_histogram(aes(fill = DriverClass), binwidth = as.numeric(input$binWidthIR), boundary = 0) +
        labs(x = "iRating",
             y = "Count") +
        scale_x_continuous(breaks = seq(0,11000,1000),
                           limits = c(0, 11000)) +
        scale_fill_manual(values = c(D = "orange",
                                     C = "yellow",
                                     B = "green",
                                     A = "dodgerblue",
                                     P = "black"),
                          guide_legend(title = "Class")) +
        theme(legend.position = c(.97, .95),
              legend.justification = c("right", "top"))
      incProgress(0.4, message = "Plotting...")
      
    })
    return(plot)
    
  })
  
  output$SRDistribution <- renderPlot({
    
    req(roadDrivers, ovalDrivers, input$roadOrOvalSR, input$countrySR, input$binWidthSR, input$driverCutSR)
    
    withProgress(message = "Plotting...", {
      
      if(input$roadOrOvalSR == "Road") {
        if(input$countrySR == "All countries") {
          drivers <- filter(roadDrivers, Starts >= as.numeric(input$driverCutSR))
        } else {
          drivers <- filter(roadDrivers, Starts >= as.numeric(input$driverCutSR), Country == input$countrySR)
        }
      }
      if(input$roadOrOvalSR == "Oval") {
        if(input$countrySR == "All countries") {
          drivers <- filter(ovalDrivers, Starts >= as.numeric(input$driverCutSR))
        } else {
          drivers <- filter(ovalDrivers, Starts >= as.numeric(input$driverCutSR), Country == input$countrySR)
        }
      }
      incProgress(0.5, message = "Plotting...")
      
      plot <- ggplot(drivers, aes(x = SR)) +
        theme_minimal(base_size = 14) +
        geom_histogram(aes(fill = DriverClass), binwidth = as.numeric(input$binWidthSR), boundary = 0) +
        labs(x = "Safety rating",
             y = "Count") +
        scale_x_continuous(breaks = seq(0, 5, 1),
                           limits = c(0, 5)) +
        scale_fill_manual(values = c(D = "orange",
                                     C = "yellow",
                                     B = "green",
                                     A = "dodgerblue",
                                     P = "black"),
                          guide_legend(title = "Class")) +
        theme(legend.position = c(.9, .95),
              legend.justification = c("right", "top"))
      incProgress(0.4)
      
    })
    return(plot)
    
  })
  
  output$winRate <- renderPlot({
    
    req(roadDrivers, ovalDrivers, input$roadOrOvalWR, input$countryWR, input$driverCutWR)
    
    withProgress(message = "Plotting...", {
      
      if(input$roadOrOvalWR == "Road") {
        if(input$countryWR == "All countries") {
          drivers <- filter(roadDrivers, Starts >= as.numeric(input$driverCutWR))
        } else {
          drivers <- filter(roadDrivers, Starts >= as.numeric(input$driverCutWR), Country == input$countryWR)
        }
      }
      if(input$roadOrOvalWR == "Oval") {
        if(input$countryWR == "All countries") {
          drivers <- filter(ovalDrivers, Starts >= as.numeric(input$driverCutWR))
        } else {
          drivers <- filter(ovalDrivers, Starts >= as.numeric(input$driverCutWR), Country == input$countryWR)
        }
      }
      incProgress(0.5)
      
      plot <- ggplot(drivers, aes(x = iRating, y = Win.rate)) +
        theme_minimal(base_size = 14) +
        geom_point(aes(colour = DriverClass), alpha = 0.5) +
        labs(x = "iRating",
             y = "Wins / Number of starts") +
        scale_x_continuous(limits = c(0, 11000),
                           breaks = seq(0, 11000, 1000)) +
        scale_y_continuous(limits = c(0, 1),
                           breaks = seq(0, 1, 0.1)) +
        scale_color_manual(values = c(D = "orange",
                                      C = "yellow",
                                      B = "green",
                                      A = "dodgerblue",
                                      P = "black"),
                           guide_legend(title = "Class")) +
        theme(legend.position = c(.97, .97),
              legend.justification = c("right", "top"))
      incProgress(0.5)
      
      input$searchButtonWR
      isolate({ 
        if(!is.null(input$searchDriverWR)) {
          plot <- plot +
            geom_point(size = 5, colour = "red", data = filter(drivers, Driver == input$searchDriverWR))
        }
      })
      return(plot)
    })
  })
  
  output$clickOutputWR <- renderTable({
    
    req(input$plotClickWR)
    
    if(input$roadOrOvalWR == "Road") {
      clickDrivers <- filter(roadDrivers,
                             iRating > input$plotClickWR$x - 250,
                             iRating < input$plotClickWR$x + 250,
                             Win.rate > input$plotClickWR$y - 0.025,
                             Win.rate < input$plotClickWR$y + 0.025,
                             Starts >= as.numeric(input$driverCutWR))  
    }
    if(input$roadOrOvalWR == "Oval") {
      clickDrivers <- filter(ovalDrivers,
                             iRating > input$plotClickWR$x - 250,
                             iRating < input$plotClickWR$x + 250,
                             Win.rate > input$plotClickWR$y - 0.025,
                             Win.rate < input$plotClickWR$y + 0.025,
                             Starts >= as.numeric(input$driverCutWR))
    }
    if(input$countryWR != "All countries") {
      clickDrivers <- filter(clickDrivers, Country == input$countryWR)
    }
    
    clickDrivers$distanceiR <- abs(clickDrivers$iRating - input$plotClickWR$x)
    clickDrivers$DistanceWR <- abs(clickDrivers$Win.rate - input$plotClickWR$y) * 10000
    clickDrivers$DistanceAvg <- sqrt(clickDrivers$distanceiR^2 + clickDrivers$DistanceWR^2)
    clickDrivers <- clickDrivers[order(clickDrivers$DistanceAvg),]
    clickDrivers <- head(clickDrivers, 5)
    clickDrivers <- clickDrivers[c("Driver","Country","Starts","Wins","iRating","Class", "Win.rate")]
    
    return(clickDrivers)
    
  })
  
}





# Run the application 
shinyApp(ui = ui, server = server)

