
#SiChen, sic2
#HW2
# create three 3 different kinds of figures
# one data table
# three 3 types of inputs
# one functioning downloadButton() 
# one observer in the server

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tibble)

#input data
ride <- read.csv('./ride.csv', head=TRUE)
ride$Starttime <- NULL
ride$Stoptime <- NULL
ride$From.station.id <- NULL
ride$To.station.id <- NULL
ride$From.station.name <- NULL
ride$To.station.name <- NULL
ride$Bikeid <- NULL

ride <- ride%>%
  mutate(speedlevel = as.character(speedlevel),
         Usertype = as.factor(Usertype))

pdf(NULL)

ui <- navbarPage("Healthy Ride in Pittsburgh", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Different speed duration select
                              selectInput("SpeedSelect",
                                          "Choose the Speed Duration:",
                                          choices = sort(unique(ride$speedlevel)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected=c("1~10","10~20","20~30")),
                              # Trip Duration Selection
                              sliderInput("TripdurationSelect",
                                          "Trip Duration:",
                                          min = min(ride$Tripduration, na.rm = T),
                                          max = max(ride$Tripduration, na.rm = T),
                                          value = c("80", "151106"),
                                          step = 1),
                              #Different Usertype select
                              checkboxGroupInput("UserSelect",
                                                 "User Type:",
                                                 choices = sort(unique(ride$Usertype)),
                                                 selected = c("Customer","Subscriber")),
                              actionButton("reset", "Reset Your Selection", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot1"),
                              plotlyOutput("plot2"),
                              plotlyOutput("plot3")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download the Ride History Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered Ride Data
  swInput <- reactive({
    ride.filter <- ride %>%
      # Slider tripduration Filter
      filter(Tripduration >= input$TripdurationSelect[1] & Tripduration <= input$TripdurationSelect[2])
    #selectinput speedlevel filter
    if (length(input$SpeedSelect) > 0 ) {
      # In general spacing here makes sense between the %in% and . 
      ride.filter <- subset(ride.filter, speedlevel %in% input$SpeedSelect)
    }
    # check box usertype Filter
    # I would have a similar if() statement as you have for SpeedSelect, as all the plots/tables break when I unselect both boxes.
    ride.filter <- subset(ride.filter, Usertype %in% input$UserSelect)

    return(ride.filter)
  })
  # Description comment missing
  mwInput <- reactive({
    swInput() %>%
      melt(id = "Usertype")
  })
  # Point plot showing speed, tripduration, and usertype
  output$plot1 <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = Speed, y = Tripduration, color = Usertype))+geom_point())})
  #point plot showing different usertype speed distribution
  # This one is a box plot! Be careful
  output$plot2 <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = Usertype, y = Speed))+geom_boxplot())})
  #point plot showing the amount of each usertype in various sppedlevel
  # This is a bar chart
  output$plot3 <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = Usertype,fill=speedlevel))+geom_bar())})
  
  # Data Table
  output$table <- DT::renderDataTable({
    ride.filter <- swInput()
    subset(ride.filter, select = c(Trip.id,Usertype,Tripduration, Speed,speedlevel))
  })
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("healthy-ride-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(swInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session,"SpeedSelect",selected = c("1~10","10~20","20~30"))
    updateSliderInput(session, "TripdurationSelect", value = c("80", "151106"))
    updateCheckboxGroupInput(session, "UserSelect", selected = c("Customer", "Subscriber"))
    showNotification("Hey! You did it! You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
