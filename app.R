## app.R ##
load("DataSources_16Aug.RData")
library(shiny)
library(shinydashboard)
library(devtools)
library(xts)
library(dplyr)
library(googleVis)
library(dygraphs)
library(streamgraph)
library(treemap)
library(bubbles)

###START THE APP

ui <- dashboardPage( 
        skin="yellow",
  dashboardHeader(
    title="Playing with Google Analytics Data",
    titleWidth = 450
  ),
  dashboardSidebar(
    radioButtons("radio", "Choose visualization period:",
                            c("Last 3 months" = "90",
                             "Last 6 months" = "180")),
    sidebarMenu(
      menuItem("Blog Post",href="http://www.analyticsforfun.com/2015/08/playing-with-r-shiny-dashboard-and.html?utm_source=shinyapps&utm_medium=referral&utm_campaign=playingGAapp", icon=icon("file-text-o")),
      menuItem("Source code for app",href="https://github.com/mcpasin/PlayingGoogleAnalyticsDataViz", icon=icon("github"))
    )
  ),
  dashboardBody(
    #boxes to be put in a row (or column)
    fluidRow(
      valueBoxOutput("sessionBox"),
      valueBoxOutput("goalBox"),
      valueBoxOutput("goalCRBox")
    ),
    
    fluidRow(
      box(title="Main KPI's trend (sessions & transactions on left axis; conversion rate on right axis)",
          status="primary",solidHeader = TRUE,dygraphOutput("dygraph"), height=480, width=12),
      box(title="Channels Performance (Transactions = size of bubbles)",status="primary",solidHeader = TRUE,
          htmlOutput("chart2"),width=12,height=450),
      box(title="Devices Share of Traffic over time",status="primary",solidHeader = TRUE,
          streamgraphOutput("streamgraph"), height=500),
      box(title="Devices & OS (Sessions = size of rectangles)",status="primary",solidHeader = TRUE,
          plotOutput("tree"), height=500),
      box(title="Mobile Screen Resolutions (Sessions = size of bubbles)",status="primary",solidHeader = TRUE,
          bubblesOutput("bubbles",width = "90%"),height=550)
     
      )

)
)

server <- function(input, output) { 
  
  
  
  output$sessionBox <- renderValueBox({
    valueBox(
      format(sum(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),sessions)),format="d",big.mark=","), 
             "Sessions", icon = icon("area-chart"), color = "blue")
  })
  
  output$goalBox <- renderValueBox({
    valueBox(
      format(sum(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),goal10Completions)),format="d",big.mark=","), 
             "Transactions", icon = icon("shopping-cart"), color = "blue")
  })
  
  output$goalCRBox <- renderValueBox({
    valueBox(
      paste(round(mean(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),goal10ConversionRate)[,1]),2),"%"), "Conversion rate", 
      icon = icon("shopping-cart"), color = "blue")
  })
  
  
  output$chart2<- renderGvis({
    
    ga3Sub<-select(subset(ga3, date>=max(ga3$date)-as.numeric(input$radio)),-date)
    by_channel <- group_by(ga3Sub, channelGrouping)
    ga3Sub<-summarise(by_channel, sum(sessions), mean(pageviewsPerSession),sum(goal10Completions))
    names(ga3Sub)<-c("channelGrouping","sessions","pageviewsPerSession","goal10Completions")
    Bubble <- gvisBubbleChart(ga3Sub, idvar="channelGrouping", 
                              xvar="sessions", yvar="pageviewsPerSession",
                              colorvar="channelGrouping", sizevar="goal10Completions",
                              options=list( 
                                #title="Channels Performance (Transactions = Size of the Bubble)",
                                vAxis="{title: 'Pages per Session'}",
                                hAxis="{title: 'Sessions'}",
                                width=990, height=350,
                                legend = T))
    Bubble
  })
    
  output$dygraph <- renderDygraph({
    
    ga1Sub<-subset(ga1, date>=max(ga1$date)-as.numeric(input$radio))
    
    ses <- zoo(ga1Sub$sessions,ga1Sub$date)
    con<-zoo(ga1Sub$goal10Completions,ga1Sub$date)
    cr<-zoo(ga1Sub$goal10ConversionRate,ga1Sub$date)
    ga2Sub<-cbind(ses, con,cr)
    
    
    dygraph(ga2Sub)%>% dySeries("ses", label = "sessions",axis="y",stepPlot=TRUE,fillGraph=TRUE) %>% dySeries("con",fillGraph=TRUE, label = "transactions",axis="y") %>% dySeries("cr", label = "conversion rate",axis="y2",strokePattern = "dashed") %>% dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>% dyLegend(width = 400)%>% dyRangeSelector()
  })
  
  output$streamgraph <- renderStreamgraph({
    
    ga2Sub<-subset(ga2, date>=max(ga2$date)-as.numeric(input$radio))
    ga2Sub %>%
     streamgraph("deviceCategory", "sessions", "date",interactive=TRUE,offset="expand",interpolate="linear") %>% sg_colors("PuOr")%>%
      sg_legend(TRUE,"Channel of Traffic: ")
  })
  
  output$tree <- renderPlot({
  
    ga4Sub<-subset(ga4, date>=max(ga4$date)-as.numeric(input$radio))
    treemap(ga4Sub, 
          index=c("deviceCategory","operatingSystem"), 
          vSize="sessions", 
          type="index",fontsize.labels = c(12,9))
  })
  
  output$bubbles<-renderBubbles({
  bubbles(value = ga5Sub$sessions, label = ga5Sub$screenResolution,
          tooltip=paste(ga5Sub$sessions,"sessions"," "), 
          color = rainbow(10, alpha=NULL)[sample(10)])
  })
  
}

shinyApp(ui, server)
