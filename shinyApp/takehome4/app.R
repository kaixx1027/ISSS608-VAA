pacman::p_load(shiny, tidyverse, shinydashboard)

visualdata <- read_csv("data/ResidentialRental_Final.csv")

ui <- dashboardPage(
  dashboardHeader(title = 'Rental market learning and valuation', titleWidth = 400),
  dashboardSidebar(width = 400,
                   sidebarMenu(id = 'a',
                               menuItem('Historical data', tabName = 'historical', icon = icon("search")),
                               menuItem('Statistics', tabName = 'Statistics', icon = icon("line-chart"))
                   )
  ),
  dashboardBody(
    tabItems(
      #——————————————————————————————————————————————————————————————————————Historical data
      tabItem(tabName = "historical",
              fluidPage(
                titlePanel("Places to Rent"),
                selectInput("region", "Planning Region", choices = unique(visualdata$Planning_Region)),
                selectInput("type", "Property Type", choices = NULL),
                tableOutput("data")
              )
      ),
      tabItem(tabName = "Statistics",
              fluidPage(
                titlePanel("Statistics"),
                fluidRow(
                  column(width = 12,
                         tabsetPanel(
                           #——————————————————————————————————————————————————————————————————————Barchart
                           tabPanel("Bar Chart",
                                    box(
                                      radioButtons('xcol1',
                                                   label = tags$strong('Analyse Sales By:'),
                                                   choices = c('Property Type' = 'Property_Type',
                                                               'Planning Region' = 'Planning_Region'),
                                                   inline = TRUE)
                                    ),
                                    box(
                                      width = 12,
                                      height = 800,
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      plotOutput('barchart', height = 750)
                                    )
                           ),
                           #——————————————————————————————————————————————————————————————————————Boxplot1
                           tabPanel("Boxplot1", 
                                    box(
                                      width = 12,
                                      height = 800,
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      plotOutput('Boxplot1', height = 750)
                                    )
                           ),
                           #——————————————————————————————————————————————————————————————————————Boxplot2
                           tabPanel("Boxplot2", 
                                    box(
                                      width = 12,
                                      height = 800,
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      plotOutput('Boxplot2', height = 750)
                                    )
                           ),
                           #——————————————————————————————————————————————————————————————————————Boxplot3
                           tabPanel("Boxplot3", 
                                    box(
                                      width = 12,
                                      height = 800,
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      plotOutput('Boxplot3', height = 750)
                                    )
                           ),
                           #——————————————————————————————————————————————————————————————————————scatterplot1
                           tabPanel("Scatterplot1", 
                                    box(
                                      width = 12,
                                      height = 800,
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      plotOutput('Scatterplot1', height = 750)
                                    )
                           ),
                           #——————————————————————————————————————————————————————————————————————scatterplot2
                           tabPanel("Scatterplot2", 
                                    box(
                                      width = 12,
                                      height = 800,
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      plotOutput('Scatterplot2', height = 750)
                                    )
                           ),
                           #——————————————————————————————————————————————————————————————————————scatterplot3
                           tabPanel("Scatterplot3", 
                                    box(
                                      width = 12,
                                      height = 800,
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      plotOutput('Scatterplot3', height = 750)
                                    )
                           ),
                           #——————————————————————————————————————————————————————————————————————scatterplot matrix
                           tabPanel("Scatterplot matrix", 
                                    box(
                                      width = 12,
                                      height = 800,
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      plotOutput('matrix', height = 750)
                                    )
                           )
                         ), #tabsetPanel(
                  ) #column(
                ) #fluidRow(
              ), #fluidPage(
      )
    ) #tabItems(
  ) #dashboardBody(
) #dashboardPage(



# Define server logic required to draw a histogram
server <- function(input, output){
  #——————————————————————————————————————————————————————————————————————historical
  region <- reactive({
    filter(visualdata, Planning_Region == input$region)
  })
  observeEvent(region(), {
    choices <- unique(region()$Property_Type)
    updateSelectInput(inputId = "type", choices = choices)
  })
  type <- reactive({
    req(input$type)
    filter(region(), Property_Type == input$type)
  })
  
  #——————————————————————————————————————————————————————————————————————Statistics
  #——————————————————————————————————————————————————————————————————————barchart
  output$barchart <- renderPlot({
    analysis <- visualdata %>%
      group_by_(.dots = input$xcol1) %>%
      summarise(basket_value = mean(`Monthly_Rent_SGD`, na.rm = T))
    
    p <- ggplot(analysis, aes_string(y = 'basket_value', x = input$xcol1)) +
      geom_bar(aes_string(fill = input$xcol1), stat = 'identity') +
      labs(title = 'Average Rental Price', subtitle = paste('by', input$xcol1), 
           x = input$xcol1, y = 'Rental Price ($)',
           fill = input$xcol1)
    return(p)
  })
  #——————————————————————————————————————————————————————————————————————Boxplot1
  output$Boxplot1 <- renderPlot({
    p1 <- ggplot(visualdata, aes(x=`Planning_Region`, y= `Monthly_Rent_SGD`, fill=`Property_Type`)) +
      geom_boxplot() +
      facet_wrap(~`Planning_Region`, scales = "free") +
      labs(x="Planning Region", y="Monthly Rent")
    return(p1)
  })
  #——————————————————————————————————————————————————————————————————————Boxplot2
  output$Boxplot2 <- renderPlot({
    p2 <- ggplot(visualdata, aes(x=`Planning_Region`, y= `Monthly_Rent_SGD`)) +
      geom_boxplot() +
      facet_wrap(~`Property_Type`, scales = "free") +
      labs(x="Planning Region", y="Monthly Rent") +
      theme(axis.text.x = element_text(angle = 90))
    return(p2)
  })
  #——————————————————————————————————————————————————————————————————————Boxplot3
  output$Boxplot3 <- renderPlot({
    p3 <- ggplot(visualdata, aes(x=`Property_Type`, y= `Monthly_Rent_SGD`)) +
      geom_boxplot() +
      facet_wrap(~`Planning_Region`, scales = "free") +
      labs(x="Property Type", y="Monthly Rent") +
      theme(axis.text.x = element_text(angle = 90))
    return(p3)
  })
  #——————————————————————————————————————————————————————————————————————Scatterplot1
  output$Scatterplot1 <- renderPlot({
    p4 <- ggplot(visualdata, aes(x=`distance_to_school`, y=`Monthly_Rent_SGD`)) +
      geom_point(size=0.5) +
      scale_x_continuous(breaks = seq(0, 2, by = 0.2)) +
      coord_cartesian(xlim = c(0, 2)) +
      facet_grid(`Planning_Region` ~ `Property_Type`, scales = "free", space ="fixed") +
      labs(x='Distance to School', y = 'Monthly Rent') +
      theme(axis.text.x = element_text(angle = 90))
    return(p4)
  })
  #——————————————————————————————————————————————————————————————————————Scatterplot2
  output$Scatterplot2 <- renderPlot({
    p5 <- ggplot(visualdata, aes(x=`distance_to_mrt`, y=`Monthly_Rent_SGD`)) +
      geom_point(size=0.5) +
      scale_x_continuous(breaks = seq(0, 2, by = 0.2)) +
      coord_cartesian(xlim = c(0, 2)) +
      facet_grid(`Planning_Region` ~ `Property_Type`, scales = "free", space ="fixed") +
      labs(x='Distance to MRT', y = 'Monthly Rent') +
      theme(axis.text.x = element_text(angle = 90))
    return(p5)
  })
  #——————————————————————————————————————————————————————————————————————Scatterplot3
  output$Scatterplot3 <- renderPlot({
    p6 <- ggplot(visualdata, aes(x=`Floor_Area_SQFT_Avg`, y=`Monthly_Rent_SGD`)) +
      geom_point(size=0.5) +
      #  scale_x_continuous(breaks = seq(0, 2, by = 0.2)) +
      #  coord_cartesian(xlim = c(0, 2)) +
      facet_grid(`Planning_Region` ~ `Property_Type`, scales = "free", space ="fixed") +
      labs(x='Floor Area SQFT', y = 'Monthly Rent') +
      theme(axis.text.x = element_text(angle = 90))
    return(p6)
  })  
  #——————————————————————————————————————————————————————————————————————matrix
  output$matrix <- renderPlot({
    pm <- pairs(~visualdata$Monthly_Rent_SGD + visualdata$Floor_Area_SQM_Avg + visualdata$Floor_Area_SQFT_Avg + visualdata$distance_to_mrt, data = visualdata,
                main = "Scatterplot Matrix")
    return(pm)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
