library(shinydashboard)
library(shiny)
library("dashboardthemes")



    
ui <- dashboardPage(
    dashboardHeader(title = span(span("Amenity Analytics", style = "color: #0080ff; font-size: 23px"), "Data Analysis"), titleWidth = 340),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Company Overview", tabName = "overview", icon = icon("chart-pie")),
            menuItem(
                "Polarity in Text Extraction",
                tabName = "expol",
                icon = icon("balance-scale")
            ),
            menuItem(
                "ESG Breakdown",
                tabName = "esg",
                icon = icon("handshake")
            ),
            menuItem(
                "Time Series of Reviews",
                tabName = "timeseries",
                icon = icon("chart-line")
            ),
            menuItem("Dataset", tabName = "dataset", icon = icon("database"))
        )
    ),
    ## Body content
    dashboardBody(
        tags$head(tags$style(
            HTML(
                '
        /* logo */
        .skin-blue .main-header .logo {
                              color: #ffffff;
                              background-color: #142334;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #142334;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #142334;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #142334;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #0080ff;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #142334;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #0080ff;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #0080ff;
                              }
                              '
            )
        )),
        
        
        
        tabItems(
            # Overview tab content
            tabItem(
                tabName = "overview",
                fluidRow(
                    # Static value boxes
                    valueBox(
                        paste0(overallScore, "%"),
                        "Overall Score",
                        icon = icon("tasks"),
                        color = "blue"
                    ),
                    valueBox(
                        paste0(workLifeScore, "%"),
                        "Work Life Score",
                        icon = icon("balance-scale"),
                        color = "blue"
                    ),
                    valueBox(
                        paste0(compensationScore, "%"),
                        "Compensation Score",
                        icon = icon("money-bill-wave"),
                        color = "blue"
                    ),
                    valueBox(
                        paste0(managementScore, "%"),
                        "Management Score",
                        icon = icon("briefcase"),
                        color = "blue"
                    ),
                    valueBox(
                        paste0(approvesCeo, "%"),
                        "CEO Approval",
                        icon = icon("smile"),
                        color = "blue"
                    ),
                    valueBox(
                        paste0(recommends, "%"),
                        "Would Recommend",
                        icon = icon("thumbs-up"),
                        color = "blue"
                    ),
                ),
                fluidRow(column(
                    width = 12,
                    box(plotlyOutput("recommendsCEO")),
                    box(plotlyOutput("recommendsCompany"))
                ), ),
                
                fluidRow(column(width = 12,
                                box((
                                    plotlyOutput("scoreBreakdown")
                                )), box(
                                    plotlyOutput("polarityOverview")
                                )))
                
                
            ),
            
            # Extraction and Polarity tab content
            tabItem(tabName = "expol",
                    fluidRow(box(
                        plotlyOutput("posExtractions")
                    ),
                    box(
                        plotlyOutput("negExtractions")
                    )),
                    fluidRow(
                        box(
                            selectInput(
                                "Extraction1",
                                "Select Input for Positive Reviews:",
                                choices = posExtractionsOptions,
                                selected = "opportunities"
                            )
                        ),
                        box(
                            selectInput(
                                "Extraction2",
                                "Select Input for Negative Reviews:",
                                choices = negExtractionsOptions,
                                selected = "management"
                            )
                        ),
                        
                        fluidRow(box(
                            DT::dataTableOutput("posExtractionSentences")
                        ),
                        box(
                            DT::dataTableOutput("negExtractionSentences")
                        ),)
                    )),
            
            # ESG tab content
            tabItem(
                tabName = "esg",
                fluidRow(box(plotlyOutput("posESG")),
                         box(plotlyOutput("negESG"))),
                fluidRow(box(width = 100,
                             plotlyOutput("esgAvgScores"))),
                fluidRow(box(
                    width = 100,
                    plotlyOutput("esgRecommandAppr")
                )),
            ),
            
            # Time Series tab content
            tabItem(tabName = "timeseries",
                    fluidRow(
                        box(width = 100,
                            plotlyOutput("avgScoreTs")),
                        box(width = 100,
                            plotlyOutput("avgRecandApproval"))
                    )),
            
            # Dataset tab content
            tabItem(tabName = "dataset",
                    DT::dataTableOutput("amenityData"))
        )
    )
)
