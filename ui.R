library(shinydashboard)
library(shiny)
conflict_prefer("box", "shinydashboard")

ui <- dashboardPage(
    dashboardHeader(title = "NLP Data Analysis", titleWidth = 230),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
            menuItem(
                "Extraction and Polarity",
                tabName = "expol",
                icon = icon("balance-scale")
            ),
            menuItem(
                "Time Series of Reviews",
                tabName = "timeseries",
                icon = icon("chart-line")
            ),
            menuItem(
                "ESG Breakdown",
                tabName = "esg",
                icon = icon("handshake")
            ),
            menuItem("Dataset", tabName = "dataset", icon = icon("database"))
        )
    ),
    ## Body content
    dashboardBody(tabItems(
        # Overview tab content
        tabItem(
            tabName = "overview",
            fluidRow(
                # Static value boxes
                valueBox(paste0(overallScore, "%"), "Overall Score", icon = icon("tasks")),
                valueBox(
                    paste0(workLifeScore, "%"),
                    "Work Life Score",
                    icon = icon("balance-scale")
                ),
                valueBox(
                    paste0(compensationScore, "%"),
                    "Compensation Score",
                    icon = icon("money-bill-wave")
                ),
                valueBox(
                    paste0(managementScore, "%"),
                    "Management Score",
                    icon = icon("briefcase")
                ),
                valueBox(paste0(approvesCeo, "%"), "CEO Approval", icon = icon("smile")),
                valueBox(paste0(recommends, "%"), "Would Recommend", icon = icon("thumbs-up")),
            ),
            fluidRow(column(
                width = 12,
                box(plotlyOutput("recommendsCEO")),
                box(plotlyOutput("recommendsCompany"))
            ),),
            
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
                    ), )
                )),
        
        # Time Series tab content
        tabItem(tabName = "timeseries",
                fluidRow(
                    box(width = 100,
                        plotlyOutput("avgScoreTs")),
                    box(width = 100,
                        plotlyOutput("avgRecandApproval"))
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
        
        # Dataset tab content
        tabItem(tabName = "dataset",
                DT::dataTableOutput("amenityData"))
    ))
)
