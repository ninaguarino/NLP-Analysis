# Output functions
library(shiny)
server <- function(input, output) {
  # Datatable for dataset menu item
  output$amenityData <- DT::renderDataTable({
    DT::datatable(
      amenityData %>% arrange(desc(`Article Date`)),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        width = "100%",
        scrollX = TRUE
      )
    )
  })
  
  # Score breakdown for overview Blox Chart
  output$scoreBreakdown <- renderPlotly({
    fig <-
      plot_ly(
        type = "box",
        y = ~ uniqueReviewData$overallScore,
        name = "Overall"
      )
    fig <-
      fig %>% add_trace(y = ~ uniqueReviewData$workLifeScore,
                        name = "Work Life")
    fig <-
      fig %>% add_trace(y = ~ uniqueReviewData$compensationScore,
                        name = "Compensation")
    fig <-
      fig %>% add_trace(y = ~ uniqueReviewData$managementScore,
                        name = "Management")
    fig <- fig %>% layout(title = "Breakdown of Company Scores",
                          yaxis = list(title = 'Average Score'))
    fig
  })
  
  # Recommends Company Pie Chart
  output$recommendsCompany <- renderPlotly({
    fig <-
      plot_ly(
        uniqueReviewData,
        labels = ~ uniqueReviewData$recommends,
        type = 'pie',
        marker = list(colors = c( "#0080ff", "#1b4388"))
      )
    fig <- fig %>% layout(title = 'Recommends Company')
    fig
  })
  
  # Recommends CEO Pie Chart
  output$recommendsCEO <- renderPlotly({
    fig <-
      plot_ly(
        uniqueReviewData,
        labels = ~ uniqueReviewData$ceo_opinion,
        type = 'pie',
        marker = list(colors = c( "#1b4388", "#0080ff"))
      )
    fig <- fig %>% layout(title = 'Recommends CEO')
    fig
  })
  
  # Overview of Polarity Pie Chart
  output$polarityOverview <- renderPlotly({
    fig <-
      plot_ly(
        amenityData,
        labels = ~ amenityData$Polarity,
        type = 'pie',
        marker = list(colors = c( "#0080ff", "#1b4388"))
      )
    fig <- fig %>% layout(title = 'Polarity of Reviews')
    fig
  })
  
  # Top Neg Extractions Bar Plot
  output$negExtractions <- renderPlotly({
    fig <- plot_ly(
      negExtractions,
      x = ~ Extraction,
      y = ~ n,
      type = 'bar',
      color = I("#0080ff")
    )
    fig <-
      fig %>% layout(
        title = "Negative Text Extractions",
        xaxis = list(title = 'Text Extraction'),
        yaxis = list(title = 'Review Count')
      )
    fig
  })
  
  # Top Pos Extractions Bar Plot
  output$posExtractions <- renderPlotly({
    fig <- plot_ly(
      posExtractions,
      x = ~ Extraction,
      y = ~ n,
      type = 'bar',
      color = I("#0080ff")
    )
    fig <-
      fig %>% layout(
        title = "Positive Text Extractions",
        xaxis = list(title = 'Text Extraction'),
        yaxis = list(title = 'Review Count')
      )
    fig
  })
  
  # Positive Output Table
  output$posExtractionSentences <- DT::renderDataTable({
    DT::datatable(
      amenityData %>% filter(Polarity == "Positive" &
                               Extraction == input$Extraction1) %>% select(`Article Date`, Sentence) %>% arrange(desc(`Article Date`)),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        width = "100%",
        scrollX = TRUE
      )
    )
  })
  
  # Negative Output Table
  output$negExtractionSentences <- DT::renderDataTable({
    DT::datatable(
      amenityData %>% filter(Polarity == "Negative" &
                               Extraction == input$Extraction2) %>% select(`Article Date`, Sentence) %>% arrange(desc(`Article Date`)),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        width = "100%",
        scrollX = TRUE
      )
    )
  })
  
  # Time Series for Avg Scores
  output$avgScoreTs <- renderPlotly({
    fig <-
      plot_ly(
        timeSeriesData,
        x = ~ `Article Date`,
        y =  ~ overallScore,
        name = "Overall Score",
        type = "scatter",
        mode = "lines"
      )
    fig <-
      fig %>% add_trace(y = ~ workLifeScore,
                        name = 'Work Life Score',
                        mode = "lines")
    fig <-
      fig %>% add_trace(y = ~ compensationScore,
                        name = 'Compensation Score',
                        mode = "lines")
    fig <-
      fig %>% add_trace(y = ~ managementScore,
                        name = 'Management Score',
                        mode = "lines")
    
    fig <- fig %>% layout(
      title = "Time Series of Average Company Scores",
      xaxis = list(title = 'Review Date'),
      yaxis = list(title = 'Average Score')
    )
    fig
  })
  
  # Time Series for Avg CEO Approval and Company Recommendation
  output$avgRecandApproval <- renderPlotly({
    fig <-
      plot_ly(
        timeSeriesData,
        x = ~ `Article Date`,
        y =  ~ recommends,
        name = "Recommends",
        type = "scatter",
        mode = "lines") 
    fig <-
      fig %>% add_trace(y = ~ ceo_opinion,
                        name = 'CEO Approval',
                        mode = "lines")
    fig <- fig %>% layout(
      title = "Time Series of Average Company Recommendations and CEO Approval",
      xaxis = list(title = 'Review Date'),
      yaxis = list(title = 'Recommends Company / Approves CEO')
    )
    fig
  })
  
  # Top Neg ESG Bar Plot
  output$negESG <- renderPlotly({
    fig <- plot_ly(
      negESG,
      x = ~ ESG,
      y = ~ n,
      type = 'bar',
      color = I("#0080ff")
    )
    fig <-
      fig %>% layout(
        title = "Negative ESG Keywords",
        xaxis = list(title = 'ESG Keywords'),
        yaxis = list(title = 'Review Count')
      )
    fig
  })
  
  # Top Pos ESG Bar Plot
  output$posESG <- renderPlotly({
    fig <- plot_ly(
      posESG,
      x = ~ ESG,
      y = ~ n,
      type = 'bar',
      color = I("#0080ff")
    )
    fig <-
      fig %>% layout(
        title = "Positive ESG Keywords",
        xaxis = list(title = 'ESG Keywords'),
        yaxis = list(title = 'Review Count')
      )
    fig
  })
  
  # Top ESG Avg Scores
  output$esgAvgScores <- renderPlotly({
    fig <-
      plot_ly(
        esgScores,
        x = ~ ESG,
        y =  ~ overallScore,
        name = "Overall Score",
        type = "scatter",
        mode = "lines"
      )
    fig <-
      fig %>% add_trace(y = ~ workLifeScore,
                        name = 'Work Life Score',
                        mode = "lines")
    fig <-
      fig %>% add_trace(y = ~ compensationScore,
                        name = 'Compensation Score',
                        mode = "lines")
    fig <-
      fig %>% add_trace(y = ~ managementScore,
                        name = 'Management Score',
                        mode = "lines")
    
    fig <- fig %>% layout(
      title = "ESG Against Average Company Scores",
      xaxis = list(title = 'ESG Keywords'),
      yaxis = list(title = 'Average Score')
    )
    fig
  })
  
  # ESG vs Company Recommendations
  output$esgRecommandAppr <- renderPlotly({
    fig <-
      plot_ly(
        esgScores,
        x = ~ ESG,
        y =  ~ recommends,
        name = "Recommends Company",
        type = "scatter",
        mode = "lines"
      )
    fig <-
      fig %>% add_trace(y = ~ ceo_opinion,
                        name = 'CEO Approval',
                        mode = "lines")
    
    fig <- fig %>% layout(
      title = "ESG against Company Recommendation and CEO Approval",
      xaxis = list(title = 'ESC Keywords'),
      yaxis = list(title = 'Average Score')
    )
    fig
  })
  
}
