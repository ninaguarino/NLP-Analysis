# Input lists for interactive plots
library("readxl")
library(dplyr)
library(plotly)
library(tidyr)
library("reprex")
library(tidyverse)
#install.packages('rsconnect')
#rsconnect::setAccountInfo(name='ninaguarino', token='BF96CE5E667AF99349E4DCCB6E322028', secret='H56gFvJg6+TYjbnN5O+VHlJcUz+plqHzv4ffAydc')
#library(rsconnect)
#rsconnect::deployApp('~/Documents/AmenityAnalytics/NLP_Project')

#setwd("~/Documents/AmenityAnalytics/NLP_Project")

# Load Data
#amenityData <- read_excel("/Users/NinaGuarino/Documents/AmenityAnalytics/NLP_Project/AmenityGD_SampleData.xlsx")
amenityData <- read_excel("AmenityGD_SampleData.xlsx")
amenityData

# Drop unnecessary columns and correct polarity string values
amenityData <-
  amenityData[setdiff(colnames(amenityData), c('GlassdoorSection', 'REVIEWDATE'))]

amenityData <- amenityData %>% mutate(Polarity = case_when(
  Polarity == "POS" ~ "Positive",
  Polarity == "NEG" ~ "Negative",
  TRUE ~ Polarity
))
amenityData

# Collect reviews by employeeReviewID
uniqueReviewData <-
  amenityData[!duplicated(amenityData[, c("employeeReviewID")]),] %>% drop_na()
colnames(uniqueReviewData)

# Calculating overview percentages
overallScore = round(mean(uniqueReviewData$overallScore, na.rm = TRUE) * 20, 0)
workLifeScore = round(mean(uniqueReviewData$workLifeScore, na.rm = TRUE) * 20, 0)
compensationScore = round(mean(uniqueReviewData$compensationScore, na.rm = TRUE) * 20, 0)
managementScore = round(mean(uniqueReviewData$managementScore, na.rm = TRUE) * 20, 0)

recommends <-
  round(nrow(
    filter(uniqueReviewData, uniqueReviewData$recommends == "Yes")
  ) / sum(uniqueReviewData$recommends != "", na.rm = TRUE) * 100,
  0)
recommends

approvesCeo <-
  round(nrow(
    filter(uniqueReviewData, uniqueReviewData$ceo_opinion == "Approves")
  ) / sum(uniqueReviewData$ceo_opinion != "", na.rm = TRUE) * 100,
  0)
approvesCeo

disapprovesCeo <-
  round(nrow(
    filter(
      uniqueReviewData,
      uniqueReviewData$ceo_opinion == "Disapproves"
    )
  ) / sum(uniqueReviewData$ceo_opinion != "", na.rm = TRUE) * 100,
  0)
disapprovesCeo

noOpinionCeo <-
  round(nrow(
    filter(uniqueReviewData, uniqueReviewData$ceo_opinion == "No Opinion")
  ) / sum(uniqueReviewData$ceo_opinion != "", na.rm = TRUE) * 100,
  0)
noOpinionCeo

# Top Positive extractions
posExtractions <-
  amenityData %>% select(Extraction, Polarity) %>% filter(Polarity == "Positive") %>% count(Extraction, sort = TRUE)
posExtractions$Extraction <-
  factor(posExtractions$Extraction,
         levels = unique(posExtractions$Extraction)[order(posExtractions$n, decreasing = TRUE)])
posExtractions

# Top Negative extractions
negExtractions <-
  amenityData %>% select(Extraction, Polarity) %>% filter(Polarity == "Negative") %>% count(Extraction, sort = TRUE) %>%  top_n(10)
negExtractions$Extraction <-
  factor(negExtractions$Extraction,
         levels = unique(negExtractions$Extraction)[order(negExtractions$n, decreasing = TRUE)])
negExtractions

# Top Extractions for those that approve CEO
approvesCeoExtractions <-
  filter(amenityData, amenityData$ceo_opinion == "Approves") %>% group_by(Extraction)  %>% tally(sort = TRUE)
approvesCeoExtractions

# Top Positive extractions
posExtractions <-
  amenityData %>% select(Extraction, Polarity) %>% filter(Polarity == "Positive") %>% mutate(
    Extraction = case_when(
      Extraction == "opportunities" ~ "opportunity",
      Extraction == "Flexibility" ~ "flexibility",
      Extraction == "technologies" ~ "technology",
      Extraction == "Work life balance" ~ "work life balance",
      Extraction == "pay" ~ "salary",
      TRUE ~ Extraction
    )
  ) %>% count(Extraction, sort = TRUE) %>%  top_n(10)
posExtractions$Extraction <-
  factor(posExtractions$Extraction,
         levels = unique(posExtractions$Extraction)[order(posExtractions$n, decreasing = TRUE)])
posExtractions

# Top Negative extractions
negExtractions <-
  amenityData %>% select(Extraction, Polarity) %>% filter(Polarity == "Negative") %>% mutate(
    Extraction = case_when(
      Extraction == "Management" ~ "management",
      Extraction == "Salary" ~ "salary",
      Extraction == "pay" ~ "salary",
      TRUE ~ Extraction
    )
  ) %>% count(Extraction, sort = TRUE) %>%  top_n(10)
negExtractions$Extraction <-
  factor(negExtractions$Extraction,
         levels = unique(negExtractions$Extraction)[order(negExtractions$n, decreasing = TRUE)])
negExtractions

# Options for user interactive plot
negExtractionsOptions = negExtractions$Extraction
negExtractionsOptions
posExtractionsOptions = posExtractions$Extraction
posExtractionsOptions

# Time Series of Reviews
timeSeriesData <-
  uniqueReviewData %>% select(
    `Article Date`,
    Extraction,
    Polarity,
    overallScore,
    workLifeScore,
    compensationScore,
    managementScore,
    recommends,
    ceo_opinion
  ) %>% mutate(recommends = case_when(recommends == "Yes" ~ 1, recommends == "No" ~ 0))  %>% mutate(ceo_opinion = case_when(ceo_opinion == "Approves" ~ 1, ceo_opinion == "Disapproves" ~ 0))
timeSeriesData$`Article Date` = substr(timeSeriesData$`Article Date`, 1, 7)
timeSeriesData <-
  timeSeriesData %>% group_by(`Article Date`) %>% summarise_at(
    vars(
      overallScore,
      workLifeScore,
      compensationScore,
      managementScore,
      recommends,
      ceo_opinion
    ),
    funs(mean(., na.rm = TRUE))
  )
timeSeriesScoreData

# Top 10 Pos and Neg ESG components
posESG <-
  amenityData %>% select(ESG, Polarity) %>% filter(Polarity == "Positive" &
                                                     ESG != "TRUE") %>% mutate(
                                                       ESG = case_when(
                                                         ESG == "Work-life balance" ~ "Work life balance",
                                                         ESG == "Pay" ~ "Salary",
                                                         ESG == "Compensation" ~ "Salary",
                                                         TRUE ~ ESG
                                                       )
                                                     ) %>% count(ESG, sort = TRUE) %>% drop_na() %>% top_n(10)
posESG$ESG <-
  factor(posESG$ESG, levels = unique(posESG$ESG)[order(posESG$n, decreasing = TRUE)])
posESG

negESG <-
  amenityData %>% select(ESG, Polarity) %>% filter(Polarity == "Negative") %>% mutate(
    ESG = case_when(
      ESG == "Pay" ~ "Salary",
      ESG == "Compensation" ~ "Salary",
      ESG == "Promotions" ~ "Promotion",
      TRUE ~ ESG
    )
  ) %>%  count(ESG, sort = TRUE) %>% drop_na() %>%  top_n(10)
negESG$ESG <-
  factor(negESG$ESG, levels = unique(negESG$ESG)[order(negESG$n, decreasing = TRUE)])
negESG

# ESG criteria against avg scores
esgScores <-
  amenityData %>% select(
    ESG,
    overallScore,
    workLifeScore,
    compensationScore,
    managementScore,
    recommends,
    ceo_opinion
  ) %>% mutate(recommends = case_when(recommends == "Yes" ~ 1, recommends == "No" ~ 0))  %>% mutate(ceo_opinion = case_when(ceo_opinion == "Approves" ~ 1, ceo_opinion == "Disapproves" ~ 0)) %>% mutate(ESG = case_when(ESG == "Work life balance" ~ "Work-life balance", TRUE ~
                                                                                                                                                                                                                           ESG))
esgScores <-
  esgScores %>% filter(ESG %in% negESG$ESG | ESG %in% posESG$ESG)
esgScores <-
  esgScores %>% group_by(ESG) %>% summarise_at(
    vars(
      overallScore,
      workLifeScore,
      compensationScore,
      managementScore,
      recommends,
      ceo_opinion
    ),
    funs(mean(., na.rm = TRUE))
  )
esgScores
