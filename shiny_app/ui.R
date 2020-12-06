library(shiny)
library(tidyverse)
library(shinythemes)
library(janitor)
library(bigrquery)
library(tidytext)
library(ggrepel)
library(wordcloud2)
library(datapasta)
library(webshot)
library(htmlwidgets)
library(mapview)
library(ggmap)
library(sf)
library(rstanarm)
library(lubridate)
library(tigris)
library(wordcloud2)
library(usmap)
library(gtsummary)
library(gt)
library(broom.mixed)

# Define UI for application that draws a histogram

state.names <- c("CA", "NY", "KS")

shinyUI(
    navbarPage(theme = shinytheme("united"),
               "UFO Sightings in the U.S.",
               tabPanel(
                   "About",
                   h2("UFO Sightings in the United States"),
                   p("GitHub Repo for the Final Project:", a("Link",
                      href = "https://github.com/UlucKadioglu/final_project"),
                   p("For this project, I decided to focus on UFO Sightings in the United States and aim to see 
                   whether certain characteristics of each state affect the number of UFOs sighted in those states. 
                   The Model tab offers a data table that shows the relationship between these characteristics and their likely effects on UFO sightings.
                     The data I used come from three main sources. The main dataset that details each recorded UFO sighting is from 
                      the National UFO Reporting Center. The dataset on meteorite landings is from NASA, and the final, smaller dataset
                     on the prevalence of binge drinking in each state is from the CDC."))), 
               
               tabPanel(
                   "Visualizations",
                     mainPanel(
                   plotOutput("shape_plot"),
                   textOutput("state_message"))),
               
               tabPanel(
                 "Model",
                 h2("Summary of the Regression Model"),
                 p("As the below table shows, population and income tend to have a very similar
                   relationship with the number of UFOs seen in each state. Though the correlation
                   does not look particularly strong, we could infer the number of UFOs seen in a given state
                   might be positively correlated with population and income. This is a pattern that would be
                   expected. In a state where more people live, there are more eyes to spot UFOs, and therefore
                   more UFOs that are seen. Again, this model is simply predictive and does not imply causation.
                   Moving on, illiteracy seems to be negatively correlated with the number of UFOs seen in a given state.
                   However, looking at the confidence interval for Illiteracy, we see that the range of possible
                   numbers is quite large, showing us that this is not a very strong correlation.
                   Lastly, prevalence of bing drinking seems to also be negatively correlated with the number of UFOs seen.
                   While the confidence interval is still quite large, the negativity is likely to remain in any given value within
                   the interval. This negative correlation might imply that in states where more people binge-drink,
                   fewer UFOs are seen."),
                 mainPanel(
                   img(src = "table.png", height = 300, width = 550))),
               
               tabPanel(
                 "Word Cloud",
                 h2("Word Cloud of the Descriptions of Each UFO Sighting"),
                 p("When people who claim to have seen a UFO (or UFOs) submit the details of the sighting to
                   NUFORC, they also send a brief description of the sighting. This description usually includes when and where
                   they saw the UFO, as well as what it looked like. The Word Cloud below visualizes the words that appear more than 50 times
                   in the recorded descriptions."),
                 mainPanel(
                   img(src = "wc2.png", height = 375, width = 550)))
               
    ))