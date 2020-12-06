#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

ufo <- read_rds("data/nuforc_reports.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output){

    output$shape_plot <- renderPlot({
            ufo %>%
            select(state, shape) %>%
            drop_na() %>%
            filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                   shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                   shape == "cylinder" | shape == "diamond" | shape == "disk" |
                   shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                   shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                   shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
            filter(state == "AL" | state == "AK" | state == "AZ" |
                   state == "AR" | state == "CA" | state == "CO" |
                   state == "CT" | state == "DE" | state == "FL" |
                   state == "GA" | state == "HI" | state == "ID" |
                   state == "IL" | state == "IN" | state == "IA" |
                   state == "KS" | state == "KY" | state == "LA" |
                   state == "ME" | state == "MD" | state == "MA" |
                   state == "MI" | state == "MN" | state == "MS" |
                   state == "MO" | state == "MT" | state == "NE" |
                   state == "NV" | state == "NH" | state == "NJ" |
                   state == "NM" | state == "NY" | state == "NC" |
                   state == "ND" | state == "OH" | state == "OK" |
                   state == "OR" | state == "PA" | state == "RI" |
                   state == "SC" | state == "SD" | state == "TN" |
                   state == "TX" | state == "UT" | state == "VT" |
                   state == "VA" | state == "WA" | state == "WV" |
                   state == "WI" | state == "WY") %>%
            drop_na() %>%
            ggplot(aes(x = state, fill = shape)) +
            geom_histogram(stat = "count") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(title = "Number of UFO Sightings by State", subtitle = "Don't forget to take the population of each state into account!", 
                 x = "States", y = "Count")

})
    
    output$table <- renderPlot({
      
      alcohol <- read.csv("shiny_app/raw_data/excessive_alcohol.csv") %>%
        rename(state = State)
      
      ufo_alcohol <- ufo %>%
        select(state) %>%
        count(state)
      
      variables <- state.x77 %>%
        as_tibble() %>%
        clean_names() %>%
        select(population, income, illiteracy) %>%
        rename(Population = population) %>%
        rename(Income = income) %>%
        rename(Illiteracy = illiteracy)
      
      alcohol_joined <- inner_join(alcohol, ufo_alcohol) %>%
        mutate(variables) %>%
        rename(Prevalence = Percentage)
      
      model <- stan_glm(data = alcohol_joined,
                        n ~ Population + Income + Illiteracy + Prevalence, # Add pluses here to control for more variables.
                        family = gaussian(),
                        refresh = 0)
      
      table <- tbl_regression(model) %>%
        as_gt() %>%
        tab_header(title = "Regression of Number of UFOs Sighted in Each State",
                   subtitle = "The Effect of Population, Income, Illiteracy and Prevalence of Binge Drinking")
      
    })
    
})


