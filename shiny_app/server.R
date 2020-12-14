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

# Define server logic required to draw a plot.

shinyServer(function(input, output){

    output$shape_plot <- renderPlot({
      
      if(input$selected_state == "AK"){
      
        # These exhaustive lines of code below are for the interactive side of the app. Apparently there is a limit as to how many if and else
        # statements there can be, so I could only write code until the state of Tennessee. I also think there should be an easier way to do this 
        # (and there probably is), since copying and pasting the code for the plot over and over again is very inefficient.
        
      ufo %>%
        select(state, shape) %>%
        drop_na() %>%
        filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                 shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                 shape == "cylinder" | shape == "diamond" | shape == "disk" |
                 shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                 shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                 shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
        filter(state == "AK") %>%
        ggplot(aes(x = shape, fill = shape)) +
        geom_histogram(stat = "count") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}

      else{
        if(input$selected_state == "AL"){
            
            ufo %>%
              select(state, shape) %>%
              drop_na() %>%
              filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                       shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                       shape == "cylinder" | shape == "diamond" | shape == "disk" |
                       shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                       shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                       shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
              filter(state == "AL") %>%
              ggplot(aes(x = shape, fill = shape)) +
              geom_histogram(stat = "count") +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 90)) +
              labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
        
        else{
          if(input$selected_state == "AR"){
            
            ufo %>%
              select(state, shape) %>%
              drop_na() %>%
              filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                       shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                       shape == "cylinder" | shape == "diamond" | shape == "disk" |
                       shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                       shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                       shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
              filter(state == "AR") %>%
              ggplot(aes(x = shape, fill = shape)) +
              geom_histogram(stat = "count") +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 90)) +
              labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
          
          
          else{
            if(input$selected_state == "AZ"){
              
              ufo %>%
                select(state, shape) %>%
                drop_na() %>%
                filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                         shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                         shape == "cylinder" | shape == "diamond" | shape == "disk" |
                         shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                         shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                         shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                filter(state == "AZ") %>%
                ggplot(aes(x = shape, fill = shape)) +
                geom_histogram(stat = "count") +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
            
            else{
              if(input$selected_state == "CA"){
                
                ufo %>%
                  select(state, shape) %>%
                  drop_na() %>%
                  filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                           shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                           shape == "cylinder" | shape == "diamond" | shape == "disk" |
                           shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                           shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                           shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                  filter(state == "CA") %>%
                  ggplot(aes(x = shape, fill = shape)) +
                  geom_histogram(stat = "count") +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle = 90)) +
                  labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
              
              else{
                if(input$selected_state == "CO"){
                  
                  ufo %>%
                    select(state, shape) %>%
                    drop_na() %>%
                    filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                             shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                             shape == "cylinder" | shape == "diamond" | shape == "disk" |
                             shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                             shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                             shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                    filter(state == "CO") %>%
                    ggplot(aes(x = shape, fill = shape)) +
                    geom_histogram(stat = "count") +
                    theme_bw() +
                    theme(axis.text.x = element_text(angle = 90)) +
                    labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                
                else{
                  if(input$selected_state == "CT"){
                    
                    ufo %>%
                      select(state, shape) %>%
                      drop_na() %>%
                      filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                               shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                               shape == "cylinder" | shape == "diamond" | shape == "disk" |
                               shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                               shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                               shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                      filter(state == "CT") %>%
                      ggplot(aes(x = shape, fill = shape)) +
                      geom_histogram(stat = "count") +
                      theme_bw() +
                      theme(axis.text.x = element_text(angle = 90)) +
                      labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                  
                  
                  else{
                    if(input$selected_state == "DC"){
                      
                      ufo %>%
                        select(state, shape) %>%
                        drop_na() %>%
                        filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                 shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                 shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                 shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                 shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                 shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                        filter(state == "DC") %>%
                        ggplot(aes(x = shape, fill = shape)) +
                        geom_histogram(stat = "count") +
                        theme_bw() +
                        theme(axis.text.x = element_text(angle = 90)) +
                        labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                    
                    else{
                      if(input$selected_state == "DE"){
                        
                        ufo %>%
                          select(state, shape) %>%
                          drop_na() %>%
                          filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                   shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                   shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                   shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                   shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                   shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                          filter(state == "DE") %>%
                          ggplot(aes(x = shape, fill = shape)) +
                          geom_histogram(stat = "count") +
                          theme_bw() +
                          theme(axis.text.x = element_text(angle = 90)) +
                          labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                      
                      
                      else{
                        if(input$selected_state == "FL"){
                          
                          ufo %>%
                            select(state, shape) %>%
                            drop_na() %>%
                            filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                     shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                     shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                     shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                     shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                     shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                            filter(state == "FL") %>%
                            ggplot(aes(x = shape, fill = shape)) +
                            geom_histogram(stat = "count") +
                            theme_bw() +
                            theme(axis.text.x = element_text(angle = 90)) +
                            labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                        
                        else{
                          if(input$selected_state == "GA"){
                            
                            ufo %>%
                              select(state, shape) %>%
                              drop_na() %>%
                              filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                       shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                       shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                       shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                       shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                       shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                              filter(state == "GA") %>%
                              ggplot(aes(x = shape, fill = shape)) +
                              geom_histogram(stat = "count") +
                              theme_bw() +
                              theme(axis.text.x = element_text(angle = 90)) +
                              labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                          
                          else{
                            if(input$selected_state == "HI"){
                              
                              ufo %>%
                                select(state, shape) %>%
                                drop_na() %>%
                                filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                         shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                         shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                         shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                         shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                         shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                filter(state == "HI") %>%
                                ggplot(aes(x = shape, fill = shape)) +
                                geom_histogram(stat = "count") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 90)) +
                                labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                            
                            
                            else{
                              if(input$selected_state == "IA"){
                                
                                ufo %>%
                                  select(state, shape) %>%
                                  drop_na() %>%
                                  filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                           shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                           shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                           shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                           shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                           shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                  filter(state == "IA") %>%
                                  ggplot(aes(x = shape, fill = shape)) +
                                  geom_histogram(stat = "count") +
                                  theme_bw() +
                                  theme(axis.text.x = element_text(angle = 90)) +
                                  labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                              
                              else{
                                if(input$selected_state == "ID"){
                                  
                                  ufo %>%
                                    select(state, shape) %>%
                                    drop_na() %>%
                                    filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                             shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                             shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                             shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                             shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                             shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                    filter(state == "ID") %>%
                                    ggplot(aes(x = shape, fill = shape)) +
                                    geom_histogram(stat = "count") +
                                    theme_bw() +
                                    theme(axis.text.x = element_text(angle = 90)) +
                                    labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                
                                else{
                                  if(input$selected_state == "IL"){
                                    
                                    ufo %>%
                                      select(state, shape) %>%
                                      drop_na() %>%
                                      filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                               shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                               shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                               shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                               shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                               shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                      filter(state == "IL") %>%
                                      ggplot(aes(x = shape, fill = shape)) +
                                      geom_histogram(stat = "count") +
                                      theme_bw() +
                                      theme(axis.text.x = element_text(angle = 90)) +
                                      labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                  
                                  
                                  else{
                                    if(input$selected_state == "IN"){
                                      
                                      ufo %>%
                                        select(state, shape) %>%
                                        drop_na() %>%
                                        filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                 shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                 shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                 shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                 shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                 shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                        filter(state == "IN") %>%
                                        ggplot(aes(x = shape, fill = shape)) +
                                        geom_histogram(stat = "count") +
                                        theme_bw() +
                                        theme(axis.text.x = element_text(angle = 90)) +
                                        labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                    
                                    else{
                                      if(input$selected_state == "KS"){
                                        
                                        ufo %>%
                                          select(state, shape) %>%
                                          drop_na() %>%
                                          filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                   shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                   shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                   shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                   shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                   shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                          filter(state == "KY") %>%
                                          ggplot(aes(x = shape, fill = shape)) +
                                          geom_histogram(stat = "count") +
                                          theme_bw() +
                                          theme(axis.text.x = element_text(angle = 90)) +
                                          labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                      
                                      
                                      else{
                                        if(input$selected_state == "KY"){
                                          
                                          ufo %>%
                                            select(state, shape) %>%
                                            drop_na() %>%
                                            filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                     shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                     shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                     shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                     shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                     shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                            filter(state == "KY") %>%
                                            ggplot(aes(x = shape, fill = shape)) +
                                            geom_histogram(stat = "count") +
                                            theme_bw() +
                                            theme(axis.text.x = element_text(angle = 90)) +
                                            labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                        
                                        
                                        
                                        else{
                                          if(input$selected_state == "LA"){
                                            
                                            ufo %>%
                                              select(state, shape) %>%
                                              drop_na() %>%
                                              filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                       shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                       shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                       shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                       shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                       shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                              filter(state == "LA") %>%
                                              ggplot(aes(x = shape, fill = shape)) +
                                              geom_histogram(stat = "count") +
                                              theme_bw() +
                                              theme(axis.text.x = element_text(angle = 90)) +
                                              labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                          
                                          else{
                                            if(input$selected_state == "MA"){
                                              
                                              ufo %>%
                                                select(state, shape) %>%
                                                drop_na() %>%
                                                filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                         shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                         shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                         shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                         shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                         shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                filter(state == "MA") %>%
                                                ggplot(aes(x = shape, fill = shape)) +
                                                geom_histogram(stat = "count") +
                                                theme_bw() +
                                                theme(axis.text.x = element_text(angle = 90)) +
                                                labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                            
                                            else{
                                              if(input$selected_state == "MD"){
                                                
                                                ufo %>%
                                                  select(state, shape) %>%
                                                  drop_na() %>%
                                                  filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                           shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                           shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                           shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                           shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                           shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                  filter(state == "MD") %>%
                                                  ggplot(aes(x = shape, fill = shape)) +
                                                  geom_histogram(stat = "count") +
                                                  theme_bw() +
                                                  theme(axis.text.x = element_text(angle = 90)) +
                                                  labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                              
                                              else{
                                                if(input$selected_state == "ME"){
                                                  
                                                  ufo %>%
                                                    select(state, shape) %>%
                                                    drop_na() %>%
                                                    filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                             shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                             shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                             shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                             shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                             shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                    filter(state == "ME") %>%
                                                    ggplot(aes(x = shape, fill = shape)) +
                                                    geom_histogram(stat = "count") +
                                                    theme_bw() +
                                                    theme(axis.text.x = element_text(angle = 90)) +
                                                    labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                
                                                else{
                                                  if(input$selected_state == "MI"){
                                                    
                                                    ufo %>%
                                                      select(state, shape) %>%
                                                      drop_na() %>%
                                                      filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                               shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                               shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                               shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                               shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                               shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                      filter(state == "MI") %>%
                                                      ggplot(aes(x = shape, fill = shape)) +
                                                      geom_histogram(stat = "count") +
                                                      theme_bw() +
                                                      theme(axis.text.x = element_text(angle = 90)) +
                                                      labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                  
                                                  
                                                  else{
                                                    if(input$selected_state == "MN"){
                                                      
                                                      ufo %>%
                                                        select(state, shape) %>%
                                                        drop_na() %>%
                                                        filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                 shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                 shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                 shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                 shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                 shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                        filter(state == "MN") %>%
                                                        ggplot(aes(x = shape, fill = shape)) +
                                                        geom_histogram(stat = "count") +
                                                        theme_bw() +
                                                        theme(axis.text.x = element_text(angle = 90)) +
                                                        labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                    
                                                    
                                                    else{
                                                      if(input$selected_state == "MO"){
                                                        
                                                        ufo %>%
                                                          select(state, shape) %>%
                                                          drop_na() %>%
                                                          filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                   shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                   shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                   shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                   shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                   shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                          filter(state == "MO") %>%
                                                          ggplot(aes(x = shape, fill = shape)) +
                                                          geom_histogram(stat = "count") +
                                                          theme_bw() +
                                                          theme(axis.text.x = element_text(angle = 90)) +
                                                          labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                      
                                                      else{
                                                        if(input$selected_state == "MS"){
                                                          
                                                          ufo %>%
                                                            select(state, shape) %>%
                                                            drop_na() %>%
                                                            filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                     shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                     shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                     shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                     shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                     shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                            filter(state == "MS") %>%
                                                            ggplot(aes(x = shape, fill = shape)) +
                                                            geom_histogram(stat = "count") +
                                                            theme_bw() +
                                                            theme(axis.text.x = element_text(angle = 90)) +
                                                            labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                        
                                                        
                                                        
                                                        else{
                                                          if(input$selected_state == "MT"){
                                                            
                                                            ufo %>%
                                                              select(state, shape) %>%
                                                              drop_na() %>%
                                                              filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                       shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                       shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                       shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                       shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                       shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                              filter(state == "MT") %>%
                                                              ggplot(aes(x = shape, fill = shape)) +
                                                              geom_histogram(stat = "count") +
                                                              theme_bw() +
                                                              theme(axis.text.x = element_text(angle = 90)) +
                                                              labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                          
                                                          else{
                                                            if(input$selected_state == "NC"){
                                                              
                                                              ufo %>%
                                                                select(state, shape) %>%
                                                                drop_na() %>%
                                                                filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                         shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                         shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                         shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                         shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                         shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                filter(state == "NC") %>%
                                                                ggplot(aes(x = shape, fill = shape)) +
                                                                geom_histogram(stat = "count") +
                                                                theme_bw() +
                                                                theme(axis.text.x = element_text(angle = 90)) +
                                                                labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                            
                                                            
                                                            
                                                            else{
                                                              if(input$selected_state == "ND"){
                                                                
                                                                ufo %>%
                                                                  select(state, shape) %>%
                                                                  drop_na() %>%
                                                                  filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                           shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                           shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                           shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                           shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                           shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                  filter(state == "ND") %>%
                                                                  ggplot(aes(x = shape, fill = shape)) +
                                                                  geom_histogram(stat = "count") +
                                                                  theme_bw() +
                                                                  theme(axis.text.x = element_text(angle = 90)) +
                                                                  labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                              
                                                              else{
                                                                if(input$selected_state == "NE"){
                                                                  
                                                                  ufo %>%
                                                                    select(state, shape) %>%
                                                                    drop_na() %>%
                                                                    filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                             shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                             shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                             shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                             shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                             shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                    filter(state == "NE") %>%
                                                                    ggplot(aes(x = shape, fill = shape)) +
                                                                    geom_histogram(stat = "count") +
                                                                    theme_bw() +
                                                                    theme(axis.text.x = element_text(angle = 90)) +
                                                                    labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                
                                                                else{
                                                                  if(input$selected_state == "NH"){
                                                                    
                                                                    ufo %>%
                                                                      select(state, shape) %>%
                                                                      drop_na() %>%
                                                                      filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                               shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                               shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                               shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                               shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                               shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                      filter(state == "NH") %>%
                                                                      ggplot(aes(x = shape, fill = shape)) +
                                                                      geom_histogram(stat = "count") +
                                                                      theme_bw() +
                                                                      theme(axis.text.x = element_text(angle = 90)) +
                                                                      labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                  
                                                                  else{
                                                                    if(input$selected_state == "NJ"){
                                                                      
                                                                      ufo %>%
                                                                        select(state, shape) %>%
                                                                        drop_na() %>%
                                                                        filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                 shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                 shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                 shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                 shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                 shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                        filter(state == "NJ") %>%
                                                                        ggplot(aes(x = shape, fill = shape)) +
                                                                        geom_histogram(stat = "count") +
                                                                        theme_bw() +
                                                                        theme(axis.text.x = element_text(angle = 90)) +
                                                                        labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                    
                                                                    else{
                                                                      if(input$selected_state == "NM"){
                                                                        
                                                                        ufo %>%
                                                                          select(state, shape) %>%
                                                                          drop_na() %>%
                                                                          filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                   shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                   shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                   shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                   shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                   shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                          filter(state == "NM") %>%
                                                                          ggplot(aes(x = shape, fill = shape)) +
                                                                          geom_histogram(stat = "count") +
                                                                          theme_bw() +
                                                                          theme(axis.text.x = element_text(angle = 90)) +
                                                                          labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                      
                                                                      else{
                                                                        if(input$selected_state == "NV"){
                                                                          
                                                                          ufo %>%
                                                                            select(state, shape) %>%
                                                                            drop_na() %>%
                                                                            filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                     shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                     shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                     shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                     shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                     shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                            filter(state == "NV") %>%
                                                                            ggplot(aes(x = shape, fill = shape)) +
                                                                            geom_histogram(stat = "count") +
                                                                            theme_bw() +
                                                                            theme(axis.text.x = element_text(angle = 90)) +
                                                                            labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                        
                                                                        else{
                                                                          if(input$selected_state == "NY"){
                                                                            
                                                                            ufo %>%
                                                                              select(state, shape) %>%
                                                                              drop_na() %>%
                                                                              filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                       shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                       shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                       shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                       shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                       shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                              filter(state == "NY") %>%
                                                                              ggplot(aes(x = shape, fill = shape)) +
                                                                              geom_histogram(stat = "count") +
                                                                              theme_bw() +
                                                                              theme(axis.text.x = element_text(angle = 90)) +
                                                                              labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                          
                                                                          else{
                                                                            if(input$selected_state == "OH"){
                                                                              
                                                                              ufo %>%
                                                                                select(state, shape) %>%
                                                                                drop_na() %>%
                                                                                filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                         shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                         shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                         shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                         shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                         shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                                filter(state == "OH") %>%
                                                                                ggplot(aes(x = shape, fill = shape)) +
                                                                                geom_histogram(stat = "count") +
                                                                                theme_bw() +
                                                                                theme(axis.text.x = element_text(angle = 90)) +
                                                                                labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                            
                                                                            else{
                                                                              if(input$selected_state == "OK"){
                                                                                
                                                                                ufo %>%
                                                                                  select(state, shape) %>%
                                                                                  drop_na() %>%
                                                                                  filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                           shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                           shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                           shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                           shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                           shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                                  filter(state == "OK") %>%
                                                                                  ggplot(aes(x = shape, fill = shape)) +
                                                                                  geom_histogram(stat = "count") +
                                                                                  theme_bw() +
                                                                                  theme(axis.text.x = element_text(angle = 90)) +
                                                                                  labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                              
                                                                              else{
                                                                                if(input$selected_state == "OR"){
                                                                                  
                                                                                  ufo %>%
                                                                                    select(state, shape) %>%
                                                                                    drop_na() %>%
                                                                                    filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                             shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                             shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                             shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                             shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                             shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                                    filter(state == "OR") %>%
                                                                                    ggplot(aes(x = shape, fill = shape)) +
                                                                                    geom_histogram(stat = "count") +
                                                                                    theme_bw() +
                                                                                    theme(axis.text.x = element_text(angle = 90)) +
                                                                                    labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                                
                                                                                else{
                                                                                  if(input$selected_state == "PA"){
                                                                                    
                                                                                    ufo %>%
                                                                                      select(state, shape) %>%
                                                                                      drop_na() %>%
                                                                                      filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                               shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                               shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                               shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                               shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                               shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                                      filter(state == "PA") %>%
                                                                                      ggplot(aes(x = shape, fill = shape)) +
                                                                                      geom_histogram(stat = "count") +
                                                                                      theme_bw() +
                                                                                      theme(axis.text.x = element_text(angle = 90)) +
                                                                                      labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                                  
                                                                                  else{
                                                                                    if(input$selected_state == "RI"){
                                                                                      
                                                                                      ufo %>%
                                                                                        select(state, shape) %>%
                                                                                        drop_na() %>%
                                                                                        filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                                 shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                                 shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                                 shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                                 shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                                 shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                                        filter(state == "RI") %>%
                                                                                        ggplot(aes(x = shape, fill = shape)) +
                                                                                        geom_histogram(stat = "count") +
                                                                                        theme_bw() +
                                                                                        theme(axis.text.x = element_text(angle = 90)) +
                                                                                        labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                                    
                                                                                    else{
                                                                                      if(input$selected_state == "SC"){
                                                                                        
                                                                                        ufo %>%
                                                                                          select(state, shape) %>%
                                                                                          drop_na() %>%
                                                                                          filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                                   shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                                   shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                                   shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                                   shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                                   shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                                          filter(state == "SC") %>%
                                                                                          ggplot(aes(x = shape, fill = shape)) +
                                                                                          geom_histogram(stat = "count") +
                                                                                          theme_bw() +
                                                                                          theme(axis.text.x = element_text(angle = 90)) +
                                                                                          labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                                      
                                                                                      else{
                                                                                        if(input$selected_state == "SD"){
                                                                                          
                                                                                          ufo %>%
                                                                                            select(state, shape) %>%
                                                                                            drop_na() %>%
                                                                                            filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                                     shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                                     shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                                     shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                                     shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                                     shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                                            filter(state == "SD") %>%
                                                                                            ggplot(aes(x = shape, fill = shape)) +
                                                                                            geom_histogram(stat = "count") +
                                                                                            theme_bw() +
                                                                                            theme(axis.text.x = element_text(angle = 90)) +
                                                                                            labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
                                                                                        
                                                                                        else{
                                                                                          if(input$selected_state == "TN"){
                                                                                            
                                                                                            ufo %>%
                                                                                              select(state, shape) %>%
                                                                                              drop_na() %>%
                                                                                              filter(shape == "changing" | shape == "chevron" | shape == "cigar" |
                                                                                                       shape == "circle" | shape == "circle" | shape == "cone" | shape == "cross" |
                                                                                                       shape == "cylinder" | shape == "diamond" | shape == "disk" |
                                                                                                       shape == "egg" | shape == "fireball" | shape == "flash" | shape == "formation" |
                                                                                                       shape == "light" | shape == "other" | shape == "oval" | shape == "rectangle" |
                                                                                                       shape == "sphere" | shape == "teardrop" | shape == "triangle" | shape == "unknown") %>%
                                                                                              filter(state == "TN") %>%
                                                                                              ggplot(aes(x = shape, fill = shape)) +
                                                                                              geom_histogram(stat = "count") +
                                                                                              theme_bw() +
                                                                                              theme(axis.text.x = element_text(angle = 90)) +
                                                                                              labs(title = "Shapes of UFOs Sighted in States", x = "Shape", y = "Count")}
 
                                                                        }
                                                                      }
                                                                    }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                }
                                            }
                                          }
                                          }
                                        }
                            }
                          }
                                  }
                                  }
                                }
                        }
                        }
                      }
                    }
        }
                    }
                  }
                }
              }
            }
          }
        }
                    }
                  }
                }
              }
            }
          }
        }
      }
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
      
      
      # These lines above form the tlb-regression table in the Model tab. Not necessarily needed here as I uploaded an image of the table, but still left them here.
      
    })
    
})


