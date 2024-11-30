library(shiny)
library(tidyverse)

superheros <- read_csv("data/superheroes.csv")

superheros_shiny <- superheros |>
  filter(complete.cases(intelligence, strength, speed, durability, combat, height_cm, weight_kg, gender, alignment, power))

ui <- navbarPage(
  theme = shinythemes::shinytheme("flatly"),
  title = "Superhero Selector-Pick your Best Hero!",
  
  tabPanel("Home Page",
           fluidPage(
             titlePanel("Welcome to the Superhero Selector!"),
             p("This application allows you to select the best superhero based on your preferences. 
               You can either choose a hero based on weighted attribute selection 
               or use various filters such as publisher, alignment, gender, eye color, 
               and hair color to find your favorite hero."),
             img(scr = "selector.jpg")
           )
  ),
  
  tabPanel("Weighted Attribute Value Selection",
           sidebarLayout(
             sidebarPanel(
               sliderInput("intelligence", "Intelligence Weight:", min = 0, max = 1, value = 0.2),
               sliderInput("strength", "Strength Weight:", min = 0, max = 1, value = 0.2),
               sliderInput("speed", "Speed Weight:", min = 0, max = 1, value = 0.2),
               sliderInput("durability", "Durability Weight:", min = 0, max = 1, value = 0.2),
               sliderInput("power", "Power Weight:", min = 0, max = 1, value = 0.1),
               sliderInput("combat", "Combat Weight:", min = 0, max = 1, value = 0.1),
               actionButton("calculate", "Find Best Hero")
             ),
             mainPanel(
               textOutput("best_hero_name"),
               textOutput("best_hero_score"),
               uiOutput("best_hero_image")
             )
           )
  ),
  
  tabPanel("Filter Selection",
           sidebarLayout(
             sidebarPanel(
               selectInput("publisher", "Publisher:", choices = c("All", unique(superheros$publisher)), selected = "All"),
               selectInput("alignment", "Alignment:", choices = c("All", unique(superheros$alignment)), selected = "All"),
               selectInput("gender", "Gender:", choices = c("All", unique(superheros$gender)), selected = "All"),
               selectInput("eye_color", "Eye Color:", choices = c("All", unique(superheros$eye_color)), selected = "All"),
               selectInput("hair_color", "Hair Color:", choices = c("All", unique(superheros$hair_color)), selected = "All")
             ),
             mainPanel(
               uiOutput("filtered_heroes_ui")
             )
           )
  )
)

server <- function(input, output) {
  
  observeEvent(input$calculate, {
    weights <- c(
      intelligence = input$intelligence,
      strength = input$strength,
      speed = input$speed,
      durability = input$durability,
      power = input$power,
      combat = input$combat
    )
    
    superheros_shiny <- superheros_shiny |>
      mutate(weighted_score = intelligence * weights["intelligence"] +
               strength * weights["strength"] +
               speed * weights["speed"] +
               durability * weights["durability"] +
               power * weights["power"] +
               combat * weights["combat"])
    
    best_hero <- superheros_shiny |>
      arrange(desc(weighted_score)) |>
      slice(1)
    
    output$best_hero_name <- renderText({ paste("Best Hero: ", best_hero$name) })
    output$best_hero_score <- renderText({ paste("Score: ", round(best_hero$weighted_score, 2)) })
    
    output$best_hero_image <- renderUI({
      tags$img(src = best_hero$url, height = "400px")
    })
  })
  
  output$filtered_heroes_ui <- renderUI({
    filtered_heroes <- superheros
    
    if (input$publisher != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(publisher == input$publisher)
    }
    if (input$alignment != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(alignment == input$alignment)
    }
    if (input$gender != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(gender == input$gender)
    }
    if (input$eye_color != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(eye_color == input$eye_color)
    }
    if (input$hair_color != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(hair_color == input$hair_color)
    }
    
    tagList(
      lapply(1:nrow(filtered_heroes), function(i) {
        hero <- filtered_heroes[i, ]
        tagList(
          tags$h4(hero$name),
          tags$img(src = hero$url, height = "200px"),
          tags$p(paste("Publisher: ", hero$publisher)),
          tags$p(paste("Alignment: ", hero$alignment)),
          tags$p(paste("Gender: ", hero$gender)),
          tags$p(paste("Eye Color: ", hero$eye_color)),
          tags$p(paste("Hair Color: ", hero$hair_color)),
          tags$hr()
        )
      })
    )
  })
}

shinyApp(ui = ui, server = server)