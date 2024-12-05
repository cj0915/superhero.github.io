library(shiny)
library(tidyverse)

superheros <- read_csv("superheroes.csv")

superheros_shiny <- superheros |>
  filter(complete.cases(intelligence, strength, speed, durability, combat, power)) |>
  mutate(across(c(intelligence, strength, speed, durability, combat, power),
                ~ 100 * (. - min(.)) / (max(.) - min(.))))

ui <- navbarPage(
  theme = shinythemes::shinytheme("flatly"),
  title = "Superhero Selector - Pick Your Best Hero!",
  
  tabPanel("Home Page",
           fluidPage(
             titlePanel("Welcome to the Superhero Selector!"),
             p("This application allows you to select the best superhero
             based on your preferences.
             You can choose two attributes, find the top 5 ranked heroes (including ties)
             based on their average score, and then filter the result further by alignment, gender, hair color, and eye color in any order.",
               style = "font-size: 20px;"),
             p("Here is the instruction:", style = "font-size: 20px;"),
             p("1. Select Attributes:", style = "font-size: 18px;"),
             p("First, click on the 'Custom Attribute Selection' tab.
               On the left side of the page, you will see a dropdown menu where you can select your two favorite attributes.
               The available attributes to choose from are: intelligence, strength, speed, durability, power, and combat skills.
               Note: You cannot select the same attribute twice. If you do, a warning message will appear. And the results may include ties, so you might see more than five heroes listed.", 
               style = "font-size: 15px;"),
             p("2. Find Top Heroes:", style = "font-size: 18px;"),
             p("After selecting your top 2 attributes, click the 'Find Top 5 Ranked Heroes' button.
               The results will appear on the right side, showing the top 5 heroes ranked based on your selected attributes.",
               style = "font-size: 15px;"),
             p("3. Filter Your Hero:", style = "font-size: 18px;"),
             p("Once the top heroes are displayed, you can narrow down your match using the filters provided for: Alignment, Gender, Hair Color, Eye Color.
               The web application will then identify the hero that best matches your criteria.
               Note: You might notice that some filtering options are missing. This is because the filters follow a logical hierarchy. During the selection process, some outcomes are automatically filtered out if they don't match the earlier criteria.",
               style = "font-size: 15px;"),
             p("4. Logic Reset:", style = "font-size: 18px;"),
             p("If you want to go back and change your top 2 attributes, you must refresh the results by clicking the 'Find Top 5 Ranked Heroes' button again. This will reset your selections and allow you to start over.",
               style = "font-size: 15px;"),
             img(src = "selector.jpg")
           )
  ),
  
  tabPanel("Custom Attribute Selection",
           sidebarLayout(
             sidebarPanel(
               selectInput("attribute1", "Select First Attribute:", 
                           choices = c("intelligence", "strength", "speed", "durability", "combat", "power")),
               selectInput("attribute2", "Select Second Attribute:", 
                           choices = c("intelligence", "strength", "speed", "durability", "combat", "power")),
               textOutput("warning_message"),
               actionButton("calculate", "Find Top 5 Ranked Heroes", class = "btn-primary", disabled = TRUE),
               br(),
               uiOutput("dynamic_filters")
             ),
             mainPanel(
               textOutput("top_hero_names"),
               uiOutput("hero_details")
             )
           )
  )
)

server <- function(input, output, session) {
  
  top_heroes <- reactiveVal(NULL)
  
  observe({
    if (input$attribute1 == input$attribute2) {
      output$warning_message <- renderText("Warning: Please select two different attributes!")
      updateActionButton(session, "calculate", disabled = TRUE)
    } else {
      output$warning_message <- renderText("")
      updateActionButton(session, "calculate", disabled = FALSE)
    }
  })
  
  observeEvent(input$calculate, {
    attr1 <- input$attribute1
    attr2 <- input$attribute2
    
    heroes <- superheros_shiny |>
      mutate(average_score = rowMeans(across(all_of(c(attr1, attr2)))),
             rank = dense_rank(desc(average_score)))
    
    top <- heroes |>
      filter(rank <= 5)
    top_heroes(top)
    
    updateSelectInput(session, "filter_alignment", 
                      choices = c("All", na.omit(unique(top$alignment))))
    updateSelectInput(session, "filter_gender", 
                      choices = c("All", na.omit(unique(top$gender))))
    updateSelectInput(session, "filter_hair_color", 
                      choices = c("All", na.omit(unique(top$hair_color))))
    updateSelectInput(session, "filter_eye_color", 
                      choices = c("All", na.omit(unique(top$eye_color))))
    
    output$top_hero_names <- renderText({
      paste("Top Ranked Heroes: ", paste(top$name, collapse = ", "))
    })
  })
  
  output$dynamic_filters <- renderUI({
    if (is.null(top_heroes())) return(NULL)
    tagList(
      selectInput("filter_alignment", "Filter by Alignment:", choices = c("All")),
      selectInput("filter_gender", "Filter by Gender:", choices = c("All")),
      selectInput("filter_hair_color", "Filter by Hair Color:", choices = c("All")),
      selectInput("filter_eye_color", "Filter by Eye Color:", choices = c("All"))
    )
  })
  
  observe({
    if (is.null(top_heroes())) return()
    
    filtered_heroes <- top_heroes()
    
    if (!is.null(input$filter_alignment) && input$filter_alignment != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(alignment == input$filter_alignment)
    }
    if (!is.null(input$filter_gender) && input$filter_gender != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(gender == input$filter_gender)
    }
    if (!is.null(input$filter_hair_color) && input$filter_hair_color != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(hair_color == input$filter_hair_color)
    }
    if (!is.null(input$filter_eye_color) && input$filter_eye_color != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(eye_color == input$filter_eye_color)
    }
    
    updateSelectInput(session, "filter_alignment", 
                      choices = c("All", na.omit(unique(filtered_heroes$alignment))),
                      selected = ifelse(input$filter_alignment %in% unique(filtered_heroes$alignment), 
                                        input$filter_alignment, "All"))
    updateSelectInput(session, "filter_gender", 
                      choices = c("All", na.omit(unique(filtered_heroes$gender))),
                      selected = ifelse(input$filter_gender %in% unique(filtered_heroes$gender), 
                                        input$filter_gender, "All"))
    updateSelectInput(session, "filter_hair_color", 
                      choices = c("All", na.omit(unique(filtered_heroes$hair_color))),
                      selected = ifelse(input$filter_hair_color %in% unique(filtered_heroes$hair_color), 
                                        input$filter_hair_color, "All"))
    updateSelectInput(session, "filter_eye_color", 
                      choices = c("All", na.omit(unique(filtered_heroes$eye_color))),
                      selected = ifelse(input$filter_eye_color %in% unique(filtered_heroes$eye_color), 
                                        input$filter_eye_color, "All"))
  })
  
  output$hero_details <- renderUI({
    if (is.null(top_heroes())) return(NULL)
    
    filtered_heroes <- top_heroes()
    if (!is.null(input$filter_alignment) && input$filter_alignment != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(alignment == input$filter_alignment)
    }
    if (!is.null(input$filter_gender) && input$filter_gender != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(gender == input$filter_gender)
    }
    if (!is.null(input$filter_hair_color) && input$filter_hair_color != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(hair_color == input$filter_hair_color)
    }
    if (!is.null(input$filter_eye_color) && input$filter_eye_color != "All") {
      filtered_heroes <- filtered_heroes |>
        filter(eye_color == input$filter_eye_color)
    }
    
    tagList(
      lapply(1:nrow(filtered_heroes), function(i) {
        hero <- filtered_heroes[i, ]
        fluidRow(
          column(3, tags$img(src = hero$url, height = "150px")),
          column(9, 
                 tags$p(strong(hero$name)),
                 tags$p(paste(input$attribute1, ":", round(hero[[input$attribute1]], 2))),
                 tags$p(paste(input$attribute2, ":", round(hero[[input$attribute2]], 2))),
                 tags$p(paste("Alignment:", hero$alignment)),
                 tags$p(paste("Gender:", hero$gender)),
                 tags$p(paste("Hair Color:", hero$hair_color)),
                 tags$p(paste("Eye Color:", hero$eye_color)),
                 tags$p(paste("Rank:", hero$rank)),
                 tags$hr())
        )
      })
    )
  })
}

shinyApp(ui = ui, server = server)