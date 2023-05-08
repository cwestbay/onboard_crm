library(shiny)

ui <- fluidPage(
  titlePanel("Heuristic Chancing Tool"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = 'u_gpa',
                  label = "Unweighted GPA:",
                  min = 1.5,
                  max = 4,
                  value = 3.33),
      sliderInput(inputId = 's_sat',
                  label = "Highest SAT score:",
                  min = 1000,
                  max = 1600,
                  value = 1380,
                  step = 10),
      sliderInput(inputId = 't_toefl',
                  label = "Highest ToEFL score:",
                  min = 70,
                  max = 120,
                  value = 104),
      sliderInput(inputId = 'r_ap',
                  label = "Sum of AP scores:",
                  min = 6, 
                  max = 70,
                  value = 28)
      ), 
      
    mainPanel(
        textOutput("calc_index"),
        textOutput("calc_rank")
      )
    
  )
)

server <-   function(input, output) {

  input_values <- observe({
    
    print(input$u_gpa) 
    print(input$s_sat)
    print(input$t_toefl)
    print(input$r_ap)
    
    })
  calc_ai <- reactive({
    round(((((input$u_gpa - 2.08)/1.92)+((input$s_sat - 1100)/500)+((input$t_toefl - 74)/46)+((input$r_ap - 6)/128))/(.035)),2)
  })
  calc_chance <- reactive({
    round(82.63184 + (-.71984 * calc_ai()),0)
  })
  
  output$calc_index <- renderText({
    paste("Based on the provided data, your calculated academic index is", calc_ai())
  })
  output$calc_rank <- renderText({
    paste("Based on the provided data, the ranking of your projected outcome is", calc_chance())
  })
}

shinyApp(ui = ui, server = server)
