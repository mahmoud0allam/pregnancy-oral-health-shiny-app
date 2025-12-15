library(shiny)

ui <- fluidPage(
  titlePanel("Pregnancy & Oral Health Checker"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "trimester",
        "Pregnancy trimester:",
        choices = c("First trimester", "Second trimester", "Third trimester")
      ),
      
      radioButtons(
        "bleeding",
        "Do your gums bleed during brushing?",
        choices = c("No", "Yes"),
        selected = "No"
      ),
      
      sliderInput(
        "brush",
        "Brushing frequency (times per day):",
        min = 0, max = 3, value = 2
      )
    ),
    
    mainPanel(
      h3("Assessment"),
      verbatimTextOutput("risk"),
      h3("Advice"),
      verbatimTextOutput("advice")
    )
  )
)

server <- function(input, output, session) {
  
  risk_score <- reactive({
    s <- 0
    
    if (input$trimester == "First trimester") s <- s + 1
    if (input$bleeding == "Yes") s <- s + 2
    if (input$brush <= 1) s <- s + 2
    
    s
  })
  
  risk_level <- reactive({
    s <- risk_score()
    if (s <= 1) "Low"
    else if (s <= 3) "Medium"
    else "High"
  })
  
  output$risk <- renderText({
    paste("Oral health risk during pregnancy:", risk_level())
  })
  
  output$advice <- renderText({
    lvl <- risk_level()
    
    if (lvl == "Low") {
      "Maintain good oral hygiene. Routine dental check-ups are safe during pregnancy, especially in the second trimester."
    } else if (lvl == "Medium") {
      "Pay attention to gum health. Brush twice daily with fluoride toothpaste and consider a dental check-up."
    } else {
      "High risk: Gum bleeding during pregnancy should not be ignored. Consult a dentist. The second trimester is usually the safest period for dental treatment."
    }
  })
}

shinyApp(ui, server)
