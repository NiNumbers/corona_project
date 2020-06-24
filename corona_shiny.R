library(shiny)

load("corona_shiny.RData")

ui <- fluidPage(
  # input field
  wellPanel(
    tags$h2("Daily COVID-19 cases"),
    fluidRow(),
    tags$h4("Choose country"),
    fluidRow(
      actionButton(inputId = "swiss", label = "Switzerland"),
      actionButton(inputId = "german", label = "Germany"),
      actionButton(inputId = "italy", label = "Italy"),
      actionButton(inputId = "usa", label = "USA")
      ),
    tags$h4("Choose scale"),
    fluidRow(
      actionButton(inputId = "log", label = "log-Plot"),
      actionButton(inputId = "lin", label = "linear-Plot")
        ),
    tags$h4("Choose loess"),
    fluidRow(
      checkboxInput(inputId = "l_yes", label = "Show loess line", TRUE)
    ),
      sliderInput(inputId = "smooth", label = " ", value = 0.5, min = 0.1, max = 0.9)
  ),
  # show results  
  plotOutput("show"), 
)

server <- function(input, output){
  
  rv <- reactiveValues(scal = "", lab = "tägliche Fallzahlen", data = corona_taegl$CHtaeglich, title = "Switzerland", datum = corona_taegl$datum)
  
  observeEvent(input$log, c({rv$scal <- "y"}, {rv$lab <- "tägliche Fallzahlen, log"}))
  observeEvent(input$lin, c({rv$scal <- ""}, {rv$lab <- "tägliche Fallzahlen"}))
  observeEvent(input$swiss, c({rv$data <- corona_taegl$CHtaeglich}, {rv$title <- "Switzerland"}, {rv$datum <- corona_taegl$datum}))
  observeEvent(input$german, c({rv$data <- corona_DE$DEtaegl}, {rv$title <- "Germany"}, {rv$datum <- corona_DE$datum }))
  observeEvent(input$italy, c({rv$data <- corona_IT$ITtaegl}, {rv$title <- "Italy"}, {rv$datum <- corona_IT$datum}))
  observeEvent(input$usa, c({rv$data <- corona_US$UStaegl}, {rv$title <- "USA"}, {rv$datum <- corona_US$datum}))

  output$show <- renderPlot({
    plot(rv$datum, rv$data, log = rv$scal, ylab = rv$lab, xlab = "", main = rv$title, type = "h",lwd = 3, lend = 1, col = "darkgrey")
      if (input$l_yes == TRUE){
      lines(lines(x = rv$datum, y = fitted(loess(rv$data ~ as.numeric(time(rv$datum)), family = "symmetric", span = input$smooth)), col = "red", lty = 2, lwd = 2))
      }
  })
}

shinyApp(server = server, ui = ui)