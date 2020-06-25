library(shiny)

load("corona_shiny.RData")

ui <- fluidPage(
  # input field
  wellPanel(
    tags$h2("Daily COVID-19 cases"),
    fluidRow(
      column(6, 
        tags$h4("Choose country"),
        actionButton(inputId = "swiss", label = "Switzerland"),
        actionButton(inputId = "german", label = "Germany"),
        actionButton(inputId = "italy", label = "Italy"),
        actionButton(inputId = "usa", label = "USA")
      ),
      column(4, 
        tags$h4("Choose scale"),
        actionButton(inputId = "log", label = "log-Plot"),
        actionButton(inputId = "lin", label = "linear-Plot")
      )
    ),
    fluidRow(
      column(6,
        tags$h4("Choose loess faktor")
      )
    ),
    fluidRow(
      column(6, 
        sliderInput(inputId = "smooth", label = " ", 
                  value = 0.5, min = 0.1, max = 0.9)
        ),
      column(4,
        checkboxInput(inputId = "l_yes", label = "Show loess line", TRUE)
      )
    )
  ),
  # show results  
  plotOutput("show_all"),
  plotOutput("show_zoom")
)

server <- function(input, output){
  rv <- reactiveValues(scal = "", lab = "tägliche Fallzahlen", 
                       data = corona_CH$CHtaegl, title = "Switzerland", 
                       datum = corona_CH$datum)
  observeEvent(input$log, c({rv$scal <- "y"}, 
                            {rv$lab <- "tägliche Fallzahlen, log"}))
  observeEvent(input$lin, c({rv$scal <- ""}, 
                            {rv$lab <- "tägliche Fallzahlen"}))
  observeEvent(input$swiss, c({rv$data <- corona_CH$CHtaegl}, 
                              {rv$title <- "Switzerland"}, 
                              {rv$datum <- corona_CH$datum}))
  observeEvent(input$german, c({rv$data <- corona_DE$DEtaegl}, 
                               {rv$title <- "Germany"}, 
                               {rv$datum <- corona_DE$datum }))
  observeEvent(input$italy, c({rv$data <- corona_IT$ITtaegl}, 
                              {rv$title <- "Italy"}, 
                              {rv$datum <- corona_IT$datum}))
  observeEvent(input$usa, c({rv$data <- corona_US$UStaegl}, 
                            {rv$title <- "USA"}, 
                            {rv$datum <- corona_US$datum}))

  output$show_all <- renderPlot({
    plot(rv$datum, rv$data, log = rv$scal, ylab = rv$lab, xlab = "", 
         main = rv$title, type = "h",lwd = 3, lend = 1, col = "darkgrey")
      if (input$l_yes == TRUE){
      lines(x = rv$datum, 
                  y = fitted(loess(rv$data ~ as.numeric(time(rv$datum)), 
                                   family = "symmetric", span = input$smooth)), 
                  col = "red", lty = 2, lwd = 2)
      }
  })
  output$show_zoom <- renderPlot({
    plot(rv$datum[(length(rv$data)-21):length(rv$data)], 
         rv$data[(length(rv$data) - 21):length(rv$data)], log = rv$scal, 
         ylab = rv$lab, xlab = "",          main = "3 weeks zoom", type = "h",
         lwd = 3, lend = 1, col = "darkgrey")
    if (input$l_yes == TRUE){
      lines(x = rv$datum, 
            y = fitted(loess(rv$data ~ as.numeric(time(rv$datum)), 
                             family = "symmetric", span = input$smooth)), 
            col = "red", lty = 2, lwd = 2)
    }
  })
}

shinyApp(server = server, ui = ui)