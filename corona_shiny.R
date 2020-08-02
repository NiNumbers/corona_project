library(shiny)
library(plotly)

load("corona_shiny.RData")

ui <- fluidPage(
  # input field
  wellPanel(
    fluidRow(
      column(6, 
        actionButton(inputId = "swiss", label = "Switzerland"),
        actionButton(inputId = "german", label = "Germany"),
        actionButton(inputId = "italy", label = "Italy"),
        actionButton(inputId = "usa", label = "USA")
      ),
      column(4, 
        actionButton(inputId = "log", label = "log-Plot"),
        actionButton(inputId = "lin", label = "linear-Plot")
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
  fluidRow(
    plotOutput("show_all", hover = hoverOpts(id = "plot_hover_all"))),
    fluidRow(
    verbatimTextOutput("hover_all") 
  ),
  fluidRow(
    plotOutput("show_zoom", hover = hoverOpts(id = "plot_hover"))),
  
  verbatimTextOutput("hover_zoom")
)

server <- function(input, output){
  rv <- reactiveValues(scal = "", lab = "tägliche Fallzahlen", 
                       data = corona_CH$CHtaegl, title = "COVID-19 cases Switzerland", 
                       datum = corona_CH$datum)
  observeEvent(input$log, c({rv$scal <- "y"}, 
                            {rv$lab <- "tägliche Fallzahlen, log"}))
  observeEvent(input$lin, c({rv$scal <- ""}, 
                            {rv$lab <- "tägliche Fallzahlen"}))
  observeEvent(input$swiss, c({rv$data <- corona_CH$CHtaegl}, 
                              {rv$title <- "COVID-19 cases Switzerland"}, 
                              {rv$datum <- corona_CH$datum}))
  observeEvent(input$german, c({rv$data <- corona_DE$DEtaegl}, 
                               {rv$title <- "COVID-19 cases Germany"}, 
                               {rv$datum <- corona_DE$datum }))
  observeEvent(input$italy, c({rv$data <- corona_IT$ITtaegl}, 
                              {rv$title <- "COVID-19 cases Italy"}, 
                              {rv$datum <- corona_IT$datum}))
  observeEvent(input$usa, c({rv$data <- corona_US$UStaegl}, 
                            {rv$title <- "COVID-19 cases USA"}, 
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
         ylim = c(0, max(rv$data[(length(rv$data) - 21):length(rv$data)])+10),
         ylab = rv$lab, xlab = "", type = "h", main = "3 weeks zoom", lwd = 3,
         lend = 1, col = "darkgrey")
    text(rv$datum[(length(rv$data)-21):length(rv$data)],
         rv$data[(length(rv$data) - 21):length(rv$data)]+10, labels = rv$data[(length(rv$data) - 21):length(rv$data)])
    if (input$l_yes == TRUE){
      lines(x = rv$datum,
            y = fitted(loess(rv$data ~ as.numeric(time(rv$datum)),
                             family = "symmetric", span = input$smooth)),
            col = "red", lty = 2, lwd = 2)
    }
  })
  output$hover_all<- renderText({
    if(!is.null(input$plot_hover_all)){
      hover=input$plot_hover_all
      plotdatum = as.Date(round(hover$x), origin = "1970-01-01")
      outputArgs = c("Date: ", as.character(plotdatum), ", COVID-19 cases: ", rv$data[match(round(hover$x), rv$datum)])
    }
    else{outputArgs = c("Hover on graph to see daily values")}
  })
  output$hover_zoom<- renderText({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      plotdatum = as.Date(round(hover$x), origin = "1970-01-01")
      outputArgs = c("Date: ", as.character(plotdatum), ", COVID-19 cases: ", rv$data[match(round(hover$x), rv$datum)])
    }
    else{outputArgs = c("Hover on graph to see daily values")}
})
}

shinyApp(server = server, ui = ui)
