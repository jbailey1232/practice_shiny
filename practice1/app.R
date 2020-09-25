library(shiny)
library(ggplot2)
library(vroom)
library(plyr)

ui <- fluidPage(
    titlePanel("Simple Histogram Plot"),
    fileInput("file", NULL, accept = c(".csv", ".tsv")),
    textInput("plot_title", label = "Enter plot title", width = 300, placeholder = "Study ABCD vs Study BCDE"),
    textInput("x_title", label = "Enter x-axis title", width = 300, placeholder ="e.g., S:N or ECLU"),
    numericInput("n", "Bin Width", value = 0.15, min = .01, step = .01, width = 150),
    tableOutput("stats"),
    plotOutput("histo2"),
    plotOutput("histo3")
)

server <- function(input, output, session) {
    data <- reactive({
        req(input$file)
        ext <- tools::file_ext(input$file$name)
        switch(ext,
               csv = vroom::vroom(input$file$datapath, delim = ","),
               tsv = vroom::vroom(input$file$datapath, delim = "\t"),
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
    })
    
    output$stats <- renderTable({
        ddply(data(), .(Study), summarize, N = length(Study), Mean = mean(Value),
              Median = median(Value), SD = sd(Value))
    })
    
    output$histo2 <- renderPlot({
        ggplot(data(), aes(x=Value, fill = Study)) +
            geom_histogram(binwidth = input$n, alpha = .7, position = "identity", aes(y=..density..)) +
            labs(title = input$plot_title, x = input$x_title) +
            theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(size = 12))
    }, res = 96)
    
    output$histo3 <- renderPlot({
        ggplot(data(), aes(x=Value, fill = Study)) +
            facet_grid(rows = vars(Study)) +
            geom_histogram(binwidth = input$n, alpha = .7, position = "identity", aes(y=..density..)) +
            labs(title = input$plot_title, x = input$x_title) +
            theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(size = 12))
    }, res = 96)
    
}

shinyApp(ui, server)