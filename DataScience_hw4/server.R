library(shiny)
library(ggvis)

d<-read.table("result.csv", header=T,sep=",")

shinyServer(function(input, output, session) {
  # A reactive subset of mtcars
  mtc <- reactive({ d[1:input$n, ] })
  # A simple visualisation. In shiny apps, need to register observers and tell shiny where to put the controls
  mtc %>%
    ggvis(~specificitys, ~sensitivitys) %>%
    layer_points() %>%
    bind_shiny("plot", "plot_ui")
  output$table <- renderTable({
    mtc()[, c("specificitys", "sensitivitys")]
  })
})