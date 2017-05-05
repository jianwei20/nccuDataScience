library(ggvis)


d<-read.table("result.csv", header=T,sep=",")

shinyUI(pageWithSidebar(
  "Data Science HW4",
  sidebarPanel(
    sliderInput("n", "Methods", min = 1, max = nrow(d),
                value = 5, step = 1),
    uiOutput("plot_ui")
  ),
  mainPanel(
    ggvisOutput("plot"),
    tableOutput("table")
  )
))