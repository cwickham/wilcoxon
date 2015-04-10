library(ggvis)
library(shinythemes)

pop_ui <- function(prefix, label){
  column(3,    
    selectInput(prefix, label,
      c("Normal" = "norm", "Gamma" = "gamma", "Mixture" = "mixnorm")),
    uiOutput(paste0(prefix, "_ui")))
}

shinyUI(
fluidPage(theme = shinytheme("spacelab"),  
  titlePanel("Population distributions"),
  fluidRow(
    pop_ui("pop1", "Population 1, F"),
    pop_ui("pop2", "Population 2, G"),
    column(6, 
      ggvisOutput("ggvis1"),
      ggvisOutput("ggvis2"),
      ggvisOutput("ggvis3"),
      p("P(X > Y) = ", textOutput("pXgreaterY", container = span)))),
  fluidRow(column(6, 
      h2("Truth of nulls"),
      withMathJax(), uiOutput("null"))),
  h2("Samples from populations"),
  fluidRow(
    column(3, wellPanel(
      numericInput("n", "n =",
        10, 500, value = 30),
      textInput("m", "m=", "n"),
      actionButton("run_sample", "Sample", class = "btn btn-small")
    )),
    column(6, offset = 1, plotOutput("samp_hist"))
  ),
  h2("Simulate Wilcoxon"),
  fluidRow(
    column(3, wellPanel(
      selectInput("nsim", "Number of simulations",
        c(500, 1000, 5000), selected = 500),
      actionButton("run_sim", "Simulate")
    )),
    column(1, p("Rejection rate:", textOutput("rej_rate"))),
    column(6, plotOutput("p_hist"))
  ) #,
  #fluidRow(verbatimTextOutput("check"))
))
