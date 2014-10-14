shinyUI(fluidPage(
  titlePanel("Population distributions"),
  fluidRow(
    column(3, wellPanel(
        selectInput("pop1", "Population 1",
          c("Normal", "Uniform")),
        uiOutput("ui1")
      )
    ),    
    column(3, wellPanel(
      selectInput("pop2", "Population 2",
        c("Normal", "Uniform")),
      uiOutput("ui2")
    )),
    column(3,
        ggvisOutput("ggvis1")
    )
  ),
  fluidRow(column(6,
      h2("Truth of hypotheses")
    )
  ),
  titlePanel("Wilcoxon p-value distribution"),
  fluidRow(
    column(3, wellPanel(
      selectInput("nsim", "Number of simulations",
        c(500, 1000, 5000), selected = 500),
      numericInput("n", "Sample size, population 1, n:",
        10, 500, value = 30),
      textInput("m", "Sample size population 2, m", "n"),
      actionButton("run_sim", "Simulate")
    )),
    column(3, p("Rejection rate:", textOutput("rej_rate"))),
    column(3, ggvisOutput("p_hist")))
    
  )
)

