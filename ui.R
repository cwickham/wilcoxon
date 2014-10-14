shinyUI(fluidPage(
  titlePanel("Choosing population distributions"),
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
    column(2,
        ggvisOutput("ggvis1")
    )
  ),
  fluidRow(
    column(3, wellPanel(
      selectInput("nsim", "Number of simulations",
        c(500, 1000, 5000), selected = 500)),
      actionButton("run_sim", "Simulate")
    )
  )
))

