library(ggvis)
library(shinythemes)

pop_ui <- function(prefix, label){
  column(3,    
    selectInput(prefix, label,
      c("Normal" = "norm", "Gamma" = "gamma", 
        "Mixture of Normals" = "mixnorm")),
    uiOutput(paste0(prefix, "_ui")))
}

construct_null <- function(name, val, cond){
  p(name, textOutput(val, container = strong), 
    span(cond, class = "text-muted"))
}

shinyUI(
fluidPage(theme = shinytheme("spacelab"),  
  p("Code available at:", a("https://github.com/cwickham/wilcoxon",
    href = "https://github.com/cwickham/wilcoxon")),
  titlePanel("Population distributions"),
  fluidRow(withMathJax(),
    pop_ui("pop1", "Population 1, F"),
    pop_ui("pop2", "Population 2, G"),
    column(6, 
      construct_null("Equal distribtions", "null4", 
        "\\(H_0^{(4)}: F = G \\)"),
      ggvisOutput("ggvis1"), 
      construct_null("Equal means", 
        "null1", "\\(H_0^{(1)}: \\mu_F = \\mu_G \\)"),
      ggvisOutput("ggvis2"),
      construct_null("Equal medians", "null2", 
        "\\(H_0^{(2)}: m_F = m_G \\)"),
      ggvisOutput("ggvis3"),
      construct_null("Symmetry of \\(P(X > Y)\\)", "null3", 
        "\\(H_0^{(3*)}: \\theta = P(X > Y) = 0.5\\)"),
      p("P(X > Y) = ", textOutput("pXgreaterY", container = span)))),
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
