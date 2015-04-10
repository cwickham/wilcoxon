# functions for determining truth of null hypotheses
source("pXgreaterYFun.R")

# Null 1: E(F) = E(G)
null1 <- reactive({
  isTRUE(all.equal(fs()$props$mean, gs()$props$mean))
})

# Null 2: median(F) = median(G)
null2 <- reactive({
  isTRUE(all.equal(fs()$props$median, gs()$props$median))
})

# Null 3: P(X > Y) = 0.5 X ~ F, Y ~ G
null3 <- reactive({
  isTRUE(all.equal(pXgreaterY(input$pop1, fs()$params, input$pop2, gs()$params), 0.5, tol = 1e-5))
})

# Null 4: F = G
null4 <- reactive({
  isTRUE(all.equal(dcurve()$y[dcurve()$pop == "Pop 1"], dcurve()$y[dcurve()$pop == "Pop 2"]))
})

pXgY <- reactive({
  pXgreaterY(input$pop1, fs()$params, input$pop2, gs()$params)
})
