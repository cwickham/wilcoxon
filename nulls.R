# functions for determining truth of null hypotheses

# Null 1: F = G
null1 <- reactive({
  isTRUE(all.equal(dcurve()$y[dcurve()$pop == "Pop 1"], dcurve()$y[dcurve()$pop == "Pop 2"]))
})
# Null 2: E(F) = E(G)
null2 <- reactive({
  isTRUE(all.equal(fs()$props$mean, gs()$props$mean))
})

# Null 3: median(F) = median(G)
null3 <- reactive({
  isTRUE(all.equal(fs()$props$median, gs()$props$median))
})

# Null 4: P(X > Y) = 0.5 X ~ F, Y ~ G
null4 <- reactive({
  NA
})