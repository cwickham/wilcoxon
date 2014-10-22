library(ggplot2)
library(ggvis)
library(dplyr)

# range for density plot
xlims <- c(-10, 10)
x <- seq(xlims[1], xlims[2], 0.1)

shinyServer(function(input, output) {
  source("pop-dists.R", local = TRUE)
  source("nulls.R", local = TRUE)
  
  # == choosing population distributions 
  # ===========================================================#
  pop_gen <- function(prefix){
    reactive({
      switch(input[[prefix]],
        "norm" = norm_ui(prefix),
        "exp" = exp_ui(prefix),
        "gamma" = gamma_ui(prefix),
        "mixnorm" = mixnorm_ui(prefix))
    })
  }
  
  pops <- c("pop1", "pop2")
   
  lapply(pops, function(prefix){
    output[[paste0(prefix, "_ui")]] <- renderUI(pop_gen(prefix)())}
  )
  
  gen_funcs <- function(prefix) {
    reactive({
      switch(input[[prefix]],
        "norm" = norm_gen_funcs(prefix)(),
        "exp" = exp_gen_funcs(prefix)(),
        "gamma" = gamma_gen_funcs(prefix)(),
        "mixnorm" = mixnorm_gen_funcs(prefix)())
    })
  }
  
  # functionalize and remove duplication at some point

fs <- reactive({
  switch(input$pop1,
    "norm" = norm_gen_funcs("pop1")(),
    "exp" = exp_gen_funcs("pop1")(),
    "gamma" = gamma_gen_funcs("pop1")(),
    "mixnorm" = mixnorm_gen_funcs("pop1")())
  
})

  gs <- reactive({
    switch(input$pop2,
      "norm" = norm_gen_funcs("pop2")(),
      "exp" = exp_gen_funcs("pop2")(),
      "gamma" = gamma_gen_funcs("pop2")(),
      "mixnorm" = mixnorm_gen_funcs("pop2")())})

  output$check <- renderPrint({reactiveValuesToList(input)})
  # == plotting population distributions 
  # ===========================================================#
  
  dcurve <- reactive({
    tmp <- do.call(fs()$dfunc, c(list(x = x), fs()$params))
    tmp2 <- do.call(gs()$dfunc, c(list(x = x), gs()$params))
    
    group_by(data.frame(x = c(min(x) -0.5, x, max(x) + 0.5, min(x) -0.5, x, max(x) +0.5), 
      y = c(0, tmp, 0, 0, tmp2, 0),
      pop = rep(c("Pop 1", "Pop 2"), c(length(x) + 2, length(x) + 2))),
      pop)
  })
  
  mean_lines <- reactive({
    data.frame(y = c(0, 1, 0, 1),
      x = rep(c(fs()$props$mean, gs()$props$mean), each = 2),
      pop = rep(c("Pop 1", "Pop 2"), c(2, 2)))
  })
  
  dcurve %>% 
    ggvis(~x, ~y) %>%
    layer_paths(fill = ~ pop, opacity := 0.4) %>%
#    layer_lines(data = mean_lines()) %>%
    scale_numeric("x", domain = xlims, expand = 0, nice = FALSE, clamp = TRUE) %>% 
    set_options(width = 300, height = 200) %>%   
    hide_axis("y") %>%
    add_axis("x", grid = FALSE) %>%
    bind_shiny("ggvis1")

  # == check truth of nulls == #
  # ===========================================================#

  output$null <- renderUI({
    div(p("Null 1, F = G:", null1()), 
      p("Null 2, E(F) = E(G):", null2()),
      p("Null 3, median(F) = median(G)", null3()), 
      p("Null 4 P(X > Y) = 0.5, X~F, Y~G", null4()))
  })
  
  
  # == simulate one sample == #
  # ===========================================================#  
  one_sample <- reactive({
    input$run_sample 
    if(input$run_sample == 0) return()
    m <- isolate(eval(parse(text = input$m), envir = reactiveValuesToList(input)))
      
    # Use isolate() to avoid dependency on input$n and input$m
    isolate(data.frame(
      x = c(do.call(fs()$rfunc, c(list(n = input$n), fs()$params)),
            do.call(gs()$rfunc, c(list(n = m), gs()$params))),
        pop = rep(c("pop1", "pop2"), c(input$n, m))) %>% group_by(pop))
    })
    
  output$samp_hist <- renderPlot({
    if(input$run_sample == 0) return()
    ggplot(one_sample()) +
      geom_histogram(aes(x = x, fill = pop), alpha = 0.4) +
      facet_grid(pop ~ .) +
      scale_fill_manual(values = c("pop1" = "#1f77b4", "pop2" = "#ff7f0e" )) +
      theme_bw() + theme(legend.position = "none")
  })
  
  # == simulate many samples == #
  # ===========================================================#  
  sim_pvals <- reactive({
    # Take a dependency on input$goButton
    input$run_sim  
    if(input$run_sim == 0) return()
    m <- isolate(eval(parse(text = input$m), envir = reactiveValuesToList(input)))
    
    # Use isolate() to avoid resimulating before pressing button
    ps <- isolate(data.frame(p = replicate(input$nsim, wilcox.test(
          do.call(fs()$rfunc, c(list(n = input$n), fs()$params)),  
          do.call(gs()$rfunc, c(list(n = m), gs()$params)), 
          exact = FALSE, correct = FALSE)$p.value)))
    ps$reject <- ifelse(ps$p < 0.05, "reject", "fail to reject")
    ps
  })
  
  output$p_hist <- renderPlot({
    if(input$run_sim == 0) return()
    ggplot(sim_pvals()) +
      geom_histogram(aes(x = p, fill= reject), breaks = seq(0, 1, 0.05), right = TRUE) +
      xlab("Wilcoxon p-value") + 
      scale_fill_manual(values = c("grey40", "#E41A1C")) +
      theme_bw() + theme(legend.position = "none") 
  })
  
  output$rej_rate <- renderText({
    paste(signif(mean(sim_pvals()$p < 0.05), 3) * 100, "%") 
  })
  
})