library(dplyr)

# range for density plot
xlims <- c(-10, 10)
x <- seq(xlims[1], xlims[2], 0.1)

shinyServer(function(input, output) {
  source("pop-dists.R", local = TRUE)
  
  # == choosing population distributions 
  # ===========================================================#
  pop_gen <- function(prefix){
    reactive({
      switch(input[[prefix]],
        "Normal" =  norm_ui(prefix),
        "Exponential" = exp_ui(prefix),
        "Gamma" = gamma_ui(prefix),
        "Mixture" = mixture_ui(prefix))
    })
  }
  
  pops <- c("pop1", "pop2")
   
  lapply(pops, function(prefix){
    output[[paste0(prefix, "_ui")]] <- renderUI(pop_gen(prefix)())}
  )
  
  fs <- reactive({
    switch(input$pop1,
      "Normal" = norm_gen_funcs("pop1")(),
      "Exponential" = exp_gen_funcs("pop1")(),
      "Gamma" = gamma_gen_funcs("pop1")(),
      "Mixture" = mixture_gen_funcs("pop1")())})
  
  gs <- reactive({
    switch(input$pop2,
      "Normal" = norm_gen_funcs("pop2")(),
      "Exponential" = exp_gen_funcs("pop2")(),
      "Gamma" = gamma_gen_funcs("pop2")(),
      "Mixture" = mixture_gen_funcs("pop2")())})

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
  
  
  observe({
    if(!is.null(dcurve())){
      dcurve %>% 
        ggvis(~x, ~y) %>%
        layer_paths(fill = ~ pop, opacity := 0.2) %>%
        scale_numeric("x", domain = xlims, expand = 0, nice = FALSE, clamp = TRUE) %>%
        bind_shiny("ggvis1")
    }})
  
  # == check truth of nulls == #
  # ===========================================================#
  # == simulate one sample == #
  # ===========================================================#  
  one_sample <- reactive({
    input$run_sample  
    m <- isolate(eval(parse(text = input$m)))
      
    # Use isolate() to avoid dependency on input$n and input$m
    isolate(data.frame(
      x = c(do.call(fs()$rfunc, c(list(n = input$n), fs()$params)),
            do.call(gs()$rfunc, c(list(n = m), gs()$params))),
        pop = rep(c("pop1", "pop2"), c(input$n, m))) %>% group_by(pop))
    })
    
  output$samp_hist <- renderPlot({
    ggplot(one_sample()) +
      geom_histogram(aes(x = x, fill = pop)) +
      facet_grid(pop ~ .) +
      scale_fill_manual(values = c("pop1" = "#1f77b4", "pop2" = "#ff7f0e" )) +
      theme_bw() + theme(legend.position = "none")
  })
  
  # == simulate many samples == #
  # ===========================================================#  
  sim_pvals <- reactive({
    # Take a dependency on input$goButton
    input$run_sim  
    m <- isolate(eval(parse(text = input$m)))
  
    # Use isolate() to avoid resimulating before pressing button
    ps <- isolate(data.frame(p = replicate(input$nsim, wilcox.test(
          do.call(fs()$rfunc, c(list(n = input$n), fs()$params)),  
          do.call(gs()$rfunc, c(list(n = m), gs()$params)), 
          exact = FALSE, correct = FALSE)$p.value)))
    ps$reject <- ifelse(ps$p < 0.05, "reject", "fail to reject")
    ps
  })
  
  output$p_hist <- renderPlot({
    ggplot(sim_pvals()) +
      geom_histogram(aes(x = p, fill= reject), breaks = seq(0, 1, 0.05), right = TRUE) +
      xlab("Wilcoxon p-value") + 
      scale_fill_manual(values = c("black", "#E41A1C")) +
      theme_bw() + theme(legend.position = "none") 
  })
  
  output$rej_rate <- renderText({
    paste(signif(mean(sim_pvals()$p < 0.05), 3) * 100, "%") 
  })
  
})