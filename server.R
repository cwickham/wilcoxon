library(ggvis)
library(ggplot2)
library(dplyr)
library(pryr)

# range for density plot
x <- seq(-5, 5, 0.1)

# Wilcox p_value function
w_sim <- function(n, m, f, g){
  wilcox.test(f(n), g(m), exact = FALSE, correct = FALSE)$p.value  
}

# ToDo:
# * need to check hypothesis truths
# * just update UI instead of generating http://shiny.rstudio.com/gallery/update-input-demo.html


shinyServer(function(input, output) {
  norm_params <- function(prefix){
      # mean <- input[[paste0(prefix, "nmean")]]
      # sd <- input[[paste0(prefix, "nsd")]]
      # defaults when inputs aren't set
      # if(is.null(mean)) mean  <- 0
      # if(is.null(sd)) sd  <- 1
      
      list(
        ui = wellPanel(
          sliderInput(paste0(prefix, "nmean"), "mean",
            min = -5, max = 5, value = 0),
          sliderInput(paste0(prefix, "nsd"), "sd",
            min = 1, max = 5, value = 1)),
        p_func = partial(dnorm, mean = ifelse(is.null(input[[paste0(prefix, "nmean")]]), 0, input[[paste0(prefix, "nmean")]]), sd = ifelse(is.null(input[[paste0(prefix, "nsd")]]), 1, input[[paste0(prefix, "nsd")]])),
        r_func = partial(rnorm, mean = ifelse(is.null(input[[paste0(prefix, "nmean")]]), 0, input[[paste0(prefix, "nmean")]]), sd = ifelse(is.null(input[[paste0(prefix, "nsd")]]), 1,input[[paste0(prefix, "nsd")]])))
  }
  
  uniform_params <- function(prefix){
    list(
    ui = wellPanel(
      sliderInput(paste0(prefix, "ucenter"), "center",
        min = -5, max = 5, value = 0),
      sliderInput(paste0(prefix, "uwidth"), "width",
        min = 1, max = 5, value = 1)),
    p_func = function(x) {
      dunif(x, min = ifelse(is.null(input[[paste0(prefix, "ucenter")]]), 0, input[[paste0(prefix, "ucenter")]] - (1/2)*input[[paste0(prefix, "uwidth")]]), 
        max = ifelse(is.null(input[[paste0(prefix, "ucenter")]]), 1, input[[paste0(prefix, "ucenter")]] + (1/2)*input[[paste0(prefix, "uwidth")]]))},
    r_func = function(n) runif(n, min = input[[paste0(prefix, "ucenter")]] - (1/2)*input[[paste0(prefix, "uwidth")]], 
      max = input[[paste0(prefix, "ucenter")]] + (1/2)*input[[paste0(prefix, "uwidth")]]))
}

  pop_gen <- function(prefix){
    reactive({
      switch(input[[prefix]],
        "Normal" =  norm_params(prefix),
        "Uniform" = uniform_params(prefix))
    })
  }
  
  pop1_gen <- pop_gen("pop1")
  pop2_gen <- pop_gen("pop2")
  
  output$ui1 <- renderUI({
    if (is.null(input$pop1))
      return()
    pop1_gen()$ui
  })
  
  
    
  output$ui2 <- renderUI({
    if (is.null(input$pop2))
      return()
    pop2_gen()$ui
  })
  
  dcurve <- reactive({
    group_by(data.frame(x = c(x, x), 
      y = c(pop1_gen()$p_func(x), pop2_gen()$p_func(x)),
      pop = rep(c("Pop 1", "Pop 2"), c(length(x), length(x)))),
      pop)
  })
  
  dcurve %>% 
    ggvis(~x, ~y) %>%
    layer_paths(fill = ~ pop, opacity := 0.2) %>%
    bind_shiny("ggvis1")
  
  one_sample <- reactive({
    input$run_sim  
    m <- isolate(eval(parse(text = input$m)))
    
    # Use isolate() to avoid dependency on input$obs
    isolate(data.frame(
        x = c(pop1_gen()$r_func(input$n), pop2_gen()$r_func(m)),
        pop = rep(c("pop1", "pop2"), c(input$n, m))) %>% group_by(pop))
  })
  
  sim_pvals <- reactive({
    # Take a dependency on input$goButton
      input$run_sim  
      m <- isolate(eval(parse(text = input$m)))
    
    # Use isolate() to avoid resimulating before pressing button
    isolate(data.frame(p = replicate(input$nsim, wilcox.test(
          pop1_gen()$r_func(input$n),  
          pop2_gen()$r_func(m), 
          exact = FALSE, correct = FALSE)$p.value)))
    
  })
  
  ggvis(one_sample) %>%
    layer_histograms(x = ~ x, fill= ~ pop, opacity := 0.2) %>%
    bind_shiny("samp_hists")
  
  ggvis(sim_pvals) %>%
    layer_histograms(x = ~ p, origin = 0, binwidth = 0.05, right = FALSE) %>%
    bind_shiny("p_hist")

  output$rej_rate <- renderText({
    paste(signif(mean(sim_pvals() < 0.05), 2) * 100, "%") 
  })
  
})
# for each dist
# function that creates UI for parameters
# construct sampling function
# constructs curve for plotting
