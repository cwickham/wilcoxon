library(ggvis)
library(dplyr)
library(pryr)

# range for density plot
x <- seq(-5, 5, 0.1)

# ToDo:
# * better way to store population dists
# * need to check hypothesis truths
# * simulate p-values from wilcox.tests


# write function that generates pick pop window
# bundle/extract normal_params, and uniform_params from pick pop
# pop1 <- gen_pop("1")
# pop2 <- gen_pop("2")
# plot_pop, sim_pop


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
  
  rfunc1 <- reactive({
    if (is.null(input$pop1))
      return()
    pop1_gen()$r_func
  })
  
  
    
  output$ui2 <- renderUI({
    if (is.null(input$pop2))
      return()
    pop2_gen()$ui
  })
  
  rfunc2 <- reactive({
    if (is.null(input$pop2))
      return()
    pop2_gen()$r_func
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
  
  
  
})
# for each dist
# function that creates UI for parameters
# construct sampling function
# constructs curve for plotting
