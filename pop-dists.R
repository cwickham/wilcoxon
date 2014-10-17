# Functions that generate parameter UIs and set up sampling
# and density functions

# == Single Normal == #
# =================== #

norm_ui <- function(prefix){
  wellPanel(
      sliderInput(paste0(prefix, "normal_mean"), "mean",
        min = -5, max = 5, value = 0),
      sliderInput(paste0(prefix, "normal_sd"), "sd",
        min = 1, max = 5, value = 1)) 
}

norm_gen_funcs <- function(prefix) {
  reactive({
    mean <- input[[paste0(prefix, "normal_mean")]]
    sd <- input[[paste0(prefix, "normal_sd")]]
    if(is.null(mean)) mean <- 0
    if(is.null(sd)) sd <- 1
    list(rfunc = rnorm, dfunc = dnorm,
      params = list(mean = mean, sd = sd),
      props = list(mean = mean, median = mean))
  })
}


# == Normal Mixture == #
# ==================== #

rmix <- function(n, means, sds, prop){
  which_dist <- sample(1:2, size = n, prob = c(prop, 1-prop), replace = TRUE)
  rnorm(n, mean = means[which_dist], sd = sds[which_dist])
}

dmix <- function(x, means, sds, prop){
  prop*dnorm(x, mean = means[1], sd = sds[1]) + (1-prop)*dnorm(x, mean = means[2], sd = sds[2])
}

mixture_ui <- function(prefix){
 wellPanel(
      sliderInput(paste0(prefix, "mix_mean1"), "mean1",
        min = -5, max = 5, value = -1),
      sliderInput(paste0(prefix, "mix_sd1"), "sd1",
        min = 1, max = 5, value = 1),
      sliderInput(paste0(prefix, "mix_mean2"), "mean2",
        min = -5, max = 5, value = 1),
      sliderInput(paste0(prefix, "mix_sd2"), "sd2",
        min = 1, max = 5, value = 1),
      sliderInput(paste0(prefix, "mix_prop"), "proportion 1",
        min = 0, max = 1, value = 0.5, step = 0.1)) 
}

mixture_gen_funcs <- function(prefix) {
  reactive({
    mean1 <- input[[paste0(prefix, "mix_mean1")]]
    mean2 <- input[[paste0(prefix, "mix_mean2")]]
    sd1 <- input[[paste0(prefix, "mix_sd1")]]
    sd2 <- input[[paste0(prefix, "mix_sd2")]]
    prop <- input[[paste0(prefix, "mix_prop")]]
    
    if(is.null(mean1)) mean1 <- -1
    if(is.null(mean2)) mean2<- 1
    if(is.null(sd1)) sd1 <- 1
    if(is.null(sd2)) sd2<- 1
    if(is.null(prop)) prop <- 0.5
    
    list(rfunc = rmix, dfunc = dmix,
      params = list(means = c(mean1, mean2), sds = c(sd1, sd2), prop = prop),
      props = list(mean = prop*mean1 + (1-prop)*mean2, median = NA))
  })
}


# == Exponential == #
# ================= #

exp_ui <- function(prefix){
  wellPanel(
      sliderInput(paste0(prefix, "exp_rate"), "rate",
        min = 1, max = 5, value = 1))
} 

exp_gen_funcs <- function(prefix){
  reactive({
    rate <- input[[paste0(prefix, "exp_rate")]]
    if(is.null(rate)) rate <- 1
    list(rfunc = rexp, dfunc = dexp,
      params = list(rate = rate),
      props = list(mean = 1/rate, median = 1/rate*log(2)))
  })
}

# == Gamma == #
# ================= #

gamma_ui <- function(prefix){
  ui = wellPanel(
      sliderInput(paste0(prefix, "gamma_shape"), "shape",
        min = 1, max = 5, value = 1),
      sliderInput(paste0(prefix, "gamma_rate"), "rate",
        min = 1, max = 5, value = 1))
} 

gamma_gen_funcs <- function(prefix){
  reactive({
    rate <- input[[paste0(prefix, "gamma_rate")]]
    if(is.null(rate)) rate <- 1
    shape <- input[[paste0(prefix, "gamma_shape")]]
    if(is.null(shape)) shape <- 1
    
    list(rfunc = rgamma, dfunc = dgamma,
      params = list(shape = shape, rate = rate),
      props = list(mean = shape*(1/rate), median = qgamma(0.5, shape = shape, rate = rate)))
  })
}

