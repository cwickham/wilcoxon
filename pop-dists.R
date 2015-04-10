# Functions that generate parameter UIs and set up sampling
# and density functions
`%||%` <- function(x, y) if (is.null(x)) y else x

# range for density plot
xlims <- c(-5, 15)
x <- seq(xlims[1], xlims[2], 0.1)

# == Single Normal == #
# =================== #

norm_ui <- function(prefix){
  wellPanel(
      sliderInput(paste0(prefix, "normal_mean"), "Mean",
        min = 0, max = 10, value = 1),
      sliderInput(paste0(prefix, "normal_sd"), "Standard deviation",
        min = 1, max = 5, value = 1)) 
}

norm_gen_funcs <- function(prefix) {
  reactive({
    mean <- input[[paste0(prefix, "normal_mean")]] %||% 1
    sd <- input[[paste0(prefix, "normal_sd")]] %||% 1
    
    list(rfunc = rnorm, dfunc = dnorm,
      params = list(mean = mean, sd = sd),
      props = list(mean = mean, median = mean))
  })
}


# == Normal Mixture == #
# ==================== #
source("mixtureDistFuns.R")

mixnorm_ui <- function(prefix){
 wellPanel(
      sliderInput(paste0(prefix, "mix_mean1"), "Mean 1",
        min = 0, max = 10, value = 2),
      sliderInput(paste0(prefix, "mix_sd1"), "Standard deviation 1",
        min = 1, max = 5, value = 1),
      sliderInput(paste0(prefix, "mix_mean2"), "Mean 2",
        min = 0, max = 10, value = 8),
      sliderInput(paste0(prefix, "mix_sd2"), "Standard deviation 2",
        min = 1, max = 5, value = 1),
      sliderInput(paste0(prefix, "mix_prop"), "Mixing proportion",
        min = 0, max = 1, value = 0.5, step = 0.1)) 
}

mixnorm_gen_funcs <- function(prefix) {
  reactive({
    mean1 <- input[[paste0(prefix, "mix_mean1")]] %||% 2
    mean2 <- input[[paste0(prefix, "mix_mean2")]] %||% 8
    sd1 <- input[[paste0(prefix, "mix_sd1")]] %||% 1
    sd2 <- input[[paste0(prefix, "mix_sd2")]] %||% 1
    prop <- input[[paste0(prefix, "mix_prop")]] %||% 0.5
    med <- qmixnorm(p = 0.5, p1 = prop, mu1 = mean1, sd1 = sd1, mu2 = mean2, sd2 = sd2)
    
    list(rfunc = rmixnorm, dfunc = dmixnorm,
      params = list(p1 = prop, mu1 = mean1, mu2 = mean2, sd1 = sd1, sd2 = sd2),
      props = list(mean = prop*mean1 + (1-prop)*mean2, 
        median = med))
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
    rate <- input[[paste0(prefix, "exp_rate")]] %||% 1
    
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
    rate <- input[[paste0(prefix, "gamma_rate")]] %||% 1
    shape <- input[[paste0(prefix, "gamma_shape")]] %||% 1
    
    list(rfunc = rgamma, dfunc = dgamma,
      params = list(shape = shape, rate = rate),
      props = list(mean = shape*(1/rate), median = qgamma(0.5, shape = shape, rate = rate)))
  })
}

