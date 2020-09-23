# R Milkbot
#
# This is a package to integrate the milkbot lactation curve model
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' @export

function_milkbot <- function(scale=30 , decay=0.00231, offset=0, ramp=23, days=1){
  a <- scale
  b <- ramp
  c <- offset #is set to 0 by default
  d <- decay
  t <- days
  milk <- a*(1-(exp((c-t)/b)/2))*exp(-d*t)
}

plot_curve <- function(scale=30 , decay=0.00231, offset=0, ramp=23, min_dim=1, max_dim=305){

  # Generate days in milk and predicted milkbot
  dim <- seq(min_dim, max_dim, by=10)
  milk_yield_kg <- function_milkbot(scale = scale, decay = decay, ramp =  ramp, offset = offset, days=dim)

  # Draw the lactation curve
  ggplot2::ggplot(mapping = ggplot2::aes(x=dim, y=milk_yield_kg, group=1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::ylim(0, 60) +
    ggplot2::ylab("Milk yield (kg)") +
    ggplot2::xlab("Days in milk")
}
