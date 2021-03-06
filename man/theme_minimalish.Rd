% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/custom_ggplot2_themes.R
\name{theme_minimalish}
\alias{theme_minimalish}
\title{Minimalish theme for ggplot2 plots.}
\usage{
theme_minimalish(base_size = 12, base_family = "")
}
\arguments{
\item{base_size}{Given reference size.}

\item{base_family}{Given font family.}
}
\value{
Prints or saves the given plot in a minimalish color scheme
}
\description{
\code{theme_minimalish} gives a ggplot2 plot a minimalist color scheme to integrate it
in publishing env's.
}
\details{
This function is part of a family of theming functions for ggplot2 plots
which includes blue, green, red, grey and minimalist combinations.
}
\examples{
p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
colour=factor(gear))) + facet_wrap(~am)

p
p + theme_minimalish()
}
\seealso{
\code{\link{theme_bluish}} for blue, \code{\link{theme_greenish}}
  for green, \code{\link{theme_redish}} for red, as well as \code{\link{theme_minimalish}}
  for a black&white minimalist plot.

Other ggplot2 theming functions: \code{\link{theme_greenish}};
  \code{\link{theme_greyish}}; \code{\link{theme_redish}}
}

