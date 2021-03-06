% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/custom_ggplot2_themes.R
\name{theme_greenish}
\alias{theme_greenish}
\title{Greenish theme for ggplot2 plots.}
\usage{
theme_greenish(base_size = 12, base_family = "")
}
\arguments{
\item{base_size}{Given reference size.}

\item{base_family}{Given font family.}
}
\value{
Prints or saves the given plot in a greenish color scheme
}
\description{
\code{theme_greenish} gives a ggplot2 plot a green color scheme to integrate
it in bluish env's.
}
\details{
This function is part of a family of theming functions for ggplot2 plots
which includes blue, green, red, grey and minimalist combinations.
}
\examples{
p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
colour=factor(gear))) + facet_wrap(~am)

p
p + theme_greenish()
}
\seealso{
\code{\link{theme_bluish}} for blue, \code{\link{theme_redish}}
  for red, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
  for a black&white minimalist plot.

Other ggplot2 theming functions: \code{\link{theme_greyish}};
  \code{\link{theme_minimalish}};
  \code{\link{theme_redish}}
}

