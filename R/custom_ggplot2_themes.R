##  Temas de ggplot2 para las gráficas
##  ggplot2 color themes for plots

#' Bluish theme for ggplot2 plots.
#'
#' \code{theme_bluish} gives a ggplot2 plot a blue color scheme to integrate it
#' in bluish env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red and grey combinations.
#'
#' @family ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a bluish color scheme
#'
#' @seealso \code{\link{theme_greenish}} for green, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_bluish()
theme_bluish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores azules:
  # ggplot2 theme with bluish colors:
  # panel_bg: #C5EFF7
  # plot_bg & strip_bg: #89C4F4
  # text_col & ticks_col & strip_col: #1F3A93
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#1F3A93",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#1F3A93"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = unit(0.15, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = unit(1.2, "lines"),
                 legend.key.height = NULL,
                 legend.key.width = NULL,
                 legend.text = element_text(size = rel(0.8)),
                 legend.text.align = NULL,
                 legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
                 legend.title.align = NULL,
                 legend.position = "right",
                 legend.direction = NULL,
                 legend.justification = "center",
                 legend.box = NULL,
                 panel.background = element_rect(fill = "#C5EFF7", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "#89C4F4", colour = "#1F3A93", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='#89C4F4', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}

#' Greenish theme for ggplot2 plots.
#'
#' \code{theme_greenish} gives a ggplot2 plot a green color scheme to integrate
#' it in bluish env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red and grey combinations.
#'
#' @family ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a greenish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_greenish()
theme_greenish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores verdes:
  # ggplot2 theme with bluish colors:
  # panel_bg: #C8F7C5
  # plot_bg & strip_bg: #87D37C
  # text_col & ticks_col & strip_col: #1E824C
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#1E824C",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#1E824C"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = unit(0.15, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = unit(1.2, "lines"),
                 legend.key.height = NULL,
                 legend.key.width = NULL,
                 legend.text = element_text(size = rel(0.8)),
                 legend.text.align = NULL,
                 legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
                 legend.title.align = NULL,
                 legend.position = "right",
                 legend.direction = NULL,
                 legend.justification = "center",
                 legend.box = NULL,
                 panel.background = element_rect(fill = "#C8F7C5", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "#87D37C", colour = "#1E824C", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='#87D37C', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}

## UNDER HEAVY MODIFICATIONS
## NOT FINISHED YET!!!!!!!!!!!!!!!!!!!
#' Redish theme for ggplot2 plots.
#'
#' \code{theme_redish} gives a ggplot2 plot a red color scheme to integrate it
#' in redish env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red and grey combinations.
#'
#' @family ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a redish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue, \code{\link{theme_greenish}}
#'   for green, \code{\link{theme_greyish}} for grey
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_redish()
theme_redish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores rojos:
  # ggplot2 theme with bluish colors:
  # panel_bg: #F1A9A0
  # plot_bg & strip_bg: #EF4836
  # text_col & ticks_col & strip_col: #CF000F
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#CF000F",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#CF000F"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = unit(0.15, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = unit(1.2, "lines"),
                 legend.key.height = NULL,
                 legend.key.width = NULL,
                 legend.text = element_text(size = rel(0.8)),
                 legend.text.align = NULL,
                 legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
                 legend.title.align = NULL,
                 legend.position = "right",
                 legend.direction = NULL,
                 legend.justification = "center",
                 legend.box = NULL,
                 panel.background = element_rect(fill = "#F1A9A0", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "#EF4836", colour = "#CF000F", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='#EF4836', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}

## UNDER HEAVY MODIFICATIONS
## NOT FINISHED YET!!!!!!!!!!!!!!!!!!!
#' Greyish theme for ggplot2 plots.
#'
#' \code{theme_greyish} gives a ggplot2 plot a grey color scheme to integrate it
#' in greyish env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red and grey combinations.
#'
#' @family ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a greyish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue, \code{\link{theme_greenish}}
#'   for green, \code{\link{theme_redish}} for red
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_greyish()
theme_greyish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores grises (NO escala de grises):
  # ggplot2 theme with bluish colors:
  # panel_bg: #ECECEC
  # plot_bg & strip_bg: #ABB7B7
  # text_col & ticks_col & strip_col: #6C7A89
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#6C7A89",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#6C7A89"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = unit(0.15, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = unit(1.2, "lines"),
                 legend.key.height = NULL,
                 legend.key.width = NULL,
                 legend.text = element_text(size = rel(0.8)),
                 legend.text.align = NULL,
                 legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
                 legend.title.align = NULL,
                 legend.position = "right",
                 legend.direction = NULL,
                 legend.justification = "center",
                 legend.box = NULL,
                 panel.background = element_rect(fill = "#ECECEC", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "#ABB7B7", colour = "#6C7A89", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='#ABB7B7', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}

#### Glass testing ---------------------------------
#
# foo.df <- data.frame(foo_v1=seq(1,10,1), foo_v2=seq(.5,5,.5), foo_f1=rep(c('a','b'),5))
# foo.plot <- ggplot(foo.df, aes(x=foo_v1, y=foo_v2, colour=foo_f1))+
#   geom_point()
#
# foo.plot + theme_bluish()
# foo.plot + theme_greenish()
# foo.plot + theme_redish()
# foo.plot + theme_greyish()
#
#### End glass testing -----------------------------

