##  Temas de ggplot2 para las gráficas
##  ggplot2 color themes for plots

#' Bluish theme for ggplot2 plots.
#'
#' \code{theme_bluish} gives a ggplot2 plot a blue color scheme to integrate it
#' in bluish env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red, grey and minimalist combinations..
#'
#' @family custom ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a bluish color scheme
#'
#' @seealso \code{\link{theme_greenish}} for green, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_bluish()
#'
#' @export
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
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "#89C4F4", colour = "#1F3A93", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='#89C4F4', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}

#' Greenish theme for ggplot2 plots.
#'
#' \code{theme_greenish} gives a ggplot2 plot a green color scheme to integrate
#' it in bluish env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red, grey and minimalist combinations.
#'
#' @family ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a greenish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_greenish()
#'
#' @export
theme_greenish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores verdes:
  # ggplot2 theme with greenish colors:
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
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "#87D37C", colour = "#1E824C", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='#87D37C', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
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
#' which includes blue, green, red, grey and minimalist combinations.
#'
#' @family ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a redish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue, \code{\link{theme_greenish}}
#'   for green, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_redish()
#'
#' @export
theme_redish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores rojos:
  # ggplot2 theme with redish colors:
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
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "#EF4836", colour = "#CF000F", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='#EF4836', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
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
#' which includes blue, green, red, grey and minimalist combinations.
#'
#' @family ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a greyish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue, \code{\link{theme_greenish}}
#'   for green, \code{\link{theme_redish}} for red, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_greyish()
#'
#' @export
theme_greyish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores grises (NO escala de grises):
  # ggplot2 theme with greyish colors:
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
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "#ABB7B7", colour = "#6C7A89", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='#ABB7B7', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}


## UNDER HEAVY MODIFICATIONS
## NOT FINISHED YET!!!!!!!!!!!!!!!!!!!
#' Minimalish theme for ggplot2 plots.
#'
#' \code{theme_minimalish} gives a ggplot2 plot a minimalist color scheme to integrate it
#' in publishing env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red, grey and minimalist combinations.
#'
#' @family ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a minimalish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue, \code{\link{theme_greenish}}
#'   for green, \code{\link{theme_redish}} for red, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_minimalish()
#'
#' @export
theme_minimalish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores minimalistas:
  # ggplot2 theme with minimalist colors:
  # panel_bg: #transparent
  # plot_bg & strip_bg: #transparent
  # text_col & ticks_col & strip_col: #black
  ggplot2::theme(line = element_line(colour = "black", size = 1, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "transparent", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "black",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(face = 'bold'),
                 strip.text = element_text(size = rel(0.7)),
                 axis.line = element_line(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "black"),
                 axis.title = element_text(face = 'italic', size=rel(1.2)),
                 axis.title.x = element_text(vjust = 0.2),
                 axis.title.y = element_text(angle = 90, vjust = 1.2),
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
                 legend.key.height = NULL,
                 legend.key.width = NULL,
                 legend.text = element_text(size = rel(0.9)),
                 legend.text.align = NULL,
                 legend.title = element_text(face = "italic", hjust = 0),
                 legend.title.align = NULL,
                 legend.position = "right",
                 legend.direction = NULL,
                 legend.justification = "center",
                 legend.box = NULL,
                 panel.background = element_rect(colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_line(colour='darkgrey', size=rel(0.2)),
                 panel.grid.minor = element_blank(),
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(colour = "black", size = rel(0.5)),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}


#' Thesish theme for ggplot2 plots.
#'
#' \code{theme_thesish} gives a ggplot2 plot a formal but elegant
#' color scheme to integrate it thesis env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red, grey and minimalist combinations..
#'
#' @family custom ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a bluish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue,
#'   \code{\link{theme_greenish}} for green, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_thesish()
#'
#' @export
theme_thesish <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores azules:
  # ggplot2 theme with bluish colors:
  # panel_bg: #FDE3A7(orange) or #ECECEC(gray)
  # plot_bg & strip_bg: transparent
  # text_col & ticks_col & strip_col: #6C7A89 (grey) or black or #E87E04 (orange)
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#E87E04",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#E87E04"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.background = element_rect(fill = "#FDE3A7", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "transparent", colour = "transparent", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='transparent', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}

#' Thesish purple version theme for ggplot2 plots.
#'
#' \code{theme_thesishPurple} gives a ggplot2 plot a formal but elegant
#' color scheme to integrate it thesis env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red, grey and minimalist combinations..
#'
#' @family custom ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a purple color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue,
#'   \code{\link{theme_greenish}} for green, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_thesishPurple()
#'
#' @export
theme_thesishPurple <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores morados:
  # ggplot2 theme with purpleish colors:
  # panel_bg: #dcc6e0
  # plot_bg & strip_bg: transparent
  # text_col & ticks_col & strip_col: #9a12b3 (purple)
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#9a12b3",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#9a12b3"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.background = element_rect(fill = "#dcc6e0", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "transparent", colour = "transparent", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='transparent', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}


#' Thesish green theme for ggplot2 plots.
#'
#' \code{theme_thesishGreen} gives a ggplot2 plot a formal but elegant
#' color scheme to integrate in thesis env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red, grey and minimalist combinations..
#'
#' @family custom ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a greenish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue,
#'   \code{\link{theme_greenish}} for green, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_thesishGreen()
#'
#' @export
theme_thesishGreen <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores azules:
  # ggplot2 theme with bluish colors:
  # panel_bg: #c8f7c5
  # plot_bg & strip_bg: transparent
  # text_col & ticks_col & strip_col: #26a65b
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#26a65b",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#26a65b"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.background = element_rect(fill = "#c8f7c5", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "transparent", colour = "#26a65b", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='transparent', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}


#' Thesish blue theme for ggplot2 plots.
#'
#' \code{theme_thesishBlue} gives a ggplot2 plot a formal but elegant
#' color scheme to integrate in thesis env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red, grey and minimalist combinations..
#'
#' @family custom ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a greenish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue,
#'   \code{\link{theme_greenish}} for green, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_thesishBlue()
#'
#' @export
theme_thesishBlue <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores azules:
  # ggplot2 theme with bluish colors:
  # panel_bg: #e4f1fe
  # plot_bg & strip_bg: transparent
  # text_col & ticks_col & strip_col: #1e8bc3
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#1e8bc3",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#1e8bc3"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.background = element_rect(fill = "#e4f1fe", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "transparent", colour = "#1e8bc3", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='transparent', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}


#' Thesish red theme for ggplot2 plots.
#'
#' \code{theme_thesishRed} gives a ggplot2 plot a formal but elegant
#' color scheme to integrate in thesis env's.
#'
#' This function is part of a family of theming functions for ggplot2 plots
#' which includes blue, green, red, grey and minimalist combinations..
#'
#' @family custom ggplot2 theming functions
#'
#' @param base_size Given reference size.
#' @param base_family Given font family.
#' @return Prints or saves the given plot in a greenish color scheme
#'
#' @seealso \code{\link{theme_bluish}} for blue,
#'   \code{\link{theme_greenish}} for green, \code{\link{theme_redish}}
#'   for red, \code{\link{theme_greyish}} for grey, as well as \code{\link{theme_minimalish}}
#'   for a black&white minimalist plot.
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#' colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_thesishRed()
#'
#' @export
theme_thesishRed <- function (base_size = 12, base_family = "") {
  # Tema de ggplot2 con colores azules:
  # ggplot2 theme with bluish colors:
  # panel_bg: #f1a9a0
  # plot_bg & strip_bg: transparent
  # text_col & ticks_col & strip_col: #cf000f
  ggplot2::theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
                 rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
                 text = element_text(family = base_family, face = "plain", colour = "#cf000f",
                                     size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                                     lineheight = 0.9),
                 axis.text = element_text(size = rel(0.8)),
                 strip.text = element_text(size = rel(0.8)),
                 axis.line = element_blank(),
                 axis.text.x = element_text(vjust = 1),
                 axis.text.y = element_text(hjust = 1),
                 axis.ticks = element_line(colour = "#cf000f"),
                 axis.title.x = element_text(),
                 axis.title.y = element_text(angle = 90),
                 axis.ticks.length = grid::unit(0.15, "cm"),
                 axis.ticks.margin = grid::unit(0.1, "cm"),
                 legend.background = element_rect(fill='transparent', colour = NA),
                 legend.margin = grid::unit(0.2, "cm"),
                 legend.key = element_rect(fill='transparent', color='transparent'),
                 legend.key.size = grid::unit(1.2, "lines"),
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
                 panel.background = element_rect(fill = "#f1a9a0", colour = NA),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.margin = grid::unit(0.1, "lines"),
                 panel.margin.x = NULL,
                 panel.margin.y = NULL,
                 strip.background = element_rect(fill = "transparent", colour = "#cf000f", size = 0.2),
                 strip.text.x = element_text(),
                 strip.text.y = element_text(angle = -90),
                 plot.background = element_rect(fill='transparent', colour=NA),
                 plot.title = element_text(size = rel(1.2)),
                 plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines"),
                 complete = TRUE)
}


#### Glass testing ---------------------------------
#
# foo.df <- data.frame(foo_v1=seq(1,10,1), foo_v2=seq(.5,5,.5), foo_f1=rep(c('a','b'),5))
# foo.plot <- ggplot(foo.df, aes(x=foo_v1, y=foo_v2, colour=foo_f1))+
#   geom_point(size=4)
#
# foo.plot + theme_bluish()
# foo.plot + theme_greenish()
# foo.plot + theme_redish()
# foo.plot + theme_greyish()
#
# foo.plot.2 <- ggplot(foo.df, aes(x=foo_v1, y=foo_v2, shape=foo_f1))+
#   geom_point(size=4)
# foo.plot.2 + theme_minimalish(base_family='Times')
#
#### End glass testing -----------------------------
