#' Plot a distribution as a hybrid containing a halfeye, a boxplot and jittered points.
#'
#' @param dat A data frame.
#' @param yvar The name of the field containing values to be plotted.
#' @param xvar The name of the field containing the grouping variable (defaults to `NULL`).
#' @param ylim Limits for the y-axis. Defaults to \code{NULL}. If provided, should be a 2-element vector containing the upper and lower limits. 
#' @param xlb Label for the x-axis.
#' @param ylb Label for the y-axis.
#' @param identity_line Show a line of identity? Default \code{FALSE}. 
#' @param identity_value If an identity line is shown, it will be drawn horizontally at this y-value (default 0).
#' @param he_adjust If \code{he_slab_type} is \code{"pdf"}, bandwidth for the density estimator is adjusted by multiplying it by this value. 
#' @param he_width Width of the halfeye component of the plot (default 0.6).
#' @param he_justification Justification of the halfeye component of the plot (default -0.2).
#' @param he_col Color for the halfeye component of the plot.
#' @param he_fill Fill color for the halfeye component of the plot.
#' @param he_alpha Alpha for the halfeye component of the plot (default 0.9).
#' @param he_slab_type The type of slab function to calculate for the halfeye component of the plot: probability density (or mass) function (\code{"pdf"}, the default), cumulative distribution function (\code{"cdf"}), complementary CDF (\code{"ccdf"}) or histogram (\code{"histogram"}).
#' @param he_breaks If slab_type is \code{"histogram"}, the breaks parameter that is passed to \code{hist()} to determine where to put breaks in the histogram.
#' @param he_outline_bars If slab_type is \code{"histogram"}, determines if outlines in between the bars are drawn when the slab_color aesthetic is used. If \code{FALSE} (the default), the outline is drawn only along the tops of the bars; if \code{TRUE}, outlines in between bars are also drawn.
#' @param he_point_interval A function from the \code{\link[ggdist:point_interval]{ggdist::point_interval}} family (e.g., \code{median_qi}, \code{mean_qi}, \code{mode_hdi}, etc), or a string giving the name of a function from that family (e.g., \code{"median_qi"}, \code{"mean_qi"}, \code{"mode_hdi"}, etc. This function determines the point summary (typically mean, median, or mode) and interval type (quantile interval, \code{qi}; highest-density interval, \code{hdi}; or highest-density continuous interval, \code{hdci}). Output will be converted to the appropriate x- or y-based aesthetics depending on the value of orientation. 
#' @param bxp_width Width of the boxplot component (default 0.12).
#' @param bxp_outlier_col Color for outliers in the boxplot component.
#' @param bxp_outlier_fill Fill color for outliers in the boxplot component.
#' @param bxp_outlier_shape Shape for outliers in the boxplot component.
#' @param bxp_outlier_size Size for outliers in the boxplot component.
#' @param bxp_col Color for the boxplot component.
#' @param bxp_fill Fill color for the boxplot component.
#' @param bxp_alpha Alpha for the boxplot component.
#' @param bxp_notch If \code{FALSE} (default) make a standard box plot. If \code{TRUE}, make a notched box plot. Notches are used to compare groups; if the notches of two boxes do not overlap, this suggests that the medians are significantly different.
#' @param bxp_notchwidth For a notched box plot, width of the notch relative to the body (default 0.5).
#' @param hp_range_scale If no `width` argument is specified in \code{hp_transformation}, used to determine the width of the jitter. Defaults to 0.75, which is half of the allotted space for the jitter-points, whereas 1 would use all of the alotted space.
#' @param hp_alpha Alpha for the jitter.
#' @param hp_col Color for the jitter.
#' @param hp_transformation An evaluated \code{position_*()} function yielding a `Position` object with specified parameters to calculate the transformation of the points. Defaults to \code{\link[ggplot2:position_jitter]{ggplot2::position_jitter}}.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' 
#' @return A plot containing jittered points, a boxplot and a density plot or histogram illustrating the distribution of every group of the data under evaluation. 
#' 
#' @author Justin Wilkins, \email{justin.wilkins@@occams.com}
#' 
#' @examples
#' \dontrun{
#'  plot_dist(dat, "ETA1", identity_line = T, he_slab_type = "histogram", he_breaks = 30)
#' }
#'
#' @export
#' @importFrom gghalves geom_half_point
#' @importFrom ggdist stat_halfeye
#' 
plot_dist <- function(dat,
                      yvar, 
                      xvar=NULL, 
                      ylim=NULL, 
                      xlb="",
                      ylb="",
                      
                      identity_line=FALSE,
                      identity_value=0,
                      
                      he_adjust=0.5,
                      he_width=0.6, 
                      he_justification=-0.2,
                      he_col="black",
                      he_fill="#F8766D",
                      he_alpha=0.9,
                      he_slab_type = "pdf",
                      he_breaks="Sturges",
                      he_outline_bars = FALSE,
                      he_point_interval = "median_qi",
                      
                      bxp_width=0.12,
                      bxp_outlier_col=NA,
                      bxp_outlier_fill = NA,
                      bxp_outlier_shape = 19,
                      bxp_outlier_size = 1.5,
                      bxp_col="black",
                      bxp_fill="#F8766D",
                      bxp_alpha=0.9,
                      bxp_notch = FALSE,
                      bxp_notchwidth = 0.5,
                      
                      hp_range_scale=0.4,
                      hp_alpha=0.25,
                      hp_col="#F8766D",
                      hp_transformation = position_jitter(),
                      
                      na.rm=FALSE
                      ) {
  
  
  Xvar <- NULL
  Yvar <- NULL
  
  dat$Xvar <- "Distribution"
  if(!is.null(xvar)) dat$Xvar <- dat[[xvar]]
  dat$Yvar <- dat[[yvar]]
  
  p <- ggplot(dat, aes(x=Xvar, y=Yvar)) 
  
  if(identity_line) p <- p + geom_hline(yintercept=identity_value)

  
  p <- p +
    ggdist::stat_halfeye(
      adjust=he_adjust, 
      width=he_width, 
      .width = 0,
      justification=he_justification, 
      fill=he_fill, 
      alpha=he_alpha,
      col=he_col,
      slab_type = he_slab_type,
      breaks = he_breaks,
      outline_bars = he_outline_bars,
      point_interval = he_point_interval,
      na.rm = na.rm) +
    geom_boxplot(
      width=bxp_width, 
      outlier.colour = bxp_outlier_col, 
      outlier.fill = bxp_outlier_fill, 
      outlier.shape = bxp_outlier_shape, 
      outlier.size = bxp_outlier_size, 
      fill=bxp_fill,
      col=bxp_col,
      alpha=bxp_alpha,
      notch=bxp_notch,
      notchwidth=bxp_notchwidth,
      na.rm = na.rm) +
    gghalves::geom_half_point(
      side = "l",
      range_scale=hp_range_scale, 
      alpha=hp_alpha, 
      col=hp_col,
      transformation = hp_transformation,
      na.rm = na.rm) 
  
  if(!is.null(ylim)) p <- p +
     coord_cartesian(ylim = ylim)
    
  p <- p +
    scale_y_continuous(ylb) +
    scale_x_discrete(xlb)
  
  suppressMessages(print(p))
}

