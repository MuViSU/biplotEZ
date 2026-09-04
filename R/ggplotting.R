#' ggplot2 plot method for biplotEZ objects
#'
#' @param x an object of class \code{biplot} with a method (PCA, CVA, ...) applied.
#' @param exp.factor expansion factor for the plotting region.
#' @param axis.predictivity logical or numeric in (0,1); as in \code{plot.biplot}.
#' @param sample.predictivity logical or numeric in (0,1); as in \code{plot.biplot}.
#' @param xlim,ylim optional axis limits.
#' @param square logical; pad the narrower data range so the panel is square,
#'   mimicking base \code{pty = "s"} with \code{asp = 1}. Default TRUE.
#' @param legend logical; show the ggplot legend for sample groups (default:
#'   shown when there is more than one group). Legends for bags, ellipses and
#'   means are controlled by \code{legend.type()}.
#' @param ... reserved.
#'
#' @return the biplot object, invisibly carrying the ggplot in \code{$gg},
#'   with class \code{gg_biplot} prepended so that printing displays the plot.
#' @export
gg_biplot <- function(x, exp.factor = 1.2,
                      axis.predictivity = NULL,
                      sample.predictivity = NULL,
                      xlim = NULL, ylim = NULL,
                      square = TRUE, draw = TRUE,
                      legend = NULL, ...)
{
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_biplot().", call. = FALSE)
  if (is.null(x$Z))
    stop("Add a biplot method (PCA, CVA, ...) before generating a plot.")
  reason <- .gg_supported(x)
  if (!is.null(reason))
    stop("gg_biplot() does not yet support ", reason,
         "; use plot() or plot(engine = \"base\").", call. = FALSE)
  
  class(x) <- setdiff(class(x), "gg_biplot")   # re-plot of a gg_biplot is fine
  Z <- x$Z
  
  # ---- default aesthetics, exactly as plot.biplot() does -------------------
  if (is.null(x$samples)) x <- biplotEZ::samples(x)
  if (is.null(x$axes))    x <- biplotEZ::axes(x)
  leg <- x$legend                                # legend.type() flags, or NULL
  ### a legend in a separate window is a base graphics feature: with ggplot2
  ### the legend is drawn beside the biplot in the ordinary way
  if (isTRUE(leg$new))
    warning("legend.type(new = TRUE) only works with engine = \"base\"; ",
            "the legend is placed next to the ggplot2 biplot.", call. = FALSE)
  
  # ---- plotting window -----------------------------------------------------
  if (is.null(xlim)) xlim <- range(Z[, 1] * exp.factor)
  if (is.null(ylim)) ylim <- range(Z[, 2] * exp.factor)
  if (isTRUE(square)) {
    # mimic base plot.biplot (pty = "s" with asp = 1): pad the narrower
    # range so both spans are equal and the panel renders square
    half <- max(diff(xlim), diff(ylim)) / 2
    xlim <- mean(xlim) + c(-half, half)
    ylim <- mean(ylim) + c(-half, half)
  }
  usr <- c(xlim, ylim)                       # (x1, x2, y1, y2), base-R style
  mm  <- 0.007 * (usr[2] - usr[1])           # tick-size unit used by biplotEZ
  
  # ---- prediction: lines drawn later; store readable values now -------
  predict.mat <- NULL
  if (!is.null(x$predict$samples)) {
    predict.mat <- Z[x$predict$samples, , drop = FALSE]
    x$predict$samples.mat <- .predicted_values(x, predict.mat)
  }
  if (!is.null(x$predict$means)) {
    pm <- x$Zmeans[x$predict$means, , drop = FALSE]
    x$predict$means.mat <- .predicted_values(x, pm)
    predict.mat <- rbind(predict.mat, pm)
  }
  
  # ---- fit measures for predictivity options -------------------------------
  ax.aes <- x$axes
  axes.too.small <- NULL
  if (!is.null(axis.predictivity)) {
    if (is.null(x$axis.predictivity)) x <- biplotEZ::fit.measures(x)
    if (is.numeric(axis.predictivity))
      axes.too.small <- (1:x$p)[x$axis.predictivity < axis.predictivity]
    if (isTRUE(axis.predictivity)) {
      dilute <- function(col, w)
        grDevices::colorRampPalette(c("white", col))(101)[round(100 * w) + 1]
      for (j in seq_along(ax.aes$which)) {
        w <- x$axis.predictivity[ax.aes$which[j]]
        ax.aes$col[j]            <- dilute(ax.aes$col[j], w)
        ax.aes$label.col[j]      <- dilute(ax.aes$label.col[j], w)
        ax.aes$tick.col[j]       <- dilute(ax.aes$tick.col[j], w)
        ax.aes$tick.label.col[j] <- dilute(ax.aes$tick.label.col[j], w)
      }
    }
  }
  samples.too.small <- NULL
  cex.vec <- rep(1, x$n)
  if (!is.null(sample.predictivity) && !inherits(x, "CVA")) {
    if (is.null(x$sample.predictivity)) x <- biplotEZ::fit.measures(x)
    if (is.numeric(sample.predictivity))
      samples.too.small <- (1:x$n)[x$sample.predictivity < sample.predictivity]
    if (isTRUE(sample.predictivity)) cex.vec <- x$sample.predictivity
  }
  
  # ---- calibrate axes using internal functions --------------------
  if (!is.null(x$Lmat))
              if (nrow(x$Lmat) == ncol(x$Lmat)) 
                Xhat <- x$Z %*% solve(x$Lmat)[x$e.vects,]
              else Xhat <- x$X
            else
              Xhat <- x$X
            if (x$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/x$sd)
            if (x$center) Xhat <- scale(Xhat, center=-1*x$means, scale=FALSE)
  
  .calibrate.axis <- utils::getFromNamespace(".calibrate.axis", "biplotEZ")
  z.axes <- lapply(seq_along(ax.aes$which), .calibrate.axis,
                   Xhat, x$means, x$sd, x$ax.one.unit,
                   ax.aes$which, ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
  
  layers <- list(); scales <- list()
  
  # ---- CVA classification regions (drawn first, underneath everything) -----
  if (isTRUE(x$classify$classify.regions)) {
    predict_regions <- utils::getFromNamespace("predict_regions", "biplotEZ")
    regs <- predict_regions(x$classify$region.midpoints, bounds = usr)
    layers <- c(layers, .gg_regions(regs, x$classify$aes))
  }
  
  # ---- density (no fill scale => no legend; fixes stray legend bug) --------
  if (!is.null(x$z.density))
    layers <- c(layers, .gg_density(x$z.density, x$density.style))
  
  # ---- linear calibrated axes ----------------------------------------------
  layers <- c(layers, .gg_linear_axes(z.axes, ax.aes, usr, mm, axes.too.small,
                                      predict.mat, x$predict$which))
  
  # ---- interpolated new axes ------------------------------------------------
  if (!is.null(x$newvariable)) {
    if (is.null(x$newaxes)) x <- biplotEZ::newaxes(x)
    new.ax.aes <- x$newaxes
    if (length(new.ax.aes$which) > 0) {
      z.axes.new <- lapply(seq_along(new.ax.aes$which), .calibrate.axis,
                           x$newvariable, x$new.means, x$new.sd,
                           x$new.ax.one.unit, new.ax.aes$which,
                           new.ax.aes$ticks, new.ax.aes$orthogx,
                           new.ax.aes$orthogy)
      layers <- c(layers, .gg_linear_axes(z.axes.new, new.ax.aes, usr, mm,
                                          NULL, predict.mat, x$predict$which))
    }
  }
  
  # ---- alpha bags and concentration ellipses --------------------------------
  if (!is.null(x$alpha.bags)) {
    b <- .gg_polygons(x$alpha.bags, x$alpha.bag.aes,
                      legend.aes = if (isTRUE(leg$bags)) "fill" else "none",
                      title = "Alpha bags")
    layers <- c(layers, b$layers); scales <- c(scales, b$scales)
  }
  if (!is.null(x$conc.ellipses)) {
    e <- .gg_polygons(x$conc.ellipses, x$conc.ellipse.aes,
                      legend.aes = if (isTRUE(leg$ellipses)) "linetype" else "none",
                      title = "Ellipses")
    layers <- c(layers, e$layers); scales <- c(scales, e$scales)
  }
  
  # ---- samples ---------------------------------------------------------------
  smp <- .gg_samples(x, Z, usr, cex.vec, samples.too.small, legend)
  layers <- c(layers, smp$layers); scales <- c(scales, smp$scales)
  
  # ---- class means -----------------------------------------------------------
  if (!isTRUE(x$class.means) && !is.null(x$means.aes))
    message("means() was set but class means are not computed; ",
            "call the method with show.class.means = TRUE.")
  if (isTRUE(x$class.means)) {
    if (is.null(x$means.aes)) x <- biplotEZ::means(x)
    mn <- .gg_means(x$Zmeans, x$means.aes, x$g.names, usr,
                    show.legend = isTRUE(leg$means))
    layers <- c(layers, mn$layers); scales <- c(scales, mn$scales)
  }
  
  # ---- new samples -----------------------------------------------------------
  if (!is.null(x$Znew)) {
    if (is.null(x$newsamples)) x <- biplotEZ::newsamples(x)
    layers <- c(layers, .gg_newsamples(x$Znew, x$newsamples, usr))
  }
  
  # ---- assemble --------------------------------------------------------------
  p <- ggplot2::ggplot() +
    layers +
    scales +
    ggplot2::coord_equal(xlim = xlim, ylim = ylim, expand = FALSE, clip = "on") +
    .theme_biplot()
  
  if (!is.null(x$Title)) p <- p + ggplot2::ggtitle(x$Title)
  
  x$gg <- p
  class(x) <- c("gg_biplot", class(x))
  # Draw as a side effect, exactly like base plot.biplot(): the plot appears
  # whether or not the result is assigned, including inside scripts, functions
  # and loops. The object is returned invisibly for capture.
  if (isTRUE(draw)) print(x)
  invisible(x)
}

#' Printing a gg_biplot displays the stored ggplot, then (as base
#' print.biplot does) reports any predictions - here with the actual
#' predicted values, which base leaves to be read off the axes.
#' @export
print.gg_biplot <- function(x, ...) {
  print(x$gg)
  if (!is.null(x$predict$samples)) {
    cat(paste0("Sample predictions for samples ",
               paste(rownames(x$X)[x$predict$samples], collapse = ", "), ":\n"))
    print(round(x$predict$samples.mat, 3))
  }
  if (!is.null(x$predict$means)) {
    cat(paste0("Class mean predictions for classes ",
               paste(x$g.names[x$predict$means], collapse = ", "), ":\n"))
    print(round(x$predict$means.mat, 3))
  }
  invisible(x)
}

#' plot() on a gg_biplot re-displays the ggplot
#' @export
plot.gg_biplot <- function(x, ...) { print(x$gg); invisible(x) }

#' `+` composes with ggplot components and returns a plain ggplot, so
#' `bp |> gg_biplot() + labs(...)` still works.
#' @export
"+.gg_biplot" <- function(e1, e2) {
  if (inherits(e1, "gg_biplot")) e1$gg + e2 else e1 + e2$gg
}

#' @export
autoplot.biplot <- function(object, ...) gg_biplot(object, draw = FALSE, ...)$gg


# ============================================================================
# utility functions 
# ============================================================================

#' Is this biplot renderable by the ggplot2 engine? NULL if yes, otherwise a
#' for a specific reason (see code). Used by gg_biplot() (which stops) and by
#' plot.biplot(engine = "ggplot2") (which warns and falls back to base).
#' @noRd
.gg_supported <- function(x, zoom = FALSE) {
  if (isTRUE(zoom)) return("interactive zooming")
  if (!is.null(x$dim.biplot) && x$dim.biplot != 2)
    return(paste0(x$dim.biplot, "D biplots"))
  if (inherits(x, "CA"))     return("CA maps")
  if (inherits(x, "catPCA")) return("catPCA nominal/ordinal axes")
  if (!is.null(x$PCOaxes) && x$PCOaxes == "splines")
    return("spline axes")
  NULL
}

#' Predicted variable values for biplot points: the marker values at the feet
#' of the perpendiculars, i.e. Z %*% t(ax.one.unit), back-scaled.
#' @noRd
.predicted_values <- function(x, Zpts) {
  # foot of the perpendicular on axis j is at scalar t = z'r_j / (r_j'r_j)
  # along r_j = ax.one.unit[j,]; markers are placed at m_std * r_j, so the
  # predicted standardised value is t, then back-scaled.
  denom <- rowSums(x$ax.one.unit^2)
  out <- sweep(Zpts %*% t(x$ax.one.unit), 2, denom, "/")
  out <- sweep(out, 2, x$sd, "*")
  out <- sweep(out, 2, x$means, "+")
  colnames(out) <- colnames(x$X)
  out
}

#' blank biplot theme (legend titles kept: bags/ellipses/means legends use
#' them; the samples scales pass name = NULL instead)
#' @noRd
.theme_biplot <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title   = ggplot2::element_blank(),
      axis.text    = ggplot2::element_blank(),
      axis.ticks   = ggplot2::element_blank(),
      panel.grid   = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA,
                                           linewidth = 0.5),
      plot.title   = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_text(size = 10)
    )
}

#' 2D density as a raster with fill given as a *parameter*, not a mapped
#' aesthetic: no fill scale is attached, so no legend can appear and the
#' fill scale stays free for the alpha-bag legend.
#' @noRd
.gg_density <- function(zd, style) {
  dens <- expand.grid(x = zd$x, y = zd$y)
  dens$z <- as.vector(zd$z)
  breaks <- pretty(range(dens$z), n = style$cuts)
  cols   <- grDevices::colorRampPalette(style$col)(length(breaks) - 1)
  idx    <- cut(dens$z, breaks, labels = FALSE, include.lowest = TRUE)
  keep   <- !is.na(idx)
  out <- list(ggplot2::geom_raster(
    data = dens[keep, ], ggplot2::aes(x = .data$x, y = .data$y),
    fill = cols[idx[keep]], interpolate = TRUE, show.legend = FALSE))
  if (isTRUE(style$contours))
    out <- c(out, list(ggplot2::geom_contour(
      data = dens, ggplot2::aes(x = .data$x, y = .data$y, z = .data$z),
      breaks = breaks, colour = style$contour.col, linewidth = 0.3,
      show.legend = FALSE)))
  out
}

#' Render calibrated linear axes: line + ticks + tick labels + axis title
#' Port of biplotEZ:::.lin.axes.plot / .marker.func / .marker.label
#' @noRd
.gg_linear_axes <- function(z.axes, ax.aes, usr, mm, too.small,
                            predict.mat, predict_which)
{
  d.list <- list()
  
  for (i in seq_along(ax.aes$which)) {
    ax.num <- ax.aes$which[i]
    if (!is.null(too.small) && ax.num %in% too.small) next
    this.axis  <- z.axes[[i]]
    marker.mat <- this.axis$coords
    marker.mat <- marker.mat[rev(order(marker.mat[, 3])), , drop = FALSE]
    
    # ---- the axis line, clipped to the panel -------------------------------
    line.df <- .clip_line(this.axis$a, this.axis$b, this.axis$v, usr)
    if (is.null(line.df)) next
    line.df$col <- ax.aes$col[i]
    line.df$lwd <- ax.aes$lwd[i]
    line.df$lty <- ax.aes$lty[i]
    d.list$line <- rbind(d.list$line, line.df)
    
    # ---- axis title at the panel edge ---------------------------------------
    ttl.df <- .axis_title_position(this.axis, marker.mat, usr, mm)
    if (!is.null(ttl.df)) {
      ttl.df$name <- ax.aes$names[i]
      ttl.df$col  <- ax.aes$label.col[i]
      ttl.df$cex  <- ax.aes$label.cex[i]
      d.list$title <- rbind(d.list$title, ttl.df)
    }
    
    # ---- tick marks and tick labels -----------------------------------------
    x.vals <- marker.mat[, 1]; y.vals <- marker.mat[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] &
      y.vals < usr[4] & y.vals > usr[3]
    if (any(invals)) {
      std.markers <- marker.mat[invals, 3]
      if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
      on.off <- if (ax.aes$tick.label[i]) rep(TRUE, sum(invals)) else {
        v <- rep(FALSE, sum(invals)); v[c(1, length(v))] <- TRUE; v }
      tick.df <- .ticks(x.vals[invals], y.vals[invals], std.markers, on.off,
                        slope = this.axis$b, vertical = is.null(this.axis$b),
                        tick.size = ax.aes$tick.size[i], mm = mm,
                        label.pos = ax.aes$tick.label.side[i])
      tick.df$seg$col <- ax.aes$tick.col[i]
      tick.df$lab$col <- ax.aes$tick.label.col[i]
      tick.df$lab$cex <- ax.aes$tick.label.cex[i]
      d.list$tick  <- rbind(d.list$tick,  tick.df$seg)
      d.list$tlab  <- rbind(d.list$tlab,  tick.df$lab)
    }
    
    # ---- prediction lines ----------------------------------------------------
    if (!is.null(predict.mat) && ax.num %in% predict_which) {
      prd.df <- .predict_segments(predict.mat, this.axis)
      prd.df$col <- ax.aes$predict.col[min(i, length(ax.aes$predict.col))]
      prd.df$lwd <- ax.aes$predict.lwd[min(i, length(ax.aes$predict.lwd))]
      prd.df$lty <- ax.aes$predict.lty[min(i, length(ax.aes$predict.lty))]
      d.list$pred <- rbind(d.list$pred, prd.df)
    }
  }
  
  out <- list()
  if (!is.null(d.list$line))
    out <- c(out, list(ggplot2::geom_segment(
      data = d.list$line,
      ggplot2::aes(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2),
      colour = d.list$line$col, linewidth = 0.35 * d.list$line$lwd,
      linetype = d.list$line$lty)))
  if (!is.null(d.list$tick))
    out <- c(out, list(ggplot2::geom_segment(
      data = d.list$tick,
      ggplot2::aes(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2),
      colour = d.list$tick$col, linewidth = 0.3)))
  if (!is.null(d.list$tlab))
    out <- c(out, list(ggplot2::geom_text(
      data = d.list$tlab,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                   angle = .data$angle),
      colour = d.list$tlab$col, size = 3.2 * d.list$tlab$cex)))
  if (!is.null(d.list$title))
    out <- c(out, list(ggplot2::geom_text(
      data = d.list$title,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      colour = d.list$title$col, size = 3.6 * d.list$title$cex,
      hjust = d.list$title$hjust, vjust = d.list$title$vjust,
      fontface = "italic")))
  if (!is.null(d.list$pred))
    out <- c(out, list(ggplot2::geom_segment(
      data = d.list$pred,
      ggplot2::aes(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2),
      colour = d.list$pred$col, linewidth = 0.35 * d.list$pred$lwd,
      linetype = d.list$pred$lty)))
  out
}

#' Clip y = a + b x (or x = v) to the panel rectangle
#' @noRd
.clip_line <- function(a, b, v, usr) {
  if (is.null(b)) {                                   # vertical axis
    if (v < usr[1] || v > usr[2]) return(NULL)
    return(data.frame(x1 = v, y1 = usr[3], x2 = v, y2 = usr[4]))
  }
  if (b == 0) {                                       # horizontal axis
    if (a < usr[3] || a > usr[4]) return(NULL)
    return(data.frame(x1 = usr[1], y1 = a, x2 = usr[2], y2 = a))
  }
  pts <- rbind(c(usr[1], a + b * usr[1]),
               c(usr[2], a + b * usr[2]),
               c((usr[3] - a) / b, usr[3]),
               c((usr[4] - a) / b, usr[4]))
  eps <- 1e-9 * max(abs(usr))
  keep <- pts[, 1] >= usr[1] - eps & pts[, 1] <= usr[2] + eps &
    pts[, 2] >= usr[3] - eps & pts[, 2] <= usr[4] + eps
  pts <- unique(pts[keep, , drop = FALSE])
  if (nrow(pts) < 2) return(NULL)
  pts <- pts[order(pts[, 1], pts[, 2]), , drop = FALSE]
  data.frame(x1 = pts[1, 1], y1 = pts[1, 2],
             x2 = pts[nrow(pts), 1], y2 = pts[nrow(pts), 2])
}

#' Position the axis title where the axis exits the panel, in the direction
#' of increasing marker values (same side logic as .lin.axes.plot)
#' @noRd
.axis_title_position <- function(this.axis, marker.mat, usr, mm) {
  inset <- 2.5 * mm
  x.vals <- marker.mat[, 1]; y.vals <- marker.mat[, 2]
  k <- nrow(marker.mat)
  side <- NULL; at <- NULL
  
  if (is.null(this.axis$b)) {                        # vertical axis
    side <- if (y.vals[1] < y.vals[k]) "bottom" else "top"
    at <- this.axis$v
  } else if (this.axis$b == 0) {                     # horizontal axis
    side <- if (x.vals[1] < x.vals[k]) "left" else "right"
    at <- this.axis$a
  } else {
    a <- this.axis$a; b <- this.axis$b
    y1 <- b * usr[1] + a; y2 <- b * usr[2] + a
    x1 <- (usr[3] - a) / b; x2 <- (usr[4] - a) / b
    if (b > 0) {
      if (x.vals[1] < x.vals[k]) {
        if (y1 <= usr[4] && y1 >= usr[3]) { side <- "left";  at <- y1 }
        else                              { side <- "bottom"; at <- x1 }
      } else {
        if (y2 <= usr[4] && y2 >= usr[3]) { side <- "right"; at <- y2 }
        else                              { side <- "top";    at <- x2 }
      }
    } else {
      if (x.vals[1] < x.vals[k]) {
        if (y1 <= usr[4] && y1 >= usr[3]) { side <- "left";  at <- y1 }
        else                              { side <- "top";    at <- x2 }
      } else {
        if (y2 <= usr[4] && y2 >= usr[3]) { side <- "right"; at <- y2 }
        else                              { side <- "bottom"; at <- x1 }
      }
    }
  }
  switch(side,
         bottom = data.frame(x = at, y = usr[3] + inset, hjust = 0.5, vjust = 0),
         top    = data.frame(x = at, y = usr[4] - inset, hjust = 0.5, vjust = 1),
         left   = data.frame(x = usr[1] + inset, y = at, hjust = 0,   vjust = 0.5),
         right  = data.frame(x = usr[2] - inset, y = at, hjust = 1,   vjust = 0.5))
}

#' Tick segments and rotated tick labels (port of .marker.label)
#' @noRd
.ticks <- function(x, y, markers, on.off, slope, vertical,
                   tick.size, mm, label.pos)
{
  lp <- if (identical(label.pos, "above")) 1 else -1
  d  <- tick.size * mm
  
  if (vertical) {
    seg <- data.frame(x1 = x - d, y1 = y, x2 = x + d, y2 = y)
    lab <- data.frame(x = x - lp * (d + 2.5 * mm), y = y,
                      label = markers, angle = 90)
  } else if (slope == 0) {
    seg <- data.frame(x1 = x, y1 = y - d, x2 = x, y2 = y + d)
    lab <- data.frame(x = x, y = y - lp * (d + 2.5 * mm),
                      label = markers, angle = 0)
  } else {
    grad <- -1 / slope
    b <- d * sqrt(1 / (1 + grad^2)); a <- b * grad
    xs <- mm * sqrt(1 / (1 + grad^2)); ys <- xs * grad
    seg <- data.frame(x1 = x - b, y1 = y - a, x2 = x + b, y2 = y + a)
    if (grad > 0) {
      lab <- data.frame(x = x + lp * (2.5 * xs + b),
                        y = y + lp * (2.5 * ys + a),
                        label = markers,
                        angle = 180 + (90 + atan(grad) * 180 / pi))
    } else {
      lab <- data.frame(x = x - lp * (2.5 * xs + b),
                        y = y - lp * (2.5 * ys + a),
                        label = markers,
                        angle = 90 + atan(grad) * 180 / pi)
    }
  }
  lab <- lab[on.off, , drop = FALSE]
  list(seg = seg, lab = lab)
}

#' Perpendicular drop lines from points to an axis (port of .predict.func)
#' @noRd
.predict_segments <- function(predict.mat, this.axis) {
  px <- predict.mat[, 1]; py <- predict.mat[, 2]
  if (is.null(this.axis$b)) {
    data.frame(x1 = px, y1 = py, x2 = this.axis$v, y2 = py)
  } else if (this.axis$b == 0) {
    data.frame(x1 = px, y1 = py, x2 = px, y2 = this.axis$a)
  } else {
    a <- this.axis$a; b <- this.axis$b
    fx <- (px + b * (py - a)) / (1 + b^2)
    fy <- a + b * fx
    data.frame(x1 = px, y1 = py, x2 = fx, y2 = fy)
  }
}

#' CVA classification regions as polygons; classify() stores colours
#' already opacity-adjusted. Fill passed as a parameter: no scale used.
#' @noRd
.gg_regions <- function(regs, cl.aes) {
  border <- cl.aes$borders
  border.col <- if (isFALSE(border) || is.null(border)) NA
  else if (isTRUE(border)) "black" else border
  lapply(seq_along(regs), function(i) {
    m <- stats::na.omit(as.matrix(regs[[i]]))
    ggplot2::geom_polygon(
      data = data.frame(x = m[, 1], y = m[, 2]),
      ggplot2::aes(x = .data$x, y = .data$y),
      fill = cl.aes$col[i], colour = border.col, linewidth = 0.3)
  })
}

#' Alpha bags / concentration ellipses as polygons.
#' legend.aes = "none": fixed colours, no legend (default behaviour).
#' legend.aes = "fill": bag legend via the fill scale (legend.type(bags=TRUE)).
#' legend.aes = "linetype": ellipse legend via the linetype scale
#' (legend.type(ellipses=TRUE)); keys show the ellipse colours through
#' guide override.
#' @noRd
.gg_polygons <- function(z.list, aes, legend.aes = "none", title = NULL) {
  k <- length(z.list)
  idx  <- function(v, i) v[(i - 1) %% length(v) + 1]
  lbls <- names(z.list)
  if (is.null(lbls)) lbls <- paste(title, seq_len(k))
  cols  <- vapply(seq_len(k), function(i) idx(aes$col, i), "")
  ltys  <- vapply(seq_len(k), function(i) idx(aes$lty, i), numeric(1))
  lwds  <- vapply(seq_len(k), function(i) idx(aes$lwd, i), numeric(1))
  fills <- vapply(seq_len(k), function(i)
    grDevices::adjustcolor(cols[i], idx(aes$opacity, i)), "")
  
  layers <- lapply(seq_len(k), function(i) {
    m  <- z.list[[i]]
    dd <- data.frame(x = m[, 1], y = m[, 2], lbl = lbls[i])
    switch(legend.aes,
           fill = ggplot2::geom_polygon(data = dd,
                                        ggplot2::aes(x = .data$x, y = .data$y, fill = .data$lbl),
                                        colour = cols[i], linetype = ltys[i], linewidth = 0.35 * lwds[i]),
           linetype = ggplot2::geom_polygon(data = dd,
                                            ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$lbl),
                                            colour = cols[i], fill = fills[i], linewidth = 0.35 * lwds[i]),
           ggplot2::geom_polygon(data = dd,
                                 ggplot2::aes(x = .data$x, y = .data$y),
                                 colour = cols[i], fill = fills[i],
                                 linetype = ltys[i], linewidth = 0.35 * lwds[i]))
  })
  
  scales <- switch(legend.aes,
                   fill = list(ggplot2::scale_fill_manual(
                     name = title, breaks = lbls, values = stats::setNames(fills, lbls),
                     guide = ggplot2::guide_legend(
                       override.aes = list(colour = cols, linetype = ltys)))),
                   linetype = list(ggplot2::scale_linetype_manual(
                     name = title, breaks = lbls, values = stats::setNames(ltys, lbls),
                     guide = ggplot2::guide_legend(
                       override.aes = list(colour = cols, fill = fills)))),
                   list())
  
  list(layers = layers, scales = scales)
}

#' Sample points, connecting lines, labels; group scales for a native legend
#' @noRd
.gg_samples <- function(x, Z, usr, cex.vec, too.small, legend) {
  layers <- list(); scales <- list()
  s.aes <- x$samples
  if (is.null(s.aes$which)) return(list(layers = layers, scales = scales))
  
  if (isTRUE(s.aes$connected))
    layers <- c(layers, list(ggplot2::geom_path(
      data = data.frame(x = Z[, 1], y = Z[, 2]),
      ggplot2::aes(x = .data$x, y = .data$y),
      colour = s.aes$connect.col, linetype = s.aes$connect.lty,
      linewidth = 0.35 * s.aes$connect.lwd)))
  
  show <- rep(FALSE, x$n)
  if (!is.null(x$alpha.bag.outside)) {
    for (j in seq_along(x$alpha.bag.aes$which))
      show[x$group.aes == x$g.names[s.aes$which[j]]] <- x$alpha.bag.outside[[j]]
  } else {
    for (j in seq_along(s.aes$which))
      show[x$group.aes == x$g.names[s.aes$which[j]]] <- TRUE
  }
  invals <- Z[, 1] > usr[1] & Z[, 1] < usr[2] & Z[, 2] > usr[3] & Z[, 2] < usr[4]
  show <- show & invals
  if (!is.null(too.small)) show[too.small] <- FALSE
  if (!any(show)) return(list(layers = layers, scales = scales))
  
  df <- data.frame(x = Z[show, 1], y = Z[show, 2],
                   group = factor(x$group.aes[show],
                                  levels = x$g.names[s.aes$which]),
                   size.w = cex.vec[show],
                   name = if (!is.null(s.aes$label.name))
                     s.aes$label.name[show] else rownames(Z)[show])
  
  shown.groups <- x$g.names[s.aes$which]
  col.map <- stats::setNames(s.aes$col, shown.groups)
  pch.map <- stats::setNames(s.aes$pch, shown.groups)
  cex.map <- stats::setNames(2.2 * s.aes$cex, shown.groups)
  show.legend <- if (is.null(legend)) length(shown.groups) > 1 else legend
  
  layers <- c(layers, list(ggplot2::geom_point(
    data = df,
    ggplot2::aes(x = .data$x, y = .data$y, colour = .data$group,
                 shape = .data$group, size = .data$group),
    alpha = s.aes$opacity[1], show.legend = show.legend)))
  
  scales <- list(
    ggplot2::scale_colour_manual(name = NULL, values = col.map, drop = FALSE),
    ggplot2::scale_shape_manual(name = NULL, values = pch.map, drop = FALSE),
    ggplot2::scale_size_manual(values = cex.map, drop = FALSE, guide = "none"))
  
  # col/pch/cex are per group, but label, label.col, label.cex, label.side and
  # label.offset are per sample. Clip those to the samples actually drawn and
  # keep them vectors, exactly as base .samples.plot does by carrying them as
  # columns of its ZZ data frame; using [1] paints every label like sample 1.
  nshow <- sum(show)
  sub <- function(v, d)
    if (length(v) == x$n) v[show] else rep_len(if (length(v)) v[1] else d, nshow)
  lab.on  <- s.aes$label
  lab.cex <- sub(s.aes$label.cex, 0.75)
  lab.col <- sub(s.aes$label.col, "black")
  if (any(stats::na.omit(lab.on == "ggrepel"))) {
    if (!requireNamespace("ggrepel", quietly = TRUE))
      stop("Package 'ggrepel' is required for label = 'ggrepel'.", call. = FALSE)
    layers <- c(layers, list(ggrepel::geom_text_repel(
      data = df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      size = 3.2 * lab.cex, colour = lab.col,
      max.overlaps = Inf, seed = 1, show.legend = FALSE)))
  } else if (any(stats::na.omit(as.logical(lab.on)))) {
    on <- sub(lab.on, TRUE)
    on <- !is.na(on) & as.logical(on)
    if (any(on)) {
      side  <- sub(s.aes$label.side, "bottom")[on]
      off   <- sub(s.aes$label.offset, 0.5)[on] * 0.02 * (usr[2] - usr[1])
      nudge <- vapply(side, function(sd)
        switch(sd, bottom = c(0, -1), top = c(0, 1),
               left = c(-1, 0), right = c(1, 0), c(0, -1)), numeric(2))
      layers <- c(layers, list(ggplot2::geom_text(
        data = df[on, , drop = FALSE],
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
        size = 3.2 * lab.cex[on], colour = lab.col[on],
        nudge_x = nudge[1, ] * off, nudge_y = nudge[2, ] * off,
        show.legend = FALSE)))
    }
  }
  list(layers = layers, scales = scales)
}

#' Class means. Aesthetic vectors are indexed POSITIONALLY within `which`,
#' exactly as base .means.plot does (`col[j]` for the j-th selected mean).
#' Legend (legend.type(means = TRUE)) rides on the alpha scale with key
#' colours/shapes set through guide override, leaving colour/shape/fill/
#' linetype free for samples, bags and ellipses.
#' @noRd
.gg_means <- function(Zmeans, m.aes, g.names, usr, show.legend = FALSE) {
  which  <- m.aes$which
  k      <- length(which)
  if (k == 0) return(list(layers = list(), scales = list()))
  ZZ     <- Zmeans[which, , drop = FALSE]
  jj     <- seq_len(k)                                   # positional index
  invals <- ZZ[, 1] > usr[1] & ZZ[, 1] < usr[2] &
    ZZ[, 2] > usr[3] & ZZ[, 2] < usr[4]
  if (!any(invals)) return(list(layers = list(), scales = list()))
  
  cols <- m.aes$col[jj][invals]
  pchs <- m.aes$pch[jj][invals]
  cexs <- m.aes$cex[jj][invals]
  lbls <- g.names[which][invals]
  df <- data.frame(x = ZZ[invals, 1], y = ZZ[invals, 2],
                   name = lbls, lbl = lbls)
  
  if (show.legend) {
    layers <- list(ggplot2::geom_point(
      data = df, ggplot2::aes(x = .data$x, y = .data$y, alpha = .data$lbl),
      colour = cols, shape = pchs, size = 2.2 * cexs))
    scales <- list(ggplot2::scale_alpha_manual(
      name = "Class means", breaks = lbls,
      values = stats::setNames(rep(1, length(lbls)), lbls),
      guide = ggplot2::guide_legend(
        override.aes = list(colour = cols, shape = pchs, size = 2.2 * cexs))))
  } else {
    layers <- list(ggplot2::geom_point(
      data = df, ggplot2::aes(x = .data$x, y = .data$y),
      colour = cols, shape = pchs, size = 2.2 * cexs, show.legend = FALSE))
    scales <- list()
  }
  
  lab <- m.aes$label[jj][invals]
  if (any(stats::na.omit(lab == "ggrepel")) &&
      requireNamespace("ggrepel", quietly = TRUE)) {
    layers <- c(layers, list(ggrepel::geom_text_repel(
      data = df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      size = 3.2 * m.aes$label.cex[jj][invals],
      colour = m.aes$label.col[jj][invals],
      seed = 1, show.legend = FALSE)))
  } else if (any(stats::na.omit(as.logical(lab)))) {
    dl <- df[as.logical(lab), , drop = FALSE]
    layers <- c(layers, list(ggplot2::geom_text(
      data = dl, ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      size = 3.2 * m.aes$label.cex[jj][invals][as.logical(lab)],
      colour = m.aes$label.col[jj][invals][as.logical(lab)],
      vjust = 1.8, show.legend = FALSE)))
  }
  list(layers = layers, scales = scales)
}

#' New (interpolated) samples
#' @noRd
.gg_newsamples <- function(Znew, ns.aes, usr) {
  out <- list()
  if (isTRUE(ns.aes$connected))
    out <- c(out, list(ggplot2::geom_path(
      data = data.frame(x = Znew[, 1], y = Znew[, 2]),
      ggplot2::aes(x = .data$x, y = .data$y),
      colour = ns.aes$connect.col, linetype = ns.aes$connect.lty,
      linewidth = 0.35 * ns.aes$connect.lwd)))
  invals <- Znew[, 1] > usr[1] & Znew[, 1] < usr[2] &
    Znew[, 2] > usr[3] & Znew[, 2] < usr[4]
  n.new <- nrow(Znew)
  nm <- if (!is.null(ns.aes$label.name[1])) ns.aes$label.name else rownames(Znew)
  sub <- function(v) if (length(v) == n.new) v[invals] else v
  Znew <- Znew[invals, , drop = FALSE]
  if (nrow(Znew) == 0) return(out)
  df <- data.frame(x = Znew[, 1], y = Znew[, 2], name = sub(nm))
  out <- c(out, list(ggplot2::geom_point(
    data = df, ggplot2::aes(x = .data$x, y = .data$y),
    colour = sub(ns.aes$col), shape = sub(ns.aes$pch),
    size = 2.2 * sub(ns.aes$cex), show.legend = FALSE)))
  lab      <- sub(ns.aes$label)
  lab.cex  <- sub(ns.aes$label.cex)
  lab.col  <- sub(ns.aes$label.col)
  if (any(stats::na.omit(lab == "ggrepel")) &&
      requireNamespace("ggrepel", quietly = TRUE)) {
    out <- c(out, list(ggrepel::geom_text_repel(
      data = df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      size = 3.2 * lab.cex, colour = lab.col,
      seed = 1, show.legend = FALSE)))
  } else if (any(stats::na.omit(as.logical(lab)))) {
    on <- !is.na(lab) & as.logical(lab)
    out <- c(out, list(ggplot2::geom_text(
      data = df[on, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
      size = 3.2 * lab.cex[on], colour = lab.col[on],
      vjust = 1.8, show.legend = FALSE)))
  }
  out
}
