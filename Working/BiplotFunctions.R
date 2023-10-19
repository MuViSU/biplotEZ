# Biplot functions to accompany Gower, Lubbe and le Roux (2011) Understanding biplots. Wiley: Chichester.
# Functions written by Sugnet Lubbe and Niel J le Roux and postgraduate students.

# - Functions in this script file
# ----------------------------------------------------------------
# CONTROL FUNCTIONS
# ----------------------------------------------------------------
# biplot.sample.control 
# biplot.mean.control 
# biplot.new.sample.control 
# biplot.ax.control 
# biplot.nominal.ax.control
# biplot.ordinal.ax.control
# biplot.trajectory.control 
# biplot.spline.axis.control 
# biplot.alpha.bag.control 
# biplot.kappa.ellipse.control 
# biplot.density.1D.control 
# biplot.density.2D.control 
# biplot.class.region.control 
# biplot.legend.control 
# biplot.legend.type.control 

# ----------------------------------------------------------------
# UTILITY FUNCTIONS
# ----------------------------------------------------------------
# indmat  
# biplot.check.G 
# biplot.check.X 
# biplot.fit.measures 
# biplot.legend 
# Eigen.twosided 
# calc.alpha.bags 
# calc.concentration.ellipse 
# calibrate.axis 
# calibrate.cat.axis
# biplot.spline.axis 
# biplot.create.regions 
# biplot.LDA.class.func 
# biplot.QDA.class.func 
# scale.3waydata
# ratio.axis
# ties
# triplot.decomp

# ----------------------------------------------------------------
# DRAWING FUNCTIONS
# ----------------------------------------------------------------
# draw.biplot 
# draw.biplot.1D 
# draw.biplot.3D 

# ----------------------------------------------------------------
# BIPLOT FUNCTIONS
# ----------------------------------------------------------------
# PCAbiplot 
# PCAbiplot.zoom
# CVAbiplot 
# Nonlinear.biplot 
# Regression.biplot 
# Spline.biplot 
# QDAbiplot 
# QDAbiplot.zoom 
# LRbiplot
# TRIplot
# CATPCAbiplot
# PLSbiplot 

# ========================================================================================================


# ----------------------------------------------------------------
# CONTROL FUNCTIONS
# ----------------------------------------------------------------

biplot.sample.control <- function (J, 
                                   col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"), 
                                   pch = 3, cex = 1, label = F, label.cex = 0.75, label.side = "bottom", connected=F, 
                                   alpha = 1) 
{  while (length(col) < J) col <- c(col, col)
   col <- as.vector(col[1:J])
   while (length(pch) < J) pch <- c(pch, pch)
   pch <- as.vector(pch[1:J])
   while (length(cex) < J) cex <- c(cex, cex)
   cex <- as.vector(cex[1:J])
   while (length(label) < J) label <- c(label, label)
   label <- as.vector(label[1:J])
   while (length(label.cex) < J) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:J])
   while (length(label.side) < J) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:J])
   while (length(alpha) < J) alpha <- c(alpha, alpha)
   if (length(connected)>1) connected <- connected[1]
   alpha <- as.vector(alpha[1:J])
   list(col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side, 
        connected=connected, alpha = alpha)
}

biplot.mean.control <- function (J, class.names, which = NULL, 
                                 col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"),
                                 pch = 16, cex = 1.5, label = T, label.cex = 1, label.side = "bottom", alpha = 1) 
{  if (!all(is.numeric(which))) which <- match(which, class.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which > 0]
   mean.num <- length(which)
   while (length(col) < mean.num) col <- c(col, col)
   col <- as.vector(col[1:mean.num])
   while (length(pch) < mean.num) pch <- c(pch, pch)
   pch <- as.vector(pch[1:mean.num])
   while (length(cex) < mean.num) cex <- c(cex, cex)
   cex <- as.vector(cex[1:mean.num])
   while (length(label) < mean.num) label <- c(label, label)
   label <- as.vector(label[1:mean.num])
   while (length(label.cex) < mean.num) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:mean.num])
   while (length(label.side) < mean.num) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:mean.num])
   while (length(alpha) < mean.num) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:mean.num])
   list(which = which, col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side, 
        alpha = alpha)
}

biplot.new.sample.control <- function (n, col = "black", pch = 1, cex = 1, label = F, label.cex = 0.75, 
                                       label.side = "bottom", alpha = 1) 
{  while (length(col) < n) col <- c(col, col)
   col <- as.vector(col[1:n])
   while (length(pch) < n) pch <- c(pch, pch)
   pch <- as.vector(pch[1:n])
   while (length(cex) < n) cex <- c(cex, cex)
   cex <- as.vector(cex[1:n])
   while (length(label) < n) label <- c(label, label)
   label <- as.vector(label[1:n])
   while (length(label.cex) < n) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:n])
   while (length(label.side) < n) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:n])
   while (length(alpha) < n) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:n])
   list(col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side, alpha = alpha)
}

biplot.ax.control <- function (p, X.names, which = 1:p, type = "prediction", col = grey(0.7), lwd = 1, lty = 1, 
                               label = "Orthog", label.col = col, label.cex = 0.75, label.dist = 0, ticks = 5, 
                               tick.col = col, tick.size = 1, tick.label = T, tick.label.col = tick.col, tick.label.cex = 0.6, 
                               tick.label.side = "left", tick.label.offset = 0.5, tick.label.pos = 1, 
                               predict.col = col, predict.lwd = lwd, predict.lty = lty, ax.names = X.names,  
                               orthogx = 0, orthogy = 0, oblique = NULL) 
{  if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
   which <- which[which <= p]
   which <- which[which > 0]
   ax.num <- length(which)
   if (type != "prediction" & type != "interpolation") stop("Incorrect type of biplot axes specified")
   while (length(col) < ax.num) col <- c(col, col)
   col <- as.vector(col[1:ax.num])
   while (length(lwd) < ax.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:ax.num])
   while (length(lty) < ax.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:ax.num])
   if (label != "Orthog" & label != "Hor" & label != "Paral") stop("Incorrect specification of axis label direction")
   while (length(label.col) < ax.num) label.col <- c(label.col, label.col)
   label.col <- as.vector(label.col[1:ax.num])
   while (length(label.cex) < ax.num) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:ax.num])
   while (length(label.dist) < ax.num) label.dist <- c(label.dist, label.dist)
   label.dist <- as.vector(label.dist[1:ax.num])
   while (length(ticks) < ax.num) ticks <- c(ticks, ticks)
   ticks <- as.vector(ticks[1:ax.num])
   while (length(tick.col) < ax.num) tick.col <- c(tick.col, tick.col)
   tick.col <- as.vector(tick.col[1:ax.num])
   while (length(tick.size) < ax.num) tick.size <- c(tick.size, tick.size)
   tick.size <- as.vector(tick.size[1:ax.num])
   while (length(tick.label) < ax.num) tick.label <- c(tick.label, tick.label)
   tick.label <- as.vector(tick.label[1:ax.num])
   while (length(tick.label.col) < ax.num) tick.label.col <- c(tick.label.col, tick.label.col)
   tick.label.col <- as.vector(tick.label.col[1:ax.num])
   while (length(tick.label.cex) < ax.num) tick.label.cex <- c(tick.label.cex, tick.label.cex)
   tick.label.cex <- as.vector(tick.label.cex[1:ax.num])
   while (length(tick.label.side) < ax.num) tick.label.side <- c(tick.label.side, tick.label.side)
   tick.label.side <- as.vector(tick.label.side[1:ax.num])
   while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
   tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
   while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
   tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
   while (length(predict.col) < ax.num) predict.col <- c(predict.col, predict.col)
   predict.col <- as.vector(predict.col[1:ax.num])
   while (length(predict.lwd) < ax.num) predict.lwd <- c(predict.lwd, predict.lwd)
   predict.lwd <- as.vector(predict.lwd[1:ax.num])
   while (length(predict.lty) < ax.num) predict.lty <- c(predict.lty, predict.lty)
   predict.lty <- as.vector(predict.lty[1:ax.num])
   ax.names <- ax.names[which]
   while (length(ax.names) < p) ax.names <- c(ax.names, "")
   ax.names <- as.vector(ax.names[1:ax.num])
   if (!is.null(oblique)) if (length(oblique) != p) stop("For oblique translations values must be specified for each variable")
   while (length(orthogx) < p) orthogx <- c(orthogx, orthogx)
   orthogx <- as.vector(orthogx[1:p])
   while (length(orthogy) < p) orthogy <- c(orthogy, orthogy)
   orthogy <- as.vector(orthogy[1:p])
   list(which = which, type = type, col = col, lwd = lwd, lty = lty, label = label, label.col = label.col, label.cex = label.cex, 
        label.dist = label.dist, ticks = ticks, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label, 
        tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side = tick.label.side, 
        tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos, 
        predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd, 
        names = ax.names, orthogx = orthogx, orthogy = orthogy, oblique = oblique)
}

biplot.nominal.ax.control <- function (p, X.names, num.levels, which = 1:p, col = NULL, lwd = 1, lty = 1, label = "Orthog", 
                                       label.col = grey(0.7), label.cex = 0.75, label.dist = 0, tick.col = grey(0.7), 
                                       tick.size = 1, tick.label = T, tick.label.col = tick.col, tick.label.cex = 0.6, 
                                       tick.label.side = "left", tick.label.offset = 0.5, tick.label.pos = 1,
                                       predict.col = grey(0.7), predict.lwd = lwd, predict.lty = lty, 
                                       ax.names = X.names, orthogx = 0, orthogy = 0, oblique = NULL) 
{  
if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
   which <- which[which <= p]
   which <- which[which > 0]
   ax.num <- length(which)
   if (is.null(col)) { require (RColorBrewer)
                       col <- c(brewer.pal(12,"Set3")[-2],brewer.pal(8,"Accent"),brewer.pal(7,"Dark2"))
                     }
   if (!is.list(col)) {  temp <- vector("list",ax.num)
                         for (j in 1:ax.num) temp[[j]] <- col
                         col <- temp
                      } 
   if (length(col) < ax.num) { temp <- vector("list",ax.num)
                               col.length <- length(col)
                               j <- 0
                               while (j < ax.num) {  for (k in 1:col.length) if (k+j<=ax.num) temp[[k+j]] <- col[[k]]
                                                     j <- j + col.length
                                                  }
                               col <- temp
                             }
   if (length(col) > ax.num) { temp <- vector("list",ax.num)
                               for (j in 1:ax.num) temp[[j]] <- col[[j]]
                               col <- temp
                             }
   num.levels <- num.levels[which]
   for (j in 1:ax.num) {  while (length(col[[j]])<num.levels[j]) col[[j]] <- c(col[[j]],col[[j]])
                          col[[j]] <- col[[j]][1:num.levels[j]]
                       }
   
   while (length(lwd) < ax.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:ax.num])
   while (length(lty) < ax.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:ax.num])
   if (label != "Orthog" & label != "Hor" & label != "Paral") stop("Incorrect specification of axis label direction")
   while (length(label.col) < ax.num) label.col <- c(label.col, label.col)
   label.col <- as.vector(label.col[1:ax.num])
   while (length(label.cex) < ax.num) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:ax.num])
   while (length(label.dist) < ax.num) label.dist <- c(label.dist, label.dist)
   label.dist <- as.vector(label.dist[1:ax.num])
   while (length(tick.col) < ax.num) tick.col <- c(tick.col, tick.col)
   tick.col <- as.vector(tick.col[1:ax.num])
   while (length(tick.size) < ax.num) tick.size <- c(tick.size, tick.size)
   tick.size <- as.vector(tick.size[1:ax.num])
   while (length(tick.label) < ax.num) tick.label <- c(tick.label, tick.label)
   tick.label <- as.vector(tick.label[1:ax.num])
   while (length(tick.label.col) < ax.num) tick.label.col <- c(tick.label.col, tick.label.col)
   tick.label.col <- as.vector(tick.label.col[1:ax.num])
   while (length(tick.label.cex) < ax.num) tick.label.cex <- c(tick.label.cex, tick.label.cex)
   tick.label.cex <- as.vector(tick.label.cex[1:ax.num])
   while (length(tick.label.side) < ax.num) tick.label.side <- c(tick.label.side, tick.label.side)
   tick.label.side <- as.vector(tick.label.side[1:ax.num])
   while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
   tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
   while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
   tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
   while (length(predict.col) < ax.num) predict.col <- c(predict.col, predict.col)
   predict.col <- as.vector(predict.col[1:ax.num])
   while (length(predict.lwd) < ax.num) predict.lwd <- c(predict.lwd, predict.lwd)
   predict.lwd <- as.vector(predict.lwd[1:ax.num])
   while (length(predict.lty) < ax.num) predict.lty <- c(predict.lty, predict.lty)
   predict.lty <- as.vector(predict.lty[1:ax.num])
   ax.names <- ax.names[which]
   while (length(ax.names) < p) ax.names <- c(ax.names, "")
   ax.names <- as.vector(ax.names[1:ax.num])
   if (!is.null(oblique)) if (length(oblique) != p) stop("For oblique translations values must be specified for each variable")
   while (length(orthogx) < p) orthogx <- c(orthogx, orthogx)
   orthogx <- as.vector(orthogx[1:p])
   while (length(orthogy) < p) orthogy <- c(orthogy, orthogy)
   orthogy <- as.vector(orthogy[1:p])
   list(which = which, col = col, lwd = lwd, lty = lty, label = label, label.col = label.col, label.cex = label.cex, 
        label.dist = label.dist, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label, tick.label.col = tick.label.col, 
        tick.label.cex = tick.label.cex, tick.label.side = tick.label.side, tick.label.offset = tick.label.offset, 
        tick.label.pos = tick.label.pos, 
        predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd, 
        names = ax.names, orthogx = orthogx, orthogy = orthogy, oblique = oblique)
}

biplot.ordinal.ax.control <- function (p, X.names, num.levels, which = 1:p, col = grey(0.7), lwd.factor = 1.5, lty = 1, 
                                       label = "Orthog", label.col = col, label.cex = 0.75, label.dist = 0, 
                                       tick.col = col, tick.size = 1, tick.label = T, tick.label.col = tick.col, tick.label.cex = 0.6, 
                                       tick.label.side = "left", tick.label.offset = 0.5, tick.label.pos = 1,
                                       predict.col = col, predict.lwd = 1, predict.lty = lty, ax.names = X.names, 
                                       orthogx = 0, orthogy = 0, oblique = NULL, reverse=rep(FALSE,p)) 
{  
if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
   which <- which[which <= p]
   which <- which[which > 0]
   ax.num <- length(which)
   while (length(col) < ax.num) col <- c(col, col)
   col <- as.vector(col[1:ax.num])
   while (length(lwd.factor) < ax.num) lwd.factor <- c(lwd.factor, lwd.factor)
   lwd.factor <- as.vector(lwd.factor[1:ax.num])
   while (length(lty) < ax.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:ax.num])
   if (label != "Orthog" & label != "Hor" & label != "Paral") stop("Incorrect specification of axis label direction")
   while (length(label.col) < ax.num) label.col <- c(label.col, label.col)
   label.col <- as.vector(label.col[1:ax.num])
   while (length(label.cex) < ax.num) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:ax.num])
   while (length(label.dist) < ax.num) label.dist <- c(label.dist, label.dist)
   label.dist <- as.vector(label.dist[1:ax.num])
   while (length(tick.col) < ax.num) tick.col <- c(tick.col, tick.col)
   tick.col <- as.vector(tick.col[1:ax.num])
   while (length(tick.size) < ax.num) tick.size <- c(tick.size, tick.size)
   tick.size <- as.vector(tick.size[1:ax.num])
   while (length(tick.label) < ax.num) tick.label <- c(tick.label, tick.label)
   tick.label <- as.vector(tick.label[1:ax.num])
   while (length(tick.label.col) < ax.num) tick.label.col <- c(tick.label.col, tick.label.col)
   tick.label.col <- as.vector(tick.label.col[1:ax.num])
   while (length(tick.label.cex) < ax.num) tick.label.cex <- c(tick.label.cex, tick.label.cex)
   tick.label.cex <- as.vector(tick.label.cex[1:ax.num])
   while (length(tick.label.side) < ax.num) tick.label.side <- c(tick.label.side, tick.label.side)
   tick.label.side <- as.vector(tick.label.side[1:ax.num])
   while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
   tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
   while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
   tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
   while (length(predict.col) < ax.num) predict.col <- c(predict.col, predict.col)
   predict.col <- as.vector(predict.col[1:ax.num])
   while (length(predict.lwd) < ax.num) predict.lwd <- c(predict.lwd, predict.lwd)
   predict.lwd <- as.vector(predict.lwd[1:ax.num])
   while (length(predict.lty) < ax.num) predict.lty <- c(predict.lty, predict.lty)
   predict.lty <- as.vector(predict.lty[1:ax.num])
   ax.names <- ax.names[which]
   while (length(ax.names) < p) ax.names <- c(ax.names, "")
   ax.names <- as.vector(ax.names[1:ax.num])
   if (!is.null(oblique)) if (length(oblique) != p) stop("For oblique translations values must be specified for each variable")
   while (length(orthogx) < p) orthogx <- c(orthogx, orthogx)
   orthogx <- as.vector(orthogx[1:p])
   while (length(orthogy) < p) orthogy <- c(orthogy, orthogy)
   orthogy <- as.vector(orthogy[1:p])
   while (length(reverse) < p) reverse <- c(reverse, reverse)
   reverse <- as.vector(reverse[1:p])
   list(which = which, col = col, lwd.factor=lwd.factor, lty = lty, label = label, label.col = label.col, 
        label.cex = label.cex, label.dist = label.dist, tick.col = tick.col, tick.size = tick.size, 
        tick.label = tick.label, tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, 
        tick.label.side = tick.label.side, tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos, 
        predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd, 
        names = ax.names, orthogx = orthogx, orthogy = orthogy, oblique = oblique, reverse = reverse)
}

biplot.trajectory.control <- function (p, X.names, which = 1:p, type = "prediction.circle", col = NULL, lwd = 1, lty = 1, 
                                       label.side = "right", label.col = col, label.cex = 0.75, label.dist = 0, ticks = 5, 
                                       tick.col = col, tick.size = 1, tick.label = T, tick.label.col = tick.col, 
                                       tick.label.cex = 0.6, tick.label.side = "left", tick.label.offset = 0.5, 
                                       tick.label.pos = 1, predict.col = col, predict.lwd = lwd, predict.lty = lty, 
                                       ax.names = X.names, orthogx = 0, orthogy = 0, oblique = NULL, 
                                       num.points=100) 
{  if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
   which <- which[which <= p]
   which <- which[which > 0]
   ax.num <- length(which)
   if (type != "prediction" & type != "interpolation" & type != "prediction.circle" & type != "prediction.normal" & type != "prediction.back") stop("Incorrect type of biplot axes specified")
   if (is.null(col)) { require (RColorBrewer)
                       col <- c(brewer.pal(12,"Set3")[-2],brewer.pal(8,"Accent"),brewer.pal(7,"Dark2"))    }           
   while (length(col) < ax.num) col <- c(col, col)
   col <- as.vector(col[1:ax.num])
   while (length(lwd) < ax.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:ax.num])
   while (length(lty) < ax.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:ax.num])
   while (length(label.side) < ax.num) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:ax.num])
   while (length(label.col) < ax.num) label.col <- c(label.col, label.col)
   label.col <- as.vector(label.col[1:ax.num])
   while (length(label.cex) < ax.num) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:ax.num])
   while (length(label.dist) < ax.num) label.dist <- c(label.dist, label.dist)
   label.dist <- as.vector(label.dist[1:ax.num])
   while (length(ticks) < ax.num) ticks <- c(ticks, ticks)
   ticks <- as.vector(ticks[1:ax.num])
   while (length(tick.col) < ax.num) tick.col <- c(tick.col, tick.col)
   tick.col <- as.vector(tick.col[1:ax.num])
   while (length(tick.size) < ax.num) tick.size <- c(tick.size, tick.size)
   tick.size <- as.vector(tick.size[1:ax.num])
   while (length(tick.label) < ax.num) tick.label <- c(tick.label, tick.label)
   tick.label <- as.vector(tick.label[1:ax.num])
   while (length(tick.label.col) < ax.num) tick.label.col <- c(tick.label.col, tick.label.col)
   tick.label.col <- as.vector(tick.label.col[1:ax.num])
   while (length(tick.label.cex) < ax.num) tick.label.cex <- c(tick.label.cex, tick.label.cex)
   tick.label.cex <- as.vector(tick.label.cex[1:ax.num])
   while (length(tick.label.side) < ax.num) tick.label.side <- c(tick.label.side, tick.label.side)
   tick.label.side <- as.vector(tick.label.side[1:ax.num])
   while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
   tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
   while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
   tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
   while (length(predict.col) < ax.num) predict.col <- c(predict.col, predict.col)
   predict.col <- as.vector(predict.col[1:ax.num])
   while (length(predict.lwd) < ax.num) predict.lwd <- c(predict.lwd, predict.lwd)
   predict.lwd <- as.vector(predict.lwd[1:ax.num])
   while (length(predict.lty) < ax.num) predict.lty <- c(predict.lty, predict.lty)
   predict.lty <- as.vector(predict.lty[1:ax.num])
   ax.names <- ax.names[which]
   while (length(ax.names) < p) ax.names <- c(ax.names, "")
   ax.names <- as.vector(ax.names[1:ax.num])
   if (!is.null(oblique)) if (length(oblique) != p) stop("For oblique translations values must be specified for each variable")
   while (length(orthogx) < p) orthogx <- c(orthogx, orthogx)
   orthogx <- as.vector(orthogx[1:p])
   while (length(orthogy) < p) orthogy <- c(orthogy, orthogy)
   orthogy <- as.vector(orthogy[1:p])
   list(which = which, type = type, col = col, lwd = lwd, lty = lty, label.side = label.side, label.col = label.col, 
        label.cex = label.cex, label.dist = label.dist, ticks = ticks, tick.col = tick.col, tick.size = tick.size, 
        tick.label = tick.label, tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side = tick.label.side, 
        tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos, predict.col = predict.col, 
        predict.lty = predict.lty, predict.lwd = predict.lwd, names = ax.names, orthogx = orthogx, 
        orthogy = orthogy, oblique = oblique, num.points=num.points)
}

biplot.spline.axis.control <- function (tau = 0.5, nmu = 100, u = 2, v = 3, lambda = 0, smallsigma = 0.01, 
                                        bigsigma = smallsigma * 10, gamma = 250, bigsigmaactivate = floor(gamma * 0.1), 
                                        eps = 0.01, tiny = 10^-30, itmax = 10000, ftol = 1.5 * (.Machine$double.eps)) 
{
    if (!is.numeric(tau) || tau <= 0) stop("value of 'tau' must be > 0")
    if (!is.numeric(nmu) || nmu <= 0) stop("value of 'nmu' must be > 0")
    if (!is.numeric(u) || u < 0) stop("value of 'u' must not be less than zero")
    if (!is.numeric(v) || v < 0) stop("value of 'v' must not be less than zero")
    if (!is.numeric(lambda) || lambda < 0) stop("value of 'lambda' must be >= 0")
    if (!is.numeric(smallsigma) || smallsigma <= 0) stop("value of 'smallsigma' must be > 0")
    if (!is.numeric(bigsigma) || bigsigma <= 0) stop("value of 'bigsigma' must be > 0")
    if (!is.numeric(gamma) || gamma <= 0) stop("value of 'gamma' must be > 0")
    if (!is.numeric(bigsigmaactivate) || bigsigmaactivate <= 0) stop("value of 'bigsigmaactivate' must be > 0")
    if (!is.numeric(eps) || eps < 0) stop("value of 'eps' must be > 0")
    if (!is.numeric(tiny) || tiny < 0) stop("value of 'tiny' must be > 0")
    if (!is.numeric(itmax) || itmax <= 0) stop("itmax number of iterations must be > 0")
    if (!is.numeric(ftol) || ftol <= 0) stop("value of 'ftol' must be > 0")
    list(tau = tau, nmu = nmu, u = u, v = v, lambda = lambda, smallsigma = smallsigma, bigsigma = bigsigma, 
         gamma = gamma, bigsigmaactivate = bigsigmaactivate, eps = eps, tiny = tiny, itmax = itmax, ftol = ftol)
}

biplot.alpha.bag.control <- function (J, bag.names, which = NULL, alpha = 0.95, 
                                      col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"), 
                                      lty = 1, lwd = 1, max = 2500, pch = 15, cex = 1) 
{  if (!all(is.numeric(which))) which <- match(which, bag.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which > 0]
   bag.num <- length(which)
   while (length(alpha) < bag.num) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:bag.num])
   if (any(alpha < 0 | alpha > 0.99)) stop(message = "alpha not to be negative or larger than 0.99")
   alpha.entered <- alpha
   while (length(col) < bag.num) col <- c(col, col)
   col <- as.vector(col[1:bag.num])
   while (length(lty) < bag.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:bag.num])
   while (length(lwd) < bag.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:bag.num])
   while (length(cex) < bag.num) cex <- c(cex, cex)
   cex <- as.vector(cex[1:bag.num])
   while (length(pch) < bag.num) pch <- c(pch, pch)
   pch <- as.vector(pch[1:bag.num])
   while (length(max) < bag.num) max <- c(max, max)
   max <- as.vector(max[1:bag.num])
   list(which = which, alpha = alpha, col = col, lty = lty, lwd = lwd, max = max, pch = pch, cex = cex)
}

biplot.kappa.ellipse.control <- function (J, ellipse.names, df=2, which = NULL, kappa = NULL, alpha = 0.95, 
                                          col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"), 
                                          lty = 1, lwd = 1, alpha.transparency = 0.5) 
{  if (!all(is.numeric(which))) which <- match(which, ellipse.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which > 0]
   ellipse.num <- length(which)
   if (!is.null(alpha)) { while (length(alpha) < ellipse.num) alpha <- c(alpha, alpha)
                          alpha <- as.vector(alpha[1:ellipse.num])
                          if (any(alpha < 0 | alpha > 0.99)) stop(message = "alpha not to be negative or larger than 0.99")
                          alpha.entered <- alpha
                          if (is.null(kappa)) kappa <- sqrt(qchisq(alpha, df))                                               }
   while (length(kappa) < ellipse.num) kappa <- c(kappa, kappa)
   kappa <- as.vector(kappa[1:ellipse.num])
   while (length(col) < ellipse.num) col <- c(col, col)
   col <- as.vector(col[1:ellipse.num])
   while (length(lty) < ellipse.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:ellipse.num])
   while (length(lwd) < ellipse.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:ellipse.num])
   while (length(alpha.transparency) < ellipse.num) alpha.transparency <- c(alpha.transparency, alpha.transparency)
   alpha.transparency <- as.vector(alpha.transparency[1:ellipse.num])
   list(which = which, kappa = kappa, col = col, lty = lty, lwd = lwd, alpha.transparency = alpha.transparency)
}

biplot.density.1D.control <- function (J, class.names, which = NULL, bw = "nrd0", kernel = "gaussian", 
                                       col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"), 
                                       lty = 1, lwd = 1) 
{  if (!all(is.numeric(which))) which <- match(which, class.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which > 0]
   class.num <- length(which)
   while (length(bw) < class.num) bw <- c(bw, bw)
   bw <- as.vector(bw[1:class.num])
   while (length(kernel) < class.num) kernel <- c(kernel, kernel)
   kernel <- as.vector(kernel[1:class.num])
   while (length(col) < class.num) col <- c(col, col)
   col <- as.vector(col[1:class.num])
   while (length(lty) < class.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:class.num])
   while (length(lwd) < class.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:class.num])
   list(which = which, bw = bw, kernel = kernel, col = col, lty = lty, lwd = lwd)
}

biplot.density.2D.control <- function (J, class.names, which = NULL, contours = F, h = NULL, n = 100, 
                                       col = c("green", "yellow", "red"), contour.col = "black", cuts = 50, cex = 0.6, 
                                       tcl = -0.2, mgp = c(0, -0.25, 0), layout.heights = c(100, 10), legend.mar = c(2, 5, 0, 5)) 
{  if (!is.null(which)) if (which == "all") which <- 0
                        else if (!all(is.numeric(which))) which <- match(which, class.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which >= 0]
   if (!is.null(which)) which <- which[1]
   list(which = which, contours = contours, h = h, n = n, col = col, contour.col = contour.col, cuts = cuts, cex = cex, 
        tcl = tcl, mgp = mgp, layout.heights = layout.heights, legend.mar = legend.mar)
}

biplot.class.region.control <- function (J, space.fill = 5, col = NULL, alpha = 1) 
{  if (is.null(col))
     col <- (colors()[round(seq(from=1,to=99,length=J+2))+261])[2:(J+1)]
   while (length(col) < J) col <- c(col, col)
   col <- as.vector(col[1:J])
   while (length(alpha) < J) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:J])
   list(space.fill = space.fill[1], col = col, alpha = alpha)
}

biplot.legend.control <- function (columns = 1, columns.betw = -1, betw = c(1, -1, 0, 1), mar = c(3, 1, 3, 1), size = 2, 
                                   text.width.multiplier = 1, label.cex = 0.7, label.col = "black", sample.cex = 1.2) 
{  list(columns = columns, columns.betw = columns.betw, betw = betw, mar = mar, size = size, text.width.multiplier = text.width.multiplier, 
        label.cex = label.cex, label.col = label.col, sample.cex = sample.cex)
}

biplot.legend.type.control <- function (means = F, samples = F, bags = F) 
{  list(means = means, samples = samples, bags = bags)
}

# ----------------------------------------------------------------
# UTILITY FUNCTIONS
# ----------------------------------------------------------------

indmat  <- function (groep.vec) 
{  elements <- levels(factor(groep.vec))
   Y <- matrix(0, nrow = length(groep.vec), ncol = length(elements))
   dimnames(Y) <- list(NULL, paste(elements))
   for (i in 1:length(elements)) Y[groep.vec == elements[i], i] <- 1
   return(Y)
}

biplot.check.G <- function (G, n) 
{  if (is.null(G)) { G <- matrix(indmat(rep(1, n)), ncol = 1)
                     dimnames(G) <- list(1:n, "AllData")      }
   if (nrow(G) != n) stop("number of rows of X and G differ")
   if (is.null(dimnames(G))) dimnames(G) <- list(NULL, paste("class", 1:ncol(G), sep = ""))
   if (length(dimnames(G)[[2]]) == 0) dimnames(G)[[2]] <- paste("class", 1:ncol(G), sep = "")
   if (ncol(G) == 1) class.vec <- rep(dimnames(G)[[2]], n) else class.vec <- apply(t(apply(G, 1, function(x) x == max(x))), 1, function(s, G) dimnames(G)[[2]][s], G = G)
   G
}

biplot.check.X <- function (X, scaled.mat, centred.mat=TRUE) 
{  X <- as.matrix(X)
   unscaled.X <- X
   means <- apply(X, 2, mean)
   sd <- sqrt(apply(X, 2, var))
   if (!centred.mat) {  X <- X
                        means <- rep(0, ncol(X))
                        sd <- rep(1, ncol(X))     }      
   else { if (scaled.mat) X <- scale(X) else { X <- scale(X, scale = FALSE)
                                               sd <- rep(1, ncol(X))        }
        }  
    if (is.null(dimnames(X))) dimnames(X) <- list(paste(1:nrow(X)), paste("V", 1:ncol(X), sep = ""))
    if (length(dimnames(X)[[1]]) == 0) dimnames(X)[[1]] <- paste(1:nrow(X))
    if (length(dimnames(X)[[2]]) == 0) dimnames(X)[[2]] <- paste("V", 1:ncol(X), sep = "")
    list(X = X, unscaled.X = unscaled.X, means = means, sd = sd)
}

biplot.fit.measures <- function (mat, mat.hat, weights=diag(nrow(mat)), orthog.metric=diag(ncol(mat)), 
                                 eigenvals, eigenvecs, dims) 
{  r <- length(dims)
   if (nrow(eigenvecs)<ncol(mat)) Jmat <- matrix (0, ncol=min(ncol(mat),nrow(mat)), nrow=min(ncol(mat),nrow(mat)))
   else Jmat <- matrix (0, ncol=nrow(eigenvecs), nrow=nrow(eigenvecs))
   diag(Jmat)[dims] <- 1
   if (nrow(Jmat)>ncol(eigenvecs)) Jmat <- diag(ncol(eigenvecs))

   quality <- sum(eigenvals[dims])/sum(eigenvals)
   quality <- paste(round(quality*100, digits=2), "%", sep="")
   adequacy <- diag(eigenvecs %*% Jmat %*% t(eigenvecs)) / diag(eigenvecs %*% t(eigenvecs))
   adequacy <- round(adequacy, digits=3)
   names(adequacy) <- dimnames(mat)[[2]]
  
   axis.predictivity <- diag(t(mat.hat) %*% weights %*% mat.hat) / diag(t(mat) %*% weights %*% mat)
   axis.predictivity <- round (axis.predictivity, digits=3)
   names(axis.predictivity) <- dimnames(mat)[[2]]
  
   item.predictivity <- diag(mat.hat %*% solve(orthog.metric) %*% t(mat.hat)) / diag(mat %*% solve(orthog.metric) %*% t(mat))
   item.predictivity <- round (item.predictivity, digits=3)
   names(item.predictivity) <- dimnames(mat)[[1]]

   list (quality=quality, adequacy=adequacy, axis.predictivity=axis.predictivity, item.predictivity=item.predictivity)  
}

biplot.legend <- function (legend.type, legend.style, mean.list, sample.list, bag.list, class.names, 
                           quality.print = FALSE, quality = NA) 
{  
   key.R <- function (x, y, ..., title = "", align = TRUE, background = 0, border = 0, between = 2, 
                      corner = c(missing(x), 1), divide = 3, transparent = FALSE, cex = par("cex"), 
                      cex.title = 1.5 * max(cex), col = par("col"), lty = par("lty"), lwd = par("lwd"), 
                      font = par("font"), pch = par("pch"), adj = 0, type = "l", size = 5, columns = 1, 
                      between.columns = 3, angle = 0, density = -1, space = NULL, text.width.multiplier = 1) 
   {  check.types <- function(x, type, types) {
         n <- length(types)
         match.type <- pmatch(type, types, duplicates.ok = TRUE)
         if (any(is.na(match.type))) stop(paste(x, " must be \"", paste(types[-n], collapse = "\", \""), ", or \"", types[n], "\"", sep = ""))
         types[match.type]
   }
   x <- x
   y <- y
   oldxpd <- par("xpd")
   on.exit(par(xpd = oldxpd), TRUE)
   rest <- list(...)
   colnames <- names(rest)
   for (i in (1:length(colnames))[colnames == "text"]) { if (!is.list(rest[[i]])) rest[[i]] <- list(rest[[i]])  }
   actions <- c("points", "lines", "rectangles", "text")
   colnames <- check.types("key components", colnames, actions)
   nrows <- max(sapply(unlist(rest, recursive = FALSE), length))
   ncols <- length(colnames)
   if (!missing(cex)) {  oldcex <- par("cex")
                         on.exit(par(cex = oldcex), TRUE)
                         par(cex = mean(cex))                 }
   cx <- par("cxy")[1]
   cy <- par("cxy")[2]
   cx1 <- cx/par("cex")
   cy1 <- cy/par("cex")
   replen <- function(a, b, n) rep(if (is.null(a)) b else a, length = n)
   for (j in seq(ncols)) {  this <- rest[[j]]
                            this$cex <- replen(this$cex, cex, nrows)
                            this$size <- replen(this$size, size, nrows)
                            this$type <- replen(this$type, type, nrows)
                            this$density <- replen(this$density, density, nrows)
                            this$angle <- replen(this$angle, angle, nrows)
                            this$col <- replen(this$col, col, nrows)
                            this$lty <- replen(this$lty, lty, nrows)
                            this$lwd <- replen(this$lwd, lwd, nrows)
                            this$adj <- replen(this$adj, adj, nrows)
                            this$font <- replen(this$font, font, nrows)
                            this$pch <- replen(this$pch, pch, nrows)
                            rest[[j]] <- this                                            }
   text.adj <- width <- height <- matrix(0, nrows, ncols)
   between <- rep(between, length = ncols)
   for (j in seq(ncols)) {  this <- rest[[j]]
                            for (i in seq(nrows)) { switch(colnames[j], points = { sx <- sy <- this$cex[i]         }, 
                                                                        lines = { sx <- this$size[i]
                                                                                  sy <- this$cex[i]                }, 
                                                                        rectangles = { sx <- this$size[i]
                                                                                       sy <- this$cex[i]           }, 
                                                                        text = { sx <- nchar(this[[1]][i]) * this$cex[i] * text.width.multiplier
                                                                                 sy <- this$cex[i]
                                                                                 text.adj[i, j] <- this$adj[i]     })
                                                    width[i, j] <- sx * cx1 + between[j] * cx
                                                    height[i, j] <- sy * cy1
                                                  }
                         }
   if (columns != 1) { slice <- function(x, p) {
                                  m <- nrow(x)
                                  n <- ncol(x)
                                  if (m%%p != 0) x <- rbind(x, matrix(0, p - m%%p, n))
                                  q <- nrow(x)/p
                                  dim(x) <- c(q, p, n)
                                  x <- aperm(x, c(1, 3, 2))
                                  dim(x) <- c(q, n * p)
                                  x
                               }
                       width[, ncols] <- width[, ncols] + cx * between.columns
                       width <- slice(width, columns)
                       nc <- ncol(width)
                       width[, nc] <- width[, nc] - cx * between.columns
                       height <- slice(height, columns)
                       text.adj <- slice(text.adj, columns)
                     }
   nc <- ncol(width)
   nr <- nrow(width)
   if (align)  for (j in seq(nc)) width[, j] <- max(width[, j]) 
   xpos <- ypos <- matrix(0, nr, nc)
   for (j in seq(length = nc - 1)) xpos[, j + 1] <- xpos[, j] + width[, j]
   xmax <- x + max(xpos + width)
   Between <- rep(between, each = nrow(xpos))
   xpos <- xpos + x + cx * Between * 0.5
   i <- text.adj != 0
   if (any(i)) xpos[i] <- (xpos + text.adj * (width - Between * cx))[i]
   for (i in seq(nr)) height[i, ] <- max(height[i, ])
   ypos[, ] <- y - cumsum(height[, 1])
   if (nchar(title)) ypos <- ypos - cy1 * cex.title
   ymin <- min(ypos) 
   title.excess <- x + cex.title * nchar(title) * cx1 * text.width.multiplier - xmax
   if (title.excess > 0) xmax <- xmax + title.excess
   x.offset <- (x - xmax) * corner[1]
   xpos <- xpos + x.offset + max(0, title.excess/2)
   y.offset <- (y - ymin) * (1 - corner[2])
   ypos <- ypos + y.offset + 0.5 * height
   if (background == 0) border <- as.numeric(border)
   if (!transparent) polygon(c(x, xmax, xmax, x) + x.offset, c(y, y, ymin, ymin) + y.offset + 0, col = background, border = border)
   if (nchar(title)) text((x + xmax)/2 + x.offset, y + y.offset - cex.title/2 * cy1, title, adj = 0.5, cex = cex.title)
   if (columns != 1) {  restack <- function(x, p) {  n <- ncol(x)/p
                                                     q <- nrow(x)
                                                     dim(x) <- c(q, n, p)
                                                     x <- aperm(x, c(1, 3, 2))
                                                     dim(x) <- c(p * q, n)
                                                     x
                                                  }
                        xpos <- restack(xpos, columns)[1:nrows, , drop = FALSE]
                        ypos <- restack(ypos, columns)[1:nrows, , drop = FALSE]
                     }
   for (j in seq(ncols)) {  this <- rest[[j]]
                            for (i in seq(nrows)) { switch(colnames[j], points = { points(xpos[i, j], ypos[i, j], cex = this$cex[i], col = this$col[i], font = this$font[i], pch = this$pch[[i]])            }, 
                                                                        lines = { if (this$type[i] != "p") { lines(xpos[i, j] + seq(0, 1, length = divide) * cx1 * this$size[i], rep(ypos[i, j], divide), cex = this$cex[i], lwd = this$lwd[i], type = this$type[i], lty = this$lty[i], pch = this$pch[[i]], font = this$font[i], col = this$col[i])
                                                                                                              if (this$type[i] == "b" || this$type[i] == "o") points(xpos[i, j] + seq(0, 1, length = divide) * cx1 * this$size[i], rep(ypos[i, j], divide), cex = this$cex[i], lwd = this$lwd[i], type = "p", lty = 1, pch = this$pch[[i]], font = this$font[i], col = this$col[i])
                                                                                                           } 
                                                                                  else points(xpos[i, j] + 0.5 * cx1 * this$size[i], ypos[i, j], cex = this$cex[i], lwd = this$lwd[i], type = this$type[i], lty = this$lty[i], pch = this$pch[[i]], font = this$font[i], col = this$col[i])
                                                                                }, 
                                                                        rectangles = { polygon(xpos[i, j] + c(0, rep(this$size[i] * cx1, 2), 0), ypos[i, j] + cy1 * this$cex[i] * c(-0.5, -0.5, 0.5, 0.5), col = this$col[i], angle = this$angle[i], density = this$density[i], border = FALSE)
                                                                                     }, 
                                                                        text = { text(xpos[i, j], ypos[i, j], this[[1]][i], adj = this$adj[i], cex = this$cex[i], col = this$col[i], font = this$font[i])            })
                                                  }
                         }
   }

   if (all(legend.type == FALSE)) return(cat("Change legend.type to obtain a legend\n"))
   par(pty = "m", mar = legend.style$mar)
   plot(x = c(0, 10), y = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
   usr <- par("usr")
   x <- usr[1]
   y <- usr[4]
   if (quality.print) mtext(text = paste("Quality of display", paste(round(quality * 100, 2), "%", sep = "")), adj = 0, cex = 0.8, at = 0, side = 3, line = 0.5, las = 0)
   if (legend.type$bags) 
     {  if (legend.type$means & legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                           size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = mean.list$pch, col = mean.list$col, 
                                                           cex = legend.style$sample.cex), points = list(pch = sample.list$pch, col = sample.list$col, cex = legend.style$sample.cex), lines = list(lty = bag.list$lty, col = bag.list$col, 
                                                           lwd = bag.list$lwd), text = list(class.names, cex = legend.style$label.cex, col = legend.style$label.col))
        if (legend.type$means & !legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                            size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = mean.list$pch, col = mean.list$col, 
                                                            cex = legend.style$sample.cex), lines = list(lty = bag.list$lty, col = bag.list$col, lwd = bag.list$lwd), text = list(class.names, cex = legend.style$label.cex, 
                                                            col = legend.style$label.col))
        if (!legend.type$means & legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                            size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = sample.list$pch, col = sample.list$col, 
                                                            cex = legend.style$sample.cex), lines = list(lty = bag.list$lty, col = bag.list$col, lwd = bag.list$lwd), text = list(class.names, cex = legend.style$label.cex, 
                                                            col = legend.style$label.col))
        if (!legend.type$means & !legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                             size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, lines = list(lty = bag.list$lty, col = bag.list$col, 
                                                             lwd = bag.list$lwd), text = list(class.names, cex = legend.style$label.cex, col = legend.style$label.col))
     }
   else 
     {  if (legend.type$means & legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                           size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = mean.list$pch, col = mean.list$col, 
                                                           cex = legend.style$sample.cex), points = list(pch = sample.list$pch, col = sample.list$col, cex = pch.samples.size[1]), text = list(class.names, cex = legend.style$label.cex, 
                                                           col = legend.style$label.col))
         if (legend.type$means & !legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                             size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = mean.list$pch, col = mean.list$col, 
                                                             cex = legend.style$sample.cex), text = list(class.names, cex = legend.style$label.cex, col = legend.style$label.col))
         if (!legend.type$means & legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                             size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = sample.list$pch, col = sample.list$col, 
                                                             cex = legend.style$sample.cex), text = list(class.names, cex = legend.style$label.cex, col = legend.style$label.col))
    }
}

Eigen.twosided <- function (A, B, eps = 1e-08) 
{  if (!(all((A - t(A)) <= 1e-06))) stop("A not symmetric")
   if (!(all((B - t(B)) <= 1e-06))) stop("B not symmetric")
   if (any(eigen(B)$values < eps)) stop("B not positive definite")
   svd.B.out <- svd(B)
   B.sqrt <- svd.B.out$u %*% diag(sqrt(svd.B.out$d)) %*% t(svd.B.out$u)
   svd.2.out <- svd(solve(B.sqrt) %*% A %*% solve(B.sqrt))
   W <- solve(B.sqrt) %*% svd.2.out$u
   Lambda.mat <- diag(svd.2.out$d)
   list(A.mat = A, B.mat = B, W.mat = W, Lambda.mat = Lambda.mat)
}

calc.alpha.bags <- function (x, y, aa=0.95, na.rm = TRUE, approx.limit = 2500, precision = 1) 
{
# based on the function compute.bagplot
# from the R package aplpack
# Wolf H (2019). _aplpack: Another Plot Package (version 190512)_. <URL: https://cran.r-project.org/package=aplpack>.

  win <- function(dx, dy) { atan2(y = dy, x = dx) }
  out.of.polygon <- function(xy, pg) {
        xy <- matrix(xy, ncol = 2)
        if (nrow(pg) == 1) return(xy[, 1] == pg[1] & xy[, 2] == pg[2])
        m <- nrow(xy)
        n <- nrow(pg)
        limit <- -abs(1e-10 * diff(range(pg)))
        pgn <- cbind(diff(c(pg[, 2], pg[1, 2])), -diff(c(pg[,1], pg[1, 1])))
        S <- colMeans(xy)
        dxy <- cbind(S[1] - pg[, 1], S[2] - pg[, 2])
        if (!all(limit < apply(dxy * pgn, 1, sum))) {
            pg <- pg[n:1, ]
            pgn <- -pgn[n:1, ]
        }
        in.pg <- rep(TRUE, m)
        for (j in 1:n) {
            dxy <- xy - matrix(pg[j, ], m, 2, byrow = TRUE)
            in.pg <- in.pg & limit < (dxy %*% pgn[j, ])
        }
        return(!in.pg)
  }
  cut.z.pg <- function(zx, zy, p1x, p1y, p2x, p2y) {
        a2 <- (p2y - p1y)/(p2x - p1x)
        a1 <- zy/zx
        sx <- (p1y - a2 * p1x)/(a1 - a2)
        sy <- a1 * sx
        sxy <- cbind(sx, sy)
        h <- any(is.nan(sxy)) || any(is.na(sxy)) || any(Inf == abs(sxy))
        if (h) {
            h <- 0 == zx
            sx <- ifelse(h, zx, sx)
            sy <- ifelse(h, p1y - a2 * p1x, sy)
            a1 <- ifelse(abs(a1) == Inf, sign(a1) * 123456789 * 1e+10, a1)
            a2 <- ifelse(abs(a2) == Inf, sign(a2) * 123456789 * 1e+10, a2)
            h <- 0 == (a1 - a2) & sign(zx) == sign(p1x)
            sx <- ifelse(h, p1x, sx)
            sy <- ifelse(h, p1y, sy)
            h <- 0 == (a1 - a2) & sign(zx) != sign(p1x)
            sx <- ifelse(h, p2x, sx)
            sy <- ifelse(h, p2y, sy)
            h <- p1x == p2x & zx != p1x & p1x != 0
            sx <- ifelse(h, p1x, sx)
            sy <- ifelse(h, zy * p1x/zx, sy)
            h <- p1x == p2x & zx != p1x & p1x == 0
            sx <- ifelse(h, p1x, sx)
            sy <- ifelse(h, 0, sy)
            h <- p1x == p2x & zx == p1x & p1x != 0
            sx <- ifelse(h, zx, sx)
            sy <- ifelse(h, zy, sy)
            h <- p1x == p2x & zx == p1x & p1x == 0 & sign(zy) == sign(p1y)
            sx <- ifelse(h, p1x, sx)
            sy <- ifelse(h, p1y, sy)
            h <- p1x == p2x & zx == p1x & p1x == 0 & sign(zy) != sign(p1y)
            sx <- ifelse(h, p1x, sx)
            sy <- ifelse(h, p2y, sy)
            h <- zx == p1x & zy == p1y
            sx <- ifelse(h, p1x, sx)
            sy <- ifelse(h, p1y, sy)
            h <- zx == p2x & zy == p2y
            sx <- ifelse(h, p2x, sx)
            sy <- ifelse(h, p2y, sy)
            h <- zx == 0 & zy == 0
            sx <- ifelse(h, 0, sx)
            sy <- ifelse(h, 0, sy)
            sxy <- cbind(sx, sy)
        }
        return(sxy)
  }
  find.cut.z.pg <- function(z, pg, center = c(0, 0)) {
        if (!is.matrix(z)) z <- rbind(z)
        if (1 == nrow(pg)) return(matrix(center, nrow(z), 2, TRUE))
        n.pg <- nrow(pg)
        n.z <- nrow(z)
        z <- cbind(z[, 1] - center[1], z[, 2] - center[2])
        pgo <- pg
        pg <- cbind(pg[, 1] - center[1], pg[, 2] - center[2])
        apg <- win(pg[, 1], pg[, 2])
        apg[is.nan(apg)] <- 0
        a <- order(apg)
        apg <- apg[a]
        pg <- pg[a, ]
        az <- win(z[, 1], z[, 2])
        segm.no <- apply((outer(apg, az, "<")), 2, sum)
        segm.no <- ifelse(segm.no == 0, n.pg, segm.no)
        next.no <- 1 + (segm.no%%length(apg))
        cuts <- cut.z.pg(z[, 1], z[, 2], pg[segm.no, 1], pg[segm.no,2], pg[next.no, 1], pg[next.no, 2])
        cuts <- cbind(cuts[, 1] + center[1], cuts[, 2] + center[2])
        return(cuts)
  }
  hdepth.of.points <- function(tp) {
        n.tp <- nrow(tp)
        tphdepth <- rep(0, n.tp)
        dpi <- 2 * pi - 1e-06
        for (j in 1:n.tp) {
            dx <- tp[j, 1] - xy[, 1]
            dy <- tp[j, 2] - xy[, 2]
            a <- win(dx, dy) + pi
            h <- a < 10
            a <- a[h]
            ident <- sum(!h)
            init <- sum(a < pi)
            a.shift <- (a + pi)%%dpi
            minusplus <- c(rep(-1, length(a)), rep(1, length(a)))
            h <- cumsum(minusplus[order(c(a, a.shift))])
            tphdepth[j] <- init + min(h) + 1
        }
        tphdepth
  }
  find.hdepths.tp <- function (tp, data, number.of.directions = 181) 
     {
         xy <- as.matrix(data)
         tp <- as.matrix(rbind(tp))
         n.tp <- dim(tp)[1]
         for (j in 1:2) {
           xy[, j] <- xy[, j] - (h <- min(xy[, j], na.rm = TRUE))
           tp[, j] <- tp[, j] - h
           if (0 < (h <- max(xy[, j], na.rm = TRUE))) {
             xy[, j] <- xy[, j]/h
             tp[, j] <- tp[, j]/h
           }
        }
        phi <- c(seq(0, 180, length = number.of.directions)[-1] * (2 * pi/360))
        sinphi <- c(sin(phi), 1)
        cosphi <- c(cos(phi), 0)
        RM1 <- round(digits = 6, rbind(cosphi, sinphi))
        hdtp <- rep(length(xy[, 1]), length(tp[, 1]))
        for (j in seq(along = sinphi)) {
          xyt <- xy %*% RM1[, j]
          tpt <- (tp %*% RM1[, j])[]
          xyt <- xyt[!is.na(xyt)]
          hdtp <- pmin(hdtp, (rank(c(tpt, xyt), ties.method = "min"))[1:n.tp] - rank(tpt, ties.method = "min"), 
                       rank(-c(tpt, xyt), ties.method = "min")[1:n.tp] - rank(-tpt, ties.method = "min"))
        }
        hdtp
  }
  expand.hull <- function(pg, k) {
        if (1 >= nrow(pg)) return(pg)
        resolution <- floor(20 * precision)
        pg0 <- xy[hdepth == 1, ]
        pg0 <- pg0[chull(pg0[, 1], pg0[, 2]), ]
        end.points <- find.cut.z.pg(pg, pg0, center = center)
        lam <- ((0:resolution)^1)/resolution^1
        pg.new <- pg
        for (i in 1:nrow(pg)) {
            tp <- cbind(pg[i, 1] + lam * (end.points[i, 1] - pg[i, 1]), pg[i, 2] + lam * (end.points[i, 2] - pg[i, 2]))
            hd.tp <- find.hdepths.tp(tp, xy)
            ind <- max(sum(hd.tp >= k), 1)
            if (ind < length(hd.tp)) {
                tp <- cbind(tp[ind, 1] + lam * (tp[ind + 1, 1] - tp[ind, 1]), tp[ind, 2] + lam * (tp[ind + 1, 2] - tp[ind, 2]))
                hp.tp <- find.hdepths.tp(tp, xy)
                ind <- max(sum(hd.tp >= k), 1)
            }
            pg.new[i, ] <- tp[ind, ]
        }
        pg.new <- pg.new[chull(pg.new[, 1], pg.new[, 2]), ]
        pg.add <- 0.5 * (pg.new + rbind(pg.new[-1, ], pg.new[1, ]))
        end.points <- find.cut.z.pg(pg.add, pg0, center = center)
        for (i in 1:nrow(pg.add)) {
            tp <- cbind(pg.add[i, 1] + lam * (end.points[i, 1] - pg.add[i, 1]), pg.add[i, 2] + lam * (end.points[i, 2] - pg.add[i, 2]))
            hd.tp <- find.hdepths.tp(tp, xy)
            ind <- max(sum(hd.tp >= k), 1)
            if (ind < length(hd.tp)) {
                tp <- cbind(tp[ind, 1] + lam * (tp[ind + 1, 1] - tp[ind, 1]), tp[ind, 2] + lam * (tp[ind + 1, 2] - tp[ind, 2]))
                hd.tp <- find.hdepths.tp(tp, xy)
                ind <- max(sum(hd.tp >= k), 1)
            }
            pg.add[i, ] <- tp[ind, ]
        }
        pg.new <- rbind(pg.new, pg.add)
        pg.new <- pg.new[chull(pg.new[, 1], pg.new[, 2]), ]
  }
  cut.p.sl.p.sl <- function(xy1, m1, xy2, m2) {
        sx <- (xy2[2] - m2 * xy2[1] - xy1[2] + m1 * xy1[1])/(m1 - m2)
        sy <- xy1[2] - m1 * xy1[1] + m1 * sx
        if (!is.nan(sy)) return(c(sx, sy))
        if (abs(m1) == Inf) return(c(xy1[1], xy2[2] + m2 * (xy1[1] - xy2[1])))
        if (abs(m2) == Inf) return(c(xy2[1], xy1[2] + m1 * (xy2[1] - xy1[1])))
  }
  pos.to.pg <- function(z, pg, reverse = FALSE) {
        if (reverse) {
            int.no <- apply(outer(pg[, 1], z[, 1], ">="), 2, sum)
            zy.on.pg <- pg[int.no, 2] + pg[int.no, 3] * (z[, 1] - pg[int.no, 1])
        }
        else {
            int.no <- apply(outer(pg[, 1], z[, 1], "<="), 2, sum)
            zy.on.pg <- pg[int.no, 2] + pg[int.no, 3] * (z[, 1] - pg[int.no, 1])
        }
        result <- ifelse(z[, 2] < zy.on.pg, "lower", "higher")
        return(result)
        if (all(result == "lower")) {
            result <- ifelse(((z[, 2] - zy.on.pg)/max(z[, 2] - zy.on.pg) + 1e-10) < 0, "lower", "higher")
        }
        if (all(result == "higher")) {
            result <- ifelse(((z[, 2] - zy.on.pg)/max(z[, 2] - zy.on.pg) - 1e-10) < 0, "lower", "higher")
        }
        print(result)
        return(result)
  }
  find.polygon.center <- function(xy) {
        if (length(xy) == 2) return(xy[1:2])
        if (nrow(xy) == 2) return(colMeans(xy))
        n <- length(xy[, 1])
        mxy <- colMeans(xy)
        xy2 <- rbind(xy[-1, ], xy[1, ])
        xy3 <- cbind(rep(mxy[1], n), mxy[2])
        S <- (xy + xy2 + xy3)/3
        F2 <- abs((xy[, 1] - xy3[, 1]) * (xy2[, 2] - xy3[, 2]) - (xy[, 2] - xy3[, 2]) * (xy2[, 1] - xy3[, 1]))
        lambda <- F2/sum(F2)
        SP <- colSums(cbind(S[, 1] * lambda, S[, 2] * lambda))
        return(SP)
  }
  xydata <- if (missing(y)) x
  else cbind(x, y)
  if (is.data.frame(xydata)) xydata <- as.matrix(xydata)
  if (any(is.na(xydata))) {
      if (na.rm) {
          xydata <- xydata[!apply(is.na(xydata), 1, any), , drop = FALSE]
          print("Warning: NA elements have been removed!!")
      }
      else {
          xy.medians <- apply(xydata, 2, function(x) median(x, na.rm = TRUE))
          for (j in 1:ncol(xydata)) xydata[is.na(xydata[, j]), j] <- xy.medians[j]
          print("Warning: NA elements have been exchanged by median values!!")
      }
  }
  if (length(xydata) < 4) {
     print("not enough data points")
     return()
  }
  if ((length(xydata)%%2) == 1) {
      print("number of values isn't even")
      return()
  }
  if (!is.matrix(xydata)) 
      xydata <- matrix(xydata, ncol = 2, byrow = TRUE)
  very.large.data.set <- nrow(xydata) > approx.limit
  if (very.large.data.set) {
      step <- (n <- nrow(xydata))/approx.limit
      ind <- round(seq(1, n, by = step))
      xy <- xydata[ind, ]
  }
  else xy <- xydata
  n <- nrow(xy)
  points.in.bag <- floor(n*aa)
  prdata <- prcomp(xydata)
  is.one.dim <- (0 == max(prdata[[1]])) || (min(prdata[[1]])/max(prdata[[1]])) < 1e-05
  if (is.one.dim) {
      center <- colMeans(xydata)
      res <- list(xy = xy, xydata = xydata, prdata = prdata, is.one.dim = is.one.dim, center = center)
      class(res) <- "bagplot"
      return(res)
  }
  if (nrow(xydata) <= 4) {
      center <- colMeans(xydata)
      res <- list(xy = xy, xydata = xydata, prdata = prdata, hdepths = rep(1, n), hdepth = rep(1, n), is.one.dim = is.one.dim, 
          center = center, hull.center = NULL, hull.bag = NULL, hull.loop = NULL, pxy.bag = NULL, pxy.outer = xydata, 
          pxy.outlier = NULL, exp.dk = xydata)
      class(res) <- "bagplot"
      return(res)
  }
  xym <- apply(xy, 2, mean)
  xysd <- apply(xy, 2, sd)
  xyxy <- cbind((xy[, 1] - xym[1])/xysd[1], (xy[, 2] - xym[2])/xysd[2])
  dx <- (outer(xy[, 1], xy[, 1], "-"))
  dy <- (outer(xy[, 2], xy[, 2], "-"))
  alpha <- atan2(y = dy, x = dx)
  diag(alpha) <- 1000
  for (j in 1:n) alpha[, j] <- sort(alpha[, j])
  alpha <- alpha[-n, ]
  m <- n - 1
  hdepth <- rep(0, n)
  dpi <- 2 * pi - 1e-06
  mypi <- pi - 1e-06
  minusplus <- c(rep(-1, m), rep(1, m))
  if (FALSE) {
      for (j in 1:n) {
          a <- alpha[, j] + pi
          h <- a < 10
          a <- a[h]
          init <- sum(a < mypi)
          a.shift <- (a + pi)%%dpi
          minusplus <- c(rep(-1, length(a)), rep(1, length(a)))
          h <- cumsum(minusplus[order(c(a, a.shift))])
          hdepth[j] <- init + min(h) + 1
      }
  }
  find.hdepths <- function(xy, number.of.directions = 181) {
      xy <- as.matrix(xy)
      for (j in 1:2) {
          xy[, j] <- xy[, j] - min(xy[, j])
          if (0 < (h <- max(xy[, j]))) xy[, j] <- xy[, j]/max(xy[, j])
      }
      phi <- c(seq(0, 180, length = number.of.directions)[-1] * (2 * pi/360))
      sinphi <- c(sin(phi), 1)
      cosphi <- c(cos(phi), 0)
      RM1 <- round(digits = 6, rbind(cosphi, sinphi))
      hd <- rep(h <- length(xy[, 1]), h)
      for (j in seq(along = sinphi)) {
          xyt <- xy %*% RM1[, j]
          hd <- pmin(hd, rank(xyt, ties.method = "min"), rank(-xyt, ties.method = "min"))
      }
      hd
  }
  hdepth <- find.hdepths(xy, 181 * precision)
  hd.table <- table(sort(hdepth))
  d.k <- cbind(dk = rev(cumsum(rev(hd.table))), k = as.numeric(names(hd.table)))
  k.1 <- sum(points.in.bag < d.k[, 1])
  k <- d.k[k.1, 2] + 1
  center <- apply(xy[which(hdepth == max(hdepth)), , drop = FALSE], 2, mean)
  hull.center <- NULL
  if (3 < nrow(xy) && length(hd.table) > 0) {
      n.p <- floor(1.5 * c(32, 16, 8)[1 + (n > 50) + (n > 200)] * precision)
      h <- unique(sort(hdepth, decreasing = TRUE))
      limit.hdepth.to.check <- sort(h)[min(length(h), 3)]
      h <- cands <- xy[limit.hdepth.to.check <= hdepth, , drop = FALSE]
      cands <- cands[chull(cands[, 1], cands[, 2]), ]
      n.c <- nrow(cands)
      if (is.null(n.c)) cands <- h
      xyextr <- rbind(apply(cands, 2, min), apply(cands, 2, max))
      if ((xyextr[2, 1] - xyextr[1, 1]) < 0.2 * (h <- diff(range(xy[, 1])))) {
          xyextr[1:2, 1] <- mean(xyextr[, 1]) + c(-0.1, 0.1) * h
      }
      if ((xyextr[2, 2] - xyextr[1, 2]) < 0.2 * (h <- diff(range(xy[, 2])))) {
          xyextr[1:2, 2] <- mean(xyextr[, 2]) + c(-0.1, 0.1) * h
      }
      h1 <- seq(xyextr[1, 1], xyextr[2, 1], length = n.p)
      h2 <- seq(xyextr[1, 2], xyextr[2, 2], length = n.p)
      tp <- cbind(as.vector(matrix(h1, n.p, n.p)), as.vector(matrix(h2, n.p, n.p, TRUE)))
      tphdepth <- max(find.hdepths.tp(tp, xy))
      tphdepth <- max(tphdepth, d.k[, 2])
      num <- floor(2 * c(417, 351, 171, 85, 67, 43)[sum(n > c(1, 50, 100, 150, 200, 250))] * precision)
      num.h <- floor(num/2)
      angles <- seq(0, pi, length = num.h)
      ang <- tan(pi/2 - angles)
      kkk <- tphdepth
      ia <- 1
      a <- angles[ia]
      xyt <- xyxy %*% c(cos(a), -sin(a))
      xyto <- order(xyt)
      ind.k <- xyto[kkk]
      cutp <- c(xyxy[ind.k, 1], -10)
      dxy <- diff(range(xyxy))
      pg <- rbind(c(cutp[1], -dxy, Inf), c(cutp[1], dxy, NA))
      ind.kk <- xyto[n + 1 - kkk]
      cutpl <- c(xyxy[ind.kk, 1], 10)
      pgl <- rbind(c(cutpl[1], dxy, -Inf), c(cutpl[1], -dxy, NA))
      for (ia in seq(angles)[-1]) {
          a <- angles[ia]
          angtan <- ang[ia]
          xyt <- xyxy %*% c(cos(a), -sin(a))
          xyto <- order(xyt)
          ind.k <- xyto[kkk]
          ind.kk <- xyto[n + 1 - kkk]
          pnew <- xyxy[ind.k, ]
          pnewl <- xyxy[ind.kk, ]
          if (abs(angtan) > 1e+10) {
              pg.no <- sum(pg[, 1] < pnew[1])
              if (0 < pg.no) {
                cutp <- c(pnew[1], pg[pg.no, 2] + pg[pg.no, 3] * (pnew[1] - pg[pg.no, 1]))
                pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
              }
              else {
                pg <- rbind(pg[1, ], c(pg[2, 1:2], NA))
              }
              pg.nol <- sum(pgl[, 1] >= pnewl[1])
              if (0 < pg.nol) {
                cutpl <- c(pnewl[1], pgl[pg.nol, 2] + pgl[pg.nol, 3] * (pnewl[1] - pgl[pg.nol, 1]))
                pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
              }
              else {
                pgl <- rbind(pgl[1, ], c(pgl[2, 1:2], NA))
              }
          }
          else {
              pg.inter <- pg[, 2] - angtan * pg[, 1]
              pnew.inter <- pnew[2] - angtan * pnew[1]
              pg.no <- sum(pg.inter < pnew.inter)
              if (is.na(pg[pg.no, 3])) pg[pg.no, 3] <- -Inf
              cutp <- cut.p.sl.p.sl(pnew, ang[ia], pg[pg.no, 1:2], pg[pg.no, 3])
              pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
              pg.interl <- pgl[, 2] - angtan * pgl[, 1]
              pnew.interl <- pnewl[2] - angtan * pnewl[1]
              pg.nol <- sum(pg.interl > pnew.interl)
              if (is.na(pgl[pg.nol, 3])) pgl[pg.nol, 3] <- Inf
              cutpl <- cut.p.sl.p.sl(pnewl, angtan, pgl[pg.nol, 1:2], pgl[pg.nol, 3])
              pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
          }
      }
      if (2 < nrow(pg) && 2 < nrow(pgl)) {
          limit <- 1e-10
          idx <- c(TRUE, (abs(diff(pg[, 1])) > limit) | (abs(diff(pg[, 2])) > limit))
          if (any(idx == FALSE)) {
              pg <- pg[idx, ]
              pg[, 3] <- c(diff(pg[, 2])/diff(pg[, 1]), NA)
          }
          idx <- c((abs(diff(pgl[, 1])) > limit) | (abs(diff(pgl[, 2])) > limit), TRUE)
          if (any(idx == FALSE)) {
              pgl <- pgl[idx, ]
              pgl[, 3] <- c(diff(pgl[, 2])/diff(pgl[, 1]), NA)
          }
          pgl[, 2] <- pgl[, 2] - 1e-05
          pg <- pg[-nrow(pg), ][-1, , drop = FALSE]
          pgl <- pgl[-nrow(pgl), ][-1, , drop = FALSE]
          indl <- pos.to.pg(round(pgl, digits = 10), round(pg, digits = 10))
          indu <- pos.to.pg(round(pg, digits = 10), round(pgl, digits = 10), TRUE)
          sr <- sl <- NULL
          if (indu[(npg <- nrow(pg))] == "lower" & indl[1] == "higher") {
              rnuml <- which(indl == "lower")[1] - 1
              rnumu <- npg + 1 - which(rev(indu == "higher"))[1]
              if (is.na(rnuml)) rnuml <- sum(pg[rnumu, 1] < pgl[, 1])
              if (is.na(rnumu)) rnumu <- sum(pg[, 1] < pgl[rnuml, 1])
              xyl <- pgl[rnuml, ]
              xyu <- pg[rnumu, ]
              sr <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
          }
          if (indl[(npgl <- nrow(pgl))] == "higher" & indu[1] == "lower") {
              lnuml <- npgl + 1 - which(rev(indl == "lower"))[1]
              lnumu <- which(indu == "higher")[1] - 1
              if (is.na(lnuml)) lnuml <- sum(pg[lnumu, 1] < pgl[, 1])
              if (is.na(lnumu)) lnumu <- sum(pg[, 1] < pgl[lnuml, 1])
              xyl <- pgl[lnuml, ]
              xyu <- pg[lnumu, ]
              sl <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
          }
          pg <- rbind(pg[indu == "higher", 1:2, drop = FALSE], sr, pgl[indl == "lower", 1:2, drop = FALSE], sl)
          if (!any(is.na(pg))) pg <- pg[chull(pg[, 1], pg[, 2]), ]
      }
      else {
          if (2 < nrow(pgl)) { pg <- rbind(pg[2, 1:2], pgl[-c(1, length(pgl[, 1])), 1:2]) }
          else { pg <- rbind(pg[-c(1, length(pg[, 1])), 1:2], pgl[2, 1:2])  }
      }
      hull.center <- cbind(pg[, 1] * xysd[1] + xym[1], pg[, 2] * xysd[2] + xym[2])
      if (!any(is.na(hull.center))) center <- find.polygon.center(hull.center)
      else hull.center <- rbind(center)
  }
    
      num <- floor(2 * c(417, 351, 171, 85, 67, 43)[sum(n > c(1, 50, 100, 150, 200, 250))] * precision)
      num.h <- floor(num/2)
      angles <- seq(0, pi, length = num.h)
      ang <- tan(pi/2 - angles)
      kkk <- k
      if (kkk <= max(d.k[, 2])) {
          ia <- 1
          a <- angles[ia]
          xyt <- xyxy %*% c(cos(a), -sin(a))
          xyto <- order(xyt)
          ind.k <- xyto[kkk]
          cutp <- c(xyxy[ind.k, 1], -10)
          dxy <- diff(range(xyxy))
          pg <- rbind(c(cutp[1], -dxy, Inf), c(cutp[1], dxy, NA))
          ind.kk <- xyto[n + 1 - kkk]
          cutpl <- c(xyxy[ind.kk, 1], 10)
          pgl <- rbind(c(cutpl[1], dxy, -Inf), c(cutpl[1], -dxy, NA))
          for (ia in seq(angles)[-1]) {
              a <- angles[ia]
              angtan <- ang[ia]
              xyt <- xyxy %*% c(cos(a), -sin(a))
              xyto <- order(xyt)
              ind.k <- xyto[kkk]
              ind.kk <- xyto[n + 1 - kkk]
              pnew <- xyxy[ind.k, ]
              pnewl <- xyxy[ind.kk, ]
              if (abs(angtan) > 1e+10) {
                pg.no <- sum(pg[, 1] < pnew[1])
                if (0 < pg.no) { cutp <- c(pnew[1], pg[pg.no, 2] + pg[pg.no, 3] * (pnew[1] - pg[pg.no, 1]))
                                 pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
                }
                else {
                  pg <- rbind(pg[1, ], c(pg[2, 1:2], NA))
                }
                pg.nol <- sum(pgl[, 1] >= pnewl[1])
                if (0 < pg.nol) { cutpl <- c(pnewl[1], pgl[pg.nol, 2] + pgl[pg.nol, 3] * (pnewl[1] - pgl[pg.nol, 1]))
                                  pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
                }
                else {
                  pgl <- rbind(pgl[1, ], c(pgl[2, 1:2], NA))
                }
              }
              else {
                pg.inter <- pg[, 2] - angtan * pg[, 1]
                pnew.inter <- pnew[2] - angtan * pnew[1]
                pg.no <- sum(pg.inter < pnew.inter)
                if (is.na(pg[pg.no, 3])) pg[pg.no, 3] <- -Inf
                cutp <- cut.p.sl.p.sl(pnew, ang[ia], pg[pg.no, 1:2], pg[pg.no, 3])
                pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
                pg.interl <- pgl[, 2] - angtan * pgl[, 1]
                pnew.interl <- pnewl[2] - angtan * pnewl[1]
                pg.nol <- sum(pg.interl > pnew.interl)
                if (is.na(pgl[pg.nol, 3])) pgl[pg.nol, 3] <- Inf
                cutpl <- cut.p.sl.p.sl(pnewl, angtan, pgl[pg.nol, 1:2], pgl[pg.nol, 3])
                pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
              }
          }
          if (2 < nrow(pg) && 2 < nrow(pgl)) {
              limit <- 1e-10
              idx <- c(TRUE, (abs(diff(pg[, 1])) > limit) | (abs(diff(pg[, 2])) > limit))
              if (any(idx == FALSE)) {
                pg <- pg[idx, ]
                pg[, 3] <- c(diff(pg[, 2])/diff(pg[, 1]), NA)
              }
              idx <- c((abs(diff(pgl[, 1])) > limit) | (abs(diff(pgl[, 2])) > limit), TRUE)
              if (any(idx == FALSE)) {
                pgl <- pgl[idx, ]
                pgl[, 3] <- c(diff(pgl[, 2])/diff(pgl[, 1]), NA)
              }
              pgl[, 2] <- pgl[, 2] - 1e-05
              pg <- pg[-nrow(pg), ][-1, , drop = FALSE]
              pgl <- pgl[-nrow(pgl), ][-1, , drop = FALSE]
              indl <- pos.to.pg(round(pgl, digits = 10), round(pg, digits = 10))
              indu <- pos.to.pg(round(pg, digits = 10), round(pgl, digits = 10), TRUE)
              sr <- sl <- NULL
              if (indu[(npg <- nrow(pg))] == "lower" & indl[1] == "higher") {
                rnuml <- which(indl == "lower")[1] - 1
                rnumu <- npg + 1 - which(rev(indu == "higher"))[1]
                if (is.na(rnuml)) rnuml <- sum(pg[rnumu, 1] < pgl[, 1])
                if (is.na(rnumu)) rnumu <- sum(pg[, 1] < pgl[rnuml, 1])
                xyl <- pgl[rnuml, ]
                xyu <- pg[rnumu, ]
                sr <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
              }
              if (indl[(npgl <- nrow(pgl))] == "higher" & indu[1] == "lower") {
                lnuml <- npgl + 1 - which(rev(indl == "lower"))[1]
                lnumu <- which(indu == "higher")[1] - 1
                if (is.na(lnuml)) lnuml <- sum(pg[lnumu, 1] < pgl[, 1])
                if (is.na(lnumu)) lnumu <- sum(pg[, 1] < pgl[lnuml, 1])
                xyl <- pgl[lnuml, ]
                xyu <- pg[lnumu, ]
                sl <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
              }
              pg <- rbind(pg[indu == "higher", 1:2, drop = FALSE], sr, pgl[indl == "lower", 1:2, drop = FALSE], sl)
              if (!any(is.na(pg))) pg <- pg[chull(pg[, 1], pg[, 2]), ]
          }
          else {
              if (2 < nrow(pgl)) { pg <- rbind(pg[2, 1:2], pgl[-c(1, length(pgl[, 1])), 1:2])  }
              else {  pg <- rbind(pg[-c(1, length(pg[, 1])), 1:2], pgl[2, 1:2])  }
          }
          exp.dk <- cbind(pg[, 1] * xysd[1] + xym[1], pg[, 2] * xysd[2] + xym[2])
      }
      else {
          exp.dk <- NULL
      }
      if (1 < kkk) kkk <- kkk - 1
      ia <- 1
      a <- angles[ia]
      xyt <- xyxy %*% c(cos(a), -sin(a))
      xyto <- order(xyt)
      ind.k <- xyto[kkk]
      cutp <- c(xyxy[ind.k, 1], -10)
      dxy <- diff(range(xyxy))
      pg <- rbind(c(cutp[1], -dxy, Inf), c(cutp[1], dxy, NA))
      ind.kk <- xyto[n + 1 - kkk]
      cutpl <- c(xyxy[ind.kk, 1], 10)
      pgl <- rbind(c(cutpl[1], dxy, -Inf), c(cutpl[1], -dxy, NA))
      for (ia in seq(angles)[-1]) {
          a <- angles[ia]
          angtan <- ang[ia]
          xyt <- xyxy %*% c(cos(a), -sin(a))
          xyto <- order(xyt)
          ind.k <- xyto[kkk]
          ind.kk <- xyto[n + 1 - kkk]
          pnew <- xyxy[ind.k, ]
          pnewl <- xyxy[ind.kk, ]
          if (abs(angtan) > 1e+10) {
              pg.no <- sum(pg[, 1] < pnew[1])
              if (0 < pg.no) { cutp <- c(pnew[1], pg[pg.no, 2] + pg[pg.no, 3] * (pnew[1] - pg[pg.no, 1]))
                               pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
              }
              else {
                pg <- rbind(pg[1, ], c(pg[2, 1:2], NA))
              }
              pg.nol <- sum(pgl[, 1] >= pnewl[1])
              if (0 < pg.nol) { cutpl <- c(pnewl[1], pgl[pg.nol, 2] + pgl[pg.nol, 3] * (pnewl[1] - pgl[pg.nol, 1]))
                                pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
              }
              else {
                pgl <- rbind(pgl[1, ], c(pgl[2, 1:2], NA))
              }
          }
          else {
              pg.inter <- pg[, 2] - angtan * pg[, 1]
              pnew.inter <- pnew[2] - angtan * pnew[1]
              pg.no <- sum(pg.inter < pnew.inter)
              if (is.na(pg[pg.no, 3])) pg[pg.no, 3] <- -Inf
              cutp <- cut.p.sl.p.sl(pnew, ang[ia], pg[pg.no, 1:2], pg[pg.no, 3])
              pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
              pg.interl <- pgl[, 2] - angtan * pgl[, 1]
              pnew.interl <- pnewl[2] - angtan * pnewl[1]
              pg.nol <- sum(pg.interl > pnew.interl)
              if (is.na(pgl[pg.nol, 3])) pgl[pg.nol, 3] <- Inf
              cutpl <- cut.p.sl.p.sl(pnewl, angtan, pgl[pg.nol, 1:2], pgl[pg.nol, 3])
              pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
          }
      }
      if (2 < nrow(pg) && 2 < nrow(pgl)) {
          limit <- 1e-10
          idx <- c(TRUE, (abs(diff(pg[, 1])) > limit) | (abs(diff(pg[, 2])) > limit))
          if (any(idx == FALSE)) {
              pg <- pg[idx, ]
              pg[, 3] <- c(diff(pg[, 2])/diff(pg[, 1]), NA)
          }
          idx <- c((abs(diff(pgl[, 1])) > limit) | (abs(diff(pgl[, 2])) > limit), TRUE)
          if (any(idx == FALSE)) {
              pgl <- pgl[idx, ]
              pgl[, 3] <- c(diff(pgl[, 2])/diff(pgl[, 1]), NA)
          }
          pgl[, 2] <- pgl[, 2] - 1e-05
          pg <- pg[-nrow(pg), ][-1, , drop = FALSE]
          pgl <- pgl[-nrow(pgl), ][-1, , drop = FALSE]
          indl <- pos.to.pg(round(pgl, digits = 10), round(pg, digits = 10))
          indu <- pos.to.pg(round(pg, digits = 10), round(pgl, digits = 10), TRUE)
          sr <- sl <- NULL
          if (indu[(npg <- nrow(pg))] == "lower" & indl[1] == "higher") {
              rnuml <- which(indl == "lower")[1] - 1
              rnumu <- npg + 1 - which(rev(indu == "higher"))[1]
              if (is.na(rnuml)) rnuml <- sum(pg[rnumu, 1] < pgl[, 1])
              if (is.na(rnumu)) rnumu <- sum(pg[, 1] < pgl[rnuml, 1])
              xyl <- pgl[rnuml, ]
              xyu <- pg[rnumu, ]
              sr <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
          }
          if (indl[(npgl <- nrow(pgl))] == "higher" & indu[1] == "lower") {
              lnuml <- npgl + 1 - which(rev(indl == "lower"))[1]
              lnumu <- which(indu == "higher")[1] - 1
              if (is.na(lnuml)) lnuml <- sum(pg[lnumu, 1] < pgl[, 1])
              if (is.na(lnumu)) lnumu <- sum(pg[, 1] < pgl[lnuml, 1])
              xyl <- pgl[lnuml, ]
              xyu <- pg[lnumu, ]
              sl <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
          }
          pg <- rbind(pg[indu == "higher", 1:2, drop = FALSE], sr, pgl[indl == "lower", 1:2, drop = FALSE], sl)
          if (!any(is.na(pg))) pg <- pg[chull(pg[, 1], pg[, 2]), ]
      }
      else {
          if (2 < nrow(pgl)) { pg <- rbind(pg[2, 1:2], pgl[-c(1, length(pgl[, 1])), 1:2]) }
          else { pg <- rbind(pg[-c(1, length(pg[, 1])), 1:2], pgl[2, 1:2]) }
      }
      exp.dk.1 <- cbind(pg[, 1] * xysd[1] + xym[1], pg[, 2] * xysd[2] + xym[2])
      if (is.null(exp.dk)) exp.dk <- exp.dk.1
    
  if (nrow(d.k) == k.1 || nrow(d.k) == 1) lambda <- 0
  else {
      ind <- sum(d.k[, 2] <= k.1)
      ind <- k.1
      ndk.1 <- d.k[ind, 1]
      ndk <- d.k[ind + 1, 1]
      lambda <- (n*aa - ndk)/(ndk.1 - ndk)
  }
  cut.on.pdk.1 <- find.cut.z.pg(exp.dk, exp.dk.1, center = center)
  cut.on.pdk <- find.cut.z.pg(exp.dk.1, exp.dk, center = center)
  h1 <- (1 - lambda) * exp.dk + lambda * cut.on.pdk.1
  h2 <- (1 - lambda) * cut.on.pdk + lambda * exp.dk.1
  h <- rbind(h1, h2)
  h <- h[!is.nan(h[, 1]) & !is.nan(h[, 2]), ]
  h[chull(h[, 1], h[, 2]), ]
}

calc.concentration.ellipse <- function (X, kappa = 2, covmat = NULL) 
{  means <- matrix(apply(X, 2, mean), nrow = 2)
   if (is.null(covmat)) covmat <- cov(X)
   range.vec <- apply(X, 2, range)
   mid.vec <- apply(range.vec, 2, function(x) (x[2] + x[1])/2)
   dif <- max(range.vec[2, ] - range.vec[1, ])/2
   xlim <- c(mid.vec[1] - dif, mid.vec[1] + dif)
   ylim <- c(mid.vec[2] - dif, mid.vec[2] + dif)
   svd.covmat <- svd(covmat)
   a <- (0:6283)/1000
   Y <- cbind(cos(a), sin(a))
   Y <- Y %*% diag(sqrt(svd.covmat$d)) %*% t(svd.covmat$v) * kappa
   Y + matrix(rep(1, 6284), ncol = 1) %*% t(means)
}

calibrate.axis <- function (j, unscaled.X, means, sd, axes.rows, ax.which, ax.tickvec, 
                            ax.orthogxvec, ax.orthogyvec, ax.oblique) 
{  ax.num <- ax.which[j]
   tick <- ax.tickvec[j]
   ax.direction <- axes.rows[ax.num, ]
   r <- ncol(axes.rows)
   ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
   if (nrow(ax.orthog) < r) ax.orthog <- rbind(ax.orthog, 0)
   if (nrow(axes.rows) > 1) phi.vec <- diag(1/diag(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num] 
   else phi.vec <- (1/(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num]
   number.points <- 100
   std.ax.tick.label <- pretty(unscaled.X[, ax.num], n = tick)
   std.range <- c(min(std.ax.tick.label), max(std.ax.tick.label))
   std.ax.tick.label.min <- std.ax.tick.label - (std.range[2] - std.range[1])
   std.ax.tick.label.max <- std.ax.tick.label + (std.range[2] - std.range[1])
   std.ax.tick.label <- c(std.ax.tick.label, std.ax.tick.label.min, std.ax.tick.label.max)
   interval <- (std.ax.tick.label - means[ax.num])/sd[ax.num]
   axis.vals <- seq(from = min(interval), to = max(interval), length = number.points)
   axis.vals <- sort(unique(c(axis.vals, interval)))
   number.points <- length(axis.vals)
   axis.points <- matrix(0, nrow = number.points, ncol = r)
   for (i in 1:r) axis.points[, i] <- ax.orthog[i, ax.num] + (axis.vals - phi.vec[ax.num]) * ax.direction[i]
   if (!is.null(ax.oblique)) for (i in 1:r) axis.points[, i] <- axis.vals * ax.direction[i] - ((ax.oblique[ax.num] - means[ax.num])/sd[ax.num]) * ax.direction[i] + (((ax.oblique - means)/sd) %*% axes.rows[, i])/p
   axis.points <- cbind(axis.points, axis.vals * sd[ax.num] + means[ax.num], 0)
   for (i in 1:number.points) if (any(zapsmall(axis.points[i, r + 1] - std.ax.tick.label) == 0)) axis.points[i, r + 2] <- 1
   axis.points
}

calibrate.cat.axis <- function (j, axes.rows, ax.which, ax.orthogxvec, ax.orthogyvec, ax.oblique, markers, labels) 
{  ax.num <- ax.which[j]
   ax.direction <- axes.rows[ax.num, ]
   r <- ncol(axes.rows)
   ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
   if (nrow(ax.orthog) < r) ax.orthog <- rbind(ax.orthog, 0)
   if (nrow(axes.rows) > 1) phi.vec <- diag(1/diag(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, j] else phi.vec <- (1/(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, j]
   std.range <- range(markers)
   tick.label.min <- min(markers) - (std.range[2] - std.range[1])
   tick.label.max <- max(markers) + (std.range[2] - std.range[1])
   axis.vals <- c(tick.label.min, markers, tick.label.max)
   number.points <- length(axis.vals)
   axis.points <- as.data.frame(matrix(0, nrow = number.points, ncol = r+2))
   axis.points[,r+1] <- c(0, labels, 0)
   axis.points[,r+2] <- 1
   axis.points[1,r+2] <- axis.points[number.points,r+2] <- 0
   for (i in 1:r) axis.points[, i] <- ax.orthog[i, ax.num] + (axis.vals - phi.vec[ax.num]) * ax.direction[i]
   if (!is.null(ax.oblique)) for (i in 1:r) axis.points[, i] <- axis.vals * ax.direction[i] - ax.oblique[ax.num] * ax.direction[i] + (ax.oblique %*% axes.rows[, i])/p
   axis.points
}

biplot.spline.axis <- function(j, X, Ytilde, means, sd, n.int, spline.control, dmeth=0, ... )
{
  n<-nrow(X)
  p<-ncol(X)
  if (n > 103)
    {  my.sample <- sample (1:n, size=103, replace=F)
       X <- X[my.sample,]
       Ytilde <- Ytilde[my.sample,]
       n <- nrow(X)
    }

  tau <- spline.control$tau
  nmu <- spline.control$nmu
  u <- spline.control$u
  v <- spline.control$v
  lambda <- spline.control$lambda
  smallsigma <- spline.control$smallsigma
  bigsigma <- spline.control$bigsigma
  gamma <- spline.control$gamma
  bigsigmaactivate <- spline.control$bigsigmaactivate
  eps <- spline.control$eps
  tiny <- spline.control$tiny
  itmax <- spline.control$itmax
  ftol <- spline.control$ftol

  cat ("Calculation spline axis for variable", j, "\n")
  require(splines)
  if(dmeth==1)stop("dmeth should be equal to zero or integer greater than 1 \n")  
  Y<-scale(Ytilde,center=means,scale=sd)
  
  ytilde<-Ytilde[,j]
  mutilde<-seq(from=min(ytilde),to=max(ytilde),length.out=nmu)
  y<-Y[,j]
  rangey<-max(y)-min(y)
  mu<-seq(from=min(y)-.3*rangey,to=max(y)+.3*rangey,length.out=nmu)
  markers <- (pretty(ytilde)-means[j])/sd[j]
  mu <- sort(c(mu,markers))
  mu <- unique(mu)
  nmu <- length(mu)
  
  if (v>0)
    {
    knots<-seq.int(from=0,to=1,length.out=v+2)[-c(1,v+2)]
    knots<-quantile(y,knots)
    M<-bs(mu,knots=knots,degree=u,intercept=FALSE)
    }
    else M<-bs(mu,df=u+v,degree=u,intercept=FALSE)
  M<-scale(M,scale=FALSE,center=M[which.min(abs(mu)),]) # To ensure that the spline passes through the origin at the calibration which represents the mean of the variable
  Breg<-t(solve(t(X)%*%X)%*%t(X)%*%y)
  Zreg<-mu%*%Breg/sum(Breg^2)
  Bvec<-as.vector(solve(t(M)%*%M)%*%t(M)%*%Zreg)  # Closest to regression biplot

  const1<-sum(y^2)
  const2<-sum(X^2)/(n*p)
  TotalNumberOfLossFunctionCalls<-0

  optimtouse<-function(Bvec)
    {
       timetemp<-proc.time()[3]
       LOSS<-1.0
       LOSS1<-1.0
       Ind<-rep(1,n)
       pred<-rep(0,nmu)
       deltmp<-0
       tau<-tau
       #.5 # the choice of tau seems to affect perfomance quite substantially.
       # tau is used to specify the points on the inital simplex.
       Ay<-rep(0,(u+v)*p+1)
       TEMPVK<-rep(0,(u+v)*p)
       iter1<-0
       iter<-0
       ERRO <-0

       # Prepare for Fortran subroutine
       storage.mode(X)<- "double"
       storage.mode(Ind)<- "integer"
       storage.mode(mu)<- "double"
       storage.mode(pred)<- "double"
       storage.mode(y)<- "double"
       storage.mode(M)<- "double"
       storage.mode(Bvec)<- "double"
       storage.mode(Ay)<- "double"
       storage.mode(TEMPVK)<- "double"

       returned_data<-.Fortran('L',LOSS=as.double(LOSS),X=X,n=as.integer(n),p=as.integer(p),nmu=as.integer(nmu),Ind=Ind,
                                   mu=mu,pred=pred,lambda=as.double(lambda),y=y,const1=as.double(const1),const2=as.double(const2),u=as.integer(u),
                                   v=as.integer(v),M=M,Bvec=Bvec,tau=as.double(tau),Ay=Ay,TEMPVEK=TEMPVK,iter=as.integer(iter),
                                   ftol=as.double(ftol),LOSS1=as.double(LOSS1),iter1=as.integer(iter1),fout = as.integer(ERRO),
                                       const3=as.double(tiny), itmax=as.integer(itmax))
       if(returned_data$fout > 0)
        {
          cat("Fout is: ", returned_data$fout, "\n")
          warning("Increase itmax for Fortran \n")
        }
       
       B<-matrix(returned_data$Bvec,ncol=p)
       Z<-M%*%B 

    aa<-list(BestValue=returned_data$LOSS,BestSolution=returned_data$Bvec,ConvergenceCode=returned_data$fout, iter1=returned_data$iter1,
             iter=returned_data$iter,TimeTaken=proc.time()[3]-timetemp)
    aa
    }
  EuclidDist2 <- function (X, Y) 
  {
    n <- nrow(X)
    m <- nrow(Y)
    bx <- rowSums(X^2)
    by <- rowSums(Y^2)
    outer(bx, by, FUN = "+") - 2 * X %*% t(Y)
  }

  ### Variable initialisation
  outBestValues<-rep(NA,gamma+1)
  outBestSolutions<-matrix(nrow=2*(u+v),ncol=gamma+1)
  outTimeTaken<-rep(NA,gamma+1) # Is made one element longer at each iteration.
  BestSolutionsFrequency<-rep(NA,gamma+1)
  BestSolutionsIndices<-rep(NA,gamma+1) # Is made one element longer at each iteration.
  SquaredDistancesBetweenBestSolutions<-matrix(nrow=gamma+1,ncol=gamma+1)

  ### Initial coefficients closest to regression biplot
  temp<-optimtouse(Bvec)
  outBestValues[1]<-temp$BestValue
  outBestSolutions[,1]<-temp$BestSolution
  outTimeTaken[1]<-temp$TimeTaken
  BestSolutionsFrequency[1]<-1
  BestSolutionsIndices[1]<-1
  DistinctSolutions<-1
  PreviousBestSolution<-NA
  nSameSolutionConsecutively<-0
  BigSigmaActivations<-NULL
  
  test.iter <- temp$iter
  test.iter1 <- temp$iter1

  ### Last best coefficients perturbed
  for (gammacounter in 2:(gamma+1))
    {
    if (nSameSolutionConsecutively>=bigsigmaactivate)
      {
      temp<-optimtouse(outBestSolutions[,which.min(outBestValues)]+rnorm((u+v)*2,mean=0,sd=bigsigma))
      BigSigmaActivations<-c(BigSigmaActivations,gammacounter)
      }
      else temp<-optimtouse(outBestSolutions[,which.min(outBestValues)]+rnorm((u+v)*2,mean=0,sd=smallsigma))
    outTimeTaken[gammacounter]<-temp$TimeTaken
    tempSquaredDistances<-EuclidDist2(matrix(temp$BestSolution,nrow=1),t(outBestSolutions[,1:DistinctSolutions]))
    if (any(tempSquaredDistances<eps))
      {
      BestSolutionsIndices[gammacounter]<-tempAA<-which.min(tempSquaredDistances)
      BestSolutionsFrequency[tempAA]<-BestSolutionsFrequency[tempAA]+1
      if (!is.na(PreviousBestSolution) && tempAA==PreviousBestSolution) nSameSolutionConsecutively<-nSameSolutionConsecutively+1
        else
          {
          PreviousBestSolution<-tempAA
          nSameSolutionConsecutively<-0
          }
      }
      else
        {
        DistinctSolutions<-DistinctSolutions+1
        outBestValues[DistinctSolutions]<-temp$BestValue
        outBestSolutions[,DistinctSolutions]<-temp$BestSolution
        BestSolutionsFrequency[DistinctSolutions]<-1
        BestSolutionsIndices[gammacounter]<-DistinctSolutions
        SquaredDistancesBetweenBestSolutions[1:(DistinctSolutions-1),DistinctSolutions]<-tempSquaredDistances
        nSameSolutionConsecutively<-0
        }
    }
  axis.points <- cbind(M%*%matrix(outBestSolutions[,which.min(outBestValues)],ncol=2), mu, 0)

  for (i in 1:nrow(axis.points)) if (any(zapsmall(axis.points[i,3] - markers) == 0)) axis.points[i, 4] <- 1
  axis.points[,3] <- axis.points[,3]*sd[j] + means[j]
  axis.points
}

biplot.create.regions <- function (region.style, plot.range, region.mid, rotate.mat=diag(2), reflect.mat=diag(2), region.func, ...) 
{
       J <- nrow(region.mid)
       x.vec <- y.vec <- seq(from=plot.range[1], to=plot.range[2], length=10)
       frame.points <- rbind(cbind(plot.range[1],seq(from=plot.range[1],to=plot.range[2],length=50)),
                             cbind(plot.range[2],seq(from=plot.range[1],to=plot.range[2],length=50)),
                             cbind(seq(from=plot.range[1],to=plot.range[2],length=50),plot.range[1]),
                             cbind(seq(from=plot.range[1],to=plot.range[2],length=50),plot.range[2]))
       colnames(frame.points) <- c("Var1","Var2")
       my.grid <- expand.grid(x.vec, y.vec)
           my.grid <- as.matrix(my.grid) %*% reflect.mat %*% t(rotate.mat)
           mid.trans <- region.mid %*% reflect.mat %*% t(rotate.mat)
       for (h in 1:region.style$space.fill)
         {
           out <- apply (my.grid, 1, region.func, ...)
           Z.region <- vector("list", J)
               for (j in 1:J) Z.region[[j]] <- my.grid[out==j,][chull(my.grid[out==j,]),]
           new.grid <- NULL
           for (i in 1:(J-1))
             for (k in (i+1):J)
                for (i1 in 1:nrow(Z.region[[i]]))
                   for (k1 in 1:nrow(Z.region[[k]]))                    
                                     {
                                            if (nrow(Z.region[[i]]) > 1 & nrow(Z.region[[k]]) > 1) add.points <- cbind(rnorm(region.style$space.fill,(Z.region[[i]][i1,1] + Z.region[[k]][k1,1])/2,(plot.range[2]-plot.range[1])/100),rnorm(region.style$space.fill,(Z.region[[i]][i1,2] + Z.region[[k]][k1,2])/2,(plot.range[2]-plot.range[1])/100))
                                            else add.points <- cbind(c(rnorm(region.style$space.fill,mid.trans[i,1],(plot.range[2]-plot.range[1])/100),rnorm(region.style$space.fill,mid.trans[k,1],(plot.range[2]-plot.range[1])/100)),c(rnorm(region.style$space.fill,mid.trans[i,2],(plot.range[2]-plot.range[1])/100),rnorm(region.style$space.fill,mid.trans[k,2],(plot.range[2]-plot.range[1])/100)))
                        new.grid <- rbind (new.grid, add.points)
                                         }      
          my.grid <- rbind(new.grid, frame.points)
        }

       out <- apply (my.grid, 1,  region.func, ...)
       Z.region <- vector("list", J)
       for (j in 1:J) Z.region[[j]] <- my.grid[out==j,][chull(my.grid[out==j,]),]
           Z.region <- lapply (Z.region, function(X) X %*% rotate.mat %*% reflect.mat)
}

biplot.LDA.class.func <- function (vec, class.means, n, Mrr, class.dim=2, prior.prob=rep(1/J,J)) 
{
   J <- nrow(class.means)
   while (length(vec) < class.dim) vec <- c(vec,0)
   mat <- (rbind (vec, class.means))
   rev(order(log(prior.prob)-as.matrix(dist(mat)^2)[1,-1]*(n-J)/2))[1]
}

biplot.QDA.class.func <- function (vec, J, phi.means, phi.sd=rep(1,length(phi.means)), Vmat=diag(2), 
                                   prior.prob=rep(1/J,J)) 
{
   phi.vec <- vec %*% t(Vmat[,1:2])*phi.sd + phi.means
   rev(order(log(prior.prob) - 0.5*phi.vec))[1]
}

scale.3waydata <- function(Xarray)
{
     p <-dim(Xarray)[2]
     k <- dim(Xarray)[3]

     unscaled_data <- Xarray
     meansk <- vector("list",k)
     sd_var <- matrix(0,p,1)

     for (i in 1:k)
       {
          meansk[[i]]<-t(as.matrix(apply(Xarray[,,i],2,mean)))
          Xarray[,,i]<-scale(Xarray[,,i],center=T,scale=F) 
       }

     for (i in 1:p)
       {  
         sd_var[i]<-sd(as.vector(Xarray[,i,]))
         Xarray[,i,]<-(Xarray[,i,])/sd(as.vector(Xarray[,i,]))
       }
	   
     X_unscaled <- vector("list",k)
     X_unscaled <- lapply(1:k, function(x)unscaled_data[,,x])
     unscaled.Xwide <- do.call(cbind, X_unscaled)   
     means_new <- do.call(cbind,meansk)
	 
     list(unscaled.Xwide=unscaled.Xwide, X=Xarray, means.wide=means_new, sd.var=sd_var)
}

ratio.axis <- function (j, X, r.vec, Gamma, ax.pairs, ax.which, ax.tickvec, ax.orthogxvec, ax.orthogyvec, eps=0.000001) 
{
    ax.num <- ax.which[j]
    tick <- ax.tickvec[j]
        as1 <- ax.pairs[ax.num,1]
        as2 <- ax.pairs[ax.num,2]
        gammai <- Gamma[as1,]
    gammaj <- Gamma[as2,]
    coef <- lm (c(gammai[2],gammaj[2]) ~ c(gammai[1],gammaj[1]))$coefficients
    gi <- gammai-c(-coef[1]/(coef[2] + 1/coef[2]),coef[1]/(coef[2] + 1/coef[2])/coef[2])
    gj <- gammaj-c(-coef[1]/(coef[2] + 1/coef[2]),coef[1]/(coef[2] + 1/coef[2])/coef[2])

    ax.direction <- gammai-c(-coef[1]/(coef[2] + 1/coef[2]),coef[1]/(coef[2] + 1/coef[2])/coef[2])
    r <- ncol(Gamma)
    ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
    if (nrow(ax.orthog) < r) ax.orthog <- rbind(ax.orthog, 0)
    phi <- sum(ax.direction * ax.orthog[, j]) / sum(ax.direction^2)
    number.points <- 20
        
        std.ax.tick.label1 <- pretty(X[,as1] / X[,as2], n=tick)
        std.ax.tick.label1 <- std.ax.tick.label1[!(std.ax.tick.label1<eps)]
        std.ax.tick.label2 <- pretty(X[,as2] / X[,as1], n=tick)
        std.ax.tick.label2 <- std.ax.tick.label2[!(std.ax.tick.label2<eps)]
        std.ax.tick.label <- sort(c(std.ax.tick.label1,std.ax.tick.label2))
        std.ax.tick.label <- c(min(std.ax.tick.label)/4, min(std.ax.tick.label)/2, std.ax.tick.label, max(std.ax.tick.label)*2, max(std.ax.tick.label)*4)
        y.diff.std.ax <-log(std.ax.tick.label) - sum(r.vec*log(X[,as1]/X[,as2]))
                
    axis.vals <- seq(from = min(y.diff.std.ax), to = max(y.diff.std.ax), length = number.points)
    axis.vals <- sort(unique(c(axis.vals, y.diff.std.ax)))
    number.points <- length(axis.vals)
    axis.points <- matrix(0, nrow = number.points, ncol = r)
    for (i in 1:r) axis.points[, i] <- ax.orthog[i, ax.num] + ((axis.vals)/(sum(gi^2)-sum(gi*gj)) - phi) * ax.direction[i]
    axis.points <- cbind(axis.points, exp(axis.vals + sum(r.vec*log(X[,as1]/X[,as2]))), 0)
    for (i in 1:number.points) if (any(zapsmall(axis.points[i, r + 1] - std.ax.tick.label) == 0)) 
        axis.points[i, r + 2] <- 1
    axis.points
}

ties <- function (d, delta, method = c("primary", "secondary")) 
{
    method <- method[1]
    if (!(identical(method, "primary") | identical(method, "secondary"))) stop("method must be either 'primary' or 'secondary' ")
    if (is.null(names(delta))) labels <- 1:length(delta) else labels <- names(delta)
    pairs <- data.frame(d, delta, labels)
    if (identical(method, "primary")) pairs <- pairs[order(d, delta), ]
    if (identical(method, "secondary")) pairs <- pairs[order(d), ]
    z <- as.vector(pairs[, 2])
    names(z) <- pairs[, 3]
    while (!all(order(z) == (1:length(z)))) 
      {  block.vec <- rep(1, length(z))
         block.num <- 1
         for (i in 2:length(z)) 
           {  if (z[i] > z[i - 1]) block.num <- block.num + 1
              block.vec[i] <- block.num
           }
         for (i in 1:max(block.vec)) z[block.vec == i] <- mean(z[block.vec == i])
      }
    z
}

triplot.decomp <- function (threeway ,r)
{
  I <-dim ( threeway ) [1]
  J <-dim ( threeway ) [2]
  K <-dim ( threeway ) [3]

  # SUBJECT MODE 
  Unf_subj <- matrix (0,I,J)
  for (i in 1:K)
    Unf_subj <-cbind (Unf_subj , threeway [,,i])
  Unf_subj <-Unf_subj [,-c (1: J)]
  U1 <-svd ( Unf_subj )$u[ ,1:r]

  # VARIABLE MODE #
  Unf_var <- matrix (0,J,K)
  for (i in 1:I)
    Unf_var <-cbind ( Unf_var , threeway [i , ,])
  Unf_var <-Unf_var [,-c(1:K)]
  U2 <-svd ( Unf_var)$u[ ,1:r]

  # TIME MODE #
  Unf_time <- matrix (0,K,I)
  for (i in 1:J)
    Unf_time <-cbind (Unf_time ,t( threeway [,i ,]) )
  Unf_time <-Unf_time [,-c (1: I)]
  U3 <-svd ( Unf_time )$u[ ,1:r]

  tr <- function (X) { return ( sum ( diag (X))) }

  oldcrit <-0
  first <- second <- matrix (NA ,I,K)
  first2 <- second2 <- matrix (NA ,K,J)
  done <-FALSE
  while (! done )
  { # SUBJECT MODE V and Sigma #
    for (i in 1:I)
     {
       first [i ,] <-t(U2 [ ,1])%*% threeway [i ,,]
       second [i ,] <-t(U2 [ ,2])%*% threeway [i ,,]
     }
    V1 <- matrix (c(t(t(U3 [ ,1])%*%t( first )),t(t(U3 [ ,2])%*%t( second ))),ncol =r)
    Sigma <-diag (c(as.double (t(U1 [ ,1])%*%V1 [ ,1]) ,as.double (t(U1 [ ,2])%*%V1 [ ,2])))
    polart <-svd(V1%*% Sigma )
    U1 <- polart $u%*%t( polart $v)

    # VARIABLE MODE V and Sigma #
    for (i in 1:K)
     {
       first2 [i ,] <-t(U1 [ ,1])%*% threeway [,,i]
       second2 [i ,] <-t(U1 [ ,2])%*% threeway [,,i]
     }
    V2 <- matrix (c(t(t(U3 [ ,1])%*%( first2 )),t(t(U3 [ ,2])%*%( second2 ))),ncol =r)
    Sigma <-diag (c(as.double (t(U2 [ ,1])%*%V2 [ ,1]) ,as.double (t(U2 [ ,2])%*%V2 [ ,2])))
    polart <-svd(V2%*% Sigma )
    U2 <- polart $u%*%t( polart $v)

    # OCCASION MODE V and Sigma #
    V3 <- matrix (c(t(t(U2 [ ,1])%*%t( first2 )),t(t(U2 [ ,2])%*%t( second2 ))),ncol =r)
    Sigma <-diag (c(as.double (t(U3 [ ,1])%*%V3 [ ,1]) ,as.double (t(U3 [ ,2])%*%V3 [ ,2])))
    polart <-svd(V3%*% Sigma )
    U3 <- polart $u%*%t( polart $v)

    newcrit <-tr(t( Sigma )%*%( Sigma ))
    print ( newcrit )
    if ( abs ( newcrit - oldcrit ) <1e-3)
    done <-TRUE
    oldcrit <- newcrit
  }
  for (i in 1:r)
    { if ( Sigma [i,i] < 0)
        { U1[,i] = -1*U1[,i]
          Sigma [i,i] = -Sigma [i,i]
        }
    }
  return ( list (U1=U1 ,U2=U2 ,U3=U3 , Sigma = Sigma ))
}

# ----------------------------------------------------------------
# DRAWING FUNCTIONS
# ----------------------------------------------------------------

draw.biplot <- function (Z, G = matrix(1, nrow = nrow(Z), ncol = 1), classes = 1, Z.means = NULL, 
                         z.axes = NULL, z.axes.nominal = NULL, z.axes.ordinal = NULL, z.trajectories = NULL, 
                         z.bags = NULL, z.ellipse = NULL, Z.new = NULL, Z.density = NULL, Z.regions = NULL, 
                         sample.style, mean.style = NULL, ax.style = NULL, ax.nominal.style = NULL, ax.ordinal.style = NULL,
                         bag.style = NULL, ellipse.style = NULL, new.sample.style = NULL, density.style = NULL, density.legend=T, 
                         region.style = NULL, predict.samples = NULL, predict.means = NULL, Title = NULL, exp.factor = 1.2, plot.coords = NULL, ...) 
{
.samples.plot <- function(Z, G, classes, sample.style) {
          x.vals <- Z[, 1]
          y.vals <- Z[, 2]
          invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
          Z <- Z[invals, ]
          G <- G[invals, ,drop=F]
          for (j in 1:length(classes)) 
            { class.num <- classes[j]
              Z.class <- Z[G[, class.num] == 1, , drop = FALSE]
              text.pos <- match(sample.style$label.side[j], c("bottom", "left", "top", "right"))
              if (sample.style$label[j]) text(Z.class[, 1], Z.class[, 2], labels = dimnames(Z.class)[[1]], 
                                              cex = sample.style$label.cex[j], col = sample.style$col[j], pos = text.pos)
              if(nrow(Z.class)>0) for (i in 1:nrow(Z.class)) points(x = Z.class[i, 1], y = Z.class[i, 2], pch = sample.style$pch[j], 
                                                                    col = sample.style$col[j], cex = sample.style$cex[j])                  
            }
}
.connected.samples <- function(Z, G, classes, sample.style) {
          mid.mat <- matrix (NA, nrow=length(classes), ncol=2)
          for (j in 1:length(classes)) 
            {  x.class <- Z[G[,classes[j]]==1,1]
               y.class <- Z[G[,classes[j]]==1,2]
               mid.mat[j,] <- c(mean(x.class),mean(y.class))
            }
          x.vals <- Z[, 1]
          y.vals <- Z[, 2]
          invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
          Z <- Z[invals, ]
          G <- G[invals, ,drop=F]
          for (j in 1:length(classes)) 
             { class.num <- classes[j]
               Z.class <- Z[G[, class.num] == 1, , drop = FALSE]
               text.pos <- match(sample.style$label.side[j], c("bottom", "left", "top", "right"))
               if (sample.style$label[j]) text(Z.class[, 1], Z.class[, 2], labels = dimnames(Z.class)[[1]], 
                                               cex = sample.style$label.cex[j], col = sample.style$col[j], pos = text.pos)
               points (x=mid.mat[j,1], y=mid.mat[j,2], pch=sample.style$pch[j], col=sample.style$col[j], cex=sample.style$cex[j])
               if(nrow(Z.class)>0) for (i in 1:nrow(Z.class)) lines(x = c(mid.mat[j,1],Z.class[i, 1]), y = c(mid.mat[j,2],Z.class[i, 2]), 
                                                                    col = sample.style$col[j])                  
             }
}
.predict.func <- function(p.point, coef, col, lty, lwd) {
          if (is.na(coef[2])) lines(c(p.point[1], coef[1]), rep(p.point[2], 2), col = col, lwd = lwd, lty = lty)
          else if (coef[2] == 0) lines(rep(p.point[1], 2), p.point[2:3], col = col, lwd = lwd, lty = lty)
               else {  intercept.projection <- p.point[2] + p.point[1]/coef[2]
                       project.on.x <- (intercept.projection - coef[1])/(coef[2] + 1/coef[2])
                       project.on.y <- coef[1] + coef[2] * project.on.x
                       lines(c(p.point[1], project.on.x), c(p.point[2], project.on.y), col = col, lwd = lwd, lty = lty)     
                    }
}
.marker.label.cm <- function(x, y, grad, marker.val, expand = 1, col, label.on.off, side, pos, offset, label.col, cex) {
          uin <- par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
          mm <- 1/(uin[1] * 25.4)
          d <- expand * mm
          if (grad == "v") 
            {  lines(rep(x, 2), c(y - d, y + d), col = col)
               if (label.on.off == 1) text(x, y - d, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)        
            }
          if (grad == "h") 
            {  lines(c(x - d, x + d), rep(y, 2), col = col)
               if (label.on.off == 1) if (side == "right") text(x + d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
                                      else text(x - d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)                     
            }
          if (is.numeric(grad)) 
            {  b <- d * sqrt(1/(1 + grad * grad))
               a <- b * grad
               lines(c(x - b, x + b), c(y - a, y + a), col = col)
               if (label.on.off == 1) if (side == "right") text(x + b, y + a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
                                                           else text(x - b, y - a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)                  
            }
}
.marker.func <- function(vec, coef, col, tick.size, side, pos, offset, label.col, cex) {
          x <- as.numeric(vec[1])
          y <- as.numeric(vec[2])
          marker.val <- vec[3]
          label.on.off <- as.numeric(vec[4])
          if (is.na(coef[2])) 
            .marker.label.cm(x, y, grad = "h", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
          else if (coef[2] == 0) 
                 .marker.label.cm(x, y, grad = "v", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
               else 
                 .marker.label.cm(x, y, grad = -1/coef[2], marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
}
.lin.axes.plot <- function(z.axes, ax.style, predict.mat) {
           for (i in 1:length(ax.style$which)) 
             {  ax.num <- ax.style$which[i]
                marker.mat <- z.axes[[i]][z.axes[[i]][, 4] == 1, 1:3]
                marker.mat <- marker.mat[rev(order(marker.mat[, 3])), ]
                x.vals <- marker.mat[, 1]
                y.vals <- marker.mat[, 2]
                lin.coef <- coefficients(lm(y.vals ~ x.vals))
                if (is.na(lin.coef[2])) 
                   abline(v = x.vals, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])
                else 
                   abline(coef = lin.coef, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])
                if (ax.style$label == "Hor") {  par(las = 1)
                                                adjust <- c(0.5, 1, 0.5, 0)       }
                if (ax.style$label == "Orthog") { par(las = 2)
                                                  adjust <- c(1, 1, 0, 0)         }
                if (ax.style$label == "Paral") {  par(las = 0)
                                                  adjust <- c(0.5, 0.5, 0.5, 0.5) }
                h <- nrow(marker.mat)
                if (is.na(lin.coef[2])) 
                  { if (y.vals[1] < y.vals[h]) 
                       mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                    else 
                       mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                  }
                else 
                  { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
                    y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
                    x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
                    x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
                    if (lin.coef[2] == 0) 
                      { if (x.vals[1] < x.vals[h]) 
                          mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                        else 
                          mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                      }
                    if (lin.coef[2] > 0) 
                      {  if (x.vals[1] < x.vals[h]) 
                           if (y1.ster <= usr[4] & y1.ster >= usr[3]) 
                             mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                           else 
                             mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                         else if (y2.ster <= usr[4] & y2.ster >= usr[3]) 
                                mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                              else 
                                mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                      }
                    if (lin.coef[2] < 0) 
                      {  if (x.vals[1] < x.vals[h]) 
                           if (y1.ster <= usr[4] & y1.ster >= usr[3]) 
                             mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                           else 
                             mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                         else if (y2.ster <= usr[4] & y2.ster >= usr[3]) 
                                mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                              else 
                                mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                       }
                  }
                invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
                std.markers <- marker.mat[invals, 3]
                if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
                x.vals <- x.vals[invals]
                y.vals <- y.vals[invals]
                if (ax.style$tick.label[i]) label.on.off <- rep(1, sum(invals)) else rep(0, sum(invals))
                if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
                if(sum(invals)>0) apply(data.frame(x.vals, y.vals, std.markers, label.on.off), 1, .marker.func, 
                                            coef = lin.coef, col = ax.style$tick.col[i], tick.size = ax.style$tick.size[i], 
                                            side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i], 
                                            offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], 
                                            cex = ax.style$tick.label.cex[i])
                if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.style$predict.col[i], lty = ax.style$predict.lty[i], lwd = ax.style$predict.lwd[i])
              }
}
.nominal.axes.plot <- function(z.axes, ax.style, predict.mat) {
          for (i in 1:length(ax.style$which)) 
            {  ax.num <- ax.style$which[i]
               marker.mat <- z.axes[[i]][z.axes[[i]][, 4] == 1, 1:3]
               marker.mat <- marker.mat[order(marker.mat[, 1]), ]
               ends.mat <- c(usr[1],NA)
               k <- nrow(marker.mat)
               for (j in 2:nrow(marker.mat))
                 ends.mat <- rbind (ends.mat, (marker.mat[j,1:2]+marker.mat[j-1,1:2])/2)
               ends.mat <- rbind (ends.mat, c(usr[2],NA))
               x.vals <- marker.mat[, 1]
               y.vals <- marker.mat[, 2]
               lin.coef <- coefficients(lm(y.vals ~ x.vals))
               if (is.na(lin.coef[2])) {  ends.mat[1,1] <- ends.mat[k+1,1] <- 0
                                          ends.mat[1,2] <- usr[3]
                                          ends.mat[k+1,2] <- usr[4]
                                       }
               else {  ends.mat[1,2] <- lin.coef[1] + ends.mat[1,1]*lin.coef[2]
                       ends.mat[k+1,2] <- lin.coef[1] + ends.mat[k+1,1]*lin.coef[2]
                    }
               if (ax.style$label == "Hor") {  par(las = 1)
                                               adjust <- c(0.5, 1, 0.5, 0)       }
               if (ax.style$label == "Orthog") { par(las = 2)
                                                 adjust <- c(1, 1, 0, 0)         }
               if (ax.style$label == "Paral") {  par(las = 0)
                                                 adjust <- c(0.5, 0.5, 0.5, 0.5) }
               if (is.na(lin.coef[2])) 
                 { if (y.vals[1] < y.vals[k]) 
                      mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                   else 
                      mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                 }
               else 
                  { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
                    y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
                    x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
                    x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
                    if (lin.coef[2] == 0) 
                      { if (x.vals[1] < x.vals[k]) 
                          mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                        else 
                          mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                      }
                    if (lin.coef[2] > 0) 
                      {  if (x.vals[1] < x.vals[k]) 
                           if (y1.ster <= usr[4] & y1.ster >= usr[3]) 
                             mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                           else 
                             mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                         else if (y2.ster <= usr[4] & y2.ster >= usr[3]) 
                                mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                              else 
                                mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                      }
                    if (lin.coef[2] < 0) 
                      {  if (x.vals[1] < x.vals[k]) 
                           if (y1.ster <= usr[4] & y1.ster >= usr[3]) 
                             mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                           else 
                             mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                         else if (y2.ster <= usr[4] & y2.ster >= usr[3]) 
                                mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                              else 
                                mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                       }
                  }
               for (j in 1:k)
                 {  lines (x=ends.mat[(0:1)+j,1], y=ends.mat[(0:1)+j,2], col=ax.style$col[[i]][j], lwd = 2*ax.style$lwd[i], lty = ax.style$lty[i])
                    .marker.func(c(marker.mat[j,],as.numeric(ax.style$tick.label[i])), coef = lin.coef, col = ax.style$tick.col[i], 
                                    tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i], 
                                    offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], 
                                    cex = ax.style$tick.label.cex[i])
                            }
               if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.style$predict.col[i], lty = ax.style$predict.lty[i], lwd = ax.style$predict.lwd[i])
          }
}
.ordinal.axes.plot <- function(z.axes, ax.style, predict.mat) {
          for (i in 1:length(ax.style$which)) 
            {  ax.num <- ax.style$which[i]
               marker.mat <- z.axes[[i]][z.axes[[i]][, 4] == 1, 1:3]
               marker.mat <- marker.mat[order(marker.mat[, 1]), ]
               ends.mat <- c(usr[1],NA)
               k <- nrow(marker.mat)
               for (j in 2:nrow(marker.mat))
                 ends.mat <- rbind (ends.mat, (marker.mat[j,1:2]+marker.mat[j-1,1:2])/2)
               ends.mat <- rbind (ends.mat, c(usr[2],NA))
               x.vals <- marker.mat[, 1]
               y.vals <- marker.mat[, 2]
               lin.coef <- coefficients(lm(y.vals ~ x.vals))
               if (is.na(lin.coef[2])) {  ends.mat[1,1] <- ends.mat[k+1,1] <- 0
                                          ends.mat[1,2] <- usr[3]
                                          ends.mat[k+1,2] <- usr[4]
                                       }
               else {  ends.mat[1,2] <- lin.coef[1] + ends.mat[1,1]*lin.coef[2]
                       ends.mat[k+1,2] <- lin.coef[1] + ends.mat[k+1,1]*lin.coef[2]
                    }
               if (ax.style$label == "Hor") {  par(las = 1)
                                               adjust <- c(0.5, 1, 0.5, 0)       }
               if (ax.style$label == "Orthog") { par(las = 2)
                                                 adjust <- c(1, 1, 0, 0)         }
               if (ax.style$label == "Paral") {  par(las = 0)
                                                 adjust <- c(0.5, 0.5, 0.5, 0.5) }
               if (is.na(lin.coef[2])) 
                 { if (y.vals[1] < y.vals[k]) 
                      mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                   else 
                      mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                 }
               else 
                  { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
                    y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
                    x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
                    x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
                    if (lin.coef[2] == 0) 
                      { if (x.vals[1] < x.vals[k]) 
                          mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                        else 
                          mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                      }
                    if (lin.coef[2] > 0) 
                      {  if (x.vals[1] < x.vals[k]) 
                           if (y1.ster <= usr[4] & y1.ster >= usr[3]) 
                             mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                           else 
                             mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                         else if (y2.ster <= usr[4] & y2.ster >= usr[3]) 
                                mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                              else 
                                mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                      }
                    if (lin.coef[2] < 0) 
                      {  if (x.vals[1] < x.vals[k]) 
                           if (y1.ster <= usr[4] & y1.ster >= usr[3]) 
                             mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                           else 
                             mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                         else if (y2.ster <= usr[4] & y2.ster >= usr[3]) 
                                mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                              else 
                                mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                       }
                  }
               if (ax.style$reverse[i]) lwd.vec <- k:1 else lwd.vec <- 1:k
               lwd.vec <- lwd.vec*ax.style$lwd.factor[i] - (ax.style$lwd.factor[i]-1)
               for (j in 1:k)
                 {  
                    lines (x=ends.mat[(0:1)+j,1], y=ends.mat[(0:1)+j,2], col=ax.style$col[i], lwd = lwd.vec[j], lty = ax.style$lty[i])
                    .marker.func(c(marker.mat[j,],as.numeric(ax.style$tick.label[i])), coef = lin.coef, col = ax.style$tick.col[i], 
                                    tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i], 
                                    offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], 
                                    cex = ax.style$tick.label.cex[i])
                            }
               if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.style$predict.col[i], lty = ax.style$predict.lty[i], lwd = ax.style$predict.lwd[i])
          }
}
.nonlin.axes.plot <- function(z.axes, ax.style, predict.mat) {
     .predict.func <- function(p.point, coef, col, lty, lwd) {
          if (is.na(coef[2])) lines(c(p.point[1], coef[1]), rep(p.point[2], 2), col = col, lwd = lwd, lty = lty)
          else if (coef[2] == 0) lines(rep(p.point[1], 2), p.point[2:3], col = col, lwd = lwd, lty = lty)
               else { intercept.projection <- p.point[2] + p.point[1]/coef[2]
                      project.on.x <- (intercept.projection - coef[1])/(coef[2] + 1/coef[2])
                      project.on.y <- coef[1] + coef[2] * project.on.x
                      lines(c(p.point[1], project.on.x), c(p.point[2], project.on.y), col = col, lwd = lwd, lty = lty)     }
     }
                       for (i in 1:length(ax.style$which)) 
                         {  ax.num <- ax.style$which[i]
                            axis.mat <- z.axes[[i]]
                            axis.mat <- axis.mat[rev(order(axis.mat[, 3])), ]
                            x.vals <- axis.mat[, 1]
                            y.vals <- axis.mat[, 2]
                            xy.before <- rbind(axis.mat[-1,1:2],axis.mat[nrow(axis.mat),1:2])
                            xy.after <- rbind(axis.mat[1,1:2],axis.mat[-nrow(axis.mat),1:2])
                            coef.mat <- matrix(NA,nrow=nrow(axis.mat),ncol=2)
                            for (j in 1:nrow(axis.mat)) coef.mat[j,] <- coefficients(lm(c(xy.after[j,2],xy.before[j,2]) ~ c(xy.after[j,1],xy.before[j,1])))
                            invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
                            axis.mat <- axis.mat[invals,,drop=F]
                            coef.mat <- coef.mat[invals,,drop=F]
                            lines(axis.mat[,1], axis.mat[,2], col=ax.style$col[i], lwd=ax.style$lwd[i], lty=ax.style$lty[i])
                            if (ax.style$label.side[i]=="below") label.pos <- 1
                            if (ax.style$label.side[i]=="left") label.pos <- 2
                            if (ax.style$label.side[i]=="above") label.pos <- 3
                            if (ax.style$label.side[i]=="right") label.pos <- 4                     
                            if (nrow(axis.mat)>0) text(x=axis.mat[1,1], y=axis.mat[1,2], label = ax.style$names[i], offset = ax.style$label.dist[i], pos = label.pos, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                            marker.mat <- axis.mat[axis.mat[,4]==1,1:3, drop=F]
                            if (nrow(marker.mat)>0) { marker.mat[,3] <- zapsmall(marker.mat[,3])
                                                      coef.mat <- coef.mat[axis.mat[,4]==1,,drop=F]
                                                      if (ax.style$tick.label[i]) label.on.off <- rep(1, nrow(marker.mat)) else rep(0, nrow(marker.mat))
                                                      if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
                                                      for (j in 1:nrow(marker.mat))                           
                                                        .marker.func (c(marker.mat[j,],label.on.off[j]), coef=coef.mat[j,], col = ax.style$tick.col[i], tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], 
                                                                      pos = ax.style$tick.label.pos[i], offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], cex = ax.style$tick.label.cex[i])
                                                    }             
                        }                 
}

.bags.plot <- function(z.bags, bag.style) {
          for (i in 1:length(z.bags)) 
            polygon (z.bags[[i]], border=bag.style$col[i], lty=bag.style$lty[i], lwd=bag.style$lwd[i])
}

.ellipse.plot <- function(z.ellipse, ellipse.style) {
          for (i in 1:length(z.ellipse)) 
            polygon(z.ellipse[[i]], border=ellipse.style$col[i], lty=ellipse.style$lty[i], lwd = ellipse.style$lwd[i])  
}

.new.samples.plot <- function(Z.new, new.sample.style) {
          points(Z.new[, 1], Z.new[, 2], pch = new.sample.style$pch, col = new.sample.style$col, cex = new.sample.style$cex)
          pos.vec <- rep(0, nrow(Z.new))
          pos.vec <- match(new.sample.style$label.side, c("bottom", "left", "top", "right"))
          if (any(new.sample.style$label)) 
            text(Z.new[new.sample.style$label, 1], Z.new[new.sample.style$label, 2], 
                   labels = dimnames(Z.new)[[1]][new.sample.style$label], 
                   cex = new.sample.style$label.cex[new.sample.style$label], pos = pos.vec[new.sample.style$label])
}

.class.means.plot <- function(Z.means, mean.style) {
          points(Z.means[, 1], Z.means[, 2], pch = mean.style$pch, col = mean.style$col, cex = mean.style$cex)
          pos.vec <- rep(0, nrow(Z.means))
          pos.vec <- match(mean.style$label.side, c("bottom", "left", "top", "right"))
          if (any(mean.style$label)) 
            text(Z.means[mean.style$label, 1], Z.means[mean.style$label, 2], 
                   labels = dimnames(Z.means)[[1]][mean.style$label], cex = mean.style$label.cex[mean.style$label], 
                   pos = pos.vec[mean.style$label])
}

.density.plot <- function(Z.density, density.style) {
          levels.rect <- pretty(range(Z.density$z), n = density.style$cuts)
          col.use <- colorRampPalette(density.style$col)
          col.use <- col.use(length(levels.rect) - 1)
          image(Z.density, breaks = levels.rect, col = col.use, add = TRUE)
          if (density.style$contours) 
            contour(Z.density, levels = levels.rect, col = density.style$contour.col, add = TRUE)
          list(levels.rect, col.use)
}

.density.legend <- function(levels.rect, col.use) {
          par(pty = "m", mar = density.style$legend.mar)
          plot(range(levels.rect), y = 1:2, ylim = c(10, 100), xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", frame.plot = FALSE)
          rect(xleft = levels.rect[-length(levels.rect)], ybottom = 10, xright = levels.rect[-1], ytop = 50, col = col.use, border = FALSE)
          axis(side = 1, at = pretty(levels.rect, n = 8), labels = pretty(levels.rect, n = 8), line = 0, cex.axis = density.style$cex, mgp = density.style$mgp, tcl = density.style$tcl, las = 0)
}

.regions.plot  <- function(Z.regions, region.style) {
          for (j in 1:length(Z.regions))
            polygon (Z.regions[[j]], col=region.style$col[j], border=NA)
}

   par(pty = "s", ...)
   if (!is.null(Z.density)) layout(mat = matrix(1:2, ncol = 1), heights = density.style$layout.heights)
   if (is.null(plot.coords))
     plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 1] * exp.factor), ylim = range(Z[, 2] * exp.factor), xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
   else
     plot(plot.coords$x, plot.coords$y, xlim = range(plot.coords$x), ylim = range(plot.coords$y), xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)  
   usr <- par("usr")

   if (!is.null(predict.samples)) predict.mat <- Z[predict.samples, , drop = F] else predict.mat <- NULL
   if (!is.null(predict.means)) predict.mat <- rbind(predict.mat, Z.means[predict.means, , drop = F])
   if (!is.null(Z.density)) density.out <- .density.plot(Z.density, density.style) else density.out <- NULL
   if (!is.null(Z.regions)) .regions.plot (Z.regions, region.style)
   if (!is.null(z.axes)) .lin.axes.plot(z.axes, ax.style, predict.mat)
   if (!is.null(z.axes.nominal)) .nominal.axes.plot(z.axes.nominal, ax.nominal.style, predict.mat)
   if (!is.null(z.axes.ordinal)) .ordinal.axes.plot(z.axes.ordinal, ax.ordinal.style, predict.mat)
   if (!is.null(z.trajectories)) .nonlin.axes.plot(z.trajectories, ax.style, predict.mat)
   if (length(classes) > 0) if(sample.style$connected) .connected.samples(Z, G, classes, sample.style) else .samples.plot(Z, G, classes, sample.style)
   if (length(mean.style$which) > 0) .class.means.plot(Z.means, mean.style)
   if (!is.null(Z.new)) .new.samples.plot(Z.new, new.sample.style)
   if (length(z.bags) > 0) .bags.plot(z.bags, bag.style)
   if (length(z.ellipse) > 0) .ellipse.plot(z.ellipse, ellipse.style)
   if (!is.null(Title)) title(main=Title)
   if (!is.null(density.out) & density.legend) .density.legend(density.out[[1]], density.out[[2]])
}


draw.biplot.1D <- function (Z, G = matrix(1, nrow = nrow(Z), ncol = 1), classes = "all", Z.means = NULL, 
                            z.axes = NULL, z.axes.nominal = NULL, z.axes.ordinal = NULL, 
                            z.bags = NULL, z.ellipse = NULL, Z.new = NULL, Z.density = NULL, sample.style, mean.style = NULL, 
                            ax.style = NULL, ax.nominal.style = NULL, ax.ordinal.style = NULL, 
                            bag.style = NULL, ellipse.style = NULL, new.sample.style = NULL, 
                            density.style = NULL, predict.samples = NULL, predict.means = NULL, Title = NULL, 
                            exp.factor = 1.2, ...) 
{
.samples.plot <- function(Z, G, classes, sample.style) {
          x.vals <- Z
          invals <- x.vals < usr[2] & x.vals > usr[1]
          Z <- Z[invals]
          for (j in 1:length(classes)) 
            {  class.num <- classes[j]
               Z.class <- Z[G[, class.num] == 1]
               text.pos <- match(sample.style$label.side[j], c("bottom", "left", "top", "right"))
               if (sample.style$label[j]) 
                  text(Z.class, 0, labels = names(Z.class)[[1]], cex = sample.style$label.cex[j], pos = text.pos)
               for (i in 1:length(Z.class)) 
                  points(x = Z.class[i], y = rep(0, length(Z.class[i])), pch = sample.style$pch[j], col = sample.style$col[j], cex = sample.style$cex[j])
            }
}

.predict.func <- function(p.point, coef, col, lty, lwd) {
          if (is.na(coef[2])) lines(c(p.point[1], coef[1]), rep(p.point[2], 2), col = col, lwd = lwd, lty = lty) 
          else if (coef[2] == 0) lines(rep(p.point[1], 2), p.point[2:3], col = col, lwd = lwd, lty = lty)
               else {  intercept.projection <- p.point[2] + p.point[1]/coef[2]
                       project.on.x <- (intercept.projection - coef[1])/(coef[2] + 1/coef[2])
                       project.on.y <- coef[1] + coef[2] * project.on.x
                       lines(c(p.point[1], project.on.x), c(p.point[2], project.on.y), col = col, lwd = lwd, lty = lty)
                    }
}

.marker.func <- function(vec, y, col, tick.size, side, pos, offset, label.col, cex) {
          x <- vec[1]
          marker.val <- vec[2]
          label.on.off <- vec[3]
          uin <- par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
          mm <- 1/(uin[1] * 25.4)
          d <- tick.size * mm
          lines(rep(x, 2), c(y - d, y + d), col = col)
          if (label.on.off == 1) 
            text(x, y - d, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
}

.lin.axes.plot <- function(z.axes, ax.style, predict.mat) {
          for (i in 1:length(ax.style$which)) 
             {  ax.num <- ax.style$which[i]
                marker.mat <- z.axes[[ax.num]][z.axes[[ax.num]][, 3] == 1, 1:2]
                marker.mat <- marker.mat[rev(order(marker.mat[, 2])), ]
                x.vals <- marker.mat[, 1]
                y.val <- -i
                abline(h = y.val, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])
                if (ax.style$label == "Hor") par(las = 1)
                if (ax.style$label == "Orthog") par(las = 2)
                if (ax.style$label == "Paral") par(las = 0)
                h <- nrow(marker.mat)
                if (x.vals[1] < x.vals[h]) 
                  mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = 1, at = y.val, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                else 
                  mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = 0, at = y.val, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                invals <- x.vals < usr[2] & x.vals > usr[1]
                std.markers <- zapsmall(as.numeric(marker.mat[invals, 2]))
                x.vals <- x.vals[invals]
                if (ax.style$tick.label[i]) label.on.off <- rep(1, sum(invals)) else rep(0, sum(invals))
                if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
                  apply(cbind(x.vals, std.markers, label.on.off), 1, .marker.func, y.val, col = ax.style$tick.col[i], 
                          tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i], 
                          offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], cex = ax.style$tick.label.cex[i])
             }
}

.nominal.axes.plot <- function(z.axes, ax.style, predict.mat, axes.above=0) {
          for (i in 1:length(ax.style$which)) 
             {  ax.num <- ax.style$which[i]
                marker.vec <- z.axes[[ax.num]][z.axes[[ax.num]][, 3] == 1, 1]
                marker.vals <- z.axes[[ax.num]][z.axes[[ax.num]][, 3] == 1, 2]
                marker.vec <- marker.vec[order(marker.vec)]
                x.vals <- marker.vec
                y.val <- -i-axes.above
                h <- length(marker.vec)

                ends.vec <- usr[1]
                for (j in 2:h)
                  ends.vec <- c(ends.vec, (marker.vec[j-1]+marker.vec[j])/2)
                ends.vec <- c(ends.vec, usr[2])

                for (j in 1:h)
                  {
                     lines (x=ends.vec[c(0,1)+j], y=rep(y.val,2), col = ax.style$col[[i]][j], lwd = 2*ax.style$lwd[i], lty = ax.style$lty[i])
                     .marker.func (c(marker.vec[j],marker.vals[j],as.numeric(ax.style$tick.label[i])),  y.val, col = ax.style$tick.col[i], 
                          tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i], 
                          offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], cex = ax.style$tick.label.cex[i])
                  }

                if (ax.style$label == "Hor") par(las = 1)
                if (ax.style$label == "Orthog") par(las = 2)
                if (ax.style$label == "Paral") par(las = 0)
                if (x.vals[1] < x.vals[h]) 
                  mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = 1, at = y.val, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                else 
                  mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = 0, at = y.val, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
             }
}

.ordinal.axes.plot <- function(z.axes, ax.style, predict.mat, axes.above=0) {
          for (i in 1:length(ax.style$which)) 
             {  ax.num <- ax.style$which[i]
                marker.vec <- z.axes[[ax.num]][z.axes[[ax.num]][, 3] == 1, 1]
                marker.vals <- z.axes[[ax.num]][z.axes[[ax.num]][, 3] == 1, 2]
                marker.vec <- marker.vec[order(marker.vec)]
                x.vals <- marker.vec
                y.val <- -i-axes.above
                h <- length(marker.vec)

                ends.vec <- usr[1]
                for (j in 2:h)
                  ends.vec <- c(ends.vec, (marker.vec[j-1]+marker.vec[j])/2)
                ends.vec <- c(ends.vec, usr[2])

                if (ax.style$reverse[i]) lwd.vec <- h:1 else lwd.vec <- 1:h
                lwd.vec <- lwd.vec*ax.style$lwd.factor[i] - (ax.style$lwd.factor[i]-1)
                for (j in 1:h)
                  {
                     lines (x=ends.vec[c(0,1)+j], y=rep(y.val,2), col = ax.style$col[i], lwd = lwd.vec[j], lty = ax.style$lty[i])
                     .marker.func (c(marker.vec[j],marker.vals[j],as.numeric(ax.style$tick.label[i])),  y.val, col = ax.style$tick.col[i], 
                          tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i], 
                          offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], cex = ax.style$tick.label.cex[i])
                  }

                if (ax.style$label == "Hor") par(las = 1)
                if (ax.style$label == "Orthog") par(las = 2)
                if (ax.style$label == "Paral") par(las = 0)
                if (x.vals[1] < x.vals[h]) 
                  mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = 1, at = y.val, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                else 
                  mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = 0, at = y.val, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
             }
}

.bags.plot <- function(z.bags, bag.style) {
          for (i in 1:length(z.bags)) 
            text(z.bags[[i]][1:2], rep(0, 2), c("(", ")"), col = bag.style$col[i], cex = bag.style$lwd[i] * 1.2)
}

.ellipse.plot <- function(z.ellipse, ellipse.style) {
          for (i in 1:length(z.ellipse)) 
            text(z.ellipse[[i]], rep(0, 2), c("(", ")"), col = ellipse.style$col[i], cex = ellipse.style$lwd[i] * 1.2)    
}

.new.samples.plot <- function(Z.new, new.sample.style) {
          points(Z.new, rep(0, length(Z.new)), pch = new.sample.style$pch, col = new.sample.style$col, cex = new.sample.style$cex)
          pos.vec <- rep(0, length(Z.new))
          pos.vec <- match(new.sample.style$label.side, c("bottom", "left", "top", "right"))
          if (any(new.sample.style$label)) 
             text(Z.new[new.sample.style$label], rep(0, length(Z.new[new.sample.style$label])), 
                    labels = dimnames(Z.new)[[1]][new.sample.style$label], 
                    cex = new.sample.style$label.cex[new.sample.style$label], pos = pos.vec[new.sample.style$label])
}

.density.plot <- function(Z.density, density.style) {
          for (i in 1:length(Z.density)) 
            lines(Z.density[[i]]$x, Z.density[[i]]$y, col = density.style$col[i], lwd = density.style$lwd[i], lty = density.style$lty[i])   
}

   par(pty = "s", ...)
   max.y <- 1
   if (length(Z.density) > 0) max.y <- max(unlist(lapply(Z.density, function(dens) { max(dens$y) })))
   plot(range(Z) * exp.factor, c(max.y, -1*(length(z.axes)+length(z.axes.nominal)+length(z.axes.ordinal)) - 1), 
           xlim = range(Z * exp.factor), xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n")
   usr <- par("usr")
   if (!is.null(z.axes)) .lin.axes.plot(z.axes, ax.style, predict.mat)
   if (!is.null(z.axes.nominal)) .nominal.axes.plot(z.axes.nominal, ax.nominal.style, predict.mat, axes.above=length(z.axes))
   if (!is.null(z.axes.ordinal)) .ordinal.axes.plot(z.axes.ordinal, ax.ordinal.style, predict.mat, 
                                                    axes.above=length(z.axes)+length(z.axes.nominal))
   if (!is.null(predict.samples)) sapply(Z[predict.samples], function(x) lines(rep(x, 2), c(0, usr[3]), col = ax.style$predict.col[1], lty = ax.style$predict.lty[1], lwd = ax.style$predict.lwd[1]))
   if (!is.null(predict.means)) sapply(Z.means[predict.means], function(x) lines(rep(x, 2), c(0, usr[3]), col = ax.style$predict.col[1], lty = ax.style$predict.lty[1], lwd = ax.style$predict.lwd[1]))
   if (length(classes) > 0) .samples.plot(Z, G, classes, sample.style)
   if (!is.null(Z.means)) points(Z.means[mean.style$which], rep(0, length(Z.means[mean.style$which])), pch = mean.style$pch, col = mean.style$col, cex = mean.style$cex)
   if (!is.null(Z.new)) .new.samples.plot(Z.new, new.sample.style)
   if (length(z.bags) > 0) .bags.plot(z.bags, bag.style)
   if (length(z.ellipse) > 0) .ellipse.plot(z.ellipse, ellipse.style)
   if (length(Z.density) > 0) .density.plot(Z.density, density.style)
   if (!is.null(Title)) title(main=Title)
}

draw.biplot.3D <- function (Z, G = matrix(1, nrow = nrow(Z), ncol = 1), classes = "all", Z.means = NULL, 
                            z.axes = NULL, z.axes.nominal = NULL, z.axes.ordinal = NULL,
                            z.bags = NULL, z.ellipse = NULL, Z.new = NULL, sample.style, mean.style = NULL, 
                            ax.style = NULL, ax.nominal.style = NULL, ax.ordinal.style = NULL, 
                            bag.style = NULL, ellipse.style = NULL, new.sample.style = NULL, predict.samples = NULL, predict.means = NULL, Title = NULL, exp.factor = 1.2, ...) 
{
.samples.plot <- function(Z, G, classes, sample.style) {
          for (j in 1:length(classes)) 
            {  class.num <- classes[j]
               Z.class <- Z[G[, class.num] == 1, , drop = FALSE]
               text.adj <- c(0.5, 0.5)
               if (sample.style$label.side[j] == "bottom") text.adj[2] <- 1
               if (sample.style$label.side[j] == "top") text.adj[2] <- 0
               if (sample.style$label.side[j] == "left") text.adj[1] <- 0
               if (sample.style$label.side[j] == "right") text.adj[1] <- 1
               if (sample.style$label[j]) text3d(Z.class[, 1], Z.class[, 2], Z.class[, 3], text = dimnames(Z.class)[[1]], cex = sample.style$label.cex[j], adj = text.adj)
               for (i in 1:nrow(Z.class)) 
                 spheres3d(x = Z.class[i, 1], y = Z.class[i, 2], z = Z.class[i, 3], col = sample.style$col[j], 
                           radius = sample.style$cex[j] * rgl.scale, alpha = sample.style$alpha[j])
            }
}

.predict.func <- function(p.point, b, col, lwd) {
           proj <- (sum(p.point*b)/sum(b^2))*b
           ab <- .abline3d (rbind (p.point,proj),stretch.amount=0, col=col, lwd=lwd)
}

.abline3d <- function (mat, eps=1e-8, stretch.amount=0.05, ...)  {         
          # mat:2x3 contains two coordinates on the line
          # z.vec = a.vec + r*b.vec
          #   a.vec is point on the line
          #   b.vec is direction vector
          # symmetric form
          #   (z1-a1)/b1 = (z2-a2)/b2 = (z3-a3)/b3

          stretch.order <- order(diff(apply(mat,2,range)))
          mat <- mat[,stretch.order]
          a.vec <- mat[1,]
          b.vec <- mat[2,]-mat[1,]

          z1 <- zz1 <- z2 <- zz2 <- z3 <- zz3 <- 0
          if (all(abs(b.vec)>eps))
          {  min3 <- min(mat[,3])
             max3 <- max(mat[,3])
             z3 <- min3-(max3-min3)*stretch.amount
             zz3 <- max3+(max3-min3)*stretch.amount
             z2 <- (b.vec[2]/b.vec[3])*(z3-a.vec[3])+a.vec[2]
             zz2 <- (b.vec[2]/b.vec[3])*(zz3-a.vec[3])+a.vec[2]
             z1 <- (b.vec[1]/b.vec[3])*(z3-a.vec[3])+a.vec[1]
             zz1 <- (b.vec[1]/b.vec[3])*(zz3-a.vec[3])+a.vec[1]
          }
          if (abs(b.vec[1])<eps & abs(b.vec[2])>eps)
          {  min3 <- min(mat[,3])
             max3 <- max(mat[,3])
             z3 <- min3-(max3-min3)*stretch.amount
             zz3 <- max3+(max3-min3)*stretch.amount
             z2 <- (b.vec[2]/b.vec[3])*(z3-a.vec[3])+a.vec[2]
             zz2 <- (b.vec[2]/b.vec[3])*(zz3-a.vec[3])+a.vec[2]
             z1 <- zz1 <- mat[1,1]
          }
          if (abs(b.vec[1])<eps & abs(b.vec[2])<eps)
          {  min3 <- min(mat[,3])
             max3 <- max(mat[,3])
             z3 <- min3-(max3-min3)*stretch.amount
             zz3 <- max3+(max3-min3)*stretch.amount
             z2 <- zz2 <- mat[1,2]
             z1 <- zz1 <- mat[1,1]
          }
          mat <- rbind (c(z1,z2,z3),c(zz1,zz2,zz3))
          mm <- mat[,order(stretch.order)]
          lines3d(x=mm[,1],y=mm[,2],z=mm[,3],...)
          list (a=a.vec[order(stretch.order)], b=b.vec[order(stretch.order)], axis.mat=mm)
}

.lin.axes.plot <- function(z.axes, ax.style, predict.mat) {
          for (i in 1:length(ax.style$which)) 
            {  ax.num <- ax.style$which[i]
               marker.dat <- z.axes[[ax.num]][z.axes[[ax.num]][, 5] == 1, 1:4]
               marker.mat <- matrix (nrow=nrow(marker.dat),ncol=ncol(marker.dat))
               for (j in 1:ncol(marker.mat)) marker.mat[,j] <- as.numeric(marker.dat[,j])
               invals <- marker.mat[,1] < usr[2] & marker.mat[,1] > usr[1] &
                         marker.mat[,2] < usr[2] & marker.mat[,2] > usr[1] &
                         marker.mat[,3] < usr[2] & marker.mat[,3] > usr[1]
               if (sum(invals)<2) {  dd <- as.matrix(dist(rbind (0, marker.mat[,1:3])))[1,-1]
                                     mid.vals <- order(dd)
                                     invals[mid.vals] <- TRUE
                                  }
               marker.mat <- marker.mat[invals,]
               marker.mat <- marker.mat[rev(order(marker.mat[, 4])), ]
               std.markers <- marker.mat[,4]
               if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
               marker.mat <- marker.mat[,1:3]
               h <- nrow(marker.mat)

               ab <- .abline3d(marker.mat[c(1,h),],col = ax.style$col[i], lwd = ax.style$lwd[i]) # no lty for rgl
               away <- abs(ab$axis.mat[1,]-ab$axis.mat[2,])*0.01+ax.style$label.dist[i]
               # find which side is ab$axis.mat is closest to highest marker
               dd <- as.matrix(dist(rbind (marker.mat[1,],ab$axis.mat)))
               if (dd[1,2]<dd[1,3]) 
                 label.coord <- ab$axis.mat[1,] - away*ab$b/sqrt(sum(ab$b^2)) 
               else 
                 label.coord <- ab$axis.mat[2,] + away*ab$b/sqrt(sum(ab$b^2))
               text3d(label.coord, text = ax.style$names[i], cex=0.8)  
               points3d(marker.mat, col=ax.style$tick.col[i])
               if (ax.style$tick.label[i]) 
                 {
                    if (ax.style$tick.label.side[i]=="left") label.coord <- marker.mat - rgl.scale
                    else label.coord <- marker.mat + rgl.scale
                    text3d (label.coord, texts=std.markers, col=ax.style$tick.label.col[i], cex=ax.style$tick.label.cex[i],
                    pos = ax.style$tick.label.pos[i], offset = ax.style$tick.label.offset[i])
                 }             
               if (!is.null(predict.mat)) apply(predict.mat, 1, .predict.func, b=ab$b, 
                                                  col = ax.style$predict.col[i], lwd = ax.style$predict.lwd[i])
             }
}

.nominal.axes.plot <- function(z.axes, ax.style, predict.mat) {
          for (i in 1:length(ax.style$which)) 
            {  ax.num <- ax.style$which[i]
               marker.dat <- z.axes[[ax.num]]
               marker.dat <- marker.dat[order(as.numeric(marker.dat[,1])),]
               marker.mat <- matrix (nrow=nrow(marker.dat),ncol=3)
               for (j in 1:3) marker.mat[,j] <- as.numeric(marker.dat[,j])
               std.markers <- marker.dat[marker.dat[,5]==1,4]
               if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
               marker.mat <- marker.mat[marker.dat[,5]==1,1:3]
               h <- nrow(marker.mat)
               ends.mat <- as.numeric(marker.dat[1,1:3])
               for (j in 2:nrow(marker.mat))
                 ends.mat <- rbind (ends.mat, (marker.mat[j,]+marker.mat[j-1,])/2)
               ends.mat <- rbind (ends.mat, as.numeric(marker.dat[nrow(marker.dat),1:3]))
               ab <- .abline3d(marker.mat[c(1,h),],alpha=0) 
               for (j in 1:h)
                 lines3d (ends.mat[j+c(0,1),], col=ax.style$col[[i]][j], lwd=2*ax.style$lwd[i])
               away <- abs(ends.mat[1,]-ends.mat[h+1,])*0.01+ax.style$label.dist[i]
               label.coord <- ends.mat[h+1,] + away*ab$b/sqrt(sum(ab$b^2)) 
               text3d(label.coord, text = ax.style$names[i], cex=0.8)  
               if (ax.style$tick.label[i]) 
                 {
                    label.coord <- marker.mat
                    if (ax.style$tick.label.pos[i]==0)
                      text3d (label.coord, texts=std.markers, col=ax.style$tick.label.col[i], cex=ax.style$tick.label.cex[i])
                    else
                      text3d (label.coord, texts=std.markers, col=ax.style$tick.label.col[i], cex=ax.style$tick.label.cex[i],
                              pos = ax.style$tick.label.pos[i], offset = ax.style$tick.label.offset[i])
                 }             
               if (!is.null(predict.mat)) apply(predict.mat, 1, .predict.func, b=ab$b, 
                                                  col = ax.style$predict.col[i], lwd = ax.style$predict.lwd[i])
             }
}

.ordinal.axes.plot <- function(z.axes, ax.style, predict.mat) {
          for (i in 1:length(ax.style$which)) 
            {  ax.num <- ax.style$which[i]
               marker.dat <- z.axes[[ax.num]]
               marker.dat <- marker.dat[order(as.numeric(marker.dat[,1])),]
               marker.mat <- matrix (nrow=nrow(marker.dat),ncol=3)
               for (j in 1:3) marker.mat[,j] <- as.numeric(marker.dat[,j])
               std.markers <- marker.dat[marker.dat[,5]==1,4]
               if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
               marker.mat <- marker.mat[marker.dat[,5]==1,1:3]
               h <- nrow(marker.mat)
               ends.mat <- as.numeric(marker.dat[1,1:3])
               for (j in 2:nrow(marker.mat))
                 ends.mat <- rbind (ends.mat, (marker.mat[j,]+marker.mat[j-1,])/2)
               ends.mat <- rbind (ends.mat, as.numeric(marker.dat[nrow(marker.dat),1:3]))
               ab <- .abline3d(marker.mat[c(1,h),],alpha=0) 
               if (ax.style$reverse[i]) lwd.vec <- h:1 else lwd.vec <- 1:h
               lwd.vec <- lwd.vec*ax.style$lwd.factor[i] - (ax.style$lwd.factor[i]-1)
               for (j in 1:h)
                 lines3d (ends.mat[j+c(0,1),], col=ax.style$col[i], lwd=lwd.vec[j])
               away <- abs(ends.mat[1,]-ends.mat[h+1,])*0.01+ax.style$label.dist[i]
               label.coord <- ends.mat[h+1,] + away*ab$b/sqrt(sum(ab$b^2)) 
               text3d(label.coord, text = ax.style$names[i], cex=0.8)  
               if (ax.style$tick.label[i]) 
                 {
                    label.coord <- marker.mat
                    if (ax.style$tick.label.pos[i]==0)
                      text3d (label.coord, texts=std.markers, col=ax.style$tick.label.col[i], cex=ax.style$tick.label.cex[i])
                    else
                      text3d (label.coord, texts=std.markers, col=ax.style$tick.label.col[i], cex=ax.style$tick.label.cex[i],
                              pos = ax.style$tick.label.pos[i], offset = ax.style$tick.label.offset[i])
                 }             
               if (!is.null(predict.mat)) apply(predict.mat, 1, .predict.func, b=ab$b, 
                                                  col = ax.style$predict.col[i], lwd = ax.style$predict.lwd[i])
             }
}

.bags.plot <- function(z.bags, bag.style) {
                for (i in 1:length(z.bags)) 
                  {  mat <- cbind(unlist(z.bags[[i]][1]), unlist(z.bags[[i]][2]))
                     mat <- rbind(mat, mat[1, ])
                     lines(mat, col = bag.style$col[i], lty = bag.style$lty[i], lwd = bag.style$lwd[i])
                  }
}

.ellipse.plot <- function(z.ellipse, ellipse.style) {
                   for (i in 1:length(z.ellipse)) plot3d(z.ellipse[[i]], col = ellipse.style$col[i], alpha = ellipse.style$alpha.transparency[i], add = T)
}

.class.means.plot <- function(Z.means, mean.style) {
          spheres3d(Z.means[, 1], Z.means[, 2], Z.means[, 3], col = mean.style$col, radius = mean.style$cex * rgl.scale, alpha = mean.style$alpha)
          text.adj <- c(0.5, 0.5)
          if (mean.style$label.side[1] == "bottom") text.adj[2] <- 1
          if (mean.style$label.side[1] == "top") text.adj[2] <- 0
          if (mean.style$label.side[1] == "left") text.adj[1] <- 0
          if (mean.style$label.side[1] == "right") text.adj[1] <- 1
          if (any(mean.style$label)) text3d(Z.means[mean.style$label, 1], Z.means[mean.style$label, 2], Z.means[mean.style$label, 3], 
                                               text = dimnames(Z.means)[[1]][mean.style$label], cex = mean.style$label.cex[mean.style$label], 
                                               adj = text.adj)
}

.new.samples.plot <- function(Z.new, new.sample.style) {
          spheres3d(Z.new[, 1], Z.new[, 2], Z.new[, 3], col = new.sample.style$col, radius = new.sample.style$cex * rgl.scale, alpha = new.sample.style$alpha)
          text.adj <- c(0.5, 0.5)
          if (new.sample.style$label.side[1] == "bottom") text.adj[2] <- 1
          if (new.sample.style$label.side[1] == "top") text.adj[2] <- 0
          if (new.sample.style$label.side[1] == "left") text.adj[1] <- 0
          if (new.sample.style$label.side[1] == "right") text.adj[1] <- 1
          if (any(new.sample.style$label)) text3d(Z.new[new.sample.style$label, 1], Z.new[new.sample.style$label, 2], Z.new[new.sample.style$label, 3], 
                                                     text = dimnames(Z.new)[[1]][new.sample.style$label], cex = new.sample.style$label.cex[new.sample.style$label], 
                                                     adj = text.adj)
}

   require(rgl)
   usr <- apply(Z, 2, range) * 1.2 * exp.factor
   usr <- c(min(usr[1, ]), max(usr[2, ]))
   rgl.scale <- (usr[2] - usr[1])/50
   open3d()
   aspect3d("iso")
   bg3d("#FFFFFF", fogtype = "lin")
   view3d(theta = 200, phi = 25, fov = 1)
   points3d(usr, usr, usr, alpha = 0)
#   bbox3d(col = "#FEFEFE", alpha = 0.25, xat = 0, yat = 0, zat = 0)
   if (!is.null(predict.samples)) predict.mat <- Z[predict.samples, , drop = F] else predict.mat <- NULL
   if (!is.null(predict.means)) predict.mat <- rbind(predict.mat, Z.means[predict.means, , drop = F])
   if (!is.null(z.axes)) .lin.axes.plot(z.axes, ax.style, predict.mat)
   if (!is.null(z.axes.nominal)) .nominal.axes.plot(z.axes.nominal, ax.nominal.style, predict.mat)
   if (!is.null(z.axes.ordinal)) .ordinal.axes.plot(z.axes.ordinal, ax.ordinal.style, predict.mat)
   if (length(classes) > 0) .samples.plot(Z, G, classes, sample.style)
   if (length(mean.style$which) > 0) .class.means.plot(Z.means, mean.style)
   if (!is.null(Z.new)) .new.samples.plot(Z.new, new.sample.style)
   if (length(z.ellipse) > 0) .ellipse.plot(z.ellipse, ellipse.style)
}

# ----------------------------------------------------------------
# BIPLOT FUNCTIONS
# ----------------------------------------------------------------

PCAbiplot <- function (X, G = NULL, scaled.mat = FALSE, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(X), 
                       correlation.biplot = FALSE, classes = 1:ncol(G), 
                       newdata = NULL, ax.new = NULL, predict.samples = NULL, predict.means = NULL, 
                       samples = list(...), ax = list(...), class.means = list(...), new.samples = list(...), 
                       alpha.bags = list(...), kappa.ellipse = list(...), 
                       density.style = list(...), density.legend=T, colour.scheme = NULL, 
                       Title = NULL, exp.factor = 1.2, plot.coords=NULL, 
                       reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                       adequacies.print = FALSE, predictivity.print = FALSE, quality.print = FALSE, 
                       legend.type = list(...), legend.format = list(...), return.coords=FALSE, ...) 
{  dim.biplot <- dim.biplot[1]
   if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
   e.vects <- e.vects[1:dim.biplot]
   reflect <- reflect[1]
   X.info <- biplot.check.X(X, scaled.mat)
   X <- X.info$X
   unscaled.X <- X.info$unscaled.X
   means <- X.info$means
   sd <- X.info$sd
   G <- biplot.check.G(G, nrow(X))
   if (!is.null(newdata)) X.new <- scale(newdata, center = means, scale = sd) else X.new <- NULL
   n <- nrow(X)
   p <- ncol(X)
   J <- ncol(G)
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]
   svd.out <- svd(X)
   V.mat <- svd.out$v
   U.mat <- svd.out$u
   Sigma.mat <- diag(svd.out$d)
   Vr <- svd.out$v[, e.vects, drop = F]
   fit.out <- biplot.fit.measures(mat = X, mat.hat = X %*% Vr %*% t(Vr), eigenvals = svd.out$d^2, eigenvecs = svd.out$v, dims = e.vects)
   quality <- fit.out$quality
   adequacy <- fit.out$adequacy
   axis.predictivity <- fit.out$axis.predictivity
   sample.predictivity <- fit.out$item.predictivity
   if (adequacies.print & predictivity.print) stop("adequacies.print and predictivity.print cannot both be set to True")
   if (adequacies.print) dimnames(X)[[2]] <- paste(dimnames(X)[[2]], " (", adequacy, ")", sep = "")
   if (predictivity.print) dimnames(X)[[2]] <- paste(dimnames(X)[[2]], " (", round(axis.predictivity, digits = 2), ")", sep = "")
   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) 
    { if (!is.null(rotate)) 
        { if (is.numeric(rotate)) 
           { radns <- pi * rotate/180
             rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)          
           }
          else 
            { if (rotate == "maxpred") { rotate <- (names(axis.predictivity))[axis.predictivity == max(axis.predictivity)]
                                         rotate <- match(ax$rotate, dimnames(X)[[2]])
                                       }
              else rotate <- match(rotate, dimnames(X)[[2]])
              radns <- -atan2(V.mat[rotate, e.vects[2]], V.mat[rotate, e.vects[1]])
              rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)
            }
         }
     }
   class.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% unscaled.X, ncol = ncol(unscaled.X))
   Z.new <- NULL
   Z.means.mat <- NULL
   if (correlation.biplot) 
     {  lambda.r <- diag(svd(t(X) %*% X)$d[1:dim.biplot])
        Z <- sqrt(n - 1) * X %*% Vr %*% rotate.mat %*% reflect.mat %*% (sqrt(solve(lambda.r)))
        if (!is.null(class.means)) 
          Z.means.mat <- sqrt(n - 1) * scale(class.means.mat, means, sd) %*% Vr %*% rotate.mat %*% reflect.mat %*% (sqrt(solve(lambda.r)))
        if (!is.null(X.new)) 
          Z.new <- sqrt(n - 1) * X.new %*% Vr %*% rotate.mat %*% reflect.mat %*% (sqrt(solve(lambda.r)))                                             
     }
   else { Z <- X %*% Vr %*% rotate.mat %*% reflect.mat
          if (!is.null(class.means)) 
          Z.means.mat <- scale(class.means.mat, means, sd) %*% Vr %*% rotate.mat %*% reflect.mat
          if (!is.null(X.new)) Z.new <- X.new %*% Vr %*% rotate.mat %*% reflect.mat
        }
   dimnames(Z) <- list(dimnames(X)[[1]], NULL)
   if (!is.null(X.new)) 
     if (is.null(dimnames(newdata)[[1]])) 
        dimnames(Z.new) <- list(paste("N", 1:nrow(Z.new), sep = ""), NULL) 
     else 
        dimnames(Z.new) <- list(dimnames(newdata)[[1]], NULL)
   if (is.matrix(ax.new)) { NewVarsMeans <- apply(ax.new, 2, mean)
                            if (scaled.mat) 
                              NewVarsSD <- sqrt(apply(ax.new, 2, var)) else NewVarsSD <- rep(1, ncol(ax.new))    
                          }
   num.vars <- p
   var.names <- dimnames(X)[[2]]
   if (!is.null(ax.new)) 
     {  means <- c(means, NewVarsMeans)
        sd <- c(sd, NewVarsSD)
        unscaled.X <- cbind(unscaled.X, ax.new)
        num.vars <- ncol(unscaled.X)
        if (!is.null(dimnames(ax.new)[[2]])) 
          var.names <- c(var.names, dimnames(ax.new)[[2]]) 
        else 
           var.names <- c(var.names, paste("NV", 1:ncol(ax.new), sep = ""))
        SigmaMinOne <- ifelse(Sigma.mat < 1e-10, 0, 1/Sigma.mat)
        Vr.new <- t(scale(ax.new, center = TRUE, scale = NewVarsSD)) %*% U.mat %*% SigmaMinOne
        Vr.new <- Vr.new[, e.vects]
        Vr.all <- rbind(Vr, Vr.new)
     }
   else Vr.all <- Vr
   ax <- do.call("biplot.ax.control", c(num.vars, list(var.names), ax))

   if (ax$type == "prediction") 
     { if (correlation.biplot) 
         axes.direction <- (sqrt(n - 1)/(diag(Vr.all %*% lambda.r %*% t(Vr.all)))) * Vr.all %*% sqrt(lambda.r) %*% rotate.mat %*% reflect.mat
       else 
         axes.direction <- 1/(diag(Vr.all %*% t(Vr.all))) * Vr.all %*% rotate.mat %*% reflect.mat                                                                    
     }
   else 
     { if (correlation.biplot) 
         axes.direction <- sqrt(lambda.r) %*% Vr.all %*% rotate.mat %*% reflect.mat
       else 
         axes.direction <- Vr %*% rotate.mat %*% reflect.mat    
     }
   if (length(ax$which) == 0) 
     z.axes <- NULL 
   else 
     z.axes <- lapply(1:length(ax$which), calibrate.axis, unscaled.X, means, sd, axes.direction, ax$which, ax$ticks, 
                                                          ax$orthogx, ax$orthogy, ax$oblique)

   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) 
     for (j in 1:length(alpha.bags$which)) 
       {  class.num <- alpha.bags$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=TRUE, approx.limit=alpha.bags$max[j])
          if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))
       }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot, kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) 
     for (j in 1:length(kappa.ellipse$which)) 
       {  class.num <- kappa.ellipse$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
          if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))
          if (dim.biplot == 3) {  require(rgl)
                                  z.ellipse[[j]] <- ellipse3d(x = var(mat), centre = apply(mat, 2, mean), t = kappa.ellipse$kappa[j])                    
                               }
       }
   if (dim.biplot == 1) 
     { density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
       z.density <- vector("list", length(density.style$which))
       if (length(density.style$which) > 0) 
         for (j in 1:length(density.style$which)) 
           {  class.num <- density.style$which[j]
              mat <- Z[G[, class.num] == 1, ]
              z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])           
           }
     }
   if (dim.biplot == 2) 
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
       if (!is.null(density.style$which)) 
          { if (density.style$which == 0) 
              mat <- Z 
            else 
              mat <- Z[G[, density.style$which] == 1, ]
            x.range <- range(Z[, 1])
            y.range <- range(Z[, 2])
            width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
            xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            if (is.null(density.style$h)) z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
            else z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
          }
        else z.density <- NULL
     }
   if (dim.biplot == 3) 
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
       if (!is.null(density.style$which)) warning("No density plots in 3D")                                    
     }

   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)           }

   samples <- do.call("biplot.sample.control", c(J, samples))
   new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(X.new)), new.samples))
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)
   if (dim.biplot == 2) draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                    z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, 
                                    Z.density = z.density, density.legend = density.legend, 
                                    sample.style = samples, mean.style = class.means, ax.style = ax, 
                                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, 
                                    density.style = density.style, predict.samples = predict.samples, predict.means = predict.means, 
                                    Title = Title, exp.factor = exp.factor, plot.coords=plot.coords, ...)
   if (dim.biplot == 1) draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, 
                                       sample.style = samples, mean.style = class.means, ax.style = ax, 
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, 
                                       density.style = density.style, predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 3) draw.biplot.3D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, 
                                       sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples,  
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                                       predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)

   if (!is.null(ax$oblique) & ax$type == "interpolation") 
     points(0, 0, pch = "+", cex = 2)
   if (!is.null(predict.samples)) 
     predict.mat <- scale(Z[predict.samples, , drop = F] %*% t(reflect.mat) %*% t(rotate.mat) %*% t(Vr), center = F, scale = 1/sd) else predict.mat <- NULL
   if (!is.null(predict.mat)) 
      predict.mat <- scale(predict.mat, center = -means, scale = F)
   if (!is.null(predict.means)) 
     predict.means.mat <- scale(Z.means.mat[predict.means, , drop = F] %*% t(reflect.mat) %*% t(rotate.mat) %*% t(Vr), center = F, scale = 1/sd) else predict.means.mat <- NULL
   if (!is.null(predict.means.mat)) 
     predict.mat <- rbind(predict.mat, scale(predict.means.mat, center = -means, scale = F))
   if (!is.null(predict.mat)) 
     dimnames(predict.mat) <- list(c(dimnames(X)[[1]][predict.samples], dimnames(G)[[2]][predict.means]), dimnames(X)[[2]])
   if (any(unlist(legend.type))) 
     {  dev.new()
        sample.list <- list(pch = samples$pch, col = samples$col)
        mean.list = list(pch = rep(NA, J), col = rep(NA, J))
        mean.list$pch[class.means$which] <- class.means$pch
        mean.list$col[class.means$which] <- class.means$col
        bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
        bag.list$lty[alpha.bags$which] <- alpha.bags$lty
        bag.list$col[alpha.bags$which] <- alpha.bags$col
        bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
        if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
          { bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
            bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
            bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd
          }
        biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                      class.names = dimnames(G)[[2]], quality.print = quality.print, quality = quality)
     }
   if (return.coords) coords <- Z else coords <- NULL
   list(predictions = predict.mat, quality = quality, adequacy = adequacy, axis.predictivity = axis.predictivity, 
        sample.predictivity = sample.predictivity, coords=coords)
}

PCAbiplot.zoom <- function (...) 
{  PCAbiplot (density.legend=F, ...)
   flush.console()
   print ("Use the mouse to select rectangle for zoom . . .")
   out <- locator(2)
   x <- out$x
   y <- out$y
   diff <- max(abs(x[2]-x[1]),abs(y[2]-y[1]))
   if (x[2]>x[1]) x <- c(x[1],x[1]+diff) else x <- c(x[1], x[1]-diff)
   if (y[2]>y[1]) y <- c(y[1],y[1]+diff) else y <- c(y[1], y[1]-diff)
   x <- sort(x)
   y <- sort(y)
   rect(xleft=x[1], ybottom=y[1], xright=x[2], ytop=y[2], border="red", lty=2, lwd=3)  
   dev.new()
   PCAbiplot (plot.coords=list(x=x,y=y), ...)
}


CVAbiplot <- function (X, G = NULL, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(X), correlation.biplot = FALSE, 
                       classes = 1:ncol(G), newdata = NULL, ax.new = NULL, 
                       samples = list(...), ax = list(...), new.samples = list(...), class.means = list(...), 
                       class.regions = list(...), alpha.bags = list(...), kappa.ellipse = list(...), density.style = list(...), 
                       predict.means = NULL, predict.samples = NULL,  colour.scheme = NULL, Title = NULL, exp.factor = 1.2, 
                       weightedCVA = c("weighted", "unweightedI", "unweightedCent"), 
                       adequacies.print = FALSE, output = 1:10, predictivity.print = FALSE, quality.print = FALSE, 
                       reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                       legend.type = list(...), legend.format = list(...), return.coords=FALSE, prior.p = NULL, class.dim=dim.biplot[1], ...) 
{  dim.biplot <- dim.biplot[1]
   if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
   e.vects <- e.vects[1:dim.biplot]
   reflect <- reflect[1]
   weightedCVA <- weightedCVA[1]
   X.info <- biplot.check.X(X, scaled.mat = F)
   X <- X.info$X
   unscaled.X <- X.info$unscaled.X
   means <- X.info$means
   G <- biplot.check.G(G, nrow(X))
   Nmat <- t(G) %*% G
   Xbar <- solve(Nmat) %*% t(G) %*% X
   if (!is.null(newdata)) X.new <- scale(newdata, center = means, scale = F) else X.new <- NULL
   n <- nrow(X)
   p <- ncol(X)
   J <- ncol(G)
   K <- min(p, J - 1)
   if (K == 1) { dim.biplot <- 1
                 e.vects <- e.vects[1]    }
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]
   SSP.T <- t(X) %*% X
   SSP.B <- t(Xbar) %*% Nmat %*% Xbar
   SSP.W <- SSP.T - SSP.B
   Wmat <- SSP.W
   svd.Wmat <- svd(Wmat)
   lambdamatI <- diag(svd.Wmat$d)
   Lmat <- svd.Wmat$u %*% solve(sqrt(lambdamatI))
   if (weightedCVA == "weighted") 
     Cmat <- Nmat
   if (weightedCVA == "unweightedI") 
     Cmat <- diag(J)
   if (weightedCVA == "unweightedCent") 
     Cmat <- diag(J) - matrix(1/J, nrow = J, ncol = J)
   if (is.na(match(weightedCVA, c("weighted", "unweightedI", "unweightedCent")))) 
     stop(" Argument 'weightedCVA' must be one of 'weighted','unweightedI','unweightedCent' ")
   svd.step2 <- svd(t(Lmat) %*% t(Xbar) %*% Cmat %*% Xbar %*% Lmat)
   Vmat <- svd.step2$v
   lambdamat <- diag(svd.step2$d)
   svd.2sided <- Eigen.twosided(t(Xbar) %*% Cmat %*% Xbar, Wmat)
   Mmat <- svd.2sided$W
   lambdamat.2sided <- svd.2sided$Lambda.mat
   vec.temp <- rep(0, p)
   vec.temp[e.vects] <- 1
   Jmat <- diag(vec.temp)
   XLVJ <- X %*% Mmat %*% Jmat
   XbarHat <- Xbar %*% Mmat %*% Jmat %*% solve(Mmat)
   XHat <- XLVJ %*% solve(Mmat)
   I.min.H <- (diag(n) - G %*% (solve(Nmat)) %*% t(G))
   fit.Canvar <- biplot.fit.measures(Xbar, XbarHat, weights = Cmat, orthog.metric = Wmat, eigenvals = diag(lambdamat.2sided), eigenvecs = Mmat, dims = e.vects)
   fit.Within <- biplot.fit.measures(I.min.H %*% X, I.min.H %*% XHat, orthog.metric = Wmat, eigenvals = diag(lambdamat.2sided), eigenvecs = Mmat, dims = e.vects)
   quality.Canvar <- fit.Canvar$quality
   quality.Origvar <- sum((diag(solve(t(Mmat) %*% Mmat)) * diag(lambdamat.2sided))[e.vects])/sum(diag(solve(t(Mmat) %*% Mmat)) * diag(lambdamat.2sided))
   adequacy <- fit.Canvar$adequacy
   axis.predictivity <- fit.Canvar$axis.predictivity
   class.predictivity <- fit.Canvar$item.predictivity
   within.class.axis.predictivity <- fit.Within$axis.predictivity
   within.class.sample.predictivity <- fit.Within$item.predictivity
   if (adequacies.print & predictivity.print) stop("adequacies.print and predictivity.print cannot both be set to True")
   if (adequacies.print) dimnames(X)[[2]] <- paste(dimnames(X)[[2]], " (", adequacy, ")", sep = "")
   if (predictivity.print) dimnames(X)[[2]] <- paste(dimnames(X)[[2]], " (", round(axis.predictivity, digits = 2), ")", sep = "")
   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) 
     { if (!is.null(rotate)) 
       { if (is.numeric(rotate)) 
          {  radns <- pi * rotate/180
             rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)    
          }
         else 
          { if (rotate == "maxpred") 
              {  rotate <- (names(axis.predictivity))[axis.predictivity == max(axis.predictivity)]
                 rotate <- match(rotate, dimnames(X)[[2]])                                          
              }
            else rotate <- match(rotate, dimnames(X)[[2]])
            radns <- -atan2(V.mat[rotate, e.vects[2]], V.mat[rotate, e.vects[1]])
            rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)                                            
          }        
       }
     } 
   Mr <- Mmat[, e.vects, drop = F]
   Z.new <- NULL
   Z.means.mat <- NULL
   Z <- X %*% Mr %*% rotate.mat %*% reflect.mat
   Z.means.mat <- Xbar %*% Mr %*% rotate.mat %*% reflect.mat
   if (!is.null(X.new)) Z.new <- X.new %*% Mr %*% rotate.mat %*% reflect.mat
   dimnames(Z) <- list(dimnames(X)[[1]], NULL)
   if (!is.null(X.new)) if (is.null(dimnames(newdata)[[1]])) dimnames(Z.new) <- list(paste("N", 1:nrow(Z.new), sep = ""), NULL)
                        else dimnames(Z.new) <- list(dimnames(newdata)[[1]], NULL)
   if (is.matrix(ax.new)) {  NewVarsMeans <- apply(ax.new, 2, mean)
                             NewVars.cent <- scale(ax.new, center = TRUE, scale = FALSE)
                             NewVars.means <- solve(Nmat) %*% t(G) %*% NewVars.cent           
                          }
   num.vars <- p
   var.names <- dimnames(X)[[2]]
   Mrr <- solve(Mmat)[e.vects, , drop = F]

   if (!is.null(ax.new)) 
     {  means <- c(means, NewVarsMeans)
        unscaled.X <- cbind(unscaled.X, ax.new)
        num.vars <- ncol(unscaled.X)
        if (!is.null(dimnames(ax.new)[[2]])) var.names <- c(var.names, dimnames(ax.new)[[2]]) else var.names <- c(var.names, paste("NV", 1:ncol(ax.new), sep = ""))
        LambdaMinOne <- ifelse(lambdamat < 1e-10, 0, 1/lambdamat)
        Mr.new <- LambdaMinOne %*% t(Mmat) %*% t(Xbar) %*% Cmat %*% NewVars.means
        Mr.new <- Mr.new[, e.vects, drop = F]
        Mrr.all <- rbind(Mrr, Mr.new)                                                                                                                                    
     }
   else Mrr.all <- Mrr
   
   ax <- do.call("biplot.ax.control", c(num.vars, list(var.names), ax))
   if (ax$type == "prediction") 
     axes.direction <- solve(diag(diag(t(Mrr.all) %*% Mrr.all))) %*% t(Mrr.all) %*% rotate.mat %*% reflect.mat
   else 
     axes.direction <- Mr %*% rotate.mat %*% reflect.mat
   if (length(ax$which) == 0) 
     z.axes <- NULL
   else 
     z.axes <- lapply(1:length(ax$which), calibrate.axis, unscaled.X, means, sd = rep(1, length(means)), axes.direction, ax$which, ax$ticks, ax$orthogx, ax$orthogy, ax$oblique)

   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) 
     for (j in 1:length(alpha.bags$which)) 
       {  class.num <- alpha.bags$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=TRUE, approx.limit=alpha.bags$max[j])
          if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))                     
       }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) 
     for (j in 1:length(kappa.ellipse$which)) 
       {  class.num <- kappa.ellipse$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
          if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))
          if (dim.biplot == 3) {  require(rgl)
                                  z.ellipse[[j]] <- ellipse3d(x=var(mat), centre=apply(mat,2,mean), t=kappa.ellipse$kappa[j])                                  
                               }
       }

   if (dim.biplot == 1) 
     {  density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
        z.density <- vector("list", length(density.style$which))
        if (length(density.style$which) > 0) 
          for (j in 1:length(density.style$which)) 
            {  class.num <- density.style$which[j]
               mat <- Z[G[, class.num] == 1, ]
               z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])
            }
      }
   if (dim.biplot == 2) 
     {  density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
        if (!is.null(density.style$which)) 
          { if (density.style$which == 0) 
              mat <- Z  else mat <- Z[G[, density.style$which] == 1, ]
            x.range <- range(Z[, 1])
            y.range <- range(Z[, 2])
            width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
            xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            if (is.null(density.style$h)) 
              z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
            else 
              z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
          }
        else z.density <- NULL
     }
   if (dim.biplot == 3)
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
       if (!is.null(density.style$which)) warning ("No density plots in 3D")
     }
   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)         }

   samples <- do.call("biplot.sample.control", c(J, samples))
   new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(X.new)), new.samples))
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
   if (length(class.means$which) == 0) 
     {  class.means$which <- 1:J
        class.means$col <- samples$col
        class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))    
     }
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)
   
   Z.region <- NULL
   if (dim.biplot == 2 & !is.null(prior.p))
     {
       class.regions <- do.call("biplot.class.region.control", c(J, class.regions))
       prior.prob <- rep(NA, J)
       if (prior.p=="equal") prior.prob <- rep (1/J, J)
       if (prior.p=="sample.size") prior.prob <- diag(Nmat)/sum(diag(Nmat))
       if (any(is.na(prior.prob))) stop ("Incorrect prior probabilities specified for classification")
       plot.range <- range(Z * exp.factor) * 1.1
       if (class.dim > K) { warning ("classification in 2D")
                            class.dim <- 2 
                          }
       Z.region <- biplot.create.regions (region.style = class.regions, plot.range = plot.range, region.mid = Z.means.mat, 
                                          rotate.mat = rotate.mat, reflect.mat = reflect.mat, region.func = biplot.LDA.class.func, 
                                          class.means = Xbar %*% Mmat[,1:class.dim,drop=F], n = n, Mrr=solve(Mmat)[1:2,,drop=F], 
                                          class.dim=class.dim, prior.prob = prior.prob) 
     }
   
   if (dim.biplot == 2) draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, Z.new = Z.new,  
                                    z.bags = z.bags, z.ellipse = z.ellipse, Z.density = z.density, Z.region = Z.region, 
                                    sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples,  
                                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                                    density.style = density.style, region.style = class.regions,
                                    predict.samples = predict.samples, predict.means = predict.means, 
                                    Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 1) draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, 
                                       sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples,  
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
                                       predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 3) draw.biplot.3D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, Z.new = Z.new,  
                                    z.bags = z.bags, z.ellipse = z.ellipse, Z.density = z.density, Z.region = Z.region, 
                                    sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples,  
                                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                                    density.style = density.style, region.style = class.regions,
                                    predict.samples = predict.samples, predict.means = predict.means, 
                                    Title = Title, exp.factor = exp.factor, ...)

   if (!is.null(ax$oblique) & ax$type == "interpolation") 
     points(0, 0, pch = "+", cex = 2)
   if (!is.null(predict.samples)) 
     predict.mat <- scale(Z[predict.samples, , drop = F] %*% t(reflect.mat) %*% t(rotate.mat) %*% Mrr, center = -means, scale = F) else predict.mat <- NULL
   if (!is.null(predict.means)) 
     predict.mat <- rbind(predict.mat, scale(Z.means.mat[predict.means, , drop = F] %*% t(reflect.mat) %*% t(rotate.mat) %*% Mrr, center = -means, scale = F))
   if (!is.null(predict.mat)) 
      dimnames(predict.mat) <- list(c(dimnames(X)[[1]][predict.samples], dimnames(G)[[2]][predict.means]), dimnames(X)[[2]])
   if (any(unlist(legend.type))) 
     {  dev.new()
        sample.list <- list(pch = samples$pch, col = samples$col)
        mean.list = list(pch = rep(NA, J), col = rep(NA, J))
        mean.list$pch[class.means$which] <- class.means$pch
        mean.list$col[class.means$which] <- class.means$col
        bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
        bag.list$lty[alpha.bags$which] <- alpha.bags$lty
        bag.list$col[alpha.bags$which] <- alpha.bags$col
        bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
        if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
          {  bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
             bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
             bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd        
          }
        biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                      class.names = dimnames(G)[[2]], quality.print = quality.print, quality = quality)
     }
   if (return.coords) coords <- Z else coords <- NULL
   list(predictions = predict.mat, quality.Canvar = quality.Canvar, quality.Origvar = quality.Origvar, adequacy = adequacy, 
         axis.predictivity = axis.predictivity, class.predictivity = class.predictivity, within.class.axis.predictivity = within.class.axis.predictivity, 
         within.class.sample.predictivity = within.class.sample.predictivity, coords=coords)
}

Nonlinear.biplot <- function (X, G = NULL, dist = c("Pythagoras", "Clark", "SqrtL1"), scaled.mat = FALSE, 
                              dim.biplot = c(2,1, 3), e.vects = 1:ncol(X), correlation.biplot = FALSE, 
                              classes = 1:ncol(G), predict.samples = NULL, predict.means = NULL, 
                              samples = list(...), ax = list(...), class.means = list(...), 
                              alpha.bags = list(...), kappa.ellipse = list(...), density.style = list(...), 
                              colour.scheme = NULL, Title = NULL, exp.factor = 1.2, 
                              reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                              legend.type = list(...), legend.format = list(...), return.coords=FALSE, ...) 
{  dist <- dist[1]
   dim.biplot <- dim.biplot[1]
   if (dim.biplot != 1 & dim.biplot != 2) stop("Only 1D and 2D biplots")
   e.vects <- e.vects[1:dim.biplot]
   reflect <- reflect[1]
        
   if (dist == "Clark") dist.expr <- function(xik, xjk) { ((xik - xjk)/(xik + xjk))^2 }
   if (dist == "Pythagoras") dist.expr <- function(xik, xjk) { (xik - xjk)^2 }
   if (dist == "SqrtL1") dist.expr <- function(xik, xjk) { abs(xik - xjk) }
   D.mat <- function(X) { n <- nrow(X)
                          D <- matrix(0, nrow = n, ncol = n)
                          for (i in 1:(n - 1)) for (j in (i + 1):n) D[i, j] <- -0.5 * sum(dist.expr(X[i, ], X[j, ]))
                          D + t(D)
                        }
   dvec.fun <- function(vec, mat) { if (length(vec) != ncol(mat)) stop("Incorrect sized vector")
                                    n <- nrow(mat)
                                    outvec <- rep(NA, n)
                                    for (i in 1:n) outvec[i] <- -0.5 * sum(dist.expr(vec, mat[i, ]))
                                    outvec
                                  }
   dnplus1 <- function(X, x.new) { apply(X, 1, function(x, x.new) -0.5 * sum(dist.expr(x,x.new)), x.new = x.new) }
   ddmu.expr <- function(x) { if (dist == "Pythagoras" || dist == "Clark") 
                                {  dist.text <- deparse(dist.expr)[3]
                                   dist.text <- paste("-0.5", dist.text, sep = "*")
                                   dist.text <- gsub("xik", x, dist.text)
                                   dist.text <- gsub("xjk", "mu", dist.text)
                                   dist.expression <- parse(text = dist.text)
                                   AA <- D(dist.expression, "mu")                       
                                 }
                              if (dist == "SqrtL1") AA <- expression(-(sign(x - mu)))
                              return(AA)
                            }
   d2dmu2.expr <- function(x) {  first <- ddmu.expr(x)
                                 D(first, "mu")
                              }

   if (scaled.mat) X.info <- biplot.check.X(X, scaled.mat) else X.info <- biplot.check.X(X, FALSE, FALSE) 
   X <- X.info$X
   unscaled.X <- X.info$unscaled.X
   means <- X.info$means
   sd <- X.info$sd
   G <- biplot.check.G(G, nrow(X))
   n <- nrow(X)
   p <- ncol(X)
   J <- ncol(G)
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]

   s <- rep(1/n, n)
   one <- rep(1, n)
   N <- one %*% t(s)
   I <- diag(n)
   D <- D.mat(X)
   B <- (I - N) %*% D %*% (I - N)
   swd <- svd(B)
   U <- (swd$u %*% diag(swd$d^0.5))[, -n]
   lambda <- t(U) %*% U
   lambda.inv <- diag(ifelse(zapsmall(diag(lambda)) > 0, 1/diag(lambda), 0))
   eigval <- diag(lambda)

   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) 
     { if (!is.null(rotate)) 
        {  radns <- pi * rotate/180
           rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)            
        }
     }
        
   d.m <- D.mat(rbind(unscaled.X, 0))[n + 1, 1:n]
   O.co <- as.vector(lambda.inv[e.vects, e.vects] %*% t(U[, e.vects]) %*% (d.m - D %*% s))
   Z <- U[, e.vects[e.vects]] %*% rotate.mat %*% reflect.mat 

   class.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% unscaled.X, ncol = ncol(unscaled.X))
   Z.means.mat <- NULL
   if (!is.null(class.means)) Z.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% Z, ncol = ncol(Z))
   dimnames(Z) <- list(dimnames(X)[[1]], NULL)
    
   num.vars <- p
   var.names <- dimnames(X)[[2]]
   ax <- do.call("biplot.trajectory.control", c(num.vars, list(var.names), ax))

   if (ax$type == "prediction.circle" | ax$type == "prediction.back" | ax$type == "prediction.normal") 
     {  z.axes <- lapply(1:length(ax$which), function(k, unscaled.X, means, sd, n.int, D, Y, lambda, s, e.vecs) 
                   {  number.points <- ax$num.points
                      std.markers <- pretty(unscaled.X[, ax$which[k]], n = n.int[k])
                      std.markers <- std.markers[std.markers >= min(unscaled.X[, ax$which[k]]) & std.markers <= max(unscaled.X[, ax$which[k]])]
                      interval <- (c(std.markers, min(unscaled.X[, ax$which[k]]), max(unscaled.X[, ax$which[k]])) - means[ax$which[k]])/sd[ax$which[k]]
                      axis.vals <- seq(from = min(interval), to = max(interval), length = number.points)
                      axis.vals <- sort(unique(c(axis.vals, interval)))
                      axis.vals <- zapsmall(axis.vals)
                      axis.vals <- axis.vals[!axis.vals == 0]
                      number.points <- length(axis.vals)
                      n <- nrow(unscaled.X)
                      p <- ncol(unscaled.X)
                      axis.points <- matrix(0, nrow = number.points, ncol = length(e.vects)+2)
                      mu <- axis.vals
                      m <- length(mu)
                      ddmu.dnplus1.mat <- t(sapply(unscaled.X[, ax$which[k]], function(x) eval(ddmu.expr(x))))
                      A.mat <- t(ddmu.dnplus1.mat) %*% Y[, e.vects] %*% solve(lambda[e.vects, e.vects])
                      m.star.vec <- apply(ddmu.dnplus1.mat, 2, sum)/(-n)
                      a1.sq.plus.a2.sq <- apply(A.mat, 1, function(a) sum(a^2))
                      LL.mat <- cbind(A.mat, m.star.vec, A.mat/sqrt(a1.sq.plus.a2.sq), m.star.vec/sqrt(a1.sq.plus.a2.sq))
                      L.a <- function(mu.val, X, k) 
                               { ddmu.dnplus1.vec <- sapply(X[, ax$which[k]], function(x, mu) eval(ddmu.expr(x)), mu = mu.val)
                                 t(ddmu.dnplus1.vec) %*% Y[, e.vects] %*% solve(lambda[e.vects, e.vects])
                               } 
                      L.m <- function(mu.val, X, k) 
                               { ddmu.dnplus1.vec <- sapply(X[, ax$which[k]], function(x, mu) eval(ddmu.expr(x)), mu = mu.val)
                                 m.star <- sum(ddmu.dnplus1.vec)/(-nrow(X))
                                 m.star/sum(L.a(mu.val, X, k)^2)
                               }
                      if (ax$type == "prediction.circle") 
                        { axis.points[, e.vects] <- cbind(LL.mat[, 4] * LL.mat[, 6], LL.mat[, 5] * LL.mat[, 6])        }
                      if (ax$type == "prediction.back") 
                        { mat <- sapply(mu, function(mm, unscaled.X, k) 
                                   {  my.vek <- rep(0, ncol(unscaled.X))
                                      my.vek[k] <- mm
                                      dnplus1(unscaled.X, my.vek)
                                   }, unscaled.X = unscaled.X, k = k)
                           xsi.k <- solve(lambda[e.vects, e.vects]) %*% t(Y[, e.vects]) %*% (mat - apply(D, 1, sum)/n)
                           axis.points[, e.vects] <- t(sapply(1:m, function(j, xsi.k, LL.mat) 
                                                     {  l.vec <- LL.mat[j, 4:5]
                                                        m.mu <- LL.mat[j, 6]
                                                        xsi <- xsi.k[, j]
                                                        (diag(2) - l.vec %*% t(l.vec)) %*% xsi + m.mu * l.vec
                                                     }, xsi.k = xsi.k, LL.mat = LL.mat))
                        }
                     if (ax$type == "prediction.normal") 
                       { epsilon <- 0.001
                         pos <- order(abs(LL.mat[, 6]))[1]
                         if (LL.mat[pos, 6] < epsilon) mu.0 <- axis.vals[pos]
                         else { interval.begin <- pos - 1
                                if (interval.begin < 1) interval.begin <- 1
                                interval.einde <- pos + 1
                                if (interval.einde > number.points) interval.einde <- number.points
                                uit <- uniroot(L.m, axis.vals[c(interval.begin, interval.einde)], X = unscaled.X, k)
                                mu.0 <- uit$root
                              }
                          skep.integrand.1 <- function(mu, X, k) 
                                                { sapply(mu, function(mu, Xmat, k) 
                                                     { ddmu.dnplus1.vec <- sapply(X[, ax$which[k]], function(x,mu) eval(ddmu.expr(x)), mu = mu)
                                                       a.vec <- t(ddmu.dnplus1.vec) %*% Y[, e.vects] %*% solve(lambda[e.vects, e.vects])
                                                       wortel <- sqrt(sum(a.vec^2))
                                                       l1.mu <- a.vec[1]/wortel
                                                       mster.mu <- sum(ddmu.dnplus1.vec)/(-nrow(X))
                                                       d2.dmu2.dnplus1.vec <- sapply(Xmat[, ax$which[k]], function(x, mu) eval(d2dmu2.expr(x)), mu = mu)
                                                       ddmu.mster.mu <- sum(d2.dmu2.dnplus1.vec)/(-nrow(X))
                                                       ddmu.a1 <- t(Y[, 1]) %*% d2.dmu2.dnplus1.vec/lambda[1, 1]
                                                       ddmu.a2 <- t(Y[, 2]) %*% d2.dmu2.dnplus1.vec/lambda[2, 2]
                                                       ddmu.wortel <- (a.vec[1] * ddmu.a1 + a.vec[2] * ddmu.a2)/wortel
                                                       ddmu.mmu.l2mu <- ((ddmu.mster.mu * wortel - ddmu.wortel * mster.mu) * a.vec[2] - (ddmu.a2 * wortel - ddmu.wortel * a.vec[2]) * mster.mu)/(wortel * a.vec[2]^2)
                                                       l1.mu * ddmu.mmu.l2mu
                                                     }, Xmat = X, k = k)
                                                 }
                            f1.int <- sapply(mu, function(mu, unscaled.X, k) my.integrate(skep.integrand.1, mu.0, mu, unscaled.X, k)$value, unscaled.X = unscaled.X, k = k)
                            axis.points[, 1] <- LL.mat[, 5] * f1.int
                            skep.integrand.2 <- function(mu, X, k) 
                                                   { sapply(mu, function(mu, Xmat, k) 
                                                        { ddmu.dnplus1.vec <- sapply(X[, ax$which[k]], function(x, u) eval(ddmu.expr(x)), mu = mu)
                                                          a.vec <- t(ddmu.dnplus1.vec) %*% Y[, e.vects] %*% solve(lambda[e.vects, e.vects])
                                                          wortel <- sqrt(sum(a.vec^2))
                                                          l2.mu <- a.vec[2]/wortel
                                                          mster.mu <- sum(ddmu.dnplus1.vec)/(-nrow(X))
                                                          d2.dmu2.dnplus1.vec <- sapply(Xmat[, ax$which[k]], function(x, mu) eval(d2dmu2.expr(x)), mu = mu)
                                                          dmu.mster.mu <- sum(d2.dmu2.dnplus1.vec)/(-nrow(X))
                                                          ddmu.a1 <- t(Y[, 1]) %*% d2.dmu2.dnplus1.vec/lambda[1, 1]
                                                          ddmu.a2 <- t(Y[, 2]) %*% d2.dmu2.dnplus1.vec/lambda[2, 2]
                                                          ddmu.wortel <- (a.vec[1] * ddmu.a1 + a.vec[2] * ddmu.a2)/wortel
                                                          ddmu.mmu.l1mu <- ((ddmu.mster.mu * wortel - ddmu.wortel * mster.mu) * a.vec[1] - (ddmu.a1 * wortel - ddmu.wortel * a.vec[1]) * mster.mu)/(wortel * a.vec[1]^2)
                                                          l2.mu * ddmu.mmu.l1mu
                                                        }, Xmat = X, k = k)
                                                    }
                            f2.int <- sapply(mu, function(mu, unscaled.X, k) my.integrate(skep.integrand.2, mu.0, mu, unscaled.X, k)$value, unscaled.X = unscaled.X, k = k)
                            axis.points[, 2] <- LL.mat[, 4] * f2.int
                         }
                     axis.points[, 3] <- axis.vals * sd[ax$which[k]] + means[ax$which[k]]
                     for (i in 1:number.points) if (any(zapsmall(axis.points[i, 3] - std.markers) == 0)) axis.points[i, 4] <- 1
                     return(axis.points)
                   }, unscaled.X = unscaled.X, means = means, sd = sd, n.int = ax$ticks, D = D, Y = U, lambda = lambda, s = s, e.vecs = e.vects)
     }
   if (ax$type == "interpolation") 
     { z.axes <- lapply(1:length(ax$which), function(j, unscaled.X, means, sd, n.int, D, Y, lambda.inv, s, dist.func, e.vecs, ax.num) 
                  { number.points <- ax$num.points
                    std.markers <- pretty(unscaled.X[, ax$which[j]], n = n.int[j])
                    std.range <- c(min(std.markers), max(std.markers))
                    std.markers.min <- std.markers - (std.range[2] - std.range[1])
                    std.markers.max <- std.markers + (std.range[2] - std.range[1])
                    interval <- (c(std.markers, std.markers.min, std.markers.max) - means[ax$which[j]])/sd[ax$which[j]]
                    axis.vals <- seq(from = min(interval), to = max(interval), length = number.points)
                    axis.vals <- sort(unique(c(axis.vals, interval)))
                    axis.vals <- zapsmall(axis.vals)
                    number.points <- length(axis.vals)
                    mat <- matrix(0, ncol = ncol(unscaled.X), nrow = length(axis.vals))
                    mat[, j] <- axis.vals
                    mat <- rbind(unscaled.X, mat)
                    D.nuut <- dist.func(mat)
                    dnplus1.mat <- D.nuut[1:n, -(1:n)]
                    as.embed <- apply(dnplus1.mat, 2, function(dn1, D, Y, lambda.inv, s) lambda.inv %*% t(Y) %*% (dn1 - D %*% s), D = D, Y = Y, lambda.inv = lambda.inv, s = s)
                    axis.points <- cbind(t(as.embed)[, e.vecs], 0, 0)
                    axis.points[, 3] <- axis.vals * sd[j] + means[j]
                    for (i in 1:number.points) 
                       if (any(zapsmall(axis.points[i, 3] - std.markers) == 0)) axis.points[i, 4] <- 1
                    axis.points[, e.vects] <- scale(axis.points[, e.vects], center = O.co, scale = FALSE)
                    axis.points[, e.vects] <- ax.num * axis.points[,e.vects]
                    axis.points[, 1] <- axis.points[, 1] + O.co[1]
                    axis.points[, 2] <- axis.points[, 2] + O.co[2]
                    return(axis.points)
                  }, unscaled.X = unscaled.X, means = means, sd = sd, n.int = ax$ticks, D = D, Y = U, lambda.inv = lambda.inv, s = s, dist.func = D.mat, e.vecs = e.vects[1:2], ax.num = length(ax$which))
     }
   for (j in 1:length(z.axes)) z.axes[[j]][,e.vects] <- z.axes[[j]][,e.vects] %*% rotate.mat %*% reflect.mat
          
   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) 
     for (j in 1:length(alpha.bags$which)) 
        { class.num <- alpha.bags$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=TRUE, approx.limit=alpha.bags$max[j])
          if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))
        }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot, kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) 
     for (j in 1:length(kappa.ellipse$which)) 
        { class.num <- kappa.ellipse$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
          if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))
          if (dim.biplot == 3) { require(rgl)
                                 z.ellipse[[j]] <- ellipse3d(x = var(mat), centre = apply(mat, 2, mean), t = kappa.ellipse$kappa[j])            }
                               }

   if (dim.biplot == 1) 
     { density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
       z.density <- vector("list", length(density.style$which))
       if (length(density.style$which) > 0) 
         for (j in 1:length(density.style$which)) 
           { class.num <- density.style$which[j]
             mat <- Z[G[, class.num] == 1, ]
             z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])           
           }
     }
   if (dim.biplot == 2) 
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
       if (!is.null(density.style$which)) 
        { if (density.style$which == 0) 
            mat <- Z else mat <- Z[G[, density.style$which] == 1, ]
          x.range <- range(Z[, 1])
          y.range <- range(Z[, 2])
          width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
          xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
          ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
          if (is.null(density.style$h)) 
              z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
          else z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
        }
      else z.density <- NULL
     }
   if (dim.biplot == 3) 
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
                          if (!is.null(density.style$which)) warning("No density plots in 3D")                                      
     }

   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)                     }

   samples <- do.call("biplot.sample.control", c(J, samples))
   new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(X.new)), new.samples))
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)

   if (dim.biplot == 2) draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = NULL, 
                                    z.trajectories = z.axes, z.bags = z.bags, z.ellipse = z.ellipse, 
                                    Z.new = Z.new, Z.density = z.density, 
                                    sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples,
                                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
                                    predict.samples = predict.samples, predict.means = predict.means, 
                                    Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 1) draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, 
                                       sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples,
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
                                       predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 3) draw.biplot.3D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, 
                                       sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples,  
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                                       predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)

   if (!is.null(ax$oblique) & ax$type == "interpolation") points(0, 0, pch = "+", cex = 2)

   if (any(unlist(legend.type))) 
     { dev.new()
       sample.list <- list(pch = samples$pch, col = samples$col)
       mean.list = list(pch = rep(NA, J), col = rep(NA, J))
       mean.list$pch[class.means$which] <- class.means$pch
       mean.list$col[class.means$which] <- class.means$col
       bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
       bag.list$lty[alpha.bags$which] <- alpha.bags$lty
       bag.list$col[alpha.bags$which] <- alpha.bags$col
       bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
       if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
         { bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
           bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
           bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd        
         }
       biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                     class.names = dimnames(G)[[2]])
    }
   if (return.coords) coords <- Z else coords <- NULL
   list(D=sqrt(-2*D), z.axes=z.axes, coords=coords)
}

Regression.biplot <- function (X, Z, G = NULL, scaled.mat = FALSE, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(X), 
                               correlation.biplot = FALSE, classes = 1:ncol(G), newdata = NULL, predict.means = NULL, predict.samples = NULL, 
                               samples = list(...), ax = list(...), new.samples = list(...), class.means = list(...), 
                               alpha.bags = list(...), kappa.ellipse = list(...), density.style = list(...), 
                               colour.scheme = NULL, Title = NULL, exp.factor = 1.2, 
                               reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                               legend.type = list(...), legend.format = list(...), return.coords=FALSE, ...) 
{  dim.biplot <- dim.biplot[1]
   if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
   reflect <- reflect[1]
   X.info <- biplot.check.X(X, scaled.mat)
   X <- X.info$X
   unscaled.X <- X.info$unscaled.X
   means <- X.info$means
   sd <- X.info$sd
   G <- biplot.check.G(G, nrow(X))
   if (!is.null(newdata)) X.new <- scale(newdata, center = means, scale = sd) else X.new <- NULL
   n <- nrow(X)
   p <- ncol(X)
   J <- ncol(G)
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]
   Bmat <- solve(t(Z) %*% Z) %*% t(Z) %*% X
   Br <- t(Bmat)

   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) { if (!is.null(ax$rotate)) { radns <- pi * rotate/180
                                                     rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)            }
                        }
   Z <- Z %*% rotate.mat %*% reflect.mat
   Br <- Br %*% rotate.mat %*% reflect.mat
   class.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% unscaled.X, ncol = ncol(unscaled.X))
   Z.new <- NULL
   Z.means.mat <- NULL
   if (!is.null(class.means)) Z.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% Z, ncol = ncol(Z))
   if (!is.null(X.new)) Z.new <- X.new %*% Br
   dimnames(Z) <- list(dimnames(X)[[1]], NULL)
   if (!is.null(X.new)) if (is.null(dimnames(newdata)[[1]])) dimnames(Z.new) <- list(paste("N", 1:nrow(Z.new), sep = ""), NULL) else dimnames(Z.new) <- list(dimnames(newdata)[[1]], NULL)
   num.vars <- p
   var.names <- dimnames(X)[[2]]
   ax <- do.call("biplot.ax.control", c(num.vars, list(var.names), ax))
   if (ax$type == "prediction") if (nrow(Br) > 1) axes.direction <- solve(diag(diag(Br %*% t(Br)))) %*% Br  
                                else axes.direction <- (1/(Br %*% t(Br))) %*% Br
   else axes.direction <- Br
   if (length(ax$which) == 0) z.axes <- NULL else z.axes <- lapply(1:length(ax$which), calibrate.axis, unscaled.X, means, sd, axes.direction, ax$which, ax$ticks, ax$orthogx, ax$orthogy, ax$oblique)
   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) 
     for (j in 1:length(alpha.bags$which)) 
       {  class.num <- alpha.bags$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=TRUE, approx.limit=alpha.bags$max[j])
          if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))                    
       }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot-1, kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) 
     for (j in 1:length(kappa.ellipse$which)) 
       {  class.num <- kappa.ellipse$which[j]
          mat <- Z[G[, class.num] == 1, ]                     
          if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
          if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))       
          if (dim.biplot == 3) { require(rgl)
                                 z.ellipse[[j]] <- ellipse3d(x=var(mat), centre=apply(mat,2,mean), t=kappa.ellipse$kappa[j])
                               }
       }

   if (dim.biplot == 1) 
     {  density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
        z.density <- vector("list", length(density.style$which))
        if (length(density.style$which) > 0) 
          for (j in 1:length(density.style$which)) 
            {  class.num <- density.style$which[j]
               mat <- Z[G[, class.num] == 1, ]
               z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])            
            }
     }
   if (dim.biplot == 2) 
     {  density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
        if (!is.null(density.style$which)) 
          {  if (density.style$which == 0) mat <- Z else mat <- Z[G[, density.style$which] == 1, ]
             x.range <- range(Z[, 1])
             y.range <- range(Z[, 2])
             width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
             xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
             ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
             if (is.null(density.style$h)) 
               z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
             else 
               z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))        
           }
         else z.density <- NULL
      }
   if (dim.biplot == 3)
     {  density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
        if (!is.null(density.style$which)) warning ("No density plots in 3D")
     }

   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)         }
   samples <- do.call("biplot.sample.control", c(J, samples))
   new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(X.new)), new.samples))
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)

   if (dim.biplot == 2) draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                    z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, 
                                    sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples, 
                                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
                                    predict.samples = predict.samples, predict.means = predict.means, 
                                    Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 1) draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, 
                                       sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples, 
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
                                       predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 3) draw.biplot.3D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                    z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new,  
                                    sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples, 
                                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
                                    predict.samples = predict.samples, predict.means = predict.means, 
                                    Title = Title, exp.factor = exp.factor, ...)

   if (!is.null(ax$oblique) & ax$type == "interpolation") 
     points(0, 0, pch = "+", cex = 2)
   if (!is.null(predict.samples)) 
     predict.mat <- Z[predict.samples, , drop = F] %*% t(Br) + matrix(1, nrow = length(predict.samples)) %*% means 
   else predict.mat <- NULL
   if (!is.null(predict.means)) 
     predict.mat <- rbind(predict.mat, Z[predict.means, , drop = F] %*% t(Br) + matrix(1, nrow = length(predict.means)) %*%  means)
   if (!is.null(predict.mat)) 
      dimnames(predict.mat) <- list(c(dimnames(X)[[1]][predict.samples], dimnames(G)[[2]][predict.means]), dimnames(X)[[2]])
   if (any(unlist(legend.type))) 
     {  dev.new()
        sample.list <- list(pch = samples$pch, col = samples$col)
        mean.list = list(pch = rep(NA, J), col = rep(NA, J))
        mean.list$pch[class.means$which] <- class.means$pch
        mean.list$col[class.means$which] <- class.means$col
        bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
        bag.list$lty[alpha.bags$which] <- alpha.bags$lty
        bag.list$col[alpha.bags$which] <- alpha.bags$col
        bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
        if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
          {  bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
             bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
             bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd        
          }
        biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                      class.names = dimnames(G)[[2]])
     }
   if (return.coords) coords <- Z else coords <- NULL
   list(predictions = predict.mat, coords=coords)
}

Spline.biplot <- function (X, Z=cmdscale(dist(X)), G = NULL, scaled.mat = FALSE, dim.biplot = c(2, 1, 3), 
                           e.vects = 1:ncol(X), correlation.biplot = FALSE, classes = 1:ncol(G), 
                           samples = list(...), ax = list(...), ax.new = NULL, newdata = NULL, new.samples = list(...), 
                           class.means = list(...), spline.control=list(...), 
                           alpha.bags = list(...), kappa.ellipse = list(...), density.style = list(...), 
                           colour.scheme = NULL, Title = NULL, exp.factor = 1.2, 
                           reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                           legend.type = list(...), legend.format = list(...),  return.coords=FALSE, ...) 
{
    dim.biplot <- dim.biplot[1]
    if (dim.biplot != 1 & dim.biplot != 2) stop("Only 1D and 2D biplots")
    reflect <- reflect[1]
    X.info <- biplot.check.X(X, scaled.mat)
    X <- X.info$X
    unscaled.X <- X.info$unscaled.X
    means <- X.info$means
    sd <- X.info$sd
    G <- biplot.check.G(G, nrow(X))
    if (!is.null(newdata)) X.new <- scale(newdata, center = means, scale = sd) else X.new <- NULL
    n <- nrow(X)
    p <- ncol(X)
    J <- ncol(G)
    if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
    classes <- classes[classes <= J]
    classes <- classes[classes > 0]
    reflect.mat <- diag(dim.biplot)
    if (reflect == "y" & dim.biplot < 3) reflect.mat[1, 1] <- -1
    if (reflect == "x" & dim.biplot == 2) reflect.mat[2, 2] <- -1
    if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
    rotate.mat <- diag(dim.biplot)
    if (dim.biplot == 2) {
        if (!is.null(rotate)) {
                radns <- pi * rotate/180
                rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)
        }
    }
                                
    var.names <- dimnames(X)[[2]]
    num.vars <- p
    ax <- do.call("biplot.trajectory.control", c(num.vars, list(var.names), ax))
    ax$type[ax$type=="prediction.circle"] <- "prediction"
    ax$type[ax$type=="prediction.normal"] <- "prediction"
    ax$type[ax$type=="prediction.back"] <- "prediction"
    spline.control <- do.call("biplot.spline.axis.control", spline.control)
    z.axes <- NULL
    if ((length(ax$which) > 0) & (ax$type == "prediction")) 
      z.axes <- lapply(1:length(ax$which), biplot.spline.axis, Z, unscaled.X, means=means, sd=sd, n.int=ax$ticks, 
                                           spline.control=spline.control)
    for (j in 1:length(z.axes))
       z.axes[[j]][,1:2] <- z.axes[[j]][,1:2] %*% rotate.mat %*% reflect.mat
    Z <- Z %*% rotate.mat %*% reflect.mat
    class.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% unscaled.X, ncol = ncol(unscaled.X))
    num.vars <- p
    class.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% unscaled.X, ncol = ncol(unscaled.X))
    Z.new <- NULL
    Z.means.mat <- NULL
    if (!is.null(class.means)) Z.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% Z, ncol = ncol(Z))
    dimnames(Z) <- list(dimnames(X)[[1]], NULL)
 
    alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
    z.bags <- vector("list", length(alpha.bags$which))
    if (length(alpha.bags$which) > 0) 
        for (j in 1:length(alpha.bags$which)) {
            class.num <- alpha.bags$which[j]
            mat <- Z[G[, class.num] == 1, ]
            if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=TRUE, approx.limit=alpha.bags$max[j])
            if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))
        }
    kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot-1, kappa.ellipse))
    z.ellipse <- vector("list", length(kappa.ellipse$which))
    if (length(kappa.ellipse$which) > 0) 
        for (j in 1:length(kappa.ellipse$which)) {
            class.num <- kappa.ellipse$which[j]
            mat <- Z[G[, class.num] == 1, ]
            if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
            if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))
        }
    if (dim.biplot == 1) {
        density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
        z.density <- vector("list", length(density.style$which))
        if (length(density.style$which) > 0) 
            for (j in 1:length(density.style$which)) {
                class.num <- density.style$which[j]
                mat <- Z[G[, class.num] == 1, ]
                z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])
            }
    }
    if (dim.biplot == 2) {
        density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
        if (!is.null(density.style$which)) {
            if (density.style$which == 0) mat <- Z else mat <- Z[G[, density.style$which] == 1, ]
            x.range <- range(Z[, 1])
            y.range <- range(Z[, 2])
            width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
            xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            if (is.null(density.style$h)) z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
            else z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
        }
        else z.density <- NULL
    }
    if (!is.null(colour.scheme)) {
        my.sample.col <- colorRampPalette(colour.scheme)
        samples$col <- my.sample.col(samples$col)
    }
    samples <- do.call("biplot.sample.control", c(J, samples))
    new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(X.new)), new.samples))
    class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
    legend.format <- do.call("biplot.legend.control", legend.format)
    legend.type <- do.call("biplot.legend.type.control", legend.type)
    if (dim.biplot == 2) 
        draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = NULL, z.trajectories = z.axes, 
                    z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, 
                    sample.style = samples, mean.style = class.means, ax.style = ax, 
                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, density.style = density.style, 
                    predict.samples = NULL, predict.means = NULL, Title = Title, exp.factor = exp.factor, ...)
    if (dim.biplot == 1) 
        draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, Z.new = Z.new, 
                       z.bags = z.bags, z.ellipse = z.ellipse, Z.density = z.density, 
                       sample.style = samples, mean.style = class.means, ax.style = ax, 
                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, 
                       density.style = density.style, predict.samples = NULL, 
                       predict.means = NULL, Title = Title, exp.factor = exp.factor, ...)
    if (!is.null(ax$oblique) & ax$type == "interpolation") 
        points(0, 0, pch = "+", cex = 2)
    if (any(unlist(legend.type))) {
        dev.new()
        sample.list <- list(pch = samples$pch, col = samples$col)
        mean.list = list(pch = rep(NA, J), col = rep(NA, J))
        mean.list$pch[class.means$which] <- class.means$pch
        mean.list$col[class.means$which] <- class.means$col
        bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
        bag.list$lty[alpha.bags$which] <- alpha.bags$lty
        bag.list$col[alpha.bags$which] <- alpha.bags$col
        bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
        if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
         {  bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
            bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
            bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd
         }
        biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                      class.names = dimnames(G)[[2]])
    }
   if (return.coords) coords <- Z else coords <- NULL
   list(coords=coords)
}

QDAbiplot <- function (X, G = NULL, scaled.mat = FALSE, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(X), 
                       correlation.biplot = FALSE, classes = 1:ncol(G), newdata = NULL, 
                       samples = list(...), ax = list(...), new.samples = list(...), class.means = list(...), 
                       alpha.bags = list(...), kappa.ellipse = list(...), density.style = list(...), class.regions = list(...),
                       predict.means = NULL, predict.samples = NULL, colour.scheme = NULL, Title = NULL, exp.factor = 1.2, 
                       reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                       legend.type = list(...), legend.format = list(...), return.coords=FALSE,
                       plot.coords=NULL, prior.p = NULL, class.dim=2, spline.control = list(...), nonlin.ax=FALSE, ...) 
{  dim.biplot <- dim.biplot[1]
   if (dim.biplot != 1 & dim.biplot != 2) stop("Only 1D and 2D biplots")
   reflect <- reflect[1]
   X.info <- biplot.check.X(X, scaled.mat)
   X <- X.info$X
   unscaled.X <- X.info$unscaled.X
   means <- X.info$means
   sd <- X.info$sd
   G <- biplot.check.G(G, nrow(X))
   Nmat <- t(G) %*% G

   if (!is.null(newdata)) X.new <- scale(newdata, center = means, scale = sd) else X.new <- NULL
   n <- nrow(X)
   p <- ncol(X)
   J <- ncol(G)
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]
   
   one <- matrix (1, nrow=n, ncol=1)
   phi <- apply (G, 2, function(g) { sigma.j <- var(X[g==1,])
                                     xbar.j <- t(g) %*% X / sum(g^2)    
                                     diag((X-one%*%xbar.j) %*% solve(sigma.j) %*% t(X-one%*%xbar.j)) + log(det(sigma.j))
                                                           })                                                    
   phi.means <- apply (phi, 2, mean)
   phi.svd <- svd(scale(phi, scale=F))
   Z <- scale(phi, scale=F) %*% phi.svd$v[,e.vects[1:dim.biplot]]

   Bmat <- solve(t(Z) %*% Z) %*% t(Z) %*% X
   Br <- t(Bmat)
   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) 
     { if (!is.null(rotate)) 
        { radns <- pi * rotate/180
          rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)            
        }
     }
   var.names <- dimnames(X)[[2]]
   num.vars <- p
   z.axes <- NULL
   z.trajectories <- NULL
   if (nonlin.ax)
     {
        ax <- do.call("biplot.trajectory.control", c(num.vars, list(var.names), ax))
        spline.control <- do.call("biplot.spline.axis.control", spline.control)
        if ((length(ax$which) > 0) & (ax$type == "prediction")) 
           z.trajectories <- lapply(1:length(ax$which), biplot.spline.axis, Z, unscaled.X, means=means, sd=sd, n.int=ax$ticks, 
                                             spline.control=spline.control)
        for (j in 1:length(z.trajectories))
          z.trajectories[[j]][,1:2] <- z.trajectories[[j]][,1:2] %*% rotate.mat %*% reflect.mat
      }
   Z <- Z %*% rotate.mat %*% reflect.mat
   Br <- Br %*% rotate.mat %*% reflect.mat
   class.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% unscaled.X, ncol = ncol(unscaled.X))
   Z.new <- NULL
   Z.means.mat <- NULL
   if (!is.null(class.means)) Z.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% Z, ncol = ncol(Z))
   if (!is.null(X.new)) Z.new <- X.new %*% Br
   dimnames(Z) <- list(dimnames(X)[[1]], NULL)
   if (!is.null(X.new)) if (is.null(dimnames(newdata)[[1]])) 
     dimnames(Z.new) <- list(paste("N", 1:nrow(Z.new), sep = ""), NULL) else dimnames(Z.new) <- list(dimnames(newdata)[[1]], NULL)

   if (!nonlin.ax)
     {
        ax <- do.call("biplot.ax.control", c(num.vars, list(var.names), ax))
        if (ax$type == "prediction") if (nrow(Br) > 1) 
          axes.direction <- solve(diag(diag(Br %*% t(Br)))) %*% Br  else axes.direction <- (1/(Br %*% t(Br))) %*% Br
        else axes.direction <- Br
        if (length(ax$which) == 0) z.axes <- NULL else z.axes <- lapply(1:length(ax$which), calibrate.axis, unscaled.X, means, sd, axes.direction, ax$which, ax$ticks, ax$orthogx, ax$orthogy, ax$oblique)
     }
     
   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) 
     for (j in 1:length(alpha.bags$which)) 
       { class.num <- alpha.bags$which[j]
         mat <- Z[G[, class.num] == 1, ]
         if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=T, approx.limit=alpha.bags$max[j])
         if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))                    
       }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot-1, kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) 
     for (j in 1:length(kappa.ellipse$which)) 
       { class.num <- kappa.ellipse$which[j]
         mat <- Z[G[, class.num] == 1, ]                     
         if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
         if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))       
       }

   if (dim.biplot == 1) 
     { density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
       z.density <- vector("list", length(density.style$which))
       if (length(density.style$which) > 0) 
         for (j in 1:length(density.style$which)) 
           { class.num <- density.style$which[j]
             mat <- Z[G[, class.num] == 1, ]
             z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])            
           }
     }
   if (dim.biplot == 2) 
    { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
      if (!is.null(density.style$which)) 
       { if (density.style$which == 0) mat <- Z else mat <- Z[G[, density.style$which] == 1, ]
         x.range <- range(Z[, 1])
         y.range <- range(Z[, 2])
         width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
         xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
         ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
         if (is.null(density.style$h)) 
           z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
         else 
           z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))        
        }
      else z.density <- NULL
    }

   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)         }
   samples <- do.call("biplot.sample.control", c(J, samples))
   new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(X.new)), new.samples))
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)

   Z.region <- NULL
   if (dim.biplot == 2 & !is.null(prior.p))
     {
       class.regions <- do.call("biplot.class.region.control", c(J, class.regions))
       prior.prob <- rep(NA, J)
       if (prior.p=="equal") prior.prob <- rep (1/J, J)
       if (prior.p=="sample.size") prior.prob <- diag(Nmat)/sum(diag(Nmat))
       if (any(is.na(prior.prob))) stop ("Incorrect prior probabilities specified for classification")
       plot.range <- range(Z * exp.factor) * 5
       if (class.dim > J) { warning ("classification in 2D")
                            class.dim <- 2 }
       Z.region <- biplot.create.regions (region.style = class.regions, plot.range = plot.range, region.mid = solve(t(G)%*%G)%*%t(G)%*%Z, rotate.mat = rotate.mat, reflect.mat = reflect.mat, region.func = biplot.QDA.class.func, 
                                              J = J, phi.means = phi.means, Vmat = phi.svd$v, prior.prob = prior.prob) 
     }
   if (dim.biplot == 2) draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, z.trajectories = z.trajectories, z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, Z.region = Z.region, 
                                    sample.style = samples, mean.style = class.means, ax.style = ax, bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, density.style = density.style, region.style = class.regions,
                                                                        predict.samples = predict.samples, predict.means = predict.means, Title = Title, exp.factor = exp.factor, plot.coords = plot.coords, ...)
   if (dim.biplot == 1) draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, sample.style = samples, 
                                       mean.style = class.means, ax.style = ax, bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, density.style = density.style, predict.samples = predict.samples, 
                                       predict.means = predict.means, Title = Title, exp.factor = exp.factor, ...)
   if (!is.null(ax$oblique) & ax$type == "interpolation") points(0, 0, pch = "+", cex = 2)
   if (!is.null(predict.samples)) predict.mat <- Z[predict.samples, , drop = F] %*% t(Vr) + matrix(1, nrow = length(predict.samples)) %*% means else predict.mat <- NULL
   if (!is.null(predict.means)) predict.mat <- rbind(predict.mat, Z[predict.means, , drop = F] %*% t(Vr) + matrix(1, nrow = length(predict.means)) %*%  means)
   if (!is.null(predict.mat)) dimnames(predict.mat) <- list(c(dimnames(X)[[1]][predict.samples], dimnames(G)[[2]][predict.means]), dimnames(X)[[2]])
   if (any(unlist(legend.type))) 
     { dev.new()
       sample.list <- list(pch = samples$pch, col = samples$col)
       mean.list = list(pch = rep(NA, J), col = rep(NA, J))
       mean.list$pch[class.means$which] <- class.means$pch
       mean.list$col[class.means$which] <- class.means$col
       bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
       bag.list$lty[alpha.bags$which] <- alpha.bags$lty
       bag.list$col[alpha.bags$which] <- alpha.bags$col
       bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
       if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
         { bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
           bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
           bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd        
         }
       biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                     class.names = dimnames(G)[[2]])
   }
   if (return.coords) coords <- Z else coords <- NULL
   list(coords=coords)
}

QDAbiplot.zoom <- function (...) 
{  QDAbiplot (...)
   flush.console()
   print ("Use the mouse to select rectangle for zoom . . .")
   out <- locator(2)
   x <- out$x
   y <- out$y
   diff <- max(abs(x[2]-x[1]),abs(y[2]-y[1]))
   if (x[2]>x[1]) x <- c(x[1],x[1]+diff) else x <- c(x[1], x[1]-diff)
   if (y[2]>y[1]) y <- c(y[1],y[1]+diff) else y <- c(y[1], y[1]-diff)
   x <- sort(x)
   y <- sort(y)
   rect(xleft=x[1], ybottom=y[1], xright=x[2], ytop=y[2], border="red", lty=2, lwd=3)  
   dev.new()
   QDAbiplot (plot.coords=list(x=x,y=y), ...)
}

LRbiplot <- function (X, G = NULL, scaled.mat = FALSE, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(X), 
                      correlation.biplot = FALSE, classes = 1:ncol(G), X.new = NULL, 
                      samples = list(...), ax = list(...), new.samples = list(...),  
                      alpha.bags = list(...), kappa.ellipse = list(...), density.style = list(...), 
                      colour.scheme = NULL, Title = NULL, exp.factor = 1.2, 
                      reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                      legend.type = list(...), legend.format = list(...),  return.coords=FALSE, ...) 
{
    dim.biplot <- dim.biplot[1]
    if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
    e.vects <- e.vects[1:dim.biplot]
    reflect <- reflect[1]
    X.info <- biplot.check.X(X, scaled.mat, centred.mat=F)
    X <- X.info$X
    G <- biplot.check.G(G, nrow(X))
    n <- nrow(X)
    p <- ncol(X)
    J <- ncol(G)
    if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
    classes <- classes[classes <= J]
    classes <- classes[classes > 0]
        
    nn <- sum(X)
    r.vec <- apply(X,1,sum)/nn
    c.vec <- apply(X,2,sum)/nn
    L <- log(X)
    Y <- (diag(nrow(X))-matrix(1,nrow=nrow(X),ncol=1) %*%t(r.vec)) %*% as.matrix(L) %*% t(diag(ncol(X))-matrix(1,nrow=ncol(X),ncol=1)%*%t(c.vec))
    S <- diag(r.vec^0.5) %*% Y %*% diag(c.vec^0.5)
    S.svd <- svd(S)
    F <- diag(r.vec^-0.5) %*% S.svd$u[,e.vects] %*% diag(S.svd$d[e.vects])
    Gamma <- diag(c.vec^-0.5) %*% S.svd$v[,e.vects]

    reflect.mat <- diag(dim.biplot)
    if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
    if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
    if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
    rotate.mat <- diag(dim.biplot)
    if (dim.biplot == 2) {
        if (!is.null(rotate)) {
                radns <- pi * rotate/180
                rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)
        }
    }
    Z <- F %*% rotate.mat %*% reflect.mat
        
    dimnames(Z) <- list(dimnames(X)[[1]], NULL)
    num.vars <- p

    if (!is.null(colour.scheme)) {
        my.sample.col <- colorRampPalette(colour.scheme)
        samples$col <- my.sample.col(samples$col)
    }
    samples <- do.call("biplot.sample.control", c(J, samples))
    new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(X.new)), new.samples))
    legend.format <- do.call("biplot.legend.control", legend.format)
    legend.type <- do.call("biplot.legend.type.control", legend.type)
        
    combin <- function (r, n, vek = 1:n)
    {
            if (r <= 0) NULL
            else if (r >= n) vek[1:n]
                 else rbind(cbind(vek[1], Recall(r - 1, n - 1, vek[-1])), Recall(r, n - 1, vek[-1]))
    }

    if (show.ax.points)
      {
        ax <- do.call("biplot.ax.control", c(p, list(dimnames(X)[[2]]), ax))
        z.axes <- NULL
      }
    else
      {
        ax.ratios <- combin (2, p)
        num.ax <- nrow(ax.ratios)
        var.names <- apply(ax.ratios,1,function(x)paste(dimnames(X)[[2]][x[1]],dimnames(X)[[2]][x[2]],sep="/"))
        ax <- do.call("biplot.ax.control", c(num.ax, list(var.names), ax))
        if (ax$type!="prediction") warning ("Only prediction biplot axes fitted")
        if (any(ax$olique!=0)) warning ("No oblique translations for Log-ratio biplots")
        if (length(ax$which) == 0) z.axes <- NULL
        else z.axes <- lapply(1:length(ax$which), ratio.axis, X, r.vec, Gamma, ax.ratios, ax$which, ax$ticks, ax$orthogx, ax$orthogy)
      }
        
    alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
    z.bags <- vector("list", length(alpha.bags$which))
    if (length(alpha.bags$which) > 0) 
        for (j in 1:length(alpha.bags$which)) {
            class.num <- alpha.bags$which[j]
            mat <- Z[G[, class.num] == 1, ]
            if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=T, approx.limit=alpha.bags$max[j])
            if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))
        }
    kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot, kappa.ellipse))
    z.ellipse <- vector("list", length(kappa.ellipse$which))
    if (length(kappa.ellipse$which) > 0) 
        for (j in 1:length(kappa.ellipse$which)) {
            class.num <- kappa.ellipse$which[j]
            mat <- Z[G[, class.num] == 1, ]
            if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
            if (dim.biplot == 1) 
                z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))
            if (dim.biplot == 3) {
                require(rgl)
                z.ellipse[[j]] <- ellipse3d(x = var(mat), centre = apply(mat, 2, mean), t = kappa.ellipse$kappa[j])
            }
        }
    if (dim.biplot == 1) {
        density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
        z.density <- vector("list", length(density.style$which))
        if (length(density.style$which) > 0) 
            for (j in 1:length(density.style$which)) {
                class.num <- density.style$which[j]
                mat <- Z[G[, class.num] == 1, ]
                z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])
            }
    }
    if (dim.biplot == 2) {
        density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
        if (!is.null(density.style$which)) 
          { if (density.style$which == 0) mat <- Z else mat <- Z[G[, density.style$which] == 1, ]
            x.range <- range(Z[, 1])
            y.range <- range(Z[, 2])
            width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
            xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            if (is.null(density.style$h)) z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
            else z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
        }
        else z.density <- NULL
    }
    if (dim.biplot == 3) {
        density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
        if (!is.null(density.style$which)) warning("No density plots in 3D")
    }

    if (dim.biplot == 2) 
        draw.biplot(Z = Z, G = G, classes = classes, z.axes = z.axes, z.bags = z.bags, z.ellipse = z.ellipse, Z.density = z.density, 
                    sample.style = samples, ax.style = ax, bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                    density.style = density.style, Title = Title, exp.factor = exp.factor, ...)
    if (dim.biplot == 1) 
        draw.biplot.1D(Z = Z, G = G, classes = classes, z.axes = z.axes, z.bags = z.bags, z.ellipse = z.ellipse, Z.density = z.density, 
                       sample.style = samples, ax.style = ax, bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                       density.style = density.style, Title = Title, exp.factor = exp.factor, ...)
    if (dim.biplot == 3) 
        draw.biplot.3D(Z = Z, G = G, classes = classes, z.axes = z.axes, z.bags = z.bags, z.ellipse = z.ellipse, 
                       sample.style = samples, ax.style = ax, bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                       Title = Title, exp.factor = exp.factor, ...)
                              
    if (any(unlist(legend.type))) {
        dev.new()
        sample.list <- list(pch = samples$pch, col = samples$col)
        mean.list = list(pch = rep(NA, J), col = rep(NA, J))
        mean.list$pch[class.means$which] <- class.means$pch
        mean.list$col[class.means$which] <- class.means$col
        bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
        bag.list$lty[alpha.bags$which] <- alpha.bags$lty
        bag.list$col[alpha.bags$which] <- alpha.bags$col
        bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
        if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) {
            bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
            bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
            bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd
        }
        biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                      class.names = dimnames(G)[[2]])
    }
   if (return.coords) coords <- Z else coords <- NULL
   list(coords=coords)
}

TRIplot <- function (X, G = NULL, modes=2:3, scaled.mat = TRUE, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(X), 
                     classes = 1:ncol(GG), 
                     samples = list(...), ax = list(...), class.means = list(...), alpha.bags = list(...), kappa.ellipse = list(...), 
                     density.style = list(...), 
                     colour.scheme = NULL, Title = NULL, exp.factor = 1.2, 
                     reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                     legend.type = list(...), legend.format = list(...), return.coords=FALSE, ...) 
{  dim.biplot <- dim.biplot[1]
   if (dim.biplot != 2) stop("Only 2D triplots")
   e.vects <- e.vects[1:dim.biplot]
   reflect <- reflect[1]

  if (scaled.mat)
    X.out <- scale.3waydata(X)
  else stop ("only implemented for scaling")    
  X <- X.out$X                                          # 3-way array
  unscaled.X <- X.out$unscaled.Xwide                    # wide matrix
  means <- X.out$means.wide                             # colmeans of unscaled.X
  sd <- rep(X.out$sd.var,dim(X)[[3]])                   # sd of each variable (2nd dimension), repeated for each occasion
        
   GG <- biplot.check.G(G, nrow(X))
   n <- dim(X)[1]
   p <- dim(X)[2]
   k <- dim(X)[3]
   J <- ncol(GG)
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(GG)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]

   output <-triplot.decomp(X,dim.biplot)
   U1 <- output $U1
   U2 <- output $U2
   U3 <- output $U3
   D <- output $ Sigma
   Aout <-cbind (D [1 ,1]^(1 /3)*U1 [ ,1],D [2 ,2]^(1 /3)*U1 [ ,2])
   Bout <-cbind (D [1 ,1]^(1 /3)*U2 [ ,1],D [2 ,2]^(1 /3)*U2 [ ,2])
   Cout <-cbind (D [1 ,1]^(1 /3)*U3 [ ,1],D [2 ,2]^(1 /3)*U3 [ ,2])
   U2 <-Bout
   U3 <-Cout

   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) { if (!is.null(rotate)) { radns <- pi * rotate/180
                                                  rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)          }
                        }

   Z <- Aout %*% rotate.mat %*% reflect.mat
   Zvars <- Bout %*% rotate.mat %*% reflect.mat
   Zocca <- Cout %*% rotate.mat %*% reflect.mat
   if (!is.null(class.means)) Z.means.mat <- solve(t(GG)%*%GG) %*% t(GG) %*% Z %*% rotate.mat %*% reflect.mat
   dimnames(Z) <- list(dimnames(X)[[1]], NULL)

   num.vars <- dim(X)[[modes[1]]] * dim(X)[[modes[2]]]
   var.names <- apply(expand.grid(dimnames(X)[[modes[1]]],dimnames(X)[[modes[2]]]),1,paste,collapse=":")
   ax <- do.call("biplot.ax.control", c(num.vars, list(var.names), ax))

   if ( modes [1]==1 && modes [2]==2) {      A <-Aout
                                             B <-Bout
                                             C <-Cout
                                             m <-n*p
                                             a1 <- n
                                             a2 <- p
                                             g <-nrow (B)
                                             h <-nrow (A)
                                             G <-B
                                             H <-A              }
   else if ( modes [1]==1 && modes [2]==3) { A <-Aout
                                             B <-Cout
                                             C <-Bout
                                             m <-n*k
                                             a1 <- n
                                             a2 <- k
                                             g <-nrow (A)
                                             h <-nrow (B)
                                             G <-A
                                             H <-B             }
   else {                                    A <-Bout
                                             B <-Cout
                                             C <-Aout
                                             m <-p*k
                                             a1 <- p
                                             a2 <- k
                                             g <-nrow (B)
                                             h <-nrow (A)
                                             G <-B
                                             H <-A              }
   l <-0
   axes.direction <- matrix (0,m,2)
   for (k in 1:g) 
     {  for (j in 1:h)
          {  l <-l+1
             b <-H[j ,]*(G[k ,])
             axes.direction [l,]= b
          }
     }
         
   if (length(ax$which) == 0) z.axes <- NULL else z.axes <- lapply(1:length(ax$which), calibrate.axis, unscaled.X, means, sd, axes.direction, ax$which, ax$ticks, ax$orthogx, ax$orthogy, ax$oblique)

   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(GG)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) 
     for (j in 1:length(alpha.bags$which)) 
       { class.num <- alpha.bags$which[j]
         mat <- Z[GG[, class.num] == 1, ]
         if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=T, approx.limit=alpha.bags$max[j])
         if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))
       }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(GG)[[2]]), dim.biplot, kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) 
     for (j in 1:length(kappa.ellipse$which)) 
       { class.num <- kappa.ellipse$which[j]
         mat <- Z[GG[, class.num] == 1, ]
         if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
         if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))
         if (dim.biplot == 3) {  require(rgl)
                                 z.ellipse[[j]] <- ellipse3d(x = var(mat), centre = apply(mat, 2, mean), t = kappa.ellipse$kappa[j])                    }
                              }

   if (dim.biplot == 2) 
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(GG)[[2]]), density.style))
       if (!is.null(density.style$which)) 
         { if (density.style$which == 0) mat <- Z else mat <- Z[GG[, density.style$which] == 1, ]
           x.range <- range(Z[, 1])
           y.range <- range(Z[, 2])
           width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
           xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
           ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
           if (is.null(density.style$h)) z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
           else z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
         }
       else z.density <- NULL
     }

   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)           }

   samples <- do.call("biplot.sample.control", c(J, samples))
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(GG)[[2]]), class.means))
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)

   draw.biplot(Z = Z, G = GG, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
               z.bags = z.bags, z.ellipse = z.ellipse, Z.new = NULL, Z.density = z.density, 
               sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples, 
               bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
               predict.samples = NULL, predict.means = NULL, Title = Title, exp.factor = exp.factor, ...)

   if (any(unlist(legend.type))) 
     { dev.new()
       sample.list <- list(pch = samples$pch, col = samples$col)
       mean.list = list(pch = rep(NA, J), col = rep(NA, J))
       mean.list$pch[class.means$which] <- class.means$pch
       mean.list$col[class.means$which] <- class.means$col
       bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
       bag.list$lty[alpha.bags$which] <- alpha.bags$lty
       bag.list$col[alpha.bags$which] <- alpha.bags$col
       bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
       if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
         { bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
           bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
           bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd
         }
       biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, class.names = dimnames(GG)[[2]], quality.print = quality.print, quality = quality)
     }
   if (return.coords) coords <- Z else coords <- NULL
   list(coords=coords)
}

CATPCAbiplot <- function (X, Xcont=NULL, factor.type=rep("nom", ncol(X)), G = NULL, scaled.mat = FALSE, 
                          dim.biplot = c(2, 1, 3), e.vects = 1:ncol(X), classes = 1:ncol(G), 
                          predict.samples = NULL, predict.means = NULL, 
                          samples = list(...), ax.nominal = list(...), ax.ordinal = list(...), ax = list(...), 
                          class.means = list(...), alpha.bags = list(...), kappa.ellipse = list(...), density.style = list(...), 
                          colour.scheme = NULL, Title = NULL, exp.factor = 1.2, 
                          reflect = c(FALSE, "x", "y"), rotate=NULL, select.origin = FALSE, 
                          legend.type = list(...), legend.format = list(...), epsilon=1e-6, return.coords=FALSE, ...) 
{  dim.biplot <- dim.biplot[1]
   if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
   e.vects <- e.vects[1:dim.biplot]
   reflect <- reflect[1]
   if (!all(factor.type %in% c("nom", "ord"))) stop("factor.type is a vector of 'nom' and 'ord' \n")
   G <- biplot.check.G(G, nrow(X))
   n <- nrow(X)
   p <- ncol(X)
   J <- ncol(G)
   if (!is.null(Xcont)) cont.means <- apply(Xcont,2,mean) else cont.means <- NULL
   if (!is.null(Xcont)) Xcont.cent <- scale(Xcont,scale=scaled.mat) else Xcont.cent <- NULL
   if (!is.null(Xcont)) { if (scaled.mat) { cont.sd <- apply(Xcont,2,sd) }
                          else { cont.sd <- rep(1,ncol(Xcont)) }
                        }
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]

    Gk <- vector("list", p)
    Lk.mat <- vector("list", p)
    Lk <- rep(NA, p)
    zlist <- vector("list", p)
    Hmat <- matrix(nrow = n, ncol = p)
    CatLabels <- NULL

    for (k in 1:p) { Gk[[k]] <- indmat(X[, k])
                     CatLabels <- append(CatLabels, dimnames(Gk[[k]])[[2]])
                     Lk[k] <- ncol(Gk[[k]])
                     Lk.mat[[k]] <- t(Gk[[k]]) %*% Gk[[k]]
                   }

    Gmat <- matrix(unlist(Gk), nrow = n)
    L.mat <- diag(diag(t(Gmat) %*% Gmat))
    L.mat.min.half <- diag(diag(L.mat)^-0.5)
    N.mat <- diag(n) - matrix(1, nrow = n, ncol = n)/n
    svd.mca <- svd(L.mat.min.half %*% t(Gmat) %*% N.mat %*% Gmat %*% L.mat.min.half/p)
    z.ini <- svd.mca$v[, 1]
    names(z.ini) <- CatLabels
    count <- 0
    for (k in 1:p) {  zlist[[k]] <- as.vector(z.ini[(count + 1):(count + ncol(Gk[[k]]))])
                      names(zlist[[k]]) <- CatLabels[(count + 1):(count + ncol(Gk[[k]]))]
                      count <- count + ncol(Gk[[k]])
                      if (identical(factor.type[k], "ord")) 
                        { 
                           gradient <- sign(lm(zlist[[k]] ~ I(1:length(zlist[[k]])))$coef[2])
                           if (gradient>0) zlist[[k]] <- ties (d = 1:length(zlist[[k]]), delta=zlist[[k]])
                           if (gradient<0) zlist[[k]] <- rev(ties (d = 1:length(zlist[[k]]), delta=rev(zlist[[k]])))
                           if (gradient==0) zlist[[k]] <- mean(zlist[[k]])         
                        }
                   }
    for (k in 1:p) 
      { labs <- names(zlist[[k]])
        scaling.factor <- as.numeric(sqrt(t(zlist[[k]]) %*% t(Gk[[k]]) %*% N.mat %*% Gk[[k]] %*% zlist[[k]]))
        if (is.na(1/scaling.factor)) scaling.factor <- 1
        scaling.vec <- rep(scaling.factor, length(zlist[[k]]))
        if (identical(factor.type[k], "nom")) zlist[[k]] <- as.vector(zlist[[k]]/scaling.vec)
        if (identical(factor.type[k], "ord")) 
          {  u.temp <- sum(diag(Lk.mat[[k]]))
             v.temp <- sum(zlist[[k]] * diag(Lk.mat[[k]]))                       
             w.temp <- c(t(zlist[[k]]) %*% Lk.mat[[k]] %*% zlist[[k]])
             b.temp <- sqrt(1/(w.temp - v.temp^2/u.temp))
             zlist[[k]] <- b.temp * zlist[[k]] - b.temp * v.temp/u.temp
          }
        names(zlist[[k]]) <- labs
        Hmat[, k] <- Gk[[k]] %*% zlist[[k]]
    }
    Hmat <- as.matrix(Hmat)

    dimnames(Hmat) <- dimnames(X)
    again <- TRUE
    RSS.old <- NULL
    Hmat.cent <- cbind(N.mat %*% Hmat, Xcont.cent)
    iter <- 0
    while (again) 
     {  iter <- iter + 1
        svd.out <- svd(Hmat.cent)
        if (dim.biplot>1)
          Yr <- scale(svd.out$u[, e.vects, drop=F] %*% diag(svd.out$d[e.vects]) %*% t(svd.out$v[, e.vects, drop=F]), scale = FALSE, center = TRUE)
        else
          Yr <- scale((svd.out$u[, e.vects, drop=F] * svd.out$d[e.vects]) %*% t(svd.out$v[, e.vects, drop=F]), scale = FALSE, center = TRUE)
        RSS <- sum((Hmat.cent - Yr)^2)
        if (!is.null(RSS.old)) again <- ((RSS.old - RSS) > epsilon)
        RSS.old <- RSS
        if (again) {
            for (k in 1:p) {
                labs <- names(zlist[[k]])
                if (identical(factor.type[k], "nom")) {
                  scale.factor <- as.numeric(sqrt(t(Yr[, k]) %*% Gk[[k]] %*% solve(Lk.mat[[k]]) %*% t(Gk[[k]]) %*% Yr[, k]))
                  zlist[[k]] <- as.vector((solve(Lk.mat[[k]]) %*% t(Gk[[k]]) %*% Yr[, k])/scale.factor)
                }
                if (identical(factor.type[k], "ord")) {
                  zlist[[k]] <- as.vector((solve(Lk.mat[[k]]) %*% t(Gk[[k]]) %*% Yr[, k]))
                           gradient <- sign(lm(zlist[[k]] ~ I(1:length(zlist[[k]])))$coef[2])
                           if (gradient>0) zlist[[k]] <- ties (d = 1:length(zlist[[k]]), delta=zlist[[k]])
                           if (gradient<0) zlist[[k]] <- rev(ties (d = 1:length(zlist[[k]]), delta=rev(zlist[[k]])))
                           if (gradient==0) zlist[[k]] <- mean(zlist[[k]])         
                  u.temp <- sum(diag(Lk.mat[[k]]))
                  v.temp <- sum(zlist[[k]] * diag(Lk.mat[[k]]))
                  w.temp <- c(t(zlist[[k]]) %*% Lk.mat[[k]] %*% zlist[[k]])
                  b.temp <- sqrt(1/(w.temp - v.temp^2/u.temp))
                  zlist[[k]] <- b.temp * zlist[[k]] - b.temp * v.temp/u.temp
                }
                names(zlist[[k]]) <- labs
                Hmat[, k] <- Gk[[k]] %*% zlist[[k]]
            }
        }
        Hmat.cent <- cbind(N.mat %*% Hmat, Xcont.cent)
    }
    Lz <- rep(NA, p)
    for (i in 1:p) Lz[i] <- sum(Lk.mat[[i]] %*% zlist[[i]])
    zLz <- rep(NA, p)
    for (i in 1:p) zLz[i] <- sum(t(zlist[[i]]) %*% Lk.mat[[i]] %*% zlist[[i]])
    Hmat.cent.svd <- svd(Hmat.cent)
    Vr <- Hmat.cent.svd$v[, e.vects, drop=F]
    Z <- Hmat.cent %*% Hmat.cent.svd$v[, e.vects, drop=F]

    if (!is.null(Xcont)) all.var.list <- vector("list",p+ncol(Xcont)) else all.var.list <- vector("list",p)
    for (j in 1:p)
      all.var.list[[j]] <- zlist[[j]]
    if (!is.null(Xcont)) for (j in 1:ncol(Xcont))
                           {  markers <- pretty(Xcont[,j])
                              all.var.list[[p+j]] <- markers-cont.means[j]
                              names(all.var.list[[p+j]]) <- markers
                           }
   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) 
     { if (!is.null(rotate)) 
         { radns <- pi * rotate/180
           rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)          
         }
     }
   Z <- Z %*% rotate.mat %*% reflect.mat
   if (!is.null(class.means)) 
   Z.means.mat <- solve(t(G) %*% G) %*% t(G) %*% Z
   dimnames(Z) <- list(dimnames(X)[[1]], NULL)
   var.names <- c(dimnames(X)[[2]],dimnames(Xcont)[[2]])
   Vr.all <- Vr

   if (!is.null(Xcont)) num.vars <- p+ncol(Xcont) else num.vars <- p
   if (!is.null(Xcont)) cont.var.names <- dimnames(Xcont)[[2]]

   ax.present <- ax.nominal.present <- ax.ordinal.present <- FALSE
   if (!is.null(Xcont)) 
     { ax <- do.call("biplot.ax.control", c(ncol(Xcont), list(cont.var.names), ax))
       if (ax$type=="interpolation") 
         { warning ("Only prediction biplot axes constructed")
           ax$type <- "prediction"
         }
       ax.present <- TRUE
     }
   if (sum(factor.type=="nom")>0) 
     {
       ax.nominal <- do.call("biplot.nominal.ax.control", c(sum(factor.type=="nom"), 
                           list(var.names[1:length(factor.type)][factor.type=="nom"]), list(Lk[factor.type=="nom"]), ax.nominal))
       ax.nominal.present <- TRUE
     }

   if (sum(factor.type=="ord")>0)
     { 
       ax.ordinal <- do.call("biplot.ordinal.ax.control", c(sum(factor.type=="ord"), 
                           list(var.names[1:length(factor.type)][factor.type=="ord"]), list(Lk[factor.type=="ord"]), ax.ordinal))
       ax.ordinal.present <- TRUE
     }
   if (!ax.present) ax <- NULL
   if (!ax.nominal.present) ax.nominal <- NULL
   if (!ax.ordinal.present) ax.ordinal <- NULL

   axes.direction <- 1/(diag(Vr.all %*% t(Vr.all))) * Vr.all %*% rotate.mat %*% reflect.mat
   axes.direction.nominal <- axes.direction[(1:length(factor.type))[factor.type=="nom"],,drop=F]
   axes.direction.ordinal <- axes.direction[(1:length(factor.type))[factor.type=="ord"],,drop=F]
   axes.direction <- axes.direction[-(1:length(factor.type)),,drop=F]
   nominal.var.list <- all.var.list[(1:length(factor.type))[factor.type=="nom"]]
   ordinal.var.list <- all.var.list[(1:length(factor.type))[factor.type=="ord"]]
   all.var.list <- all.var.list[-(1:length(factor.type))]
   if (!is.null(ax))
     {
       z.axes <- vector("list",length(ax$which))
       for (j in 1:length(ax$which))
         z.axes[[j]] <- calibrate.cat.axis(j, axes.direction, ax$which, ax$orthogx, ax$orthogy, ax$oblique, 
                         markers=all.var.list[[ax$which[j]]], labels=names(all.var.list[[ax$which[j]]]))
     }
   else z.axes <- NULL

   if (!is.null(ax.nominal))
     {
       z.axes.nominal <- vector("list",length(ax.nominal$which))
       for (j in 1:length(ax.nominal$which))
         z.axes.nominal[[j]] <- calibrate.cat.axis(j, axes.direction.nominal, ax.nominal$which, ax.nominal$orthogx, ax.nominal$orthogy, ax.nominal$oblique, 
                         markers=nominal.var.list[[ax.nominal$which[j]]], labels=names(nominal.var.list[[ax.nominal$which[j]]]))
     }
   else z.axes.nominal <- NULL

   if (!is.null(ax.ordinal))
     {
       z.axes.ordinal <- vector("list",length(ax.ordinal$which))
       for (j in 1:length(ax.ordinal$which))
         {  current.markers <- ordinal.var.list[[ax.ordinal$which[j]]]
            i <- 1
            while (i < length(current.markers)-1)
              {
                 if (abs(current.markers[i]-current.markers[i+1])<1.5*.Machine$double.eps)
                   { names(current.markers)[i] <- paste(names(current.markers)[i],names(current.markers)[i+1],sep="*")
                     current.markers <- current.markers[-(i+1)]
                   }
                 else
                   i <- i+1
              }
            z.axes.ordinal[[j]] <- calibrate.cat.axis(j, axes.direction.ordinal, ax.ordinal$which, ax.ordinal$orthogx, ax.ordinal$orthogy, ax.ordinal$oblique, 
                         markers=current.markers, labels=names(current.markers))
         }
     }
   else z.axes.ordinal <- NULL

   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) 
     for (j in 1:length(alpha.bags$which)) 
       { class.num <- alpha.bags$which[j]
         mat <- Z[G[, class.num] == 1, ]
         if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=TRUE, approx.limit=alpha.bags$max[j])
         if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))
       }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot, kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) 
     for (j in 1:length(kappa.ellipse$which)) 
       { class.num <- kappa.ellipse$which[j]
         mat <- Z[G[, class.num] == 1, ]
         if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
         if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))
         if (dim.biplot == 3) {  require(rgl)
         z.ellipse[[j]] <- ellipse3d(x = var(mat), centre = apply(mat, 2, mean), t = kappa.ellipse$kappa[j])                    
       }
                                                                                 }
   if (dim.biplot == 1) 
    { density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
      z.density <- vector("list", length(density.style$which))
      if (length(density.style$which) > 0) 
        for (j in 1:length(density.style$which)) 
           { class.num <- density.style$which[j]
             mat <- Z[G[, class.num] == 1, ]
             z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])       
           }
     }
   if (dim.biplot == 2) 
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
       if (!is.null(density.style$which)) 
         { if (density.style$which == 0) mat <- Z else mat <- Z[G[, density.style$which] == 1, ]
           x.range <- range(Z[, 1])
           y.range <- range(Z[, 2])
           width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
           xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
           ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
           if (is.null(density.style$h)) z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
           else z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
         }
       else z.density <- NULL
     }
   if (dim.biplot == 3) 
    { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
      if (!is.null(density.style$which)) warning("No density plots in 3D")                                   
    }

   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)           }

   samples <- do.call("biplot.sample.control", c(J, samples))
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)

   if (dim.biplot == 2) draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, 
                                    z.axes = z.axes, z.axes.nominal=z.axes.nominal, z.axes.ordinal=z.axes.ordinal,
                                    z.bags = z.bags, z.ellipse = z.ellipse, Z.density = z.density, 
                                    sample.style = samples, mean.style = class.means, 
                                    ax.style = ax, ax.nominal.style = ax.nominal, ax.ordinal.style = ax.ordinal,
                                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
                                    predict.samples = predict.samples, predict.means = predict.means, 
                                    Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 1) draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, 
                                       z.axes = z.axes, z.axes.nominal = z.axes.nominal, z.axes.ordinal = z.axes.ordinal, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.density = z.density, 
                                       sample.style = samples, mean.style = class.means, 
                                       ax.style = ax, ax.nominal.style = ax.nominal, ax.ordinal.style = ax.ordinal,
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, density.style = density.style, 
                                       predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 3) draw.biplot.3D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, 
                                       z.axes = z.axes, z.axes.nominal=z.axes.nominal, z.axes.ordinal=z.axes.ordinal, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, 
                                       sample.style = samples, mean.style = class.means, 
                                       ax.style = ax, ax.nominal.style = ax.nominal, ax.ordinal.style = ax.ordinal, 
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                                       predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)

#   if (!is.null(predict.samples)) predict.mat <- scale(Z[predict.samples, , drop = F] %*% t(reflect.mat) %*% t(rotate.mat) %*% t(Vr), center = F, scale = 1/sd) else predict.mat <- NULL
#   if (!is.null(predict.mat)) predict.mat <- scale(predict.mat, center = -means, scale = F)
#   if (!is.null(predict.means)) predict.means.mat <- scale(Z.means.mat[predict.means, , drop = F] %*% t(reflect.mat) %*% t(rotate.mat) %*% t(Vr), center = F, scale = 1/sd) else predict.means.mat <- NULL
#   if (!is.null(predict.means.mat)) predict.mat <- rbind(predict.mat, scale(predict.means.mat, center = -means, scale = F))
#   if (!is.null(predict.mat)) dimnames(predict.mat) <- list(c(dimnames(X)[[1]][predict.samples], dimnames(G)[[2]][predict.means]), dimnames(X)[[2]])
   if (any(unlist(legend.type))) 
     { dev.new()
       sample.list <- list(pch = samples$pch, col = samples$col)
       mean.list = list(pch = rep(NA, J), col = rep(NA, J))
       mean.list$pch[class.means$which] <- class.means$pch
       mean.list$col[class.means$which] <- class.means$col
       bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
       bag.list$lty[alpha.bags$which] <- alpha.bags$lty
       bag.list$col[alpha.bags$which] <- alpha.bags$col
       bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
       if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
         { bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
           bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
           bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd
         }
       biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                     class.names = dimnames(G)[[2]])
    }
   names(zlist) <- dimnames(X)[[2]]
   if (return.coords) coords <- Z else coords <- NULL
   list (zlist=zlist, H=Hmat.cent, coords=coords)
}

PLSbiplot <- function (X, Y, G = NULL, scaled.mat = FALSE, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(X), 
                       classes = 1:ncol(G), 
                       newdata = NULL, predict.samples = NULL, predict.means = NULL, 
                       samples = list(...), ax = list(...), class.means = list(...), new.samples = list(...), 
                       alpha.bags = list(...), kappa.ellipse = list(...), 
                       density.style = list(...), density.legend=T, colour.scheme = NULL, 
                       Title = NULL, exp.factor = 1.2, plot.coords=NULL, 
                       reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, 
                       adequacies.print = FALSE, predictivity.print = FALSE, quality.print = FALSE, 
                       legend.type = list(...), legend.format = list(...), return.coords=FALSE, PLS.method="kernelpls", ...) 
{  dim.biplot <- dim.biplot[1]
   if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
   e.vects <- e.vects[1:dim.biplot]
   reflect <- reflect[1]
   X.info <- biplot.check.X(X, scaled.mat)
   X <- X.info$X
   unscaled.X <- X.info$unscaled.X
   means <- X.info$means
   sd <- X.info$sd
   Y.info <- biplot.check.X(Y, scaled.mat)
   Y <- Y.info$X
   unscaled.Y <- Y.info$unscaled.X
   Ymeans <- Y.info$means
   Ysd <- Y.info$sd
   G <- biplot.check.G(G, nrow(X))
   if (!is.null(newdata)) X.new <- scale(newdata, center = means, scale = sd) else X.new <- NULL
   n <- nrow(X)
   p <- ncol(X)
   pY <- ncol(Y)
   J <- ncol(G)
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]

   require(pls)
   PLS.out <- plsr(Y ~ X, ncomp=max(e.vects), method=PLS.method, scale=scaled.mat)

   Z <- PLS.out$scores[,e.vects, drop = F]
   Pr <- PLS.out$loadings[,e.vects, drop = F]
   Qr <- PLS.out$Yloadings[,e.vects, drop = F]
   Wr <- PLS.out$loading.weights[,e.vects, drop = F]   

   quality <- sum(PLS.out$Xvar[e.vects])/PLS.out$Xtotvar
   adequacy <- NULL
   Xhat <- Z %*% t(Pr)
   axis.predictivity <- diag(t(Xhat)%*%Xhat)/diag(t(X)%*%X)
   sample.predictivity <- NULL

   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) 
    { if (!is.null(rotate)) 
        { if (is.numeric(rotate)) 
           { radns <- pi * rotate/180
             rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)          
           }
          else 
            { if (rotate == "maxpred") { rotate <- (names(axis.predictivity))[axis.predictivity == max(axis.predictivity)]
                                         rotate <- match(ax$rotate, dimnames(X)[[2]])
                                       }
              else rotate <- match(rotate, dimnames(X)[[2]])
              radns <- -atan2(V.mat[rotate, e.vects[2]], V.mat[rotate, e.vects[1]])
              rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)
            }
         }
     }
   class.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% unscaled.X, ncol = ncol(unscaled.X))
   Z.new <- NULL
   Z.means.mat <- NULL
   Z <- Z %*% rotate.mat %*% reflect.mat
   if (!is.null(class.means)) Z.means.mat <- class.means.mat %*% Wr %*% rotate.mat %*% reflect.mat
   if (!is.null(X.new)) Z.new <- X.new %*% Wr %*% rotate.mat %*% reflect.mat

   dimnames(Z) <- list(dimnames(X)[[1]], NULL)
   if (!is.null(X.new)) 
     if (is.null(dimnames(newdata)[[1]])) 
        dimnames(Z.new) <- list(paste("N", 1:nrow(Z.new), sep = ""), NULL) 
     else 
        dimnames(Z.new) <- list(dimnames(newdata)[[1]], NULL)

   means <- c(means, Ymeans)
   sd <- c(sd, Ysd)
   unscaled.X <- cbind(unscaled.X, unscaled.Y)
   num.vars <- ncol(unscaled.X)
   var.names <- c(dimnames(X)[[2]], dimnames(Y)[[2]]) 
   Vr.all <- rbind(Pr, Qr)
   ax <- do.call("biplot.ax.control", c(num.vars, list(var.names), ax))

   if (ax$type == "prediction") 
     axes.direction <- 1/(diag(Vr.all %*% t(Vr.all))) * Vr.all %*% rotate.mat %*% reflect.mat                                                                    
   else 
     axes.direction <- Vr.all %*% rotate.mat %*% reflect.mat    

   if (length(ax$which) == 0) 
     z.axes <- NULL 
   else 
     z.axes <- lapply(1:length(ax$which), calibrate.axis, unscaled.X, means, sd, axes.direction, ax$which, ax$ticks, 
                                                          ax$orthogx, ax$orthogy, ax$oblique)

   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) 
     for (j in 1:length(alpha.bags$which)) 
       {  class.num <- alpha.bags$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, aa=alpha.bags$alpha[j], na.rm=TRUE, approx.limit=alpha.bags$max[j])
          if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))
       }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot, kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) 
     for (j in 1:length(kappa.ellipse$which)) 
       {  class.num <- kappa.ellipse$which[j]
          mat <- Z[G[, class.num] == 1, ]
          if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
          if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))
          if (dim.biplot == 3) {  require(rgl)
                                  z.ellipse[[j]] <- ellipse3d(x = var(mat), centre = apply(mat, 2, mean), t = kappa.ellipse$kappa[j])                    
                               }
       }
   if (dim.biplot == 1) 
     { density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
       z.density <- vector("list", length(density.style$which))
       if (length(density.style$which) > 0) 
         for (j in 1:length(density.style$which)) 
           {  class.num <- density.style$which[j]
              mat <- Z[G[, class.num] == 1, ]
              z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])           
           }
     }
   if (dim.biplot == 2) 
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
       if (!is.null(density.style$which)) 
          { if (density.style$which == 0) 
              mat <- Z 
            else 
              mat <- Z[G[, density.style$which] == 1, ]
            x.range <- range(Z[, 1])
            y.range <- range(Z[, 2])
            width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
            xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
            if (is.null(density.style$h)) z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
            else z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
          }
        else z.density <- NULL
     }
   if (dim.biplot == 3) 
     { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
       if (!is.null(density.style$which)) warning("No density plots in 3D")                                    
     }

   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)           }

   samples <- do.call("biplot.sample.control", c(J, samples))
   new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(X.new)), new.samples))
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)
   if (dim.biplot == 2) draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                    z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, 
                                    Z.density = z.density, density.legend = density.legend, 
                                    sample.style = samples, mean.style = class.means, ax.style = ax, 
                                    bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, 
                                    density.style = density.style, predict.samples = predict.samples, predict.means = predict.means, 
                                    Title = Title, exp.factor = exp.factor, plot.coords=plot.coords, ...)
   if (dim.biplot == 1) draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, 
                                       sample.style = samples, mean.style = class.means, ax.style = ax, 
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, 
                                       density.style = density.style, predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)
   if (dim.biplot == 3) draw.biplot.3D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, 
                                       z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, 
                                       sample.style = samples, mean.style = class.means, ax.style = ax, new.sample.style = new.samples,  
                                       bag.style = alpha.bags, ellipse.style = kappa.ellipse, 
                                       predict.samples = predict.samples, predict.means = predict.means, 
                                       Title = Title, exp.factor = exp.factor, ...)

   if (!is.null(ax$oblique) & ax$type == "interpolation") 
     points(0, 0, pch = "+", cex = 2)
   if (!is.null(predict.samples)) 
     predict.mat <- scale(Z[predict.samples, , drop = F] %*% t(reflect.mat) %*% t(rotate.mat) %*% t(Pr), center = F, scale = 1/sd) else predict.mat <- NULL
   if (!is.null(predict.mat)) 
      predict.mat <- scale(predict.mat, center = -means, scale = F)
   if (!is.null(predict.means)) 
     predict.means.mat <- scale(Z.means.mat[predict.means, , drop = F] %*% t(reflect.mat) %*% t(rotate.mat) %*% t(Pr), center = F, scale = 1/sd) else predict.means.mat <- NULL
   if (!is.null(predict.means.mat)) 
     predict.mat <- rbind(predict.mat, scale(predict.means.mat, center = -means, scale = F))
   if (!is.null(predict.mat)) 
     dimnames(predict.mat) <- list(c(dimnames(X)[[1]][predict.samples], dimnames(G)[[2]][predict.means]), dimnames(X)[[2]])
   if (any(unlist(legend.type))) 
     {  dev.new()
        sample.list <- list(pch = samples$pch, col = samples$col)
        mean.list = list(pch = rep(NA, J), col = rep(NA, J))
        mean.list$pch[class.means$which] <- class.means$pch
        mean.list$col[class.means$which] <- class.means$col
        bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
        bag.list$lty[alpha.bags$which] <- alpha.bags$lty
        bag.list$col[alpha.bags$which] <- alpha.bags$col
        bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
        if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) 
          { bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
            bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
            bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd
          }
        biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, 
                      class.names = dimnames(G)[[2]], quality.print = quality.print, quality = quality)
     }
   if (return.coords) coords <- Z else coords <- NULL
   list(predictions = predict.mat, quality = quality, adequacy = adequacy, axis.predictivity = axis.predictivity, 
        sample.predictivity = sample.predictivity, coords=coords)
}

