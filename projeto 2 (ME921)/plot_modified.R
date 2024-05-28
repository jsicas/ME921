plot_modified <- function (x, y, what=c('classification', 'uncertainty'), dimens=NULL, xlab=NULL, ylab=NULL,
                           addEllipses=TRUE, main=FALSE, ...) 
{
  object <- x
  object1 <- y
  if (!inherits(object, "Mclust")) 
    stop("object not of class 'Mclust'")
  data <- object$data
  p <- ncol(data)
  dimens <- if (is.null(dimens)) 
    seq(p)
  else dimens[dimens <= p]
  d <- length(dimens)
  main <- if (is.null(main) || is.character(main)) 
    FALSE
  else as.logical(main)
  what <- match.arg(what, several.ok = TRUE)
  oldpar <- par(no.readonly = TRUE)
  plot.Mclust.classification <- function(...) {
    if (d > 2) {
      pars <- object$parameters
      pars$mean <- pars$mean[dimens, , drop = FALSE]
      pars$variance$d <- length(dimens)
      pars$variance$sigma <- pars$variance$sigma[dimens, 
                                                 dimens, , drop = FALSE]
      on.exit(par(oldpar))
      par(mfrow=c(d, d), mar=rep(0.2/2, 4), oma=rep(3,4))
      for (i in seq(d)) {
        for (j in seq(d)) {
          if (i == j) {
            plot(data[, dimens[c(j, i)]], type='n', 
                 xlab = '', ylab = '', axes = FALSE)
            text(mean(par('usr')[1:2]), mean(par('usr')[3:4]), 
                 labels = colnames(data[, dimens])[i], cex = 1.5, 
                 adj = 0.5)
            box()
          }
          else if (j < i) {
            coordProj(data=data, dimens=dimens[c(j,i)],
                      what='classification',classification=object1$classification, 
                      parameters=object$parameters, addEllipses=addEllipses, 
                      main=FALSE, xaxt='n', yaxt='n',
                      col=seq(1,length(unique(object1$classification))), ...)
          }
          else {
            coordProj(data=data, dimens=dimens[c(j,i)],
                      what='classification',classification=object$classification, 
                      parameters=object$parameters, addEllipses=addEllipses, 
                      main=FALSE, xaxt='n', yaxt='n', ...)
          }
          if (i == 1 && (!(j%%2))) 
            axis(3)
          if (i == d && (j%%2)) 
            axis(1)
          if (j == 1 && (!(i%%2))) 
            axis(2)
          if (j == d && (i%%2)) 
            axis(4)
        }
      }
    }
  }
  plot.Mclust.uncertainty <- function(...) {
    pars <- object$parameters
    if (p > 2 && d > 2) {
      on.exit(par(oldpar))
      par(mfrow = c(d, d), mar = rep(0, 4), mar = rep(0.2/2, 
                                                      4), oma = rep(3, 4))
      for (i in seq(d)) {
        for (j in seq(d)) {
          if (i == j) {
            plot(data[, dimens[c(j, i)]], type = "n", 
                 xlab = "", ylab = "", axes = FALSE)
            text(mean(par("usr")[1:2]), mean(par("usr")[3:4]), 
                 labels = colnames(data[, dimens])[i], cex = 1.5, 
                 adj = 0.5)
            box()
          }
          else if (j < i) {
            coordProj(data = data, what = "uncertainty",
                      parameters = object$parameters, z = object1$z,
                      classification = object$classification,
                      dimens = dimens[c(j, i)], main = FALSE,
                      addEllipses = addEllipses, xaxt = "n",
                      yaxt = "n", ...)
          }
          else {
            coordProj(data = data, what = "uncertainty", 
                      parameters = object$parameters, z = object$z, 
                      classification = object$classification, 
                      dimens = dimens[c(j, i)], main = FALSE, 
                      addEllipses = addEllipses, xaxt = "n", 
                      yaxt = "n", ...)
          }
          if (i == 1 && (!(j%%2))) 
            axis(3)
          if (i == d && (j%%2)) 
            axis(1)
          if (j == 1 && (!(i%%2))) 
            axis(2)
          if (j == d && (i%%2)) 
            axis(4)
        }
      }
    }
  }
  if (any(what == 'classification'))
    plot.Mclust.classification(...)
  if (any(what == 'uncertainty')) 
    plot.Mclust.uncertainty(...)  
}
