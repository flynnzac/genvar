#' command to graph bivariate relationships
#'
#' bigraph plots bivariate relationships.  it can plot multiple relationships on the same graph.
#' @param type a quoted list of plot types ("line" for line graphs, "connected" for line graphs with points indicating data points, and "scatter" for graphs with points for the data points).  For example, to plot two lines and one scatter plot on the same graph: "line line scatter".
#' @param xvars a varlist in "x1 x2 x3" form giving the variables to plot on the horizontal axis.
#' @param yvars a varlist in "y1 y2 y3" form giving the variables to plot on the vertical axis
#' @param ... other options passed to directly to \code{xyplot} from the \code{lattice} package
#' @return returns NULL, invisibly
#' @importFrom lattice xyplot
#' @importFrom lattice panel.lines
#' @importFrom lattice panel.points
#' @importFrom lattice panel.abline
#' @export
bigraph <- function (type, xvars, yvars,
                     xlines=NULL, ylines=NULL,
                     title=NULL, xlabel=NULL, ylabel=NULL,
                     xrange, yrange,
                     style=NULL, color=NULL, size=NULL,
                     output="screen", resolution="480x480",
                     file, ...)
{
  assert_loaded()
  
  xvars <- varlist(xvars, type="vector")
  yvars <- varlist(yvars, type="vector")
  
  if (!is.null(xlines))
  {
    if (inherits(xlines, "character"))
    {
      xlines <- as.numeric(strsplit(xlines, " ")[[1]])
    }
  }

  if (!is.null(ylines))
  {
    if (inherits(ylines, "character"))
    {
      ylines <- as.numeric(strsplit(ylines, " ")[[1]])
    }
  }

  if (!is.null(xlines))
  {
    if (inherits(xlines, "character"))
    {
      xlines <- as.numeric(strsplit(xlines, " ")[[1]])
    }
  }

  if (!missing(xrange))
  {
    xrange <- as.numeric(strsplit(xrange, " ")[[1]])
    if (length(xrange) != 2)
    {
      stop("xrange should have two elements.")
    }
  }

  if (!missing(yrange))
  {
    yrange <- as.numeric(strsplit(yrange, " ")[[1]])
    if (length(yrange) != 2)
    {
      stop("yrange should have two elements.")
    }
  }

  type <- strsplit(type, " ")[[1]]

  if (!is.null(style))
  {
    style <- strsplit(style, " ")[[1]]
  } 

  if (!is.null(color))
  {
    color <- strsplit(color, " ")[[1]]
  }

  if (!is.null(size))
  {
    size <- strsplit(size, " ")[[1]]
  }


  n <- length(type)

  if (length(xvars) != n || length(yvars) != n ||
      (!is.null(size) && length(size) != n) ||
      (!is.null(style) && length(style) != n) ||
      (!is.null(color) && length(color) != n))
  {
    stop("type, xvars, yvars, size, style, and color do not all have the same length")
  }

  if (output == "png" && missing(file))
  {
    stop("file must be specified when output is png")
  }

  if (!grepl("[0-9]+x[0-9]+", resolution))
  {
    stop("resolution must be in format WIDTHxHEIGHT.")
  }
  
  
  form <- as.formula(paste0(paste0(yvars, collapse="+"),"~",
                            paste0(xvars, collapse="+")))

  eval(substitute({

    plot_function <- function (...)
    {
      for (tp in 1:length(type))
      {
        if (type[tp] == "line")
        {
          panel.lines(data[,xvars[tp]],
                      data[,yvars[tp]],
                      lty=ifelse(is.null(style), quote(expr =), style[tp]),
                      lwd=ifelse(is.null(size), quote(expr =), size[tp]),
                      col=ifelse(is.null(color), quote(expr =), color[tp]))
        }

        if (type[tp] == "connected")
        {
          panel.lines(data[,xvars[tp]],
                      data[,yvars[tp]],
                      type="b",
                      lty=ifelse(is.null(style), quote(expr =), style[tp]),
                      lwd=ifelse(is.null(size), quote(expr =), size[tp]),
                      col=ifelse(is.null(color), quote(expr =), color[tp]))
        }

        if (type[tp] == "scatter")
        {
          panel.points(data[,xvars[tp]],
                       data[,yvars[tp]],
                       cex=ifelse(is.null(size), quote(expr =), size[tp]),
                       col=ifelse(is.null(color), quote(expr =), color[tp]))
        }
                       
      }

      if (!is.null(xlines))
      {
        for (i in 1:length(xlines))
        {
          panel.abline(v=xlines[i])
        }
      }

      if (!is.null(ylines))
      {
        for (i in 1:length(ylines))
        {
          panel.abline(h=ylines[i])
        }
      }

    }

    curplot <- xyplot(form, data=data, panel=plot_function,
                      xlab=xlabel, ylab=ylabel, main=title,
                      xlim=xrange, ylim=yrange)

    if (output=="screen")
    {
      print(curplot)
    }

    if (output=="png")
    {
      res <- as.numeric(strsplit(resolution, "x")[[1]])
      png(file, width=res[1], height=res[2])
      print(curplot)
      dev.off()
    }

    
  }), envir=data.env)

  invisible(NULL)
  
}

