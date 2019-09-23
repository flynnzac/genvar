#' command to graph bivariate relationships
#'
#' bigraph plots bivariate relationships.  it can plot multiple relationships on the same graph.
#' @param type a quoted list of plot types ("line" for line graphs, "connected" for line graphs with points indicating data points, and "scatter" for graphs with points for the data points)
#' 
#' @export
bigraph <- function (type, xvars, yvars,
                     xlines=NULL, ylines=NULL,
                     title=NULL, xlabel=NULL, ylabel=NULL,
                     xrange, yrange,
                     style=NULL, color=NULL, size=NULL,
                     output="screen", resolution="480x480",
                     file)
{
  if (!isloaded())
    stop("Data not loaded.")
  
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
  
}

