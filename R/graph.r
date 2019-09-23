#' @export
bigraph <- function (type, yvars, xvars, by=NULL,
                     xlines=NULL, ylines=NULL,
                     title=NULL, xlabel=NULL, ylabel=NULL,
                     xrange=NULL, yrange=NULL,
                     style=NULL, color=NULL, size=NULL,
                     output="screen", resolution="480x480")
{
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

  if (!is.null(xrange))
  {
    xrange <- as.numeric(strsplit(xrange, " ")[[1]])
  }

  if (!is.null(yrange))
  {
    yrange <- as.numeric(strsplit(yrange, " ")[[1]])
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
                      lty=style[tp],
                      lwd=size[tp], col=color[tp])
        }

        if (type[tp] == "connected")
        {
          panel.lines(data[,xvars[tp]],
                      data[,yvars[tp]],
                      type="b",
                      lty=style[tp],
                      lwd=size[tp], col=color[tp])
        }

        if (type[tp] == "points")
        {
          panel.points(data[,xvars[tp]],
                       data[,yvars[tp]],
                       cex=size[tp],
                       col=color[tp])
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
          panel.abline(v=ylines[i])
        }
      }

      

    }
    
    curplot <- xyplot(form, data=data, panel=plot_function)
    print(curplot)
  }), envir=data.env)
  
}

##    showgraph  - a function to show graph with other font, resolution, output options from xyplot object
