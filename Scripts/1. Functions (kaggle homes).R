# Dependencies: None
require(dplyr)
require(ggplot2)
require(Amelia)
require(mice)
require(lattice)

# Function 1: check.na()
  ## Computes the total number of NAs from vector 'x'.
check.na <- function(x) {
  sum(is.na(x))
}


# Function 2: show.na()
  ## Wrapper for Function 1: check.na(). Prints the number of NAs in each variable.
  ## Inputs: .data is a data frame. temp is a temporary storage variable with a default pew.
show.na <- function(.data, temp = pew) {
  print( noquote("Number of NAs in each of the variables:") )
  pew <- sapply(.data, check.na)
  print( sort(pew[pew > 0], decreasing = TRUE))
}


# Function 3: impute.na()
  ## Replaces NA values in a factor with the level "None"
  ## Inputs: '.data' is a .data frame, 'x' is the index position (i.e., numeric) of a variable that is a factor.
impute.na <- function(.data, x) {
  levels(.data[, x]) <- c(levels(.data[, x]), "None")
  .data[, x][is.na(.data[, x])] <- "None"
  return(.data[, x])
}

# Function 4: multiplot() -- From RCookBook.com
  ## ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - ncol:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'ncol' is ignored.
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
multiplot <- function(..., plotlist = NULL, file, ncol = 1, layout = NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots <- length(plots)

  # If layout is NULL, then use 'ncol' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of ncol
    layout <- matrix(seq(1, ncol * ceiling(numPlots/ncol)),
                    ncol = ncol, nrow = ceiling(numPlots/ncol))
  }

 if (numPlots == 1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


