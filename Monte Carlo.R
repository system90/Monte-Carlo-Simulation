library (plotrix)         # Draws out shapes
x = runif(100, -1, 1)                             # Generate uniform random variables between -1 and 1, (x-axis)
y = runif(100, -1, 1)                             # Generate uniform random variables between -1 and 1, (y-axis)
plot(x, y, asp = 1, xlim = c(-1, 1))              # Plots x and y axis with limits
                                                  # asp: aspect ratio of y/x
draw.circle(0, 0, 1, nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1) # nv: number of vertices to draw circle
                                                                           # border: the colour
                                                                           # col: colour of circle filling
                                                                           # lty: Circumference line type
                                                                           # lwd: Circumference line width
                                                                          


# Estimate Pi
Estpi.f <- function(n,a=1) {    # a: range of values of uniform (1 is default) 
  x=runif(n,-a,a)               # x and y are two iid U(-1,1) variables
  y=runif(n,-a,a)               # x and y are two iid U(-1,1) variables
  z=ifelse(x^2+y^2<=1,1,0)      # z counts the number of points within  the circle
  prob=mean(z)                  # prob computes the proportion of number n points within the circle
  
  4*prob                        # 4 times prob is just an approximation of the vale of pi
}

Estpi.f(5000)

# Now check the convergence:
Convergepi.f=function(m) {    # check m different n in Estpi.f(n)
  out=rep(0,m)                # set a zero-vector to save outputs. rep means repeat
  for(n in 1:m) {             # 
  out[n]=Estpi.f(n)           # replace each zero-vector with probability of pi
}
plot(1:m, out, type="l")      # type = line
}

Convergepi.f(1000)



