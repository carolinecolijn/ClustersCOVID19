
# CDF of convolution of k gen times and an incubation period 
Fk <- function(t, ninters, genshape, incshape , comscale) {
  return(pgamma(q = t, 
                shape = ninters*genshape+incshape, 
                scale = comscale))}

#Li under right trunctation with k intermediates (Lirt k from eq 1 in C's notes) 
lirtk <- function(maxtime, mintime,k, rtTime, genshape, incshape, comscale, delta = 0.5) {
  if (maxtime == mintime) {maxtime = mintime + delta} # to avoid issues whwne max = min
  Top1 = Fk(maxtime, ninters = k, genshape, incshape, comscale)
  Top2 = Fk(mintime,  ninters = k, genshape, incshape, comscale)
  Bottom = Fk(rtTime, ninters = k, genshape, incshape, comscale)
  return((Top1-Top2)/Bottom)
}

# probability of k intermediates , eq 4 in C's notes 
pk <- function(k,maxinters=n, rate=r, maxtime, mintime) {
  if (k > maxinters)  {return(0)}
  if (k <= maxinters) {
    lam = 0.5*rate*(maxtime+mintime)
    return( dpois(k, lam)/ppois(maxinters, lam))
  }
}

# log of sum over k of pk*lirtk to get log(lirt) (eq 2 of C's notes) 
lirt = function(maxtime, mintime, rtTime, maxinters=n, rate=r,genshape = ag, 
                incshape=ai, comscale = b) {
  pks = vapply(0:maxinters, 
               function(x) pk(x, maxinters = maxinters, rate=rate,
                              maxtime=maxtime,mintime=mintime),
               FUN.VALUE = 0)
  
  
  lirtks = vapply(0:maxinters,
                  function(x) lirtk(maxtime, mintime,k =x, rtTime,
                                    genshape,   incshape, comscale), 
                  FUN.VALUE = 0)
  
  return(log(sum(1e-4+pks*lirtks)))
}