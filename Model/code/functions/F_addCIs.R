
#----------

#----------
AddCIs <- function(upper, lower, seq.years, col = 1, segments = FALSE){
  # add CIs to a plot.
  col = adjustcolor(col, alpha.f = 0.1)
  CI.low.t <- lower
  CI.up.t <- upper
  if (!segments){
    for (t in 2:length(seq.years))
      polygon(c(seq.years[t-1], seq.years[t-1], seq.years[t], seq.years[t],seq.years[t-1]),
              c(CI.low.t[t-1], CI.up.t[t-1], CI.up.t[t], CI.low.t[t], CI.low.t[t-1]),
              col=col, border = NA)
  } else {
    for (t in 2:length(seq.years))
      polygon(c(seq.years[t-1], seq.years[t-1], seq.years[t], seq.years[t],seq.years[t-1]),
              c(CI.low.t[t-1], CI.up.t[t-1], CI.up.t[t-1], CI.low.t[t-1], CI.low.t[t-1]),
              #  c(CI.low.t[t], CI.up.t[t], CI.up.t[t], CI.low.t[t], CI.low.t[t]),
              col=col, border = NA)
  }
}

#----------
