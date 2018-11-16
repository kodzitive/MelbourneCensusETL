mt <- mtcars
sub <- mtcars[,1:4]
class(mt)
class(sub)
class(mtcars[,1:4]) # all data.frame; good!

sub <- as.tibble(sub)
test_rescale01 <- function(df) {
  ans <- apply(df, 2, function(x)
    {(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T) )} )
  return(ans)
}

(ans1 <- test_rescale01(sub)) # works as expected w pair-wise complete.

sub2  <- sub[1:30,]
sub2[1:2, ] <- NA
(ans2 <- test_rescale01(sub2))
# HAWT. use na.rm=T within ABS_rescale01(apply(function(x, na.rm = T)))


