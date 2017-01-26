left_join.sf <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x",".y"), ...) {
    ret <- NextMethod("left_join")
    st_as_sf(ret)
  }