left_join.sf <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x",".y"), ...) {
  # sf
    ret <- NextMethod("left_join")
    st_as_sf(ret)
}

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(icon=icon("question"),
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      tags$i(class="icon-question-sign")
    )
  )
}