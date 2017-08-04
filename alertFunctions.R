mappingAlert <- function(message, suggest, status) {
  insertUI(
    selector = "#mapPanelSidebar",
    where = "beforeEnd",
    ui = tags$div(
      id = 'mappingAlert',
      class = if (status == 'error' | status == 'empty') {
        "alert alert-dismissible alert-danger"
      } else if (status == 'warn') {
        "alert alert-dismissible alert-warning"
      } else if (status == 'success') {
        "alert alert-dismissible alert-success"
      },
      tags$button(
        HTML("&times;"),
        type = "button",
        class = "close",
        `data-dismiss` = "alert"),
      message,
      if (!is.null(suggest)) {
        actionLink(
          inputId = 'remap',
          label = suggest)
      },
      if (status == 'warn') {
        "Please contact me on twitter with the details!"
      }
    )
  )
}
