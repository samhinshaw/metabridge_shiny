mappingAlert <- function(alertMsg, mappingError) {
  insertUI(
    selector = "#mappedMetaboliteTable",
    where = "beforeBegin",
    ui = tags$div(
      class = "alert alert-dismissible alert-danger",
      tags$button(
        HTML("&times;"),
        type = "button",
        class = "close",
        `data-dismiss` = "alert"),
      if (identical(mappingError, character(0))) {
        tags$strong('Warning: ')
      } else if (str_detect('error', tolower(paste0(mappingError)))) {
        tags$strong('Error: ')
      } else if (str_detect('warning', tolower(paste0(mappingError)))) {
        tags$strong('Warning: ')
      },
      alertMsg,
      actionLink(
        inputId = 'remap',
        label = 'Try changing your mapping parameters.'
      )
    )
  )
}