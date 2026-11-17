#' @keywords internal

#' @param data A path to variant call (VCF), alignment, or a matrix dataframe.
#' @param metadata Tabular data or a path to tabular data.
#' @param kind A string indicating the type of the input data.
#' @param options A list of other msnwidget options.
#'
#' @import htmlwidgets
#'
#' @export
msnwidget <- function(data = NULL, kind = NULL, metadata = NULL, options = NULL) {
  createWidget(
    name = 'msnwidget',
    x = list(
      data = data,
      metadata = metadata,
      kind = kind,
      options = options
    ),
    package = 'msnwidgetR'
  )
}
