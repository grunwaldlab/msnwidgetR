#' Create an interactive minimum spanning network widget
#'
#' @param data A character string containing input text or a dataset object.
#'   The supported dataset fields are `fasta`, `vcf`, `compressedVcf`,
#'   `compressedFormat`, and `distanceMatrix`. For a FASTA string, set
#'   `kind = "fasta"`.
#' @param kind Input format for a character `data` value. Supported values are
#'   `"fasta"`, `"vcf"`, and `"matrix"`.
#' @param metadata Optional sample metadata. Provide either a data frame with an
#'   `id` column, a list of row records with an `id` field, or the widget
#'   metadata structure `list(list(name, data, idColumn))`.
#' @param options A list of widget options, such as network metric and
#'   algorithm.
#' @param datasets Optional list of datasets. Each dataset is a named list with
#'   a display `name`, one supported input field (usually `fasta`), and
#'   optional `metadata`. Use this argument to provide multiple selectable
#'   datasets in one widget.
#'
#' @return An `htmlwidget` containing the interactive MSN visualization.
#'
#' @examples
#' fasta <- c(">sample-a", "ACGT", ">sample-b", "ACGA")
#' metadata <- data.frame(id = c("sample-a", "sample-b"), group = c("A", "B"))
#' msnwidget(
#'   data = paste(fasta, collapse = "\n"),
#'   kind = "fasta",
#'   metadata = metadata
#' )
#'
#' @export
msnwidget <- function(data = NULL, kind = NULL, metadata = NULL, options = NULL,
                      datasets = NULL) {
  if (!is.null(datasets) && !is.null(data)) {
    stop("Supply either `data` or `datasets`, not both.", call. = FALSE)
  }

  if (is.null(datasets)) {
    datasets <- if (is.null(data)) {
      list()
    } else {
      list(.normalize_msn_dataset(data, kind, metadata))
    }
  } else {
    if (!is.list(datasets)) {
      stop("`datasets` must be a list of named dataset lists.", call. = FALSE)
    }
    datasets <- lapply(datasets, function(dataset) {
      if (!is.list(dataset)) {
        stop("Each item in `datasets` must be a list.", call. = FALSE)
      }
      .normalize_msn_dataset(dataset, NULL, NULL)
    })
  }

  if (length(datasets) && any(!vapply(datasets, function(x) {
    is.list(x) && any(c("fasta", "vcf", "compressedVcf", "distanceMatrix", "samples") %in% names(x))
  }, logical(1)))) {
    stop(
      "Each dataset must contain one of: `fasta`, `vcf`, `compressedVcf`, ",
      "`distanceMatrix`, or `samples`.",
      call. = FALSE
    )
  }

  htmlwidgets::createWidget(
    name = "msnwidget",
    x = list(
      datasets = datasets,
      options = options
    ),
    package = "msnwidgetR"
  )
}

.normalize_msn_dataset <- function(data, kind = NULL, metadata = NULL) {
  supported_kinds <- c("fasta", "vcf", "matrix")

  if (is.character(data) && length(data) == 1L && !is.null(kind)) {
    if (!kind %in% supported_kinds) {
      stop("Unsupported `kind`: ", kind, call. = FALSE)
    }
    input_field <- switch(
      kind,
      fasta = "fasta",
      vcf = "vcf",
      matrix = "distanceMatrix"
    )
    dataset <- stats::setNames(list(data), input_field)
  } else if (is.list(data)) {
    dataset <- data
    if (!is.null(kind)) {
      stop("Do not supply `kind` when `data` is already a dataset list.", call. = FALSE)
    }
  } else {
    stop(
      "`data` must be FASTA/input text with `kind`, or a dataset list.",
      call. = FALSE
    )
  }

  if (!is.null(metadata)) {
    dataset$metadata <- .normalize_msn_metadata(metadata)
  } else if (!is.null(dataset$metadata)) {
    dataset$metadata <- .normalize_msn_metadata(dataset$metadata)
  }
  dataset
}

.normalize_msn_metadata <- function(metadata) {
  if (is.data.frame(metadata)) {
    if (!"id" %in% names(metadata)) {
      stop("Metadata data frames must contain an `id` column.", call. = FALSE)
    }
    records <- lapply(seq_len(nrow(metadata)), function(i) {
      as.list(metadata[i, , drop = FALSE])
    })
    return(list(list(name = "Sample metadata", data = records, idColumn = "id")))
  }

  if (is.list(metadata) && length(metadata) &&
      all(vapply(metadata, function(row) is.list(row) && "id" %in% names(row), logical(1)))) {
    return(list(list(name = "Sample metadata", data = metadata, idColumn = "id")))
  }

  metadata
}
