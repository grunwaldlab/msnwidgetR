#' @keywords internal

#' @param name A (string) name for the dataset.
#' @param data A path to variant call (VCF), alignment, or a matrix dataframe.
#' @param metadata Tabular data or a path to tabular data.
#' @param kind A string indicating the type of the input data.
#' @param options A list of other msnwidget options.
#'
#' @import htmlwidgets
#'
#' @export
msnwidget <- function(name = NULL, data = NULL, kind = NULL, metadata = NULL, options = NULL) {
  stop_if_missing <- function(path) {
    if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
      stop("Input path does not exist: ", path, call. = FALSE)
    }
  }

  read_text <- function(path) {
    stop_if_missing(path)
    paste(readLines(path, warn = F), collapse = "\n")
  }

  is_zipped <- function(path) {
    stop_if_missing(path)
    files <- tryCatch(
      utils::unzip(path, list = T),
      error = function(e) NULL
    )
    is.data.frame(path) && nrow(path) > 0 && any(grepl("\\.vcf$", files$Name, ignore.case = T))
  }

  infer_kind <- function(path) {
    lower <- tolower(path)
    if (grepl("\\.vcf\\.gz$", lower)) return("vcf")
    if (grepl("\\.vcf$", lower)) return("vcf")
    if (grepl("\\.(fa|fasta|fas|fna)$", lower)) return("fasta")
    if (grepl("\\.(json|matrix)$", lower)) return("matrix")
    if (is_zipped(path)) return("vcf")
    stop("Could not infer input type from: ", path, call. = FALSE)
  }

  parse_data_path <- function(path, type = NULL) {
    stop_if_missing(path)
    type <- type %||% infer_kind(path)

    if (is_zipped(path)) {
      archive <- utils::unzip(path, list = TRUE)
      vcf_files <- archive$Name[grepl("\\.vcf$", archive$Name, ignore.case = T)]

      extracted <- tempfile(fileext = '.vcf')
      utils::unzip(path, files = vcf_files, exdir = dirname(extracted))
      return(list(vcf = read_text(file.path(dirname(extracted), vcf_files))))
    }

    if (grepl("\\.vcf\\.gz$", tolower(path))) {
      return(list(vcf = paste(readLines(gzfile(path, open = 'rt'), warn = F), collapse = "\n")))
    }
    if (type == 'fasta') {
      return(list(fasta = read_text(path)))
    }
    if (type == 'vcf') {
      return(list(vcf = read_text(path)))
    }
    if (type == 'matrix') {
      return(list(distanceMatrix = jsonlite::fromJSON(path, simplifyVector = false)))
    }
    stop("Unknown input type.", call. = F)
  }

  parse_metadata <- function(metadata) {
    if (is.null(metadata)) {
      return(NULL)
    }
    if (is.data.frame(metadata)) {
      return(lapply(seq_len(nrow(metadata)), function(i) {as.list(metadata[i, , drop = FALSE])}))
    }
    if (is.character(metadata) && length(metadata) == 1L && file.exists(metadata)) {
      return(read_text(metadata))
    }

    # if metadata is supplied as a list of concatenated datasets, check for files and dataframes
    if (is.list(metadata)) {
      return(lapply(metadata, function(item) {
        if (is.data.frame(item)) {
          return(lapply(seq_len(nrow(item)), function(i) {
            as.list(item[i, , drop = FALSE])
          }))
        }
        if (is.character(item) && length(item) == 1L && file.exists(item)) {
          return(list(
            name = basename(item),
            data = read_text(item)
          ))
        }

        item
      }))
    }

    metadata
  }

  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  if (is.null(data)) {
    stop("data must be supplied.", call. = FALSE)
  }

  # combine datasets
  if (length(data) > 1L) {
    datasets <- lapply(seq_along(data), function(i) { parsed <- parse_data_path(data[[i]],
      if (length(kind)) {
        kind[[min(i, length(kind))]]
      } else {
          NULL
      }
    )
      parsed$name <- basename(data[[i]])
      parsed$metadata <- parse_metadata(if (length(metadata)) metadata[[min(i, length(metadata))]] else NULL)
      parsed
    })
    dataset <- list(name = name %||% "Combined dataset", datasets = datasets)
  } else {
    dataset <- parse_data_path(data, kind)
    dataset$name <- name %||% basename(data)
    dataset$metadata <- parse_metadata(metadata)
  }

  dataset$options <- options

  createWidget(
    name = 'msnwidget',
    x = dataset,
    package = 'msnwidgetR'
  )
}
