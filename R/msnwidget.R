#' Pipe
#'
#' @param x Arbitrary object
#' @param y Arbitrary object
#' @return An arbitrary object
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create an interactive minimum spanning network widget
#'
#' @param data Input text, a file path, a vector of paths, or a dataset object.
#'   Multiple paths are combined into one disconnected dataset.
#' @param kind Input format for a character `data` value. Supported values are
#'   `"fasta"`, `"vcf"`, and `"matrix"`.
#' @param metadata Optional data frame, metadata path, or named list of data
#'   frames/paths. ID columns are detected in JavaScript by matching cells to
#'   loaded sample IDs.
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

msnwidget <- function(name = NULL, data = NULL, kind = NULL, metadata = NULL, options = NULL, datasets = NULL) {
  if (!is.null(datasets) && !is.null(data)) {
    stop("Supply either `data` or `datasets`, not both.", call. = FALSE)
  }

  if (is.null(options)) {
    options <- list()
  } else if (!is.list(options) || is.data.frame(options)) {
    stop("`options` must be a list.", call. = FALSE)
  }
  color_by_list <- character()
  if (!is.null(datasets)) {
    if (!is.list(datasets)) {
      stop("`datasets` must be a list.", call. = FALSE)
    }
    normalized <- lapply(datasets, .normalize_msn_dataset)
    extracted <- lapply(normalized, function(dataset) .extract_msn_color_by(dataset$metadata))
    output <- list(datasets = Map(function(dataset, result) {
      dataset$metadata <- result$metadata
      dataset
    }, normalized, extracted))
    color_by_list <- unique(unlist(lapply(extracted, `[[`, "fields")))
  } else if (is.null(data)) {
    output <- list(dataset = list(name = name %||% "Dataset"))
  } else if (is.list(data) && !is.data.frame(data) && !.is_msn_dataset(data)) {
    parts <- lapply(data, function(item) .normalize_msn_dataset(item, kind))
    extracted <- .extract_msn_color_by(.normalize_msn_metadata(metadata))
    output <- list(dataset = list(
      name = name %||% "Combined dataset",
      datasets = parts,
      metadata = extracted$metadata
    ))
    color_by_list <- extracted$fields
  } else if (length(data) > 1L && identical(kind, "fasta")) {
    fasta_inputs <- lapply(data, function(item) .normalize_msn_dataset(item, kind)$fasta)
    extracted <- .extract_msn_color_by(.normalize_msn_metadata(metadata))
    output <- list(dataset = list(
      name = name %||% "Combined dataset",
      fasta = .combine_msn_fasta(fasta_inputs),
      metadata = extracted$metadata
    ))
    color_by_list <- extracted$fields
  } else if (length(data) > 1L) {
    extracted <- .extract_msn_color_by(.normalize_msn_metadata(metadata))
    output <- list(dataset = list(
      name = name %||% "Combined dataset",
      datasets = lapply(data, function(item) .normalize_msn_dataset(item, kind)),
      metadata = extracted$metadata
    ))
    color_by_list <- extracted$fields
  } else {
    output <- list(dataset = .normalize_msn_dataset(data, kind, metadata))
    output$dataset$name <- name %||% output$dataset$name %||% "Dataset"
    extracted <- .extract_msn_color_by(output$dataset$metadata)
    output$dataset$metadata <- extracted$metadata
    color_by_list <- extracted$fields
  }

  options$color_by_list <- if (length(color_by_list)) color_by_list else NULL

  htmlwidgets::createWidget(
    name = "msnwidget",
    x = c(output, list(options = options)),
    package = "msnwidgetR"
  )
}

.normalize_msn_dataset <- function(data, kind = NULL, metadata = NULL) {
  supported_kinds <- c("fasta", "vcf", "matrix")

  if (is.data.frame(data)) {
    stop("`data` must be text, a file path, or a dataset list; use `metadata` for data frames.", call. = FALSE)
  }

  if (is.list(data) && .is_msn_dataset(data)) {
    dataset <- data
  } else if (is.character(data) && length(data) == 1L && file.exists(data)) {
    dataset <- .read_msn_dataset_path(data, kind)
  } else if (is.character(data) && length(data) == 1L && !is.null(kind)) {
    if (!kind %in% supported_kinds) {
      stop("Unsupported `kind`: ", kind, call. = FALSE)
    }
    dataset <- stats::setNames(list(data), switch(kind, fasta = "fasta", vcf = "vcf", matrix = "distanceMatrix"))
  } else {
    stop("`data` must be input text, a file path, or a dataset list.", call. = FALSE)
  }

  if (!is.null(kind) && is.list(data)) {
    stop("Do not supply `kind` when `data` is already a dataset list.", call. = FALSE)
  }
  if (!is.null(metadata)) {
    dataset$metadata <- .normalize_msn_metadata(metadata)
  } else if (!is.null(dataset$metadata)) {
    dataset$metadata <- .normalize_msn_metadata(dataset$metadata)
  }
  dataset
}

.is_msn_dataset <- function(data) {
  is.list(data) && any(c("name", "fasta", "vcf", "distanceMatrix", "samples", "datasets") %in% names(data))
}

.normalize_msn_metadata <- function(metadata) {
  if (is.data.frame(metadata)) {
    records <- lapply(seq_len(nrow(metadata)), function(i) {
      as.list(metadata[i, , drop = FALSE])
    })
    return(records)
  }

  if (is.character(metadata) && length(metadata) == 1L && file.exists(metadata)) {
    return(list(list(
      name = basename(metadata),
      data = paste(readLines(metadata, warn = FALSE), collapse = "\n")
    )))
  }

  if (is.list(metadata) && length(metadata)) {
    return(lapply(metadata, function(item) {
      if (is.data.frame(item)) {
        return(list(name = "Metadata", data = lapply(seq_len(nrow(item)), function(i) as.list(item[i, , drop = FALSE]))))
      }
      if (is.character(item) && length(item) == 1L && file.exists(item)) {
        return(list(name = basename(item), data = paste(readLines(item, warn = FALSE), collapse = "\n")))
      }
      item
    }))
  }

  metadata
}

.extract_msn_color_by <- function(metadata) {
  fields <- character()
  extract <- function(value) {
    if (is.data.frame(value)) {
      if (!"color_by" %in% names(value)) {
        return(value)
      }
      entries <- as.character(value$color_by)
      fields <<- unique(c(fields, unlist(strsplit(entries[!is.na(entries)], "[;,]"))))
      value$color_by <- NULL
      return(value)
    }
    if (!is.list(value) || !length(value)) return(value)
    if (!is.null(value$data) && length(value) <= 4L) {
      if (is.character(value$data) && any(grepl("(^|[\t,])color_by([\t,]|$)", value$data[1L]))) {
        parsed <- utils::read.table(
          text = paste(value$data, collapse = "\n"),
          header = TRUE,
          sep = if (grepl("\t", value$data[1L], fixed = TRUE)) "\t" else ",",
          quote = "\"",
          comment.char = "",
          check.names = FALSE,
          stringsAsFactors = FALSE,
          na.strings = c("", "NA")
        )
        result <- extract(parsed)
        value$data <- lapply(seq_len(nrow(result)), function(i) as.list(result[i, , drop = FALSE]))
        return(value)
      }
      value$data <- extract(value$data)
      return(value)
    }
    if (all(vapply(value, function(row) is.list(row) && any(c("id", "color_by") %in% names(row)), logical(1)))) {
      return(lapply(value, function(row) {
        if (!is.null(row$color_by)) {
          entries <- as.character(row$color_by)
          fields <<- unique(c(fields, unlist(strsplit(entries[!is.na(entries)], "[;,]"))))
          row$color_by <- NULL
        }
        row
      }))
    }
    lapply(value, extract)
  }
  cleaned <- extract(metadata)
  fields <- trimws(fields[nzchar(trimws(fields))])
  list(metadata = cleaned, fields = unique(fields))
}

.combine_msn_fasta <- function(inputs) {
  parse <- function(text) {
    lines <- unlist(strsplit(sub("^\\ufeff", "", paste(text, collapse = "\n")), "\\r?\\n"))
    headers <- which(grepl("^>", lines))
    if (!length(headers)) stop("FASTA input must contain sequence headers.", call. = FALSE)
    ends <- c(headers[-1L] - 1L, length(lines))
    records <- lapply(seq_along(headers), function(i) {
      header <- sub("^>", "", trimws(lines[headers[i]]))
      id <- strsplit(header, "[[:space:]]+")[[1L]][1L]
      sequence <- paste(gsub("[[:space:]]", "", lines[(headers[i] + 1L):ends[i]]), collapse = "")
      if (!nzchar(id) || !nzchar(sequence)) stop("FASTA records must have IDs and nonempty sequences.", call. = FALSE)
      list(id = id, header = header, sequence = sequence)
    })
    ids <- vapply(records, `[[`, character(1), "id")
    if (anyDuplicated(ids)) stop("FASTA sample IDs must be unique within each input.", call. = FALSE)
    records
  }

  records <- lapply(inputs, parse)
  ids <- vapply(records[[1L]], `[[`, character(1), "id")
  if (any(vapply(records[-1L], function(set) !setequal(ids, vapply(set, `[[`, character(1), "id")), logical(1)))) {
    stop("FASTA alignments being concatenated must contain the same sample IDs.", call. = FALSE)
  }
  by_id <- lapply(records, function(set) setNames(set, vapply(set, `[[`, character(1), "id")))
  paste(vapply(ids, function(id) {
    header <- by_id[[1L]][[id]]$header
    sequence <- paste(vapply(by_id, function(set) set[[id]]$sequence, character(1)), collapse = "")
    paste0(">", header, "\n", sequence)
  }, character(1)), collapse = "\n")
}

.read_msn_dataset_path <- function(path, kind = NULL) {
  lower <- tolower(path)
  if (is.null(kind)) kind <- if (grepl("\\.vcf(\\.gz)?$", lower)) "vcf" else if (grepl("\\.(fa|fasta|fas|fna)(\\.gz)?$", lower)) "fasta" else if (grepl("\\.(json|matrix)$", lower)) "matrix" else NULL
  if (is.null(kind)) stop("Could not infer input type from: ", path, call. = FALSE)
  if (kind == "matrix") return(list(distanceMatrix = jsonlite::fromJSON(path, simplifyVector = FALSE)))
  text <- if (grepl("\\.(vcf|fa|fasta|fas|fna)\\.gz$", lower)) paste(readLines(gzfile(path, open = "rt"), warn = FALSE), collapse = "\n") else paste(readLines(path, warn = FALSE), collapse = "\n")
  stats::setNames(list(text), kind)
}
