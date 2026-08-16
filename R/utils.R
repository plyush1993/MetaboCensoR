# ==============================
# Helpers
# ==============================
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

clean_mzmine_export <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(df) > 0 && all(is.na(df[[ncol(df)]]))) df <- df[, -ncol(df), drop = FALSE]
  df[is.na(df)] <- ""
  #df[is.na(df)] <- 0
  #df[df == ""] <- 0
  df
}

format_final_table_as_input <- function(final_df_with_fid, type,
                                        export_template = NULL,
                                        export_colmap = NULL,
                                        export_rt_factor = 1) {
  df <- as.data.frame(final_df_with_fid, check.names = FALSE, stringsAsFactors = FALSE)

  # 1) drop internal columns EXACTLY as requested
  if (identical(type, "mzmine")) {
    df <- df[, setdiff(names(df), ".FID"), drop = FALSE]
  } else {
    df <- df[, setdiff(names(df), c("feature_id", ".FID")), drop = FALSE]
  }

  # 2) restore xcms RT units (undo /60)
  if (is.finite(export_rt_factor) && export_rt_factor != 1 && "rt" %in% names(df)) {
    df$rt <- suppressWarnings(as.numeric(df$rt)) * export_rt_factor
  }

  # 3) rename mz/rt back to original column names (and feature_id if ever needed)
  if (!is.null(export_colmap) && length(export_colmap)) {
    for (std_nm in names(export_colmap)) {
      orig_nm <- export_colmap[[std_nm]]
      if (std_nm %in% names(df) && nzchar(orig_nm)) {
        names(df)[names(df) == std_nm] <- orig_nm
      }
    }
  }

  # 4) reorder columns exactly like original input
  if (!is.null(export_template) && length(export_template)) {
    ord  <- intersect(export_template, names(df))
    rest <- setdiff(names(df), ord)
    df <- df[, c(ord, rest), drop = FALSE]
  }

  df
}

write_final_table_csv <- function(path, df_export, type, msdial_preamble = NULL) {
  df_export <- as.data.frame(df_export, check.names = FALSE, stringsAsFactors = FALSE)

  if (identical(type, "msdial") && !is.null(msdial_preamble) && nrow(msdial_preamble) > 0) {
    pre <- as.data.frame(msdial_preamble, check.names = FALSE, stringsAsFactors = FALSE)

    # match preamble columns to export columns (order + names)
    pre <- pre[, intersect(names(df_export), names(pre)), drop = FALSE]
    pre <- pre[, names(df_export), drop = FALSE]

    cleaned_names <- gsub("_[0-9]+$", "", names(df_export))
    names(df_export) <- cleaned_names

    # 1) preamble WITHOUT header
    write.table(pre, file = path, sep = ",",
                row.names = FALSE, col.names = FALSE,
                quote = TRUE, na = "", append = FALSE)

    # 2) real table WITH header appended
    write.table(df_export, file = path, sep = ",",
                row.names = FALSE, col.names = TRUE,
                quote = TRUE, na = "", append = TRUE)
  } else {
    write.csv(df_export, file = path, row.names = FALSE, quote = TRUE, na = "")
  }
}

multi_sample_idx <- function(cols, kws) {
  kws <- as.character(kws)
  kws <- kws[nzchar(kws)]
  if (!length(kws)) return(integer(0))
  hits <- Reduce(`|`, lapply(kws, function(k) grepl(k, cols, fixed = TRUE)))
  which(hits)
}

calc_rsd <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(m) || m == 0) return(NA_real_)
  (s / m) * 100
}

log10_plus_one_trans <- function() {
  scales::trans_new(
    "log10_plus_one",
    function(x) log10(x + 1),
    function(x) 10^x - 1,
    domain = c(0, Inf)
  )
}

numeric_feature_names <- function(ds) {
  is_num <- vapply(ds, is.numeric, logical(1))
  if ("Label" %in% names(is_num)) is_num["Label"] <- FALSE
  names(ds)[is_num]
}

# ---- Peak Filters helpers ----
calculate_RMD <- function(exact_mass) {
  exact_mass <- suppressWarnings(as.numeric(exact_mass))
  nominal_mass <- floor(exact_mass + 0.5)
  md <- exact_mass - nominal_mass
  (1e6 * md) / exact_mass
}
calculate_AMD <- function(exact_mass, absolute = TRUE) {
  exact_mass <- suppressWarnings(as.numeric(exact_mass))
  nominal_mass <- floor(exact_mass + 0.5)
  md <- exact_mass - nominal_mass
  if (isTRUE(absolute)) abs(md) else md
}

empty_peak_targets <- function() {
  tibble::tibble(
    target_id = character(),
    target_mz = numeric(),
    target_rt = numeric(),
    source = character()
  )
}

empty_peak_matches <- function() {
  tibble::tibble(
    target_id = character(),
    target_mz = numeric(),
    target_rt = numeric(),
    Feature = character(),
    mz = numeric(),
    rt = numeric(),
    delta_mz = numeric(),
    delta_mz_ppm = numeric(),
    delta_rt = numeric(),
    mz_tol_used = numeric(),
    rt_tol_used = numeric(),
    source = character()
  )
}

extract_peak_targets <- function(x, source = "typed") {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(empty_peak_targets())

  x <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  nm <- gsub("[^a-z0-9]", "", tolower(names(x)))

  mz_i <- which(nm %in% c("mz", "moverz", "mass", "targetmz", "mzvalue"))[1]
  rt_i <- which(nm %in% c("rt", "retentiontime", "retentiontimemin", "rtmin", "targetrt"))[1]

  # fallback: first two columns = mz and rt
  if (is.na(mz_i) || is.na(rt_i)) {
    if (ncol(x) >= 2) {
      mz_i <- 1
      rt_i <- 2
    } else {
      return(empty_peak_targets())
    }
  }

  out <- tibble::tibble(
    target_id = paste0(source, "_", seq_len(nrow(x))),
    target_mz = suppressWarnings(as.numeric(x[[mz_i]])),
    target_rt = suppressWarnings(as.numeric(x[[rt_i]])),
    source = source
  )

  out <- out[is.finite(out$target_mz) & is.finite(out$target_rt), , drop = FALSE]
  out
}

read_target_csv_smart <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)

  first_line <- readLines(path, n = 1, warn = FALSE)
  first_line <- trimws(first_line)

  # If first row contains letters, treat it as header.
  # If it is only numbers/separators, treat it as data.
  has_header <- grepl("[A-Za-z]", first_line)

  tryCatch(
    read.csv(
      path,
      header = has_header,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )
}

read_peak_targets <- function(text_input = NULL, upload_input = NULL) {
  out <- list()

  txt <- text_input %||% ""
  txt <- trimws(txt)

  if (nzchar(txt)) {
    first_line <- strsplit(txt, "\n", fixed = TRUE)[[1]][1]
    has_header <- grepl("[A-Za-z]", first_line)

    typed_df <- tryCatch(
      read.csv(
        text = txt,
        header = has_header,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )

    out[["typed"]] <- extract_peak_targets(typed_df, source = "typed")
  }

  if (!is.null(upload_input) && !is.null(upload_input$datapath)) {
    csv_df <- read_target_csv_smart(upload_input$datapath)
    out[["csv"]] <- extract_peak_targets(csv_df, source = "csv")
  }

  if (!length(out)) return(empty_peak_targets())

  dplyr::bind_rows(out) %>%
    dplyr::distinct(target_mz, target_rt, .keep_all = TRUE)
}

find_target_peak_matches <- function(df, rid_col, mz_col, rt_col, targets,
                                     mz_tol_type = "da",
                                     mz_tol_da = 0.005,
                                     mz_tol_ppm = 10,
                                     rt_tol = 0.05) {
  if (is.null(targets) || nrow(targets) == 0) return(empty_peak_matches())
  if (!all(c(rid_col, mz_col, rt_col) %in% names(df))) return(empty_peak_matches())

  mzv <- suppressWarnings(as.numeric(df[[mz_col]]))
  rtv <- suppressWarnings(as.numeric(df[[rt_col]]))
  fid <- as.character(df[[rid_col]])

  mz_tol_type <- mz_tol_type %||% "da"
  mz_tol_da <- suppressWarnings(as.numeric(mz_tol_da %||% 0.005))
  mz_tol_ppm <- suppressWarnings(as.numeric(mz_tol_ppm %||% 10))
  rt_tol <- suppressWarnings(as.numeric(rt_tol %||% 0.05))

  if (!is.finite(mz_tol_da)) mz_tol_da <- 0.005
  if (!is.finite(mz_tol_ppm)) mz_tol_ppm <- 10
  if (!is.finite(rt_tol)) rt_tol <- 0.05

  res <- list()

  for (i in seq_len(nrow(targets))) {
    tmz <- targets$target_mz[i]
    trt <- targets$target_rt[i]

    if (!is.finite(tmz) || !is.finite(trt)) next

    mz_tol_used <- if (identical(mz_tol_type, "ppm")) {
      abs(tmz) * mz_tol_ppm / 1e6
    } else {
      mz_tol_da
    }

    hit <- which(
      is.finite(mzv) &
        is.finite(rtv) &
        abs(mzv - tmz) <= mz_tol_used &
        abs(rtv - trt) <= rt_tol
    )

    if (!length(hit)) next

    res[[length(res) + 1]] <- tibble::tibble(
      target_id = targets$target_id[i],
      target_mz = tmz,
      target_rt = trt,
      Feature = fid[hit],
      mz = mzv[hit],
      rt = rtv[hit],
      delta_mz = mzv[hit] - tmz,
      delta_mz_ppm = 1e6 * (mzv[hit] - tmz) / tmz,
      delta_rt = rtv[hit] - trt,
      mz_tol_used = mz_tol_used,
      rt_tol_used = rt_tol,
      source = targets$source[i]
    )
  }

  if (!length(res)) return(empty_peak_matches())

  dplyr::bind_rows(res) %>%
    dplyr::arrange(target_id, abs(delta_mz), abs(delta_rt))
}

# ---- Pretty summary table UI ----
summary_table_ui <- function(title, rows_df) {
  rows_df <- as.data.frame(rows_df, stringsAsFactors = FALSE)
  stopifnot(all(c("Metric", "Value") %in% names(rows_df)))

  div(
    style = "margin: 1rem 0; padding: .75rem 1rem; border: 2px solid #000; border-radius: 10px; background: rgba(255,255,255,0.92);",
    tags$h3(style="margin-top:0;", title),
    tags$table(class="table table-sm table-bordered",
               tags$thead(tags$tr(tags$th("Metric"), tags$th("Value"))),
               tags$tbody(
                 lapply(seq_len(nrow(rows_df)), function(i) {
                   tags$tr(tags$td(rows_df$Metric[i]), tags$td(rows_df$Value[i]))
                 })
               )
    )
  )
}

# ---- ID building ----
make_internal_ids <- function(raw_df, rid_col, mz_col = NULL, rt_col = NULL) {
  raw_df <- as.data.frame(raw_df, check.names = FALSE, stringsAsFactors = FALSE)
  cols <- names(raw_df)

  rid_col <- if (!is.null(rid_col) && rid_col %in% cols) rid_col else cols[1]
  ids_raw <- trimws(as.character(raw_df[[rid_col]]))

  empty <- which(!nzchar(ids_raw))
  if (length(empty)) {
    if (!is.null(mz_col) && !is.null(rt_col) && mz_col %in% cols && rt_col %in% cols) {
      mzv <- as.character(raw_df[[mz_col]])
      rtv <- as.character(raw_df[[rt_col]])
      ids_raw[empty] <- paste0(mzv[empty], "@", rtv[empty])
    } else {
      ids_raw[empty] <- paste0("feat_", empty)
    }
  }

  ids_unique <- make.unique(ids_raw, sep = "_")
  list(ids_raw = ids_raw, ids_unique = ids_unique, rid_col = rid_col)
}

build_matrix_from_raw <- function(raw_df_fid, sample_keywords = NULL, sample_cols = NULL) {
  raw_df_fid <- as.data.frame(raw_df_fid, check.names = FALSE, stringsAsFactors = FALSE)
  cols <- names(raw_df_fid)
  stopifnot(".FID" %in% cols)

  if (!is.null(sample_cols) && length(sample_cols)) {
    sample_cols <- intersect(sample_cols, cols)
    if (!length(sample_cols)) stop("No sample columns found (explicit selection).")
  } else {
    sample_idx <- multi_sample_idx(cols, sample_keywords)
    if (!length(sample_idx)) stop("No sample columns found. Adjust sample keywords to match headers.")
    sample_cols <- cols[sample_idx]
  }

  df_sub <- raw_df_fid[, sample_cols, drop = FALSE]
  mat <- as.data.frame(data.table::transpose(df_sub), stringsAsFactors = FALSE, check.names = FALSE)
  colnames(mat) <- raw_df_fid$.FID
  rownames(mat) <- sample_cols
  mat[] <- lapply(mat, function(x) {
  num_x <- suppressWarnings(as.numeric(x))
  num_x[is.na(num_x)] <- 0  # <--- Converts NA to 0
  num_x
  })
  mat
}

build_feature_meta <- function(raw_df_fid, mz_col, rt_col, ds_matrix = NULL) {
  raw_df_fid <- as.data.frame(raw_df_fid, check.names = FALSE, stringsAsFactors = FALSE)
  stopifnot(".FID" %in% names(raw_df_fid))
  cols <- names(raw_df_fid)

  mz_col <- if (!is.null(mz_col) && mz_col %in% cols) mz_col else NULL
  rt_col <- if (!is.null(rt_col) && rt_col %in% cols) rt_col else NULL

  mzv <- if (!is.null(mz_col)) suppressWarnings(as.numeric(raw_df_fid[[mz_col]])) else rep(NA_real_, nrow(raw_df_fid))
  rtv <- if (!is.null(rt_col)) suppressWarnings(as.numeric(raw_df_fid[[rt_col]])) else rep(NA_real_, nrow(raw_df_fid))

  ft <- tibble::tibble(
    Feature = raw_df_fid$.FID,
    mz      = mzv,
    rt      = rtv
  )

  if (!is.null(ds_matrix)) {
    ints <- suppressWarnings(colMeans(ds_matrix, na.rm = TRUE))
    ft <- ft %>% left_join(tibble(Feature = names(ints), intensity = as.numeric(ints)), by = "Feature")
  } else {
    ft$intensity <- NA_real_
  }
  ft
}

standardize_peak_table <- function(df, type) {
  type <- match.arg(type, c("mzmine", "default", "xcms", "msdial"))

  df <- as.data.frame(df)
  # Basic name cleanup to start
  names(df) <- trimws(names(df))

  export_template  <- names(df)
  export_colmap    <- c()
  export_rt_factor <- 1
  msdial_preamble <- NULL
  msdial_export_names <- NULL

  # Helper: Normalize strings for soft matching
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

  # Helper: Find a column in the CURRENT names matching a target
  find_col <- function(target, current_names) {
    n_target <- norm(target)
    n_curr   <- norm(current_names)
    idx <- match(n_target, n_curr)
    if (!is.na(idx)) current_names[idx] else NA_character_
  }

  if (type == "mzmine") {
    mz_col <- find_col("row m/z", names(df))
    if (is.na(mz_col)) mz_col <- find_col("mz", names(df))

    rt_col <- find_col("row retention time", names(df))
    if (is.na(rt_col)) rt_col <- find_col("rt", names(df))

    export_colmap <- c(mz = mz_col, rt = rt_col)
    export_template <- names(df)

    if (is.na(mz_col) || is.na(rt_col)) {
      stop("MZmine table missing m/z or RT column.")
    }
    df <- dplyr::rename(df, mz = !!mz_col, rt = !!rt_col)

    id_col <- find_col("row id", names(df))
    if (!is.na(id_col)) {
      export_colmap <- c(export_colmap, feature_id = id_col)
      df$feature_id <- df[[id_col]]
    }

  } else if (type == "default") {
    req_cols <- c("Feature", "mz", "rt")
    miss <- setdiff(req_cols, names(df))
    export_colmap <- c(mz = "mz", rt = "rt")
    export_template <- names(df)
    if (length(miss)) stop("DEFAULT table missing: ", paste(miss, collapse = ", "))
    df <- dplyr::rename(df, mz = `mz`, rt = `rt`)

  } else if (type == "msdial") {
    # 1. Try to find the header row by scanning for "Alignment ID" and "Average Mz"
    #    We assume the file might be read as V1, V2... or have metadata rows.

    header_keywords <- c("Alignment ID", "Average Mz", "Average Rt")

    # Scan the first 30 rows for a row containing ANY of the strict keywords
    hdr_i <- NA
    for (i in 1:min(30, nrow(df))) {
      row_txt <- as.character(unlist(df[i, ]))
      # Check if this row looks like the header
      # We check if "Average Mz" (normalized) is present
      if ("averagemz" %in% norm(row_txt) || "alignmentid" %in% norm(row_txt)) {
        hdr_i <- i
        break
      }
    }

    if (!is.na(hdr_i)) {
      # Store preamble
      if (hdr_i > 1) msdial_preamble <- df[1:(hdr_i - 1), , drop = FALSE]

      # Extract new header names
      new_names <- trimws(as.character(unlist(df[hdr_i, , drop = TRUE])))

      # Handle potential empty names or NAs in header
      new_names[is.na(new_names) | new_names == ""] <- paste0("Unknown_", seq_along(new_names))[is.na(new_names) | new_names == ""]
      new_names <- make.unique(new_names, sep = "_")

      # Apply names to metadata (for reference)
      if (!is.null(msdial_preamble)) {
        # Ensure preamble has same ncol as new_names.
        # If preamble is narrower, pad it? Usually read.csv handles this with 'fill=TRUE'
        if (ncol(msdial_preamble) == length(new_names)) names(msdial_preamble) <- new_names
      }

      msdial_export_names <- new_names
      export_template <- new_names # Update template to matched names

      # Slice the dataframe: Keep rows AFTER header
      if (hdr_i < nrow(df)) {
        df <- df[(hdr_i + 1):nrow(df), , drop = FALSE]
        names(df) <- new_names
      } else {
        # Edge case: Header is last row? Empty data
        df <- df[0, , drop = FALSE]
        names(df) <- new_names
      }
    } else {
      # If header row not found via scan, assume current headers are correct
      msdial_export_names <- names(df)
    }

    # Now identify Mz/Rt columns in the (possibly renamed) dataframe
    mz_col <- find_col("Average Mz", names(df))
    rt_col <- find_col("Average Rt(min)", names(df))

    if (is.na(mz_col) || is.na(rt_col)) {
      stop("MS-DIAL table missing: 'Average Mz' or 'Average Rt(min)'. Check file format.")
    }

    export_colmap <- c(mz = mz_col, rt = rt_col)

    df <- dplyr::rename(df, mz = !!mz_col, rt = !!rt_col)

    # Ensure numeric
    df$mz <- suppressWarnings(as.numeric(df$mz))
    df$rt <- suppressWarnings(as.numeric(df$rt))

    attr(df, "msdial_preamble") <- msdial_preamble
    attr(df, "msdial_export_names") <- msdial_export_names

  } else if (type == "xcms") {
    req_cols <- c("mzmed", "rtmed")
    miss <- setdiff(req_cols, names(df))
    if (length(miss)) stop("XCMS table missing: ", paste(miss, collapse = ", "))

    export_template <- names(df)
    export_colmap <- c(mz = "mzmed", rt = "rtmed")

    df <- dplyr::rename(df, mz = mzmed, rt = rtmed)
    export_rt_factor <- 60
    df$rt <- df$rt / 60
  }

  # Final cleanup
  df$mz <- suppressWarnings(as.numeric(df$mz))
  df$rt <- suppressWarnings(as.numeric(df$rt))
  if (!"feature_id" %in% names(df)) df$feature_id <- seq_len(nrow(df))

  attr(df, "export_template")  <- export_template
  attr(df, "export_colmap")    <- export_colmap
  attr(df, "export_rt_factor") <- export_rt_factor

  df
}

# --------------------------
# Labels parsing / upload helper
# --------------------------
labels_from_sample_names <- function(sample_names, token_sep = "_", token_index = 2, show_notification = TRUE) {
  token_sep <- token_sep %||% "_"
  token_index <- as.integer(token_index %||% 2)

  parts <- strsplit(sample_names, token_sep, fixed = TRUE)
  has_ix <- vapply(parts, function(v) length(v) >= token_index, logical(1))
  if (!all(has_ix)) {
    msg <- sprintf("Warning: Token %d missing in some sample names. Falling back to the full sample name.", token_index)
    warning(msg)
    if (isTRUE(show_notification) &&
    !is.null(shiny::getDefaultReactiveDomain())) {
      shiny::showNotification(msg, type = "warning", duration = 8)
    }
  }
  labs <- vapply(seq_along(parts), function(i) {
    if (has_ix[i] && nzchar(parts[[i]][[token_index]])) {
      parts[[i]][[token_index]]
    } else {
      sample_names[i]
    }
  }, FUN.VALUE = character(1))
  labs
}

make_label_table <- function(sample_names, labels) {
  tibble::tibble(
    Sample = as.character(sample_names),
    Label  = as.character(labels)
  )
}

# --------------------------
# STEP 1: Blank / Media Removal
# --------------------------
compute_blank_keep_mean <- function(ds_with_label, groups, cutoff) {
  stopifnot("Label" %in% names(ds_with_label))
  feats <- numeric_feature_names(ds_with_label)
  if (!length(feats)) return(character(0))
  if (is.null(groups) || !length(groups) || !is.finite(cutoff)) return(feats)

  mean_table <- ds_with_label %>%
    group_by(Label) %>%
    summarise(across(all_of(feats), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  blanks <- mean_table %>%
    filter(Label %in% groups) %>%
    select(-Label) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

  exps <- mean_table %>% filter(!Label %in% groups) %>% select(-Label)
  if (nrow(exps) == 0) return(character(0))

  max_exp <- vapply(exps, function(x) max(x, na.rm = TRUE), numeric(1))
  blank_vec <- as.numeric(blanks[1, ]); names(blank_vec) <- colnames(blanks)

  keep <- names(blank_vec)[blank_vec < cutoff * pmax(max_exp, 0)]
  keep[!is.na(keep)]
}

compute_blank_keep_detected <- function(ds_with_label, groups) {
  stopifnot("Label" %in% names(ds_with_label))
  feats <- numeric_feature_names(ds_with_label)
  if (!length(feats)) return(character(0))
  if (is.null(groups) || !length(groups)) return(feats)

  blanks <- ds_with_label %>% filter(Label %in% groups)
  if (nrow(blanks) == 0) return(feats)

  detected <- vapply(feats, function(f) any(blanks[[f]] > 0, na.rm = TRUE), logical(1))
  feats[!detected]
}

# ==============================
# MS Step 2 helpers
# ==============================

mz_tol_fun_factory <- function(type = c("da","ppm"), mz_tol_da = 0.005, ppm = 10) {
  type <- match.arg(type)
  function(mz_mean) {
    if (type == "ppm") abs(mz_mean) * ppm / 1e6 else mz_tol_da
  }
}

safe_read_csv_any <- function(path_or_upload) {
  # path_or_upload: either a path string or Shiny fileInput list
  if (is.list(path_or_upload) && !is.null(path_or_upload$datapath)) {
    return(as.data.frame(vroom::vroom(path_or_upload$datapath, delim = ",", show_col_types = FALSE)))
  }
  if (is.character(path_or_upload) && length(path_or_upload) == 1 && file.exists(path_or_upload)) {
    return(as.data.frame(vroom::vroom(path_or_upload, delim = ",", show_col_types = FALSE)))
  }
  NULL
}

cor_for_pairs <- function(X, idx1, idx2) {
  # X = numeric matrix [samples x features], idx vectors are same length
  out <- rep(NA_real_, length(idx1))
  for (k in seq_along(idx1)) {
    i <- idx1[k]; j <- idx2[k]
    if (!is.finite(i) || !is.finite(j)) next
    out[k] <- suppressWarnings(stats::cor(X[, i], X[, j], use = "pairwise.complete.obs", method = "pearson"))
  }
  out
}

split_rt_strict <- function(rt, tol) {
  rt <- as.numeric(rt)
  o <- order(rt)
  rts <- rt[o]

  grp <- integer(length(rts))
  g <- 1L
  start <- rts[1]
  grp[1] <- g

  if (length(rts) > 1) {
    for (i in 2:length(rts)) {
      if ((rts[i] - start) > tol) {
        g <- g + 1L
        start <- rts[i]
      }
      grp[i] <- g
    }
  }

  out <- integer(length(rt))
  out[o] <- grp
  out
}

make_rt_pairs <- function(pk, rt_tol) {
  # pk: data.frame with Feature, mz, rt, intensity
  pk <- pk[is.finite(pk$rt) & is.finite(pk$mz), , drop = FALSE]
  if (nrow(pk) <= 1) return(tibble::tibble(Feature1=character(), Feature2=character()))
  o <- order(pk$rt)
  pk <- pk[o, , drop = FALSE]

  edges <- list()
  n <- nrow(pk)
  for (i in 1:(n-1)) {
    j <- i + 1L
    while (j <= n && (pk$rt[j] - pk$rt[i]) <= rt_tol) {
      edges[[length(edges)+1]] <- tibble::tibble(Feature1 = pk$Feature[i], Feature2 = pk$Feature[j])
      j <- j + 1L
    }
  }
  if (!length(edges)) return(tibble::tibble(Feature1=character(), Feature2=character()))
  dplyr::bind_rows(edges)
}

make_mass_shift_edges <- function(pk, shifts, rt_tol, tol_fun) {
  # pk sorted by mz; shifts: name, delta_mz
  pk <- pk[is.finite(pk$mz) & is.finite(pk$rt), , drop = FALSE]
  if (nrow(pk) <= 1 || nrow(shifts) == 0) {
    return(tibble::tibble(Feature1=character(), Feature2=character(), shift=character(),
                          delta_mz=numeric(), delta_mz_target=numeric()))
  }

  o <- order(pk$mz)
  pk <- pk[o, , drop = FALSE]
  max_shift <- max(shifts$delta_mz, na.rm = TRUE)

  edges <- list()
  n <- nrow(pk)
  for (i in 1:(n-1)) {
    j <- i + 1L
    while (j <= n && (pk$mz[j] - pk$mz[i]) <= (max_shift + tol_fun(mean(c(pk$mz[i], pk$mz[j]))))) {
      if (abs(pk$rt[j] - pk$rt[i]) <= rt_tol) {
        dm <- pk$mz[j] - pk$mz[i]
        diffs <- abs(dm - shifts$delta_mz)
        w <- which.min(diffs)
        if (length(w) == 1) {
          tol <- tol_fun(mean(c(pk$mz[i], pk$mz[j])))
          if (diffs[w] <= tol) {
            edges[[length(edges)+1]] <- tibble::tibble(
              Feature1 = pk$Feature[i],
              Feature2 = pk$Feature[j],
              shift    = shifts$name[w],
              delta_mz = dm,
              delta_mz_target = shifts$delta_mz[w]
            )
          }
        }
      }
      j <- j + 1L
    }
  }
  if (!length(edges)) return(tibble::tibble(Feature1=character(), Feature2=character(), shift=character(),
                                            delta_mz=numeric(), delta_mz_target=numeric()))
  dplyr::bind_rows(edges)
}

collapse_components_keep_most_intense <- function(edges_df, pk, group_col = "group_id") {
  # edges_df: Feature1, Feature2
  if (nrow(edges_df) == 0) {
    return(list(
      keep = pk$Feature,
      delete = character(0),
      table = pk %>% dplyr::mutate(!!group_col := NA_integer_, status = "kept"),
      merge_map = tibble::tibble(
        Feature = character(),
        Rep = character(),
        group = integer()
      )
    ))
  }

  g <- igraph::graph_from_data_frame(edges_df[, c("Feature1", "Feature2")], directed = FALSE)
  memb <- igraph::components(g)$membership

  comp_tbl <- tibble::tibble(
    Feature = names(memb),
    !!group_col := as.integer(memb)
  ) %>%
    dplyr::left_join(pk, by = "Feature")

  reps_tbl <- comp_tbl %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::arrange(dplyr::desc(intensity), mz, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      !!group_col := .data[[group_col]],
      Rep = Feature
    )

  comp_tbl2 <- comp_tbl %>%
    dplyr::left_join(reps_tbl, by = group_col)

  reps <- unique(comp_tbl2$Rep)
  all_in_graph <- unique(comp_tbl2$Feature)
  delete <- setdiff(all_in_graph, reps)

  # Features not in graph are kept unchanged
  keep <- union(setdiff(pk$Feature, all_in_graph), reps)

  out_tbl <- pk %>%
    dplyr::left_join(
      comp_tbl2 %>% dplyr::select(Feature, !!group_col, Rep),
      by = "Feature"
    ) %>%
    dplyr::mutate(
      status = dplyr::if_else(
        Feature %in% delete,
        "filtered",
        dplyr::if_else(Feature %in% reps, "keep_rep", "kept")
      )
    )

  merge_map <- comp_tbl2 %>%
    dplyr::transmute(
      Feature = Feature,
      Rep = Rep,
      group = .data[[group_col]]
    )

  list(
    keep = keep,
    delete = delete,
    table = out_tbl,
    merge_map = merge_map
  )
}

make_mispicked_edges <- function(pk, rt_tol, tol_fun) {
  pk <- pk[is.finite(pk$mz) & is.finite(pk$rt), , drop = FALSE]
  if (nrow(pk) <= 1) return(tibble::tibble(Feature1=character(), Feature2=character()))

  o <- order(pk$mz)
  pk <- pk[o, , drop = FALSE]

  edges <- list()
  n <- nrow(pk)
  for (i in 1:(n-1)) {
    j <- i + 1L
    while (j <= n) {
      dm <- pk$mz[j] - pk$mz[i]
      tol <- tol_fun(mean(c(pk$mz[i], pk$mz[j])))
      if (dm > tol) break

      if (abs(pk$rt[j] - pk$rt[i]) <= rt_tol) {
        edges[[length(edges)+1]] <- tibble::tibble(Feature1 = pk$Feature[i], Feature2 = pk$Feature[j])
      }
      j <- j + 1L
    }
  }
  if (!length(edges)) return(tibble::tibble(Feature1=character(), Feature2=character()))
  dplyr::bind_rows(edges)
}

make_ringing_edges <- function(pk, rt_tol, tol_fun, min_anchor_int, max_ratio, bidirectional) {
  pk <- pk[is.finite(pk$mz) & is.finite(pk$rt), , drop = FALSE]

  if (nrow(pk) <= 1) {
    return(tibble::tibble(
      Feature1 = character(),
      Feature2 = character(),
      delta_mz = numeric()
    ))
  }

  o <- order(pk$mz)
  pk <- pk[o, , drop = FALSE]

  edges <- list()
  n <- nrow(pk)

  for (i in 1:(n - 1)) {
    j <- i + 1L

    while (j <= n) {
      dm <- pk$mz[j] - pk$mz[i]

      tol <- tol_fun(mean(c(pk$mz[i], pk$mz[j])))

      if (dm > tol) break

      if (abs(pk$rt[j] - pk$rt[i]) <= rt_tol) {
        int_i <- pk$intensity[i]
        int_j <- pk$intensity[j]

        if (bidirectional) {
          max_int <- max(int_i, int_j, na.rm = TRUE)
          min_int <- min(int_i, int_j, na.rm = TRUE)

          if (is.finite(max_int) &&
              is.finite(min_int) &&
              min_int > 0 &&
              max_int >= min_anchor_int &&
              (max_int / min_int) >= max_ratio) {

            if (int_i >= int_j) {
              edges[[length(edges) + 1]] <- tibble::tibble(
                Feature1 = pk$Feature[i],
                Feature2 = pk$Feature[j],
                delta_mz = dm
              )
            } else {
              edges[[length(edges) + 1]] <- tibble::tibble(
                Feature1 = pk$Feature[j],
                Feature2 = pk$Feature[i],
                delta_mz = -dm
              )
            }
          }

        } else {
          # Strict TOF mode:
          # lower m/z = strong anchor
          # higher m/z = weak ringing artifact
          if (is.finite(int_i) &&
              is.finite(int_j) &&
              int_j > 0 &&
              int_i >= min_anchor_int &&
              (int_i / int_j) >= max_ratio) {

            edges[[length(edges) + 1]] <- tibble::tibble(
              Feature1 = pk$Feature[i],
              Feature2 = pk$Feature[j],
              delta_mz = dm
            )
          }
        }
      }

      j <- j + 1L
    }
  }

  if (!length(edges)) {
    return(tibble::tibble(
      Feature1 = character(),
      Feature2 = character(),
      delta_mz = numeric()
    ))
  }

  dplyr::bind_rows(edges)
}

apply_merge_map_to_matrix <- function(ds, merge_map) {
  if (is.null(merge_map) || nrow(merge_map) == 0) return(ds)

  mm <- as.data.frame(merge_map, stringsAsFactors = FALSE)
  mm <- mm[mm$Feature %in% colnames(ds) & mm$Rep %in% colnames(ds), , drop = FALSE]

  if (!nrow(mm)) return(ds)

  for (rep in unique(mm$Rep)) {
    members <- unique(mm$Feature[mm$Rep == rep])
    members <- intersect(members, colnames(ds))

    if (length(members) <= 1) next

    ds[, rep] <- rowSums(
      as.data.frame(lapply(ds[, members, drop = FALSE], function(x) suppressWarnings(as.numeric(x)))),
      na.rm = TRUE
    )
  }

  ds
}

apply_merge_map_to_raw <- function(raw_df_fid, merge_map, sample_cols) {
  raw_df_fid <- as.data.frame(raw_df_fid, check.names = FALSE, stringsAsFactors = FALSE)

  if (is.null(merge_map) || nrow(merge_map) == 0) return(raw_df_fid)
  if (!".FID" %in% names(raw_df_fid)) return(raw_df_fid)

  sample_cols <- intersect(sample_cols, names(raw_df_fid))
  if (!length(sample_cols)) return(raw_df_fid)

  mm <- as.data.frame(merge_map, stringsAsFactors = FALSE)
  mm <- mm[mm$Feature %in% raw_df_fid$.FID & mm$Rep %in% raw_df_fid$.FID, , drop = FALSE]

  if (!nrow(mm)) return(raw_df_fid)

  for (rep in unique(mm$Rep)) {
    members <- unique(mm$Feature[mm$Rep == rep])
    members <- intersect(members, raw_df_fid$.FID)

    if (length(members) <= 1) next

    rep_idx <- match(rep, raw_df_fid$.FID)
    member_idx <- match(members, raw_df_fid$.FID)

    for (sc in sample_cols) {
      raw_df_fid[rep_idx, sc] <- sum(
        suppressWarnings(as.numeric(raw_df_fid[member_idx, sc])),
        na.rm = TRUE
      )
    }
  }

  # Remove non-representative merged rows
  remove_features <- setdiff(unique(mm$Feature), unique(mm$Rep))
  raw_df_fid <- raw_df_fid[!raw_df_fid$.FID %in% remove_features, , drop = FALSE]

  raw_df_fid
}

built_in_adducts_full <- function() {
  tibble::tibble(
    Name = c("M+H", "M+NH4", "M+Na", "M+K", "M+", "M-H", "M-2H", "M-3H", "M+FA-H", "M+Hac-H", "M-", "M+3H", "M+2H+Na", "M+H+2Na", "M+3Na", "M+2H", "M+H+NH4", "M+H+Na", "M+H+K", "M+ACN+2H", "M+2Na", "M+2ACN+2H", "M+3ACN+2H", "M+CH3OH+H", "M+ACN+H", "M+2Na-H", "M+IsoProp+H", "M+ACN+Na", "M+2K-H", "M+DMSO+H", "M+2ACN+H", "M+IsoProp+Na+H", "2M+H", "2M+NH4", "2M+Na", "2M+3H2O+2H", "2M+K", "2M+ACN+H", "2M+ACN+Na", "M-H2O-H", "M+Na-2H", "M+Cl", "M+K-2H", "M+Br", "M+TFA-H", "2M-H", "2M+FA-H", "2M+Hac-H", "3M-H"),
    calc = c("M+1.007276", "M+18.033823", "M+22.989218", "M+38.963158", "M-0.00054858", "M-1.007276", "M/2-1.007276", "M/3-1.007276", "M+44.998201", "M+59.013851", "M+0.00054858", "M/3+1.007276", "M/3+8.334590", "M/3+15.7661904", "M/3+22.989218", "M/2+1.007276", "M/2+9.520550", "M/2+11.998247", "M/2+19.985217", "M/2+21.520550", "M/2+22.989218", "M/2+42.033823", "M/2+62.547097", "M+33.033489", "M+42.033823", "M+44.971160", "M+61.06534", "M+64.015765", "M+76.919040", "M+79.02122", "M+83.060370", "M+84.05511", "2M+1.007276", "2M+18.033823", "2M+22.989218", "M+28.02312", "2M+38.963158", "2M+42.033823", "2M+64.015765", "M-19.01839", "M+20.974666", "M+34.969402", "M+36.948606", "M+78.918885", "M+112.985586", "2M-1.007276", "2M+44.998201", "2M+59.013851", "3M-1.007276"),
    Charge = c("1", "1", "1", "1", "1", "-1", "-2", "-3", "-1", "-1", "-1", "3", "3", "3", "3", "2", "2", "2", "2", "2", "2", "2", "2", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "-1", "-1", "-1", "-1", "-1", "-1", "-1", "-1", "-1", "-1"),
    Mult = c("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "2", "1", "1", "1", "1", "1", "1", "2", "2", "2", "3"),
    Mass = c("1.007276", "18.033823", "22.989218", "38.963158", "-5.49E-04", "-1.007276", "-1.007276", "-1.007276", "44.998201", "59.013851", "5.49E-04", "1.007276", "8.33459", "15.76619", "22.989218", "1.007276", "9.52055", "11.998247", "19.985217", "21.52055", "22.989218", "42.033823", "62.547097", "33.033489", "42.033823", "44.97116", "61.06534", "64.015765", "76.91904", "79.02122", "83.06037", "84.05511", "1.007276", "18.033823", "22.989218", "28.02312", "38.963158", "42.033823", "64.015765", "-19.01839", "20.974666", "34.969402", "36.948606", "78.918885", "112.985586", "-1.007276", "44.998201", "59.013851", "1.007276"),
    Ion_mode = c("positive", "positive", "positive", "positive", "positive", "negative", "negative", "negative", "negative", "negative", "negative", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "negative", "negative", "negative", "negative", "negative", "negative", "negative", "negative", "negative", "negative"),
    Formula_add = c("H1", "N1H4", "Na1", "K1", "FALSE", "FALSE", "FALSE", "FALSE", "C1O2H2", "C2O2H4", "FALSE", "H3", "H2Na1", "H1Na2", "Na3", "H2", "H1N1H4", "H1Na1", "H1K1", "C2H5N1", "Na2", "C4H8N2", "C6H11N3", "C1H5O1", "C2H4N1", "Na2", "C3H9O1", "C2H3N1Na1", "K2", "C2H7S1O1", "C4H7N2", "C3H9O1Na1", "H1", "N1H4", "Na1", "H8O6", "K1", "C2H4N1", "C2H3N1Na1", "FALSE", "Na1", "Cl1", "K1", "Br1", "C2F3O2H1", "FALSE", "C1O2H2", "C2O2H4", "FALSE"),
    Formula_ded = c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "H1", "H2", "H3", "H1", "H1", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "H1", "FALSE", "FALSE", "H1", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "H3O1", "H2", "FALSE", "H2", "FALSE", "H1", "H1", "H1", "H1", "H1"),
    Multi = c("1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "2 ", "2 ", "2 ", "2 ", "2 ", "2 ", "2 ", "1 ", "1 ", "1 ", "1 ", "1 ", "1 ", "2 ", "2 ", "2 ", "3 ")
  ) |>
    dplyr::mutate(
      Charge = as.integer(Charge),
      Mult   = as.integer(Mult),
      Mass   = as.numeric(Mass),
      Multi  = as.integer(trimws(Multi))
    )
}

built_in_neutral_losses_full <- function() {
  tibble::tibble(
    `Accurate Mass` = c("1.008", "15.023", "15.995", "16.019", "16.031", "17.003", "17.027", "18.011", "20.006", "26.003", "27.011", "27.995", "28.019", "28.031", "29.003", "29.039", "29.998", "30.011", "31.006", "31.018", "31.042", "31.972", "32.026", "33.034", "33.988", "34.969", "35.977", "36.021", "42.011", "42.022", "42.047", "43.006", "43.055", "43.99", "44.037", "44.05", "44.998", "45.021", "45.058", "45.993", "46.005", "47.001", "48.003", "48.009", "48.988", "49.992", "55.006", "55.99", "56.998", "57.021", "57.993", "58.005", "59.073", "59.996", "60.021", "60.021", "61.016", "62", "62.996", "63.962", "63.972", "63.998", "68.995", "71.037", "71.985", "73.004", "73.988", "74", "74.019", "75.032", "75.991", "76.031", "77.039", "77.998", "78.047", "78.918", "79.926", "79.957", "79.966", "80.965", "81.045", "81.972", "83.985", "84.094", "85.028", "85.988", "87.032", "87.991", "90.032", "91.967", "91.986", "93.96", "97.977", "99.98", "102.068", "103.986", "105.025", "107.05", "108.058", "109.011", "111.98", "119.981", "120.042", "121.02", "123.958", "127.912", "129.043", "129.043", "130.063", "132.042", "133.984", "141.019", "146.058", "146.069", "154.003", "156.115", "161.069", "162.053", "163.03", "164.068", "171.09", "172.014", "176.026", "176.032", "178.041", "179.079", "185.009", "189.04", "194.043", "197.045", "203.079", "221.09", "225.077", "228.04", "248.053", "250.062", "260.03", "261.028", "266.064", "273.096", "275.112", "277.056", "306.076", "307.084", "316.056", "341.132"),
    `Neutral Loss` = c("H", "CH3", "O", "NH2", "CH4", "OH", "NH3", "H2O", "HF", "CN", "HCN", "CO", "H2CN", "C2H4", "HCO", "C2H5", "NO", "formaldehyde", "HNO", "CH2OH", "CH3NH2", "S", "CH3OH", "CH3+H2O", "H2S", "Cl", "HCl", "H2O+H2O", "CH2CO", "CH2N2", "C3H6", "HNCO", "C3H7", "CO2", "NH3+HCN", "N(CH3)2", "COOH", "NH3+CO or HCN+H2O", "C2H7N", "NO2", "HCOOH(H2+CO2 or H2O+CO)", "NO+OH", "CH4S", "H2O+NO", "H3PO4 from 2+ charge state", "CH3Cl", "HCN+CO", "CO+CO", "CO+CO+H", "Methyl isocyanate", "NO+CO", "C2H2O2", "trimethylamine", "NO+NO", "CH3COOH", "HCOO+CH3", "CO2+NH3", "H2O+CO2", "HNO3", "SO2", "HCl+CO", "methylsulfenic acid", "CF3", "dehydroalanine", "CO2+CO", "NO2+HCN", "NO2+CO", "H2O+CO+CO", "C3H6S", "Glycine", "NO+NO2", "benzyne", "phenyl radical", "CH2N2+HCl", "benzene", "Br", "HBr", "SO3", "HPO3", "HSO3", "Histidine side chain", "H2SO3", "CO+CO+CO", "C3H6+C3H6", "HCNO+CH2N2", "NO+CO+CO", "C3H5NO2", "NO+NO+CO", "C3H6O3", "CO+CO+HCl", "NO2+NO2", "SO2+NO", "H3PO4", "CO2+CO+CO", "HC(=O)OC4H9", "NO+NO2+CO", "C3H7NOS", "tyrosine side-chain", "tyrosine side-chain+hydrogen", "phenylthio radical", "4CO", "NO2+NO2+CO", "C4H8O4", "Cystein", "SO2+NO+NO", "HI", "Pyroglutamic acid", "C5H7NO3", "Anhydro-dideoxyhexose", "Anhydro-pentose", "NO2+NO+NO+CO", "C2H8O4NP", "Anhydro-deoxyhexose", "Glutamine", "C3H7O5P", "HNE", "Anhydro-aminodeoxyhexose", "Anhydrohexose", "acetylcysteine", "Rhamnose", "C8H13NO3", "C3H9O6P", "CysGly(neg)", "Anhydroglucuronic-acid", "CysGly(pos)", "C6H13NO5", "C3H8NO6P", "C3H12NO6P", "Glucuronic-acid", "C5H12NO5P", "Anhydro-N-acetylglucosamine", "N-acetylglucosamine", "C7H16NO5P", "C6H13O7P", "AnhydromalonylGlc", "gamma-GluCys", "C6H13O9P", "C6H13O9S", "MalonylGlc", "Î³-GluAlaGly-2H", "Î³-GluAlaGly(pos)", "C6H16NO9P", "Glutathione(neg)", "Glutathione", "C9H17O10P", "C12H23NO10"),
    Formula = c("H", "CH3", "O", "NH2", "CH4", "OH", "NH3", "H2O", "HF", "CN", "HCN", "CO", "H2CN", "C2H4", "HCO", "C2H5", "NO", "CH2O", "HNO", "CH3O", "CH5N", "S", "CH4O", "CH5O", "H2S", "Cl", "HCl", "H4O2", "C2H2O", "CH2N2", "C3H6", "CHNO", "C3H7", "CO2", "CH4N2", "C2H6N", "CHO2", "CH3NO", "C2H7N", "NO2", "CH2O2", "HO2N", "CH4S", "H2NO2", "H3O4P/2", "CH3Cl", "C2HNO", "CO+CO", "C2HO2", "C2H3NO", "CNO2", "C2H2O2", "C3H9N", "N2O2", "C2H4O2", "C2H4O2", "CH3NO2", "CH2O3", "HNO3", "SO2", "CHOCl", "CH4OS", "CF3", "C3H5NO", "C2O3", "CHNO2", "CNO3", "C2H2O3", "C3H6S", "C2H5NO2", "N2O3", "C6H4", "C6H5", "CH3N2Cl", "C6H6", "Br", "HBr", "SO3", "HPO3", "HSO3", "C4H5N2", "H2SO3", "C3O3", "C6H12", "C2H3N3O", "C2NO3", "C3H5NO2", "CN2O3", "C3H6O3", "C2HO2Cl", "N2O4", "NO3S", "H3PO4", "C3O4", "C5H10O2", "CN2O4", "C3H7NOS", "C7H7O", "C7H8O", "C6H5S", "4CO", "CN2O5", "C4H8O4", "C3H7NO2S", "N2O4S", "HI", "C5H7NO3", "C5H7NO3", "C6H10O3", "C5H8O4", "CN3O5", "C2H8O4NP", "C6H10O4", "C5H10N2O3", "C3H7O5P", "C9H16O2", "C6H11NO4", "C6H10O5", "C5H9NO3S", "C6H12O5", "C8H13NO3", "C3H9O6P", "C5H8N2O3S", "C6H8O6", "C5H10N2O3S", "C6H13NO5", "C3H8NO6P", "C3H12NO6P", "C6H10O7", "C5H12NO5P", "C8H13NO5", "C8H15NO6", "C7H16NO5P", "C6H13O7P", "C9H12O8", "C8H14N2O5S", "C6H13O9P", "C6H13O9S", "C9H14O9", "C10H15N3O6", "C10H17N3O6", "C6H16NO9P", "C10H16N3O6S", "C10H17N3O6S", "C9H17O10P", "C12H23NO10"),
    Pos = c("-", "+", "-", "+", "-", "+", "+", "+", "-", "-", "+", "+", "-", "-", "-", "-", "+", "+", "-", "+", "+", "-", "+", "-", "+", "-", "+", "-", "+", "-", "+", "+", "+", "+", "-", "-", "+", "+", "+", "+", "+", "-", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "-", "+", "-", "+", "-", "-", "-", "-", "+", "+", "+", "+", "-", "+", "-", "+", "-", "-", "-", "+", "+", "+", "-", "-", "+", "+", "+", "-", "+", "-", "-", "-", "-", "-", "+", "+", "-", "-", "-", "+", "+", "+", "+", "+", "-", "+", "+", "+", "+", "+", "+", "-", "+", "+", "-", "+", "+", "-", "+", "+", "+", "+", "+", "+", "+", "+", "-", "-", "+", "-", "+", "+", "+", "+", "+", "+", "-", "+", "+", "-", "-", "+", "+", "+", "+", "+", "+", "+", "+", "-", "+", "-", "+"),
    Neg = c("-", "-", "-", "-", "-", "+", "-", "-", "-", "-", "+", "+", "+", "-", "-", "-", "+", "+", "+", "-", "-", "-", "-", "+", "-", "-", "-", "-", "-", "+", "-", "-", "-", "+", "-", "-", "-", "-", "-", "+", "+", "+", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "+", "-", "+", "+", "+", "-", "-", "-", "-", "-", "-", "+", "-", "-", "-", "-", "+", "-", "-", "-", "-", "-", "+", "-", "-", "+", "-", "-", "-", "+", "-", "+", "-", "-", "-", "-", "+", "-", "-", "-", "-", "-", "+", "-", "-", "-", "-", "-", "+", "+", "-", "-", "+", "-", "-", "+", "-", "+", "-", "-", "-", "-", "-", "-", "+", "+", "-", "+", "+", "-", "-", "-", "-", "-", "+", "+", "-", "+", "+", "-", "+", "-", "-", "-", "+", "-", "-", "+", "-", "+", "-"),
    `Ion source` = c("ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI/NCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI", "ESI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI", "ESI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI", "ESI", "ESI", "ESI", "ESI/APCI", "ESI", "ESI", "ESI", "ESI/APCI", "ESI/APCI", "ESI/APCI", "ESI", "ESI", "ESI", "ESI/APCI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI", "ESI/APCI", "ESI", "ESI", "ESI/APCI", "ESI", "ESI"),
    `substructure or compound class` = c("Aromatic amines ", "Aromatic N-methylamines, methoxy derivative, tert. Butyl ", "Nitrotoluenes ", "Aromatic amines ", "Methoxy derivatives ", "Nitroaromatic compounds[1], N-oxides[2] ", "Aliph. amines (arom. amines), oximes ", "Carboxylic acids, aldehydes, ester ", "Fluorides ", "Nitrile ", "Amines, aromatic nitrile, aminosulfonic acids ", "aldehydes,ketones, nitroaromatic compounds ", "Aromatic amine ", "Triazines ", "Nitroaromatics, anthraquinones ", "Ethyl derivatives ", "Nitroaromatics ", "Aldehydes* ", "Nitroaromatics ", "Ethanolaminoanthraquinones ", "Aromatic N-methyl groups ", "Sulfur compounds ", "Methyl esters ", "Aromatic methoxy group ", "Thiols* ", "Chlorides ", "Chlorides ", "Dihydroxy compounds ", "N-acetyl derivatives ", "Arginine ", "Triazines ", "Citrulline containing peptides ", "Isopropyl derivatives ", "Carboxylic acids, carbamates ", "Aromatic diamines ", "Aromatic dimethylamines ", "Carboxylic acids ", "Aminoanthraquinones ", "Amitriptyline ", "Nitroaromatics ", "Carboxylic acids ", "Nitroaromatics ", "Methyl sufide* ", "Nitroaldehydes ", "Phosphates of 2+ charge state (especially phosphoserine and phosphothreonine) ", "Chloride adduct of Phosphatidylcholines (PC) ", "Aminoanthrachinones ", "Methoxycarboxylic acids, anthraquinones, hydroxycarboxylic acids ", "Anthraquinones ", "N-methyl carbamate ", "Nitroaromatics, hydroxyaldehydes ", "Naphthoxylcarboxylic acid ", "Phosphatidylcholines (PC) ", "Nitroaromatics ", "Î±,Î²-unsaturated acids* ", "Phosphatidylcholines (PC) ", "Aminocarboxylic acids ", "Carboxylic acids ", "Nitrate group ", "Sulfonic acids, sulfonates ", "Chloroanthraquinones ", "Methionine sulfoxide ", "Aromatic trifluoromethyl ", "Serine residue ", "Hydroxycarboxylic acids ", "Nitroanilines ", "Nitroaldehydes, nitroaromatics ", "Hydroxyanthraquinones ", "Methionine side chain ", "Glycine conjugate, CysGly conjugate, glutathione conjugate ", "Nitroaromatics ", "Phenyl(in deferasirox) ", "Phenyl ", "Triazines ", "Phenyl ", "Bromides ", "Bromides ", "Sulfonic acids ", "Phophates (especially phosphotyrosine and phosphohistidine) ",
                                         "Sulfonic acids ", "Histidine residue ", "Sulfonate group ", "Anthraquinones ", "Triazines ", "Hydroxytriazines ", "Nitroaromatics ", "Phosphatidylserines (PS) ", "Nitroaromatics ", "Metal-monosaccharide complexes ", "Chloroanthraquinones ", "Nitroaromatics ", "Nitrosulfonic acids ", "Phophates (especially phosphoserine and phosphothreonine) ", "Anthraquinone carboxylic acids ", "Butylesters of most amino acids ", "Nitroaromatics ", "Alkylated methionine ", "Peptides with tyrosyl residue ", "Peptides with tyrosyl residue ", "Phenylthio compounds ", "Hydroxyanthraquinones ", "Nitroaromatics ", "Metal-monosaccharide complexes ", "Cysteine conjugates ", "Nitrosulfonic acids ", "Aromatic iodides ", "Aliphatic-GSH(glutathione conjugates) ", "N-Acetylcysteines ", "Dideoxyhexoside ", "Pentoside ", "Nitroaromatics, trinitrocarboxylic acids ", "Phosphatidylethanolamines (PE) or Lyso-PE  ", "Deoxyhexoside ", "Conjugate with gamma-GluCys or glutathione ", "Lysophosphatidic acids (Lyso-PA) ", "4-hydroxy-2-nonenal (HNE)-containing peptides ", "Aminodeoxyhexoside ", "Hexoside ", "N-acetylcysteine conjugate ", "Rhamonoside ", "N-acetyl-L-lysine side chain ", "Lysophosphatidylglycerols (Lyso-PG) ", "Gly-cysteinyl ", "Glucuronides ", "Gly-cysteinyl ", "Monogalactosyldiacylglycerols (MGDG) ", "Phosphatidylserines (PS) ", "Phosphatidylglycerols (PG) ", "Glucuronides (benzylic) ", "Lysophosphatidylethanolamines (Lyso-PE) ", "Conjugate with N-acetylglucosamine (benzylic) ", "Conjugate with N-acetylglucosamine ", "Lysophosphatidylcholines (Lyso-PC) ", "Lysophosphatidylglycerols (Lyso-PG) ", "Malonylglucuronides ", "Conjugate with gamma-GluCys ", "Lysophosphatidylinositols (Lyso-PI) ", "Sulfoquinovosyldiacylglycerols (SQDG) ", "Malonylglucuronides (benzylic) ", "GSH(glutathione conjugates) ", "Aryl-GSH(glutathione conjugates) ", "Phosphatidylinositols (PI) ", "GSH(glutathione conjugates) ", "Glutathione conjugates ", "Lysophosphatidylinositols (Lyso-PI) ", "Digalactosyldiacylglycerols (DGDG) ")
  ) |> dplyr::mutate(`Accurate Mass` = as.numeric(`Accurate Mass`))
}

# --------------------------
# STEP 3: QC value filters
# --------------------------
resolve_labs <- function(ds_with_label, value_groups) {
  if ("Label" %in% names(ds_with_label)) {
    labs_all <- unique(ds_with_label$Label)
    if (!is.null(value_groups) && length(value_groups) > 0) {
      intersect(as.character(value_groups), labs_all)
    } else {
      labs_all
    }
  } else character(0)
}

keep_true_cols <- function(one_row_df) {
  if (!nrow(one_row_df)) return(character(0))
  nm <- names(one_row_df)
  ok <- vapply(one_row_df, function(x) isTRUE(x[[1]]), logical(1))
  nm[ok]
}

keep_by_zeros <- function(ds, feats, mode, labs, zero_metric, zero_cutoff) {
  zcut <- suppressWarnings(as.numeric(zero_cutoff))
  if (!is.finite(zcut)) return(character(0))
  zm <- zero_metric %||% "count"
  if (!zm %in% c("count", "percent")) zm <- "count"
  if (!"Label" %in% names(ds)) mode <- "pooled"

  if (mode == "pooled") {
    ds_sub <- if ("Label" %in% names(ds) && length(labs)) filter(ds, Label %in% labs) else ds
    if (!nrow(ds_sub)) return(character(0))
    vec <- vapply(ds_sub[feats], function(x) {
      if (zm == "percent") mean(x == 0, na.rm = TRUE) * 100 else sum(x == 0, na.rm = TRUE)
    }, numeric(1))
    return(names(vec)[is.finite(vec) & vec <= zcut])
  }

  st <- ds %>%
    group_by(Label) %>%
    summarise(across(all_of(feats),
                     ~ if (zm == "percent") mean(.x == 0, na.rm = TRUE) * 100 else sum(.x == 0, na.rm = TRUE)),
              .groups = "drop") %>%
    filter(Label %in% labs)
  if (!nrow(st)) return(character(0))

  pred <- if (mode == "group_every") all else any
  keep_df <- st %>% select(-Label) %>% summarise(across(everything(), ~ pred(.x <= zcut, na.rm = TRUE)))
  keep_true_cols(keep_df)
}

keep_by_mean <- function(ds, feats, mode, labs, mean_cutoff) {
  mcut <- suppressWarnings(as.numeric(mean_cutoff))
  if (!is.finite(mcut)) return(character(0))
  if (!"Label" %in% names(ds)) mode <- "pooled"

  if (mode == "pooled") {
    ds_sub <- if ("Label" %in% names(ds) && length(labs)) filter(ds, Label %in% labs) else ds
    if (!nrow(ds_sub)) return(character(0))
    vec <- vapply(ds_sub[feats], function(x) mean(x, na.rm = TRUE), numeric(1))
    return(names(vec)[is.finite(vec) & vec >= mcut])
  }

  st <- ds %>%
    group_by(Label) %>%
    summarise(across(all_of(feats), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    filter(Label %in% labs)
  if (!nrow(st)) return(character(0))

  pred <- if (mode == "group_every") all else any
  keep_df <- st %>% select(-Label) %>% summarise(across(everything(), ~ pred(.x >= mcut, na.rm = TRUE)))
  keep_true_cols(keep_df)
}

keep_by_rsd <- function(ds, feats, mode, labs, rsd_cutoff) {
  rcut <- suppressWarnings(as.numeric(rsd_cutoff))
  if (!is.finite(rcut)) return(character(0))
  if (!"Label" %in% names(ds)) mode <- "pooled"

  if (mode == "pooled") {
    ds_sub <- if ("Label" %in% names(ds) && length(labs)) filter(ds, Label %in% labs) else ds
    if (!nrow(ds_sub)) return(character(0))
    vec <- vapply(ds_sub[feats], calc_rsd, numeric(1))
    return(names(vec)[is.finite(vec) & vec <= rcut])
  }

  st <- ds %>%
    group_by(Label) %>%
    summarise(across(all_of(feats), ~ calc_rsd(.x)), .groups = "drop") %>%
    filter(Label %in% labs)
  if (!nrow(st)) return(character(0))

  pred <- if (mode == "group_every") all else any
  #keep_df <- st %>% select(-Label) %>% summarise(across(everything(), ~ pred(.x <= rcut, na.rm = TRUE)))
  keep_df <- st %>%
  select(-Label) %>%
  summarise(across(everything(), ~ {
    x <- .x[is.finite(.x)]
    length(x) > 0 && pred(x <= rcut)
  }))

  keep_true_cols(keep_df)
}

keep_by_min <- function(ds, feats, mode, labs, min_cutoff) {
  mcut <- suppressWarnings(as.numeric(min_cutoff))
  if (!is.finite(mcut)) return(character(0))
  if (!"Label" %in% names(ds)) mode <- "pooled"

  if (mode == "pooled") {
    ds_sub <- if ("Label" %in% names(ds) && length(labs)) filter(ds, Label %in% labs) else ds
    if (!nrow(ds_sub)) return(character(0))
    vec <- vapply(ds_sub[feats], function(x) {
      v <- suppressWarnings(min(x, na.rm = TRUE))
      if (!is.finite(v)) NA_real_ else v
    }, numeric(1))
    return(names(vec)[is.finite(vec) & vec >= mcut])
  }

  st <- ds %>%
    group_by(Label) %>%
    summarise(across(all_of(feats), ~ {
      v <- suppressWarnings(min(.x, na.rm = TRUE))
      if (!is.finite(v)) NA_real_ else v
    }), .groups = "drop") %>%
    filter(Label %in% labs)
  if (!nrow(st)) return(character(0))

  pred <- if (mode == "group_every") all else any
  keep_df <- st %>% select(-Label) %>% summarise(across(everything(), ~ pred(.x >= mcut, na.rm = TRUE)))
  keep_true_cols(keep_df)
}

run_qc_value_filters <- function(ds_with_label, feats_base,
                                filters, mode, value_groups,
                                zero_metric, zero_cutoff, mean_cutoff, rsd_cutoff, min_cutoff) {

  filters <- (filters %||% character(0))
  filters <- filters[filters %in% c("zeros", "mean", "rsd", "min")]

  stats <- list(
    before        = length(feats_base),
    after_final   = length(feats_base),
    removed_zeros = 0L,
    removed_mean  = 0L,
    removed_rsd   = 0L,
    removed_min   = 0L,
    removed_total = 0L
  )

  if (!length(feats_base)) return(list(keep = character(0), stats = stats))
  if (!length(filters)) return(list(keep = feats_base, stats = stats))

  mode <- mode %||% "group_any"
  labs <- resolve_labs(ds_with_label, value_groups)

  kept_lists <- list()

  if ("zeros" %in% filters) {
    k <- keep_by_zeros(ds_with_label, feats_base, mode, labs, zero_metric, zero_cutoff)
    k <- intersect(feats_base, k)
    stats$removed_zeros <- length(setdiff(feats_base, k))
    kept_lists[["zeros"]] <- k
  }
  if ("mean" %in% filters) {
    k <- keep_by_mean(ds_with_label, feats_base, mode, labs, mean_cutoff)
    k <- intersect(feats_base, k)
    stats$removed_mean <- length(setdiff(feats_base, k))
    kept_lists[["mean"]] <- k
  }
  if ("rsd" %in% filters) {
    k <- keep_by_rsd(ds_with_label, feats_base, mode, labs, rsd_cutoff)
    k <- intersect(feats_base, k)
    stats$removed_rsd <- length(setdiff(feats_base, k))
    kept_lists[["rsd"]] <- k
  }
  if ("min" %in% filters) {
    k <- keep_by_min(ds_with_label, feats_base, mode, labs, min_cutoff)
    k <- intersect(feats_base, k)
    stats$removed_min <- length(setdiff(feats_base, k))
    kept_lists[["min"]] <- k
  }

  keep <- if (length(kept_lists)) Reduce(intersect, kept_lists) else feats_base
  stats$after_final <- length(keep)
  stats$removed_total <- stats$before - stats$after_final
  list(keep = keep, stats = stats)
}

# --------------------------
# Final table builder (aligned)
# --------------------------
build_final_feature_table <- function(raw_df_fid,
                                      sample_keywords = NULL,
                                      sample_cols = NULL,
                                      blank_keep = NULL,
                                      merge_map = NULL,
                                      del_ms = character(0),
                                      final_keep = NULL) {

  raw_df_fid <- as.data.frame(raw_df_fid, check.names = FALSE, stringsAsFactors = FALSE)
  cols <- names(raw_df_fid)

  if (!".FID" %in% cols) {
    stop("Internal .FID column is missing in final table builder.")
  }

  # Validate sample columns; optional merging/summing may be applied via merge_map
  if (!is.null(sample_cols) && length(sample_cols)) {
    sample_cols <- intersect(sample_cols, cols)
    if (!length(sample_cols)) {
      stop("No sample columns found for final build (explicit selection).")
    }
  } else {
    sample_idx <- multi_sample_idx(cols, sample_keywords)
    if (!length(sample_idx)) {
      stop("No sample columns found for final build; check sample keywords.")
    }
    sample_cols <- cols[sample_idx]
  }

  raw2 <- raw_df_fid

  # Step 1: blank/media filter
  if (!is.null(blank_keep)) {
    raw2 <- raw2[raw2$.FID %in% blank_keep, , drop = FALSE]
  }

  # Step 2a: optional real merging for selected MS filters
  # Metadata is kept from representative feature; sample intensities are summed.
    if (!is.null(merge_map) && nrow(merge_map) > 0) {
      raw2 <- apply_merge_map_to_raw(raw2, merge_map, sample_cols)
    }

    # Step 2b: remove non-representative redundant/artifact features
    if (length(del_ms)) {
      raw2 <- raw2[!raw2$.FID %in% del_ms, , drop = FALSE]
    }

  # Steps 3/4: QC + peak filters
  if (!is.null(final_keep)) {
    raw2 <- raw2[raw2$.FID %in% final_keep, , drop = FALSE]
  }

  clean_mzmine_export(raw2)
}

# Metadata csv
parse_suffix_list <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))

  x <- as.character(x)
  x <- x[!is.na(x)]

  out <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  out[nzchar(out)]
}

clean_sample_names_for_match <- function(x, enabled = FALSE, remove_suffixes = NULL) {
  x0 <- as.character(x)

  # Always normalize basic invisible/string issues
  out <- x0
  out <- gsub("\u00A0", " ", out, fixed = TRUE)
  out <- gsub("[[:space:]]+", " ", out)
  out <- trimws(out)
  out <- gsub('^"|"$', "", out)

  if (!isTRUE(enabled)) {
    return(out)
  }

  suffixes <- unique(parse_suffix_list(remove_suffixes))
  suffixes <- suffixes[nzchar(suffixes)]

  strip_one_suffix <- function(v, sfx) {
    n <- nchar(sfx)
    if (!is.finite(n) || n < 1) return(v)

    hit <- nchar(v) >= n &
      tolower(substr(v, nchar(v) - n + 1, nchar(v))) == tolower(sfx)

    v[hit] <- substr(v[hit], 1, nchar(v[hit]) - n)
    v <- gsub("[[:space:]]+", " ", v)
    trimws(v)
  }

  for (pass in seq_len(20)) {
    old <- out

    for (sfx in suffixes) {
      out <- strip_one_suffix(out, sfx)
    }

    if (identical(old, out)) break
  }

  out[!nzchar(out)] <- x0[!nzchar(out)]
  out
}

guess_metadata_sample_col <- function(cols) {
  candidates <- c(
    "Sample", "sample",
    "SampleName", "sample_name",
    "Filename", "FileName", "filename",
    "File", "Name", "Injection", "Run"
  )

  hit <- candidates[candidates %in% cols]
  if (length(hit)) hit[1] else cols[1]
}

guess_metadata_label_col <- function(cols, sample_col = NULL) {
  cols2 <- setdiff(cols, sample_col)

  candidates <- c(
    "Condition", "condition",
    "Label", "label",
    "Group", "group",
    "Treatment", "treatment",
    "Class", "class"
  )

  hit <- candidates[candidates %in% cols2]
  if (length(hit)) hit[1] else cols2[1]
}

read_metadata_csv <- function(upload, context = "metadata") {
  req(upload)

  ext <- tolower(tools::file_ext(upload$name))
  validate(need(ext == "csv", paste0("Upload a .csv file for ", context, ".")))

  as.data.frame(
    vroom::vroom(
      upload$datapath,
      delim = ",",
      col_names = TRUE,
      show_col_types = FALSE
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

metadata_labels_by_sample <- function(upload,
                                      sample_names,
                                      sample_col,
                                      label_col,
                                      clean_enabled = FALSE,
                                      remove_suffixes = NULL,
                                      context = "metadata labels") {
  meta <- read_metadata_csv(upload, context = context)

  validate(
    need(sample_col %in% names(meta), "Selected metadata sample-name column was not found."),
    need(label_col %in% names(meta), "Selected metadata label column was not found.")
  )

  app_key <- clean_sample_names_for_match(
    sample_names,
    enabled = clean_enabled,
    remove_suffixes = remove_suffixes
  )

  meta_key <- clean_sample_names_for_match(
    meta[[sample_col]],
    enabled = clean_enabled,
    remove_suffixes = remove_suffixes
  )

  validate(
    need(!anyDuplicated(app_key),
         "Detected sample names are duplicated after optional cleaning."),
    need(!anyDuplicated(meta_key),
         "Metadata sample names are duplicated after optional cleaning.")
  )

  idx <- match(app_key, meta_key)

  if (any(is.na(idx))) {
    missing_samples <- app_key[is.na(idx)]

    validate(
      need(
        FALSE,
        paste0(
          "Metadata file is missing these sample names: ",
          paste(head(missing_samples, 10), collapse = ", "),
          if (length(missing_samples) > 10) " ..." else ""
        )
      )
    )
  }

  labs <- trimws(as.character(meta[[label_col]][idx]))

  validate(
    need(!any(is.na(labs) | labs == ""),
         "Selected metadata label column contains empty values.")
  )

  labs
}
