# Sys.setlocale("LC_ALL", "English_United States.1252")
# Sys.setenv(LANG = "en_US.UTF-8")
# options(encoding = "UTF-8")
# options(timeout = 600)
# options(rsconnect.http.timeout = 600)
# rsconnect::deployApp()

# app.R --------------------------------------------------------------------
# MetaboCensoR 

# ==============================
# Libraries
# ==============================
suppressPackageStartupMessages({
  library(shiny)
  library(vroom)
  library(DT)
  library(shinythemes)
  library(shinyjs)
  library(shinyWidgets)
  library(dplyr)
  library(data.table)
  library(tibble)
  library(htmltools)
  library(igraph)
  library(tidyr)
  library(plotly)
  library(scales)
  library(ggplot2)
  library(tools)
  library(Spectra)
  library(MsBackendMgf)
  library(shinycssloaders)
  library(waiter)
  library(shinyBS)
})

options(shiny.maxRequestSize = 1024 * 1024^2)

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
labels_from_sample_names <- function(sample_names, token_sep = "_", token_index = 2) {
  token_sep <- token_sep %||% "_"
  token_index <- as.integer(token_index %||% 2)

  parts <- strsplit(sample_names, token_sep, fixed = TRUE)
  has_ix <- vapply(parts, function(v) length(v) >= token_index, logical(1))
  if (!all(has_ix)) stop(sprintf("Token %d missing in some sample names.", token_index))
  labs <- vapply(parts, function(v) v[[token_index]], FUN.VALUE = character(1))
  if (!all(nzchar(labs))) stop("Parsed empty labels — adjust separator/index.")
  labs
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
    return(list(keep = pk$Feature, delete = character(0),
                table = pk %>% dplyr::mutate(!!group_col := NA_integer_, status="kept")))
  }

  g <- igraph::graph_from_data_frame(edges_df[, c("Feature1","Feature2")], directed = FALSE)
  memb <- igraph::components(g)$membership

  comp_tbl <- tibble::tibble(Feature = names(memb), !!group_col := as.integer(memb)) %>%
    dplyr::left_join(pk, by = "Feature")

  reps <- comp_tbl %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::arrange(dplyr::desc(intensity), mz, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(Feature)

  all_in_graph <- unique(comp_tbl$Feature)
  delete <- setdiff(all_in_graph, reps)

  # features not in graph are kept
  keep <- union(setdiff(pk$Feature, all_in_graph), reps)

  out_tbl <- pk %>%
    dplyr::left_join(comp_tbl %>% dplyr::select(Feature, !!group_col), by="Feature") %>%
    dplyr::mutate(status = dplyr::if_else(Feature %in% delete, "filtered",
                          dplyr::if_else(Feature %in% reps, "keep_rep", "kept")))

  list(keep = keep, delete = delete, table = out_tbl)
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
  keep_df <- st %>% select(-Label) %>% summarise(across(everything(), ~ pred(.x <= rcut, na.rm = TRUE)))
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
build_final_feature_table <- function(raw_df_fid, sample_keywords = NULL, sample_cols = NULL,
                                      blank_keep = NULL,
                                      merge_map = NULL,
                                      del_ms = character(0),
                                      final_keep = NULL) {

  raw_df_fid <- as.data.frame(raw_df_fid, check.names = FALSE, stringsAsFactors = FALSE)
  cols <- names(raw_df_fid)

  if (!is.null(sample_cols) && length(sample_cols)) {
  sample_cols <- intersect(sample_cols, cols)
  if (!length(sample_cols)) stop("No sample columns found for final build (explicit selection).")
  } else {
  sample_idx <- multi_sample_idx(cols, sample_keywords)
  if (!length(sample_idx)) stop("No sample columns found for final build; check sample keywords.")
  sample_cols <- cols[sample_idx]
  }

  if (!is.null(blank_keep) && length(blank_keep)) {
    raw_df_fid <- raw_df_fid[raw_df_fid$.FID %in% blank_keep, , drop = FALSE]
  }
  if (!nrow(raw_df_fid)) return(raw_df_fid)

  # Merge: sum intensities but keep representative metadata
  if (!is.null(merge_map) && nrow(merge_map) > 0) {
    mm <- as.data.frame(merge_map, stringsAsFactors = FALSE)
    grp_map <- setNames(mm$Group, mm$Feature)

    raw_df_fid$.Group <- ifelse(raw_df_fid$.FID %in% names(grp_map), unname(grp_map[raw_df_fid$.FID]), raw_df_fid$.FID)

    reps <- raw_df_fid %>%
      group_by(.Group) %>%
      arrange(desc(as.numeric(rowMeans(across(all_of(sample_cols)), na.rm = TRUE))), .by_group = TRUE) %>%
      slice(1) %>%
      ungroup()

    summed <- raw_df_fid %>%
      group_by(.Group) %>%
      summarise(across(all_of(sample_cols), ~ sum(suppressWarnings(as.numeric(.x)), na.rm = TRUE)), .groups = "drop")

    reps2 <- reps %>% select(-all_of(sample_cols))
    merged_table <- reps2 %>%
      left_join(summed, by = c(".Group" = ".Group")) %>%
      mutate(.FID = .Group) %>%
      select(-.Group)

    raw2 <- merged_table
  } else {
    raw2 <- raw_df_fid
  }

  if (length(del_ms)) raw2 <- raw2[!raw2$.FID %in% del_ms, , drop = FALSE]
  if (!is.null(final_keep) && length(final_keep)) raw2 <- raw2[raw2$.FID %in% final_keep, , drop = FALSE]

  clean_mzmine_export(raw2)
}

# ==============================
# UI ----
# ==============================
ui <- fluidPage(
  use_waiter(),
  use_hostess(),
  theme = shinytheme("flatly"),
  setBackgroundColor(color = c("#43cea2", "#185a9d"), gradient = "linear", direction = "bottom"),
  useShinyjs(),
  
  tags$head(
    tags$title("MetaboCensoR"),
    tags$link(
      rel  = "icon",
      type = "image/png",
      href = "https://raw.githubusercontent.com/plyush1993/MetaboCensoR/main/metabocensor_logo.png"
    ),
    tags$style(HTML("
      .shiny-output-error-validation { color:#000 !important; font-size: 22px !important; font-weight:800 !important; padding:12px; }
      .highlight { background:#fff; border:2px solid #000; color:#000; padding:8px; border-radius:8px; font-weight:bold; }
      .nav-tabs>li>a { font-size: 20px; padding: 12px 18px; }
      .nav-tabs > li > a,
      .nav-tabs > li > a:focus,
      .nav-tabs > li > a:hover {
        color: #000 !important;
        opacity: 1 !important;
        border: 2px solid #000 !important;
        border-radius: 0 !important;
        margin-right: 1px !important;
        background: rgba(255,255,255,0.10) !important;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover{
        border: 4px solid #000 !important;
        background:#2c3e50 !important;
        color:#fff !important;
        border-color:#2c3e50 !important;
      }

      /* 1. Keep the outer container transparent so no white square appears */
      .html-widget.datatables {
        background-color: transparent !important;
      }

      /* 2. Only color the actual table once it is generated */
      .dataTable, .dataTables_scroll {
        background-color: #ffffff !important;
        color: #2c3e50 !important;
        border-radius: 8px !important;
        overflow: hidden !important;
        border: 1px solid #dee2e6 !important;
      }

      /* 3. Ensure the search box and pagination stay visible against the gradient */
      .dataTables_wrapper .dataTables_length, 
      .dataTables_wrapper .dataTables_filter, 
      .dataTables_wrapper .dataTables_info, 
      .dataTables_wrapper .dataTables_paginate {
        color: #000000 !important; 
        font-weight: bold;
        padding: 5px;
      }

      /* 4. Ensure Tooltip is 100% solid and readable */
      .tooltip {
        opacity: 1 !important; /* Forces the outer wrapper to be solid */
      }
      .tooltip.show { opacity: 1 !important; }

      .tooltip-inner {
        background-color: #2c3e50 !important; /* Solid dark blue-gray */
        opacity: 1 !important;               /* Forces the inner box to be solid */
        color: #ffffff !important;
        font-size: 15px !important;
        text-align: left !important;
        max-width: 450px !important;
        border: 2px solid #ffffff;            /* Slightly thicker border helps it pop */
        border-radius: 4px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.3); /* Adds depth over the gradient */
      }
      
      /* Optional: Make the little arrow solid too */
      .tooltip .tooltip-arrow::before {
        opacity: 1 !important;
        border-top-color: #2c3e50 !important;
        border-right-color: #2c3e50 !important;
      }

      :root{
        --cornerW: 110px;
        --logoH:   120px;
        --headerH: 125px;
        --tabsDown: 35px;
      }
      #corner_logo{
        position: absolute;
        top: 3px;
        left: 12px;
        z-index: 99999;
        pointer-events: auto;
      }
      #corner_logo img{
        height: var(--logoH);
        width: auto;
        filter: drop-shadow(0 2px 6px rgba(0,0,0,0.25));
        display: block;
      }
      .tabbable > .nav-tabs{
        padding-left: calc(var(--cornerW) + 20px);
        min-height: var(--headerH);
        padding-top: var(--tabsDown);
        padding-bottom: 10px;
        margin: 0;
        border-bottom: 0 !important;
        background: transparent;
      }
      .tabbable > .tab-content{ padding-top: 10px; border-top: 0 !important; }
      .nav-tabs{ border-bottom: 0 !important; box-shadow: none !important; background-image: none !important; }

      .app-footer{
        position: fixed; left:0; right:0; bottom:0;
        text-align:center;
        font-size: 12px; line-height: 1.2;
        padding: 2px 6px;
        opacity: 0.75;
        background: rgba(255,255,255,0.65);
        border-top: 0px;
        z-index: 9999;
      }
      .app-footer .footer-text{ color: red; font-weight: 600; }
      .app-footer a.footer-link{ color: blue; font-weight: 700; text-decoration: none; }
      .app-footer a.footer-link:hover{ text-decoration: underline; }
      body{ padding-bottom: 22px; }
      
      .btn-gray-red {
        background-color: #ecf0f1 !important; 
        color: #2c3e50 !important; 
        border-color: #000000 !important; 
        border-width: 2px !important;
      }
      
      /* 2. HOVER state when inactive (Slightly darker gray) */
      .btn-gray-red:hover {
        background-color: #e1e8ed !important;
      }
      
      /* 3. ACTIVE/CLICKED state (Danger Red from Flatly theme) */
      .btn-gray-red.active, 
      .btn-gray-red:active, 
      .btn-gray-red.active:hover {
        background-color: #e74c3c !important; 
        color: white !important;
        border-color: #c0392b !important;
        box-shadow: inset 0 3px 5px rgba(0,0,0,0.2) !important;
      }
      
      /* 1. Make the container for the loading bar taller */
    .progress.shiny-file-input-progress {
      height: 20px !important;        /* Increase this number to make it even thicker */
      margin-top: 10px !important;    /* Adds a little breathing room above the bar */
      border-radius: 5px !important;  /* Softens the edges */
    }
    
    /* 2. Make the moving line inside and its text fit the new height */
    .progress.shiny-file-input-progress .progress-bar {
      line-height: 20px !important;   /* Keeps the text vertically centered */
      font-size: 16px !important;     /* Makes the 'Upload complete' text larger */
      font-weight: bold !important;
    }
      
    "))
  ),

  div(
    id = "corner_logo",
    tags$a(
      href   = "https://github.com/plyush1993/MetaboCensoR",
      target = "_blank",
      tags$img(
        src = "https://raw.githubusercontent.com/plyush1993/MetaboCensoR/main/metabocensor_logo.png",
        alt = "MetaboCensoR logo"
      )
    )
  ),

  div(class = "app-footer", HTML('
    <span class="footer-text">by Plyushchenko I.V.</span>
    <span class="footer-sep">&nbsp;|&nbsp;</span>
    <span class="footer-text">2026</span>
    <span class="footer-sep">&nbsp;|&nbsp;</span>
    <a class="footer-link" href="https://github.com/plyush1993/MetaboCensoR" target="_blank">GitHub</a>
    <span class="footer-sep">&nbsp;|&nbsp;</span>
    <a class="footer-link" href="https://www.doi.org/" target="_blank">Cite</a>
  ')),

  tabsetPanel(id = "tabs",

    # =========================
    # TAB 0: Upload Data
    # =========================
    tabPanel("Upload Data", value = "upload",
      sidebarLayout(
        sidebarPanel(
          h3(class = "highlight", "Upload"),
          div(style = "display: flex; align-items: center; margin-bottom: 15px;", 
              div(
                style = "margin-bottom: -15px;", 
          selectInput(
            "data_type0", "Data table type:",
            choices = c(
              "mzMine"   = "mzmine",
              "xcms"     = "xcms",
              "MS-DIAL"  = "msdial",
              "Default"  = "default"
            ),
            selected = "mzmine"
          ),
              ),
              actionButton(
                inputId = "btn", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px; margin-left: 10px; margin-top: -45px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btn", 
                title = "<b>Choose peak table type and then select specified columns</b><br><br>Examples of accessible data are provided in the GitHub repository (see details in About Tab)<br><br>Try the Example dataset by clicking the button to overview the functionality<br><br>Note: The `feature_id` column is auto-generated by default. To avoid conflicts, please do not include a column with this exact name in your uploaded dataset<br><br>Note: We recommend letting the application use its auto-generated `feature_id` to prevent any conflicts during  processing<br><br>Note: for xcms type rt column is automatically converted to min", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          fileInput("file0", "Upload Peak Table (*.csv)", accept = ".csv"),
          tags$hr(),
          h3(class = "highlight", "Global parsing settings"),
          uiOutput("global_controls0"),
          tags$hr(),
          actionButton("clear_shared", "Clear dataset", class = "btn btn-warning"),
          tags$hr(),
          div(style = "position: relative; margin-bottom: 15px;",
              div(
                style = "margin-bottom: -15px;",
          actionButton("load_example", "Example dataset", class = "btn btn-info", icon = icon("upload"))
              ),
              actionButton(
                inputId = "btned", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "position: absolute; top: 12px; left: 165px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btned", 
                title = "<b>Example Dataset</b><br><br>If use this example, run the Blank filter first to save memory for further steps. You can keep all parameters by default.<br><br>The LC-MS profiling dataset, described in the study [DOI: 10.1021/acs.analchem.4c05577] (see details in About Tab). <br><br>Briefly, methanol extracts from the plant ashwagandha [<i>Withania somnifera</i> (L.) Dunal] together with blanks were analyzed on Thermo Q-Exactive Plus Orbitrap in DDA positive mode. Raw mzML files were then processed in mzMine in default pre-settings for UPLC-DDA.", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            )
        ),
        mainPanel(
          uiOutput("upload_tab_error"),
          uiOutput("quick_stats_ui"),
          uiOutput("shared_header"),
          DTOutput("shared_preview")
        )
      )
    ),

    # =========================
    # TAB 1: Blank Filters
    # =========================
    tabPanel("Blank Filters", value = "blank",
      sidebarLayout(
        sidebarPanel(
          h3(class = "highlight", "Sample Label"),
          #helpText("Labels are parsed from sample column names (or uploaded). Used to select Media/Blank groups."),
           div(style = "position: relative; margin-bottom: 15px;", 
              div(
                style = "margin-bottom: -15px;", 
          radioButtons("label_source", "Label source:",
                       c("From sample names" = "from_rows",
                         "From custom CSV (one column, no header)" = "from_custom"),
                       selected = "from_rows")
              ),
              actionButton(
                inputId = "btnl2", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "position: absolute; top: 0px; left: 100px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
          tags$hr(),
              bsTooltip(
                id = "btnl2", 
                title = "<b>Choose Label/Group source for your table</b><br><br>From sample names (token + separator)<br><br>Upload labels in CSV (one column no headers)", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          conditionalPanel(
            condition = "input.label_source == 'from_rows'",
            numericInput("label_index", "Token index", value = 2, min = 1, step = 1),
            textInput("token_sep", "Token separator", value = "_")
          ),
          conditionalPanel(
            condition = "input.label_source == 'from_custom'",
            fileInput("meta_csv", "Upload labels CSV", accept = ".csv")
          ),
          prettyCheckbox("show_labels_table", "Show labels table", TRUE,icon = icon("check"), status = "primary", animation = "jelly"),

          tags$hr(),
          h3(class = "highlight", "Blank / Media Removal"),
          
          div(style = "display: flex; align-items: center; margin-bottom: 15px;", 
              div(
                style = "margin-bottom: -15px;", 
                materialSwitch(
                  inputId = "enable_blank", 
                  label = "Enable Blank filter", 
                  value = FALSE, 
                  status = "danger"
                )
              ),
              actionButton(
                inputId = "btn1", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px; margin-left: 10px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btn1", 
                title = "<b>Filter peaks by presence in Blank group(s)</b><br><br>Select Blank group(s) for removal (at least one)<br><br>By cutoff: keep if Mean(Blank group) < cutoff * Max(Mean(Other group(s))), the default is 0.1, meaning the blank signal should be less than 10% of the signal in the sample group in which it is most abundant<br><br>Drop any: delete any feature detected in Blank group(s)", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          
          conditionalPanel(
            condition = "input.enable_blank",
            uiOutput("blank_controls1")
          ),

          tags$hr(),
          actionButton("plot_blank", "Plot values distribution", class = "btn btn-secondary"),
          br(),
          prettyCheckbox("show_blank_plot", "Show Blank plot panel", TRUE, icon = icon("check"), status = "primary", animation = "jelly"),
          
          tags$hr(),
          actionButton("apply_blank", "Apply", class = "btn btn-info"),
          tags$hr(),
          actionButton("reset_blank", "Clear", class = "btn btn-warning")
        ),
        mainPanel(
          conditionalPanel(
            condition = "!output.sharedUploaded",
            div(class="alert alert-warning text-center", 
            style="font-size: 18px; font-weight: bold; margin-top: 15px;", 
            icon("exclamation-triangle"), " No dataset loaded. Please go to the 'Upload Data' tab.")
          ),
          uiOutput("blank_header_in"),
          DTOutput("blank_table_in"),
          conditionalPanel(condition = "input.show_labels_table", h4("Check matching Sample Names and Group Labels:"), DTOutput("labels_table")),
          conditionalPanel(
            condition = "output.blankPlotReady && input.show_blank_plot",
            h4("Check Blank Ratio Distribution:"),
            withSpinner(plotlyOutput("blank_plot", height = "450px"), 
                        type = 8,      
                        color = "white", 
                        size = 2)
          ),
          uiOutput("blank_header_out"),
          DTOutput("blank_table_out"),
          
          withSpinner(uiOutput("blank_filter_summary"), type = 8, color = "white", size = 2)
        )
      )
    ),

    # =========================
    # TAB 2: MS Filters 
    # =========================
    tabPanel("MS Filters", value = "ms",
      sidebarLayout(
        sidebarPanel(
          #h3(class = "highlight", "Input = output of Blank Filters"),
          #helpText("Step 2 runs sequentially: Isotopes/Dimer -> Adduct families -> Neutral losses -> Fragments (each optional)."),
    
          # -------------------------
          # A) Isotopes / dimer-series
          # -------------------------
          h3(class = "highlight", "A) Isotopes & Dimers"),
          
          div(style = "display: flex; align-items: center; margin-bottom: 12px;",
              div(
                style = "margin-bottom: -15px;", 
          materialSwitch("enable_iso2", "Enable isotope/dimer collapse", TRUE, status = "danger")
              ),
              actionButton(
                inputId = "btnli", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px; margin-left: 8px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnli", 
                title = "<b>Enable isotope and dimer filtering</b><br><br>Define number of C13 isotopes (n) and possible charges (z_max), and dimer seria for C13*(n_d + 0.5), see details in About Tab<br><br>Uses m/z and RT shifts + correlation threshold to detect isotopes/dimers features by graph and retains most intense isotope/dimer in each family<br><br>Note: isotope/dimer family is determined by graph, thus, we recommend to keep the default value of n is 1 and n_d 3", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          
          conditionalPanel(
            condition = "input.enable_iso2",
            numericInput("iso_cor2", "Correlation ≥", value = 0.80, min = 0, max = 1, step = 0.01),
            numericInput("iso_rt2",  "rt tolerance (min)", value = 0.01, min = 0, step = 0.001),
    
            radioButtons("iso_tol_type2", "m/z tolerance type:", choices = c("Da"="da","ppm"="ppm"),
                         selected = "da", inline = TRUE),
            conditionalPanel(
              condition = "input.iso_tol_type2 == 'da'",
              numericInput("iso_mz_tol2", "m/z tolerance (Da)", value = 0.005, min = 0, step = 0.001)
            ),
            conditionalPanel(
              condition = "input.iso_tol_type2 == 'ppm'",
              numericInput("iso_ppm2", "m/z tolerance (ppm)", value = 10, min = 0, step = 0.5)
            ),
    
            numericInput("iso_zmax2", "Max charge (z_max)", value = 3, min = 1, step = 1),
            numericInput("iso_n2",    "Max isotope order (n)", value = 1, min = 1, step = 1),
            numericInput("iso_nd2",   "Dimer-series max n_d", value = 3, min = 1, step = 1)
          ),
    
          tags$hr(),
    
          # -------------------------
          # B) Adduct families
          # -------------------------
          h3(class = "highlight", "B) Adduct families (neutral mass)"),
          div(style = "display: flex; align-items: center; margin-bottom: 12px;",
              div(
                style = "margin-bottom: -15px;", 
          materialSwitch("enable_add2", "Enable adduct-family collapse", TRUE, status = "danger")
              ),
              actionButton(
                inputId = "btnla", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px; margin-left: 8px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnla", 
                title = "<b>Enable adduct filtering</b><br><br>Define polarity and minimal neutral mass<br><br>Uses m/z and RT shifts + correlation threshold to detect adduct features by graph and retains most intense adduct in each family<br><br>Note: By default employs built-in Adducts list (see details in About Tab)<br><br>Note: we recommend to enable `Strict RT split inside clusters` option, that check that adducts always fulfill defined rt tolerance even after grouping by graph", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          conditionalPanel(
            condition = "input.enable_add2",
            radioButtons("add_pol2", "Polarity", choices = c("positive","negative"), selected = "positive", inline = TRUE),
    
            fileInput("adduct_file2", "Adducts CSV (optional)", accept = ".csv", placeholder = "Built-in list"),
            #helpText("If not uploaded, uses a built-in list."),
    
            numericInput("add_cor2", "Correlation ≥", value = 0.80, min = 0, max = 1, step = 0.01),
            numericInput("add_rt2",  "rt tolerance (min)", value = 0.005, min = 0, step = 0.001),
            radioButtons("add_tol_type2", "m/z tolerance type:", choices = c("Da"="da","ppm"="ppm"),
                         selected = "da", inline = TRUE),
            conditionalPanel(
              condition = "input.add_tol_type2 == 'da'",
              numericInput("add_mz_tol2", "m/z tolerance (Da)", value = 0.005, min = 0, step = 0.001)
            ),
            conditionalPanel(
              condition = "input.add_tol_type2 == 'ppm'",
              numericInput("add_ppm2", "m/z tolerance (ppm)", value = 10, min = 0, step = 0.5)
            ),
            numericInput("add_min_nm2", "Minimum neutral mass", value = 50, min = 0, step = 1),
            prettyCheckbox("add_strict_rt2", "Strict RT split inside clusters", T, icon = icon("check"), status = "primary", animation = "jelly")
          ),
         
          tags$hr(),
    
          # -------------------------
          # C) Neutral losses
          # -------------------------
          h3(class = "highlight", "C) Neutral losses"),
          div(style = "display: flex; align-items: center; margin-bottom: 12px;",
              div(
                style = "margin-bottom: -15px;", 
          materialSwitch("enable_nl2", "Enable neutral loss collapse", FALSE, status = "danger")
              ),
              actionButton(
                inputId = "btnlnl", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px; margin-left: 8px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnlnl", 
                title = "<b>Enable neutral loses filtering</b><br><br>Define polarity<br><br>Uses m/z and RT shifts + correlation threshold to detect neutral loses features by graph and retains ion with highest m/z in each family<br><br>Note: By default employs built-in Neutral Losses list (see details in About Tab)<br><br>Note: we recommend to enable `Strict RT split inside clusters` option, that check that fragments always fulfill defined rt tolerance even after grouping by graph", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          conditionalPanel(
            condition = "input.enable_nl2",
            fileInput("nl_file2", "Neutral loss CSV (optional)", accept = ".csv", placeholder = "Built-in list"),
            #helpText("If not uploaded, uses a built-in list."),
    
            radioButtons("nl_pol2", "Polarity", choices = c("positive","negative"), selected = "positive", inline = TRUE),
            numericInput("nl_cor2", "Correlation ≥", value = 0.95, min = 0, max = 1, step = 0.01),
            numericInput("nl_rt2",  "rt tolerance (min)", value = 0.002, min = 0, step = 0.001),
    
            radioButtons("nl_tol_type2", "m/z tolerance type:", choices = c("Da"="da","ppm"="ppm"),
                         selected = "da", inline = TRUE),
            conditionalPanel(
              condition = "input.nl_tol_type2 == 'da'",
              numericInput("nl_mz_tol2", "m/z tolerance (Da)", value = 0.005, min = 0, step = 0.001)
            ),
            conditionalPanel(
              condition = "input.nl_tol_type2 == 'ppm'",
              numericInput("nl_ppm2", "m/z tolerance (ppm)", value = 10, min = 0, step = 0.5)
            ),
    
            prettyCheckbox("nl_strict_rt2", "Strict RT split inside clusters", TRUE,icon = icon("check"), status = "primary", animation = "jelly")
          ),
    
          tags$hr(),
    
          # -------------------------
          # D) Fragments (ISF)
          # -------------------------
          h3(class = "highlight", "D) In-source fragments"),
          div(style = "display: flex; align-items: center; margin-bottom: 12px;",
              div(
                style = "margin-bottom: -15px;",
          materialSwitch("enable_isf2", "Enable in-source fragment collapse", FALSE, status = "danger")
              ),
              actionButton(
                inputId = "btnlf", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px; margin-left: 8px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnlf", 
                title = "<b>Enable in-source fragment filtering</b><br><br>Uses RT shift + correlation threshold to detect in-source fragment features by graph and retains ion with highest m/z in each family<br><br>Note: we recommend to enable `Strict RT split inside clusters` option, that check that fragments always fulfill defined rt tolerance even after grouping by graph<br><br>Note: we recommend to enable `Control intensity ratio` option, that check precursor / fragment intensity ratio", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          conditionalPanel(
            condition = "input.enable_isf2",
            numericInput("isf_cor2", "Correlation ≥", value = 0.95, min = 0, max = 1, step = 0.01),
            numericInput("isf_rt2",  "rt tolerance (min)", value = 0.002, min = 0, step = 0.001),
            prettyCheckbox("isf_strict_rt2", "Strict RT split inside clusters", TRUE,icon = icon("check"), status = "primary", animation = "jelly"),
            prettyCheckbox("isf_control_int2", "Control intensity ratio", TRUE,icon = icon("check"), status = "primary", animation = "jelly"),
            conditionalPanel(
              condition = "input.isf_control_int2",
              numericInput("isf_ratio_min2", "frag/prec ≥", value = 0.001, min = 0, step = 0.001),
              numericInput("isf_ratio_max2", "frag/prec ≤", value = 2.5, min = 0, step = 0.1)
            )
          ),
    
          tags$hr(),
          prettyCheckbox("show_iso_table2", "Show isotope/dimer groups", FALSE,icon = icon("check"), status = "primary", animation = "jelly"),
          prettyCheckbox("show_add_table2", "Show adduct families", FALSE,icon = icon("check"), status = "primary", animation = "jelly"),
          prettyCheckbox("show_nl_table2",  "Show neutral loss hits", FALSE,icon = icon("check"), status = "primary", animation = "jelly"),
          prettyCheckbox("show_isf_table2", "Show fragment clusters", FALSE,icon = icon("check"), status = "primary", animation = "jelly"),
          
          tags$hr(),
          actionButton("apply_ms", "Apply", class = "btn btn-info"),
          tags$hr(),
          actionButton("reset_ms", "Clear", class = "btn btn-warning"),
        ),
    
        mainPanel(
          conditionalPanel(
            condition = "!output.sharedUploaded",
            div(class="alert alert-warning text-center", 
            style="font-size: 18px; font-weight: bold; margin-top: 15px;", 
            icon("exclamation-triangle"), " No dataset loaded. Please go to the 'Upload Data' tab.")
          ),
          conditionalPanel(
            condition = "output.sharedUploaded && !output.msInputReady",
            div(class="highlight", "MS input not ready. Complete Upload first.")
          ),
    
          uiOutput("ms_header_in"),
          DTOutput("ms_table_in"),
    
          uiOutput("ms_header_out"),
          DTOutput("ms_table_out"),
    
          conditionalPanel(condition = "input.show_iso_table2", h4("Isotopes table:"), DTOutput("iso2_table")),
          conditionalPanel(condition = "input.show_add_table2", h4("Adducts table:"), DTOutput("add2_table")),
          conditionalPanel(condition = "input.show_nl_table2",  h4("Neutral Loses table:"), DTOutput("nl2_table")),
          conditionalPanel(condition = "input.show_isf_table2", h4("In-Source Fragments table:"), DTOutput("isf2_table")),
    
          withSpinner(uiOutput("ms_filter_summary"), type = 8, color = "white", size = 2)
        )
      )
    ),

    # =========================
    # TAB 3: QC Filters
    # =========================
    tabPanel("QC Filters", value = "qc",
      sidebarLayout(
        sidebarPanel(
          #h3(class = "highlight", "Input = output of MS Filters"),

          h3(class = "highlight", "Sample Label"),
          div(style = "position: relative; margin-bottom: 15px;",
              div(
                style = "margin-bottom: -15px;", 
          radioButtons("qc_label_source", "Label source:",
                       c("Use labels from Blank step" = "inherit",
                         "From sample names" = "from_rows",
                         "From custom CSV (one column, no header)" = "from_custom"),
                       selected = "inherit")
              ),
              actionButton(
                inputId = "btnllqc", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "position: absolute; top: 0px; left: 100px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnllqc", 
                title = "<b>Choose Label/Group source for your table</b><br><br>From Blank tab<br><br>From sample names (token + separator)<br><br>Upload labels in CSV (one column no headers)", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          tags$hr(),
          conditionalPanel(
            condition = "input.qc_label_source == 'from_rows'",
            numericInput("qc_label_index", "Token index", value = 2, min = 1, step = 1),
            textInput("qc_token_sep", "Token separator", value = "_")
          ),
          conditionalPanel(
            condition = "input.qc_label_source == 'from_custom'",
            fileInput("qc_meta_csv", "Upload labels CSV", accept = ".csv")
          ),
          prettyCheckbox("show_qc_labels_table", "Show labels table", TRUE,icon = icon("check"), status = "primary", animation = "jelly"),

          tags$hr(),
          h3(class = "highlight", "QC value filters"),
          div(style = "position: relative; margin-bottom: 15px;",
              div(
                style = "margin-bottom: -15px;", 
          checkboxGroupButtons(
              inputId = "value_filters",
              label = "Enable filters:",
              choices = c("Zeros" = "zeros", 
                          "Mean" = "mean", 
                          "RSD" = "rsd",
                          "Min" = "min"),
              justified = F, individual = T,
              size = "norm",
              status = "gray-red"
            )
              ),
              actionButton(
                inputId = "btnqcv", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "position: absolute; top: 0px; left: 95px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnqcv", 
                title = "<b>Enable statistical value filtering</b><br><br>Select values for filtering<br><br>Zero values could be by Count or Percentage", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          br(),
          div(style = "position: relative; margin-bottom: 15px;",
              div(
                style = "margin-bottom: -15px;", 
          radioButtons(
            "value_mode", "Evaluation mode:",
            choices = c(
              "Groupwise (ANY selected group passes)"    = "group_any",
              "Groupwise (EVERY selected group passes)"  = "group_every",
              "POOLED across selected groups"            = "pooled"
            ),
            selected = "group_any",
            inline = FALSE
          )
              ),
              actionButton(
                inputId = "btnqcm", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "position: absolute; top: 0px; left: 120px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnqcm", 
                title = "<b>Choose type of filtering for selected groups</b><br><br>Select groups for filtering and specify type of filtering<br><br>ANY-satisfies threshold in at least one of the selected groups<br><br>EVERY-in any of the selected groups<br><br>POOLED-calculates average values among all of the selected groups and then compares<br><br>If no group is selected, all groups are considered", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          br(),
          uiOutput("value_groups_ui"),

          conditionalPanel(condition = "input.value_filters.indexOf('zeros') >= 0",
                           radioButtons("zero_metric", "Zeros metric", inline = TRUE,
                                        choices = c("Count" = "count", "Percent" = "percent"), selected = "count"),
                           numericInput("zero_cutoff", label = HTML("Zeros &le; threshold (count or %)"), value = 0, min = 0, step = 1)
          ),
          conditionalPanel(condition = "input.value_filters.indexOf('mean') >= 0",
                           numericInput("mean_cutoff", label = HTML("Mean &ge; threshold"), value = 5000, min = 0, step = 100)
          ),
          conditionalPanel(condition = "input.value_filters.indexOf('rsd') >= 0",
                           numericInput("rsd_cutoff",  label = HTML("RSD &le; threshold"),  value = 30, min = 0, step = 1)
          ),
          conditionalPanel(condition = "input.value_filters.indexOf('min') >= 0",
                           numericInput("min_cutoff",  label = HTML("Min &ge; threshold"),  value = 1000, min = 0, step = 100)
          ),

          tags$hr(),
          actionButton("plot_values_qc", "Plot values distribution", class = "btn btn-secondary"),
          br(),
          prettyCheckbox("show_qc_plot", "Show QC plot panel", TRUE,icon = icon("check"), status = "primary", animation = "jelly"),
          tags$hr(),
          actionButton("apply_qc", "Apply", class = "btn btn-info"),
          tags$hr(),
          actionButton("reset_qc", "Clear", class = "btn btn-warning")
        ),
        mainPanel(
          conditionalPanel(
            condition = "!output.sharedUploaded",
            div(class="alert alert-warning text-center", 
            style="font-size: 18px; font-weight: bold; margin-top: 15px;", 
            icon("exclamation-triangle"), " No dataset loaded. Please go to the 'Upload Data' tab.")
          ),
          conditionalPanel(
            condition = "output.sharedUploaded && !output.qcInputReady",
            div(class="highlight", "QC input not ready. Complete Upload first.")
          ),

          uiOutput("qc_header_in"),
          DTOutput("qc_table_in"),

          conditionalPanel(condition = "input.show_qc_labels_table", h4("Check matching Sample Names and Group Labels:"), DTOutput("qc_labels_table")),

          conditionalPanel(
            condition = "output.qcPlotReady && input.show_qc_plot",
            h4("Check Values Distribution:"),
            withSpinner(plotlyOutput("qc_value_plot", height = "750px"),
                        type = 8,      
                        color = "white", 
                        size = 2)
          ),

          uiOutput("qc_header_out"),
          DTOutput("qc_table_out"),

          withSpinner(uiOutput("qc_filter_summary"), type = 8, color = "white", size = 2)
        )
      )
    ),

    # =========================
    # TAB 4: Peak Filters
    # =========================
    tabPanel("Peak Filters", value = "peak",
      sidebarLayout(
        sidebarPanel(
          #h3(class = "highlight", "Input = output of QC Filters"),
          #helpText("Filters features by peak meta (m/z, rt, RMD, AMD) after QC."),

          tags$hr(),
          h3(class = "highlight", "Peak filters"),
          
          div(style = "position: relative; margin-bottom: 15px;",
              div(
                style = "margin-bottom: -15px;", 
          checkboxGroupButtons(
              inputId = "peak_filters",
              label = "Enable filters:",
              choices = c("m/z" = "mz", 
                          "rt" = "rt", 
                          "RMD" = "rmd", 
                          "AMD" = "amd"),
              justified = F, individual = T,
              size = "norm",
              status = "gray-red"
            ),
              ),
          actionButton(
                inputId = "btn4", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "position: absolute; top: 0px; left: 100px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btn4", 
                title = "<b>Filter peaks by m/z, rt, or mass defect (absolute or relative) values</b>", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),
          br(),
          conditionalPanel(
            condition = "input.peak_filters.indexOf('mz') >= 0",
            fluidRow(
              column(6, 
                prettyCheckbox("mz_min_enable", "m/z ≥", TRUE, icon = icon("check"), status = "primary", animation = "jelly"),
                conditionalPanel("input.mz_min_enable", numericInput("peak_mz_min", label = NULL, value = 200, min = 0, step = 0.1))
              ),
              column(6, 
                prettyCheckbox("mz_max_enable", "m/z ≤", FALSE, icon = icon("check"), status = "primary", animation = "jelly"),
                conditionalPanel("input.mz_max_enable", numericInput("peak_mz_max", label = NULL, value = 1500, min = 0, step = 0.1))
              )
            )
          ),
          conditionalPanel(
            condition = "input.peak_filters.indexOf('rt') >= 0",
            fluidRow(
              column(6, 
                prettyCheckbox("rt_min_enable", "rt ≥ (min)", TRUE, icon = icon("check"), status = "primary", animation = "jelly"),
                conditionalPanel("input.rt_min_enable", numericInput("peak_rt_min", label = NULL, value = 1, min = 0, step = 0.1))
              ),
              column(6, 
                prettyCheckbox("rt_max_enable", "rt ≤ (min)", FALSE, icon = icon("check"), status = "primary", animation = "jelly"),
                conditionalPanel("input.rt_max_enable", numericInput("peak_rt_max", label = NULL, value = 30, min = 0, step = 0.1))
              )
            )
          ),
          conditionalPanel(
            condition = "input.peak_filters.indexOf('rmd') >= 0",
            fluidRow(
              column(6, 
                prettyCheckbox("rmd_min_enable", "RMD ≥ (ppm)", TRUE, icon = icon("check"), status = "primary", animation = "jelly"),
                conditionalPanel("input.rmd_min_enable", numericInput("peak_rmd_min", label = NULL, value = -2000, min = -1000000, max = 1000000, step = 100))
              ),
              column(6, 
                prettyCheckbox("rmd_max_enable", "RMD ≤ (ppm)", TRUE, icon = icon("check"), status = "primary", animation = "jelly"),
                conditionalPanel("input.rmd_max_enable", numericInput("peak_rmd_max", label = NULL, value =  2000, min = -1000000, max = 1000000, step = 100))
              )
            )
          ),
          conditionalPanel(
            condition = "input.peak_filters.indexOf('amd') >= 0",
            fluidRow(
              column(6, 
                prettyCheckbox("amd_min_enable", "AMD ≥ (Da)", TRUE, icon = icon("check"), status = "primary", animation = "jelly"),
                conditionalPanel("input.amd_min_enable", numericInput("peak_amd_min", label = NULL, value = 0.00, min = -1, max = 1, step = 0.001))
              ),
              column(6, 
                prettyCheckbox("amd_max_enable", "AMD ≤ (Da)", TRUE, icon = icon("check"), status = "primary", animation = "jelly"),
                conditionalPanel("input.amd_max_enable", numericInput("peak_amd_max", label = NULL, value = 0.50, min = -1, max = 1, step = 0.001))
              )
            )
          ),
          tags$hr(),
          actionButton("plot_peak", "Plot values distribution", class = "btn btn-secondary"),
          br(),
          prettyCheckbox("show_peak_plot", "Show Peak plot panel", TRUE,icon = icon("check"), status = "primary", animation = "jelly"),
          tags$hr(),
          actionButton("apply_peak", "Apply", class = "btn btn-info"),
          tags$hr(),
          actionButton("reset_peak", "Clear", class = "btn btn-warning")
        ),
        mainPanel(
          conditionalPanel(
            condition = "!output.sharedUploaded",
            div(class="alert alert-warning text-center", 
            style="font-size: 18px; font-weight: bold; margin-top: 15px;", 
            icon("exclamation-triangle"), " No dataset loaded. Please go to the 'Upload Data' tab.")
          ),
          conditionalPanel(
            condition = "output.sharedUploaded && !output.peakInputReady",
            div(class="highlight", "Peak input not ready. Complete Upload first.")
          ),

          uiOutput("peak_header_in"),
          DTOutput("peak_table_in"),

          conditionalPanel(
            condition = "output.peakPlotReady && input.show_peak_plot",
            h4("Check values map:"),
            withSpinner(plotlyOutput("peak_plot", height = "750px"), 
                        type = 8,      
                        color = "white", 
                        size = 2)
          ),

          uiOutput("peak_header_out"),
          DTOutput("peak_table_out"),

          withSpinner(uiOutput("peak_filter_summary"), type = 8, color = "white", size = 2)
        )
      )
    ),

    # =========================
    # TAB 5: Final Summary
    # =========================
    tabPanel("Final Summary", value = "final",
      sidebarLayout(
        sidebarPanel(
          h3(class = "highlight", "Compile final output"),
          #helpText("Final Output is aligned with all steps. If a step wasn’t applied, it passes through unchanged."),
          br(),
          div(style = "position: relative; margin-bottom: 15px;",
              div(
                style = "margin-bottom: -15px;", 
          actionButton("compile_final", "Compile output", class = "btn btn-info")
              ),
              actionButton(
                inputId = "btnfin", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "position: absolute; top: 12px; left: 140px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnfin", 
                title = "<b>Compile Final Output</b><br><br>Note: after applying, MGF Filtering become available", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            ),

          br(),
          downloadButton("dl_final_table",  "Final peak table (.csv)", class = "btn btn-success"),
          br(),br(),
          h3(class = "highlight", "MGF Filtering"),
          uiOutput("mgf_toggle_ui"), 
          br(),
          conditionalPanel(
            condition = "input.enable_mgf_filter == true",
            fileInput("mgf_input", "Upload MGF file", accept = ".mgf"),
            uiOutput("mgf_matching_ui"),
            #helpText("Defaults to FEATURE_ID. This must match the Feature ID from your table."),
            div(style = "margin-bottom: 15px;",
              actionButton("run_mgf_filter", "Filter MGF", class = "btn btn-info"),
              actionButton("clear_mgf", "Clear MGF", class = "btn btn-warning", style = "margin-left: 10px;")
            ),
            shinycssloaders::withSpinner(
            uiOutput("mgf_status_ui"), 
            type = 7,      # You can choose types 1-8
            color = "#2c3e50", 
            proxy.height = "100px",
            size = 1
          ),
            br(), 
            downloadButton("dl_mgf", "Download Filtered MGF", class = "btn btn-success")
          )
        ),
        mainPanel(
          conditionalPanel(
            condition = "!output.sharedUploaded",
            div(class="alert alert-warning text-center", 
            style="font-size: 18px; font-weight: bold; margin-top: 15px;", 
            icon("exclamation-triangle"), " No dataset loaded. Please go to the 'Upload Data' tab.")
          ),
          conditionalPanel(
            condition = "output.sharedUploaded && !output.finalReady",
            div(class="highlight", "FINAL not compiled yet. Click 'Compile FINAL (aligned)'.")
          ),

          uiOutput("final_report_header"),
          uiOutput("final_report_body"),
          h3("Final table"),
          DTOutput("final_preview_table")
        )
      )
    ),

    # =========================
    # TAB 6: About
    # =========================
    tabPanel("About", value = "help",
    sidebarLayout(
    sidebarPanel(
      h3(class = "highlight", "How to:"),
      selectInput(
        "help_section",
        "Select a section:",
        choices = c(
          "Quick start"   = "quick",
          "Data upload"   = "upload",
          "Blank filters" = "blank",
          "MS filters"    = "ms",
          "QC filters"    = "qc",
          "Peak filters"  = "peak",
          "Final summary" = "final",
          "Troubleshooting" = "trouble",
          "Project Details" = "about"
        ),
        selected = "quick"
      ),
      conditionalPanel(
      condition = "['upload','blank','ms','qc','peak', 'final'].indexOf(input.help_section) >= 0",
      actionButton("help_go_tab", "Go to selected tab", class = "btn btn-primary")
      ),
      conditionalPanel(
      condition = "['upload','blank','ms','qc','peak'].indexOf(input.help_section) < 0",
      )
    ),
    mainPanel(
      div(
        style = "font-size: 18px; line-height: 1.6;",
      uiOutput("help_body")
    )
    )
  )
)
    
  )
)

# ==============================
# Server ----
# ==============================
server <- function(input, output, session) {

  # --------------------------
  # Shared upload hub (Tab 0)
  # --------------------------
  file_state <- reactiveValues(valid = FALSE)
  shared <- reactiveValues(raw = NULL, raw_orig = NULL, name = NULL, data_type = NULL,
                           export_template = NULL, export_colmap = NULL, export_rt_factor = 1,
                           msdial_preamble = NULL, msdial_export_names = NULL)
  
  observeEvent(list(input$file0, input$data_type0), {
    
    # 1. Check if file exists AND is marked as valid (not cleared)
    req(input$file0)
    file_state$valid <- TRUE
    
    w_up <- Waiter$new(
    html = tagList(spin_6(), h4("Reading Table...", style="color:white;")),
    color = "rgba(44, 62, 80, 0.8)"
    )
    w_up$show()
      
    shinyjs::delay(100, {
    tryCatch({

    ext <- tools::file_ext(input$file0$name)
    validate(need(ext == "csv", "Error: Please upload a .csv file."))

    type <- input$data_type0 %||% "mzmine"

    if (type == "msdial") {
      df0 <- as.data.frame(vroom::vroom(input$file0$datapath,
                delim = ",", col_names = FALSE, col_types = vroom::cols(.default = "c"), na = ""))
    } else {
      df0 <- as.data.frame(vroom::vroom(input$file0$datapath, delim = ",", show_col_types = FALSE))
    }

    # Wrap standardization in tryCatch to handle format mismatches gracefully
    df_std <- tryCatch({
      standardize_peak_table(df0, type = type)
    }, error = function(e) {
      # If standardization fails (e.g. wrong type selected), show error but don't crash
      upload_error(paste0("Parsing error: ", e$message))
      return(NULL)
    })
    
    req(df_std) # Stop if parsing failed

    # If successful, clear any previous error
    upload_error(NULL)

    shared$raw_orig  <- df0
    shared$raw       <- df_std
    shared$export_template  <- attr(df_std, "export_template")
    shared$export_colmap    <- attr(df_std, "export_colmap")
    shared$export_rt_factor <- attr(df_std, "export_rt_factor") %||% 1

    shared$msdial_preamble  <- attr(df_std, "msdial_preamble")
    shared$msdial_export_names <- attr(df_std, "msdial_export_names")
    
    shared$name      <- input$file0$name
    shared$data_type <- type
    showNotification("Dataset loaded successfully!", type = "message", duration = 3)
    }, error = function(e) {
      upload_error(paste0("Parsing error: ", e$message))
      showNotification(paste0("Parsing error: ", e$message), type = "error", duration = 5)
    }, finally = {
      # 3. Always hide the waiter when done
      w_up$hide()
    })
  })
}, ignoreInit = TRUE)

observeEvent(input$clear_shared, {
    
    # 1. Reset Upload Data (Shared)
    shared$raw <- NULL
    shared$raw_orig <- NULL
    shared$name <- NULL
    shared$data_type <- NULL
    shared$export_template <- NULL
    shared$export_colmap <- NULL
    shared$export_rt_factor <- 1
    shared$msdial_preamble <- NULL
    shared$msdial_export_names <- NULL
    file_state$valid <- FALSE
    # 2. Reset Error Messages
    upload_error(NULL)
    
    # 3. Reset UI Input (Visual reset of the file bar)
    # This requires useShinyjs() in UI, which you have.
    shinyjs::reset("file0") 
    shinyjs::reset("meta_csv")    
    shinyjs::reset("qc_meta_csv")
    
    # 4. Reset Step 1 (Blank)
    blank_state$applied <- FALSE
    blank_state$show_summary <- FALSE
    blank_state$kept <- NULL
    blank_state$stats <- list(before = 0L, after = 0L, removed_blank = 0L)
    
    # 5. Reset Step 2 (MS)
    ms_state$applied <- FALSE
    ms_state$show_summary <- FALSE
    ms_state$matrix <- NULL
    ms_state$isf_status <- NULL
    
    ms_state$del_iso <- character(0)
    ms_state$del_add <- character(0)
    ms_state$del_nl  <- character(0)
    ms_state$del_isf <- character(0)
    
    ms_state$iso_table <- NULL
    ms_state$add_table <- NULL
    ms_state$nl_table  <- NULL
    ms_state$isf_table <- NULL
    
    ms_state$stats <- list(before=0L, after=0L, removed_iso=0L, removed_add=0L, removed_nl=0L, removed_isf=0L)
    
    # 6. Reset Step 3 (QC)
    qc_state$applied <- FALSE
    qc_state$show_summary <- FALSE
    qc_state$kept <- NULL
    qc_state$stats <- list(before = 0L, after_final = 0L,
                           removed_zeros = 0L, removed_mean = 0L, removed_rsd = 0L, removed_min = 0L, removed_total = 0L)
    
    # 7. Reset Step 4 (Peak)
    peak_state$applied <- FALSE
    peak_state$show_summary <- FALSE
    peak_state$kept <- NULL
    peak_state$stats <- list(before=0L, after=0L,
                             removed_mz=0L, removed_rt=0L, removed_rmd=0L, removed_amd=0L, removed_total=0L)
    
    # 8. Reset Final Results
    final_table(NULL)

    # Reset mgf processing
    shinyjs::reset("mgf_input")
    current_mgf(NULL)
    parsed_mgf(NULL)
    mgf_true_names(NULL)
    filtered_mgf_sps(NULL)
    mgf_processing(FALSE)
    mgf_attempted(FALSE)
    
    # 10. Notification
    showNotification("Dataset, files and all processing steps have been completely cleared.", type = "warning", duration = 4)
    
  }, ignoreInit = TRUE)

  observeEvent(input$load_example, {
    file_state$valid <- TRUE
    
    w_up <- Waiter$new(
      html = tagList(spin_6(), h4("Loading Example Dataset...", style="color:white;")),
      color = "rgba(44, 62, 80, 0.8)"
    )
    w_up$show()
    
    shinyjs::delay(100, {
      tryCatch({
        # 1. Update the dropdown visually so the user knows it's an mzMine file
        updateSelectInput(session, "data_type0", selected = "mzmine")
        
        # 2. Use the RAW GitHub URL (vroom needs the raw CSV, not the HTML page)
        example_url <- "https://raw.githubusercontent.com/plyush1993/MetaboCensoR/main/Input_Examples/orbi_iimn_gnps_quant.csv"
        
        # 3. Read data directly from GitHub
        df0 <- as.data.frame(vroom::vroom(example_url, delim = ",", show_col_types = FALSE))
        
        # 4. Standardize
        type <- "mzmine"
        df_std <- standardize_peak_table(df0, type = type)
        
        req(df_std) # Stop if parsing failed
        
        # 5. Clear any previous errors
        upload_error(NULL)
        
        # 6. Populate the shared reactiveValues just like a normal upload
        shared$raw_orig  <- df0
        shared$raw       <- df_std
        shared$export_template  <- attr(df_std, "export_template")
        shared$export_colmap    <- attr(df_std, "export_colmap")
        shared$export_rt_factor <- attr(df_std, "export_rt_factor") %||% 1
        
        shared$msdial_preamble  <- attr(df_std, "msdial_preamble")
        shared$msdial_export_names <- attr(df_std, "msdial_export_names")
        
        shared$name      <- "Example"
        shared$data_type <- type
        
        showNotification("Example dataset loaded successfully!", type = "message", duration = 3)
        
      }, error = function(e) {
        upload_error(paste0("Error loading example dataset: ", e$message))
      }, finally = {
        w_up$hide()
      })
    })
  })

  output$shared_header <- renderUI({
    req(shared$raw)
    h3(sprintf("Dataset: %s", shared$name))
  })

  output$shared_preview <- renderDT({
    req(shared$raw)
    df_show <- clean_mzmine_export(shared$raw)
    
    colmap <- shared$export_colmap
    if (!is.null(colmap)) {
      colmap <- colmap[names(colmap) != "feature_id"]
      for (std_nm in names(colmap)) {
        orig_nm <- colmap[[std_nm]]
        if (std_nm %in% names(df_show) && nzchar(orig_nm)) {
          names(df_show)[names(df_show) == std_nm] <- orig_nm
        }
      }
    }
    
    datatable(df_show, options = list(scrollX = TRUE, pageLength = 6))
  })

  output$quick_stats_ui <- renderUI({
    # Wait until the raw data is loaded
    req(shared$raw)
    
    # Safely try to get sample columns (won't display if there's a keyword error)
    sc <- try(sample_cols0(), silent = TRUE)
    if (inherits(sc, "try-error")) return(NULL)
    
    n_features <- format(nrow(as.data.frame(shared$raw)), big.mark = ",")
    n_samples  <- format(length(sc), big.mark = ",")
    
    # Render two side-by-side dashboard metric cards
    div(style = "display: flex; gap: 15px; margin-bottom: 15px; margin-top: 5px;",
        div(style = "flex: 1; background-color: #f8f9fa; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #EE2C2C; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
            h5("Total Samples Detected", style = "margin: 0 0 5px 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h3(n_samples, style = "margin: 0; color: #EE2C2C; font-weight: 800; font-size: 28px;")
        ),
        div(style = "flex: 1; background-color: #f8f9fa; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #3498db; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
            h5("Total Features (Peaks)", style = "margin: 0 0 5px 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h3(n_features, style = "margin: 0; color: #3498db; font-weight: 800; font-size: 28px;")
        )
    )
  })
  
  output$global_controls0 <- renderUI({
    req(shared$raw)
    df <- as.data.frame(shared$raw, check.names = FALSE)
    cols_std <- names(df)

    # Create named choices: UI shows original names, server receives standard names
    display_names <- cols_std
    colmap <- shared$export_colmap
    if (!is.null(colmap)) {
      colmap <- colmap[names(colmap) != "feature_id"]
      for (std_nm in names(colmap)) {
        orig_nm <- colmap[[std_nm]]
        idx <- match(std_nm, cols_std)
        if (!is.na(idx) && nzchar(orig_nm)) {
          display_names[idx] <- orig_nm
        }
      }
    }
    # setNames creates a vector like c("mzmed" = "mz", "rtmed" = "rt")
    choices_list <- setNames(cols_std, display_names)

    default_rid <- if ("feature_id" %in% cols_std) "feature_id" else cols_std[1]
    default_mz  <- if ("mz" %in% cols_std) "mz" else cols_std[1]
    default_rt  <- if ("rt" %in% cols_std) "rt" else cols_std[1]

    tagList(
      selectInput("row_id_col0", "Feature ID (auto-generated):", choices = choices_list, selected = default_rid),
      selectInput("mz_col0",     "m/z column:", choices = choices_list, selected = default_mz),
      selectInput("rt_col0",     "rt column:",  choices = choices_list, selected = default_rt),
      radioButtons(
        "sample_mode0", "How to define sample columns?",
        choices = c(
          "Auto-detect numeric sample columns" = "auto",
          "By keyword match" = "kws",
          "Pick columns manually" = "manual"
        ),
        selected = "kws"
      ),

      conditionalPanel(
        condition = "input.sample_mode0 == 'kws'",
        selectizeInput(
          "sample_keyword0",
          "Sample column keywords (pick/add multiple):",
          choices  = c(".mzML", ".mzXML", ".raw", "_Area", "_Height", "Area", "Height"),
          selected = c(".mzML", ".mzXML"),
          multiple = TRUE,
          options  = list(create = TRUE, createOnBlur = TRUE,
                          placeholder = "Type to add (e.g. _Area) and press Enter")
        )
      ),

      conditionalPanel(
        condition = "input.sample_mode0 == 'manual'",
        selectizeInput(
          "sample_cols0",
          "Pick sample columns:",
          choices = choices_list,
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select all sample intensity columns")
        )
      ),

    )
  })

  output$sharedUploaded <- reactive({ !is.null(shared$raw) && nrow(as.data.frame(shared$raw)) > 0 })
  outputOptions(output, "sharedUploaded", suspendWhenHidden = FALSE)

  output$msInputReady <- reactive({
  !is.null(shared$raw) && (
    (input$sample_mode0 %||% "auto") == "auto" ||
    ((input$sample_mode0 %||% "auto") == "kws"    && !is.null(input$sample_keyword0) && length(input$sample_keyword0) > 0) ||
    ((input$sample_mode0 %||% "auto") == "manual" && !is.null(input$sample_cols0)    && length(input$sample_cols0) > 0)
  )
  })
  outputOptions(output, "msInputReady", suspendWhenHidden = FALSE)

  output$qcInputReady <- reactive({
  !is.null(shared$raw) && (
    (input$sample_mode0 %||% "auto") == "auto" ||
    ((input$sample_mode0 %||% "auto") == "kws"    && !is.null(input$sample_keyword0) && length(input$sample_keyword0) > 0) ||
    ((input$sample_mode0 %||% "auto") == "manual" && !is.null(input$sample_cols0)    && length(input$sample_cols0) > 0)
  )
  })
  outputOptions(output, "qcInputReady", suspendWhenHidden = FALSE)

  output$peakInputReady <- reactive({
  !is.null(shared$raw) && (
    (input$sample_mode0 %||% "auto") == "auto" ||
    ((input$sample_mode0 %||% "auto") == "kws"    && !is.null(input$sample_keyword0) && length(input$sample_keyword0) > 0) ||
    ((input$sample_mode0 %||% "auto") == "manual" && !is.null(input$sample_cols0)    && length(input$sample_cols0) > 0)
  )
  })
  outputOptions(output, "peakInputReady", suspendWhenHidden = FALSE)

  # --------------------------
  # Canonical raw df with .FID
  # --------------------------
  raw_fid <- reactive({
    req(shared$raw, input$row_id_col0)
    raw0 <- clean_mzmine_export(shared$raw)

    idres <- make_internal_ids(
      raw_df = raw0,
      rid_col = input$row_id_col0,
      mz_col = input$mz_col0,
      rt_col = input$rt_col0
    )
    raw0$.FID <- idres$ids_unique
    raw0
  })

  upload_error <- reactiveVal(NULL)

  output$upload_tab_error <- renderUI({
    msg <- upload_error()
    if (is.null(msg)) return(NULL)
    # Styled box for the error
    div(
      style = "color: #a94442; background-color: #f2dede; border-color: #ebccd1; 
               padding: 20px; margin-bottom: 20px; border: 1px solid transparent; 
               border-radius: 4px; font-size: 18px; font-weight: bold; text-align: center;",
      icon("exclamation-triangle"), " ", msg
    )
  })
  
  sample_cols0 <- reactive({
    # Reset error at the start of every check
    upload_error(NULL) 
    
    req(raw_fid())
    df <- raw_fid()
    cols <- names(df)
   
    mode <- input$sample_mode0 %||% "auto"
   
    # always exclude known meta columns
    meta <- unique(c(".FID", input$row_id_col0, input$mz_col0, input$rt_col0, "feature_id", "mz", "rt"))
    meta <- meta[!is.na(meta) & nzchar(meta)]
   
    if (mode == "manual") {
      validate(need(!is.null(input$sample_cols0) && length(input$sample_cols0) > 0,
                    "Pick sample columns (Upload tab)."))
      sc <- intersect(input$sample_cols0, cols)
      validate(need(length(sc) > 0, "Manual sample columns not found in table."))
      return(sc)
    }
   
    if (mode == "kws") {
      kws <- input$sample_keyword0 %||% character(0)
      idx <- multi_sample_idx(cols, kws)
      
      # --- UPDATED ERROR LOGIC ---
      if (length(idx) == 0) {
        msg <- paste0("Error: No sample columns matched the keywords: ", paste(kws, collapse = ", "))
        
        # A) Show Corner Notification
        shiny::showNotification(msg, type = "error", duration = 8)
        
        # B) Show Big Error in Main Panel
        upload_error(msg)
        
        # C) Stop processing
        validate(need(FALSE, msg))
      }
      # ---------------------------
      
      sc <- cols[idx]
      sc <- setdiff(sc, meta)
      validate(need(length(sc) > 0, "Keyword hits were only meta columns. Use Manual/Auto."))
      return(sc)
    }
   
    # AUTO: numeric-like columns not meta and not 'row ...' helpers
    cand <- setdiff(cols, meta)
    cand <- cand[!grepl("^row\\b", cand, ignore.case = TRUE)] 
   
    prop_num <- vapply(df[cand], function(x) {
      x2 <- suppressWarnings(as.numeric(as.character(x)))
      mean(is.finite(x2), na.rm = TRUE)
    }, numeric(1))
   
    sc <- cand[prop_num >= 0.7] 
   
    validate(need(length(sc) > 0,
                  "Auto-detect found no numeric sample columns. Switch to Manual or Keywords."))
    sc
  })
  
  sample_names <- reactive({
  req(sample_cols0())
  sample_cols0()
  })

  raw_zeroed <- reactive({
    req(raw_fid(), sample_cols0())
    df <- raw_fid()
    sc <- sample_cols0()
    
    # Replace NA with 0 ONLY in the confirmed sample intensity columns
    for (col in sc) {
      num_vec <- suppressWarnings(as.numeric(df[[col]]))
      num_vec[is.na(num_vec)] <- 0
      df[[col]] <- num_vec
    }
    df
  })
  
  # ---- Labels for Blank step ----
  custom_labels_blank <- reactive({
    req(input$meta_csv, sample_names())
    ext <- tools::file_ext(input$meta_csv$name)
    validate(need(ext == "csv", "Upload a .csv for labels (one column, no header)."))
    vec <- vroom::vroom(input$meta_csv$datapath, col_names = FALSE, delim = ",") |> pull(1)
    validate(need(length(vec) == length(sample_names()),
                  sprintf("Label count (%d) must match #samples (%d).", length(vec), length(sample_names()))))
    as.character(vec)
  })

  label_vector_blank <- reactive({
    req(sample_names())
    if (input$label_source == "from_custom") {
      custom_labels_blank()
    } else {
      labels_from_sample_names(sample_names(), token_sep = input$token_sep %||% "_", token_index = input$label_index %||% 2)
    }
  })

  output$labels_table <- renderDT({
    req(sample_names(), label_vector_blank())
    datatable(tibble(Sample = sample_names(), Label = label_vector_blank()),
              options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE)
  })

  # --------------------------
  # STEP 1 state: Blank filter
  # --------------------------
  blank_state <- reactiveValues(
    applied = FALSE,
    show_summary = FALSE,
    kept = NULL,
    stats = list(before = 0L, after = 0L, removed_blank = 0L)
  )

  mat0 <- reactive({
  req(raw_fid(), raw_zeroed(), sample_cols0())
  build_matrix_from_raw(raw_zeroed(), sample_cols = sample_cols0())
  })

  ds0_with_label <- reactive({
    req(mat0(), label_vector_blank())
    tibble(Label = label_vector_blank()) %>% bind_cols(as_tibble(mat0()))
  })

  output$blank_controls1 <- renderUI({
    req(ds0_with_label())
    labs <- sort(unique(ds0_with_label()$Label))
    tagList(
      selectizeInput(
        "groups_to_remove",
        "Choose blank/media group(s):",
        choices = labs, multiple = TRUE,
        options = list(placeholder = "Pick one or more (e.g., Media, Blank)…")
      ),
      radioButtons(
        "blank_mode", "Filtering mode:",
        choices = c(
          "Blank < Cutoff * Sample Max" = "mean_cutoff",
          "Drop any feature detected in Blank" = "drop_detected"
        ),
        selected = "mean_cutoff"
      ),
      conditionalPanel(
        condition = "input.blank_mode == 'mean_cutoff'",
        numericInput("blank_cutoff",
                     "Cutoff (fraction of max experimental mean):",
                     value = 0.10, min = 0, step = 0.01)
      )
    )
  })

  blank_plot_data <- eventReactive(input$plot_blank, {
    req(ds0_with_label(), input$groups_to_remove)
    ds <- ds0_with_label()
    groups <- input$groups_to_remove
    feats <- numeric_feature_names(ds)
    validate(need(length(feats) > 0, "No numeric features to plot."))

    # Calculate means for all groups
    mean_table <- ds %>%
      group_by(Label) %>%
      summarise(across(all_of(feats), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

    # Get Blank means
    blanks <- mean_table %>% filter(Label %in% groups) %>% select(-Label)
    validate(need(nrow(blanks) > 0, "Selected Blank groups not found in data."))
    blank_means <- blanks %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
    blank_vec <- as.numeric(blank_means[1, ])

    # Get Experimental maximum means
    exps <- mean_table %>% filter(!Label %in% groups) %>% select(-Label)
    validate(need(nrow(exps) > 0, "No experimental groups remaining to compare against."))
    max_exp <- vapply(exps, function(x) max(x, na.rm = TRUE), numeric(1))

    # Calculate Ratio: Mean(Blank) / Max(Mean(Experimental))
    # Replace 0s in max_exp with a tiny number to avoid Infinity errors when dividing
    max_exp_safe <- ifelse(max_exp == 0, 1e-9, max_exp)
    ratio <- blank_vec / max_exp_safe

    df <- tibble::tibble(Feature = feats, Ratio = ratio)
    df <- df[is.finite(df$Ratio), , drop = FALSE]
    df
  }, ignoreInit = TRUE)

  # Flag to trigger the UI ConditionalPanel
  output$blankPlotReady <- reactive({ !is.null(blank_plot_data()) })
  outputOptions(output, "blankPlotReady", suspendWhenHidden = FALSE)

  # Render the Plotly histogram
  output$blank_plot <- renderPlotly({
    df <- blank_plot_data()
    req(nrow(df) > 0)
    
    current_cutoff <- input$blank_cutoff %||% 0.10
    
    # Calculate % to be filtered
    total_feats <- nrow(df)
    removed_count <- sum(df$Ratio >= current_cutoff, na.rm = TRUE)
    perc_removed <- round((removed_count / total_feats) * 100, 1)

    gg <- ggplot(df, aes(x = Ratio)) +
      geom_histogram(alpha = 0.8, color = "black", fill = "royalblue3", bins = 50) +
      geom_vline(xintercept = current_cutoff, color = "red", linetype = "dashed", size = 1) +
      theme_minimal() +
      labs(x = "Ratio: Mean(Blank) / Max(Mean(Sample)) (log10 scale)", 
           y = "Count",
           title = "Blank to Sample Ratio Distribution") + 
      scale_x_continuous(trans = log10_plus_one_trans())

    # Convert to plotly and add the annotation via plotly's layout
    # This is much more stable than ggplot2's annotate()
    ggplotly(gg) %>% 
      layout(
        annotations = list(
          x = 0.95, y = 0.95, # Relative coordinates (top right)
          text = paste0("<b>Filtered: ", perc_removed, "%</b>"),
          showarrow = FALSE,
          xref = 'paper', yref = 'paper',
          font = list(color = "red", size = 16)
        ),
        margin = list(t = 50)
      )
  })
  
  run_step1_blank <- function() {
    req(ds0_with_label())
    ds <- ds0_with_label()
    feats <- numeric_feature_names(ds)

    blank_state$stats$before <- length(feats)

    keep <- feats
    if (isTRUE(input$enable_blank)) {
      groups <- input$groups_to_remove
      if (identical(input$blank_mode, "mean_cutoff")) {
        cutoff <- suppressWarnings(as.numeric(input$blank_cutoff))
        keep <- compute_blank_keep_mean(ds, groups, cutoff)
      } else {
        keep <- compute_blank_keep_detected(ds, groups)
      }
    }

    blank_state$applied <- TRUE
    blank_state$show_summary <- TRUE
    blank_state$kept <- keep
    blank_state$stats$after <- length(keep)
    blank_state$stats$removed_blank <- blank_state$stats$before - blank_state$stats$after

    if (!isTRUE(input$enable_blank)) {
      showNotification("Blank filter disabled. Step 1 frozen as pass-through.", type="message", duration=4)
    } else if (blank_state$stats$after == 0) {
      showNotification("Blank filters removed all features. Try relaxing cutoffs.", type="error", duration=5)
    } else {
      showNotification(sprintf("Step 1 frozen: kept %d of %d features.", blank_state$stats$after, blank_state$stats$before), type="message", duration=4)
    }
  }

  observeEvent(input$apply_blank, {
    
    if (isTRUE(input$enable_blank) && length(input$groups_to_remove) == 0) {
      showNotification("Error: Please select at least one Blank/Media group before applying.", type = "error", duration = 5)
      return() # Stop execution immediately
    }
    
    w <- Waiter$new(
      id = "blank_table_out", 
      html = tagList(spin_6(), h4("Applying Blank Filters...", style="color:white;")),
      color = "rgba(44, 62, 80, 0.8)"
    )
    w$show()
    
    shinyjs::disable("apply_blank")
    
    shinyjs::delay(50, {
      tryCatch({
        run_step1_blank()
      }, finally = {
        w$hide()
        shinyjs::enable("apply_blank")
      })
    })
  }, ignoreInit = TRUE)

  observeEvent(input$reset_blank, {
    blank_state$applied <- FALSE
    blank_state$show_summary <- FALSE
    blank_state$kept <- NULL
    blank_state$stats <- list(before = 0L, after = 0L)
    showNotification("Step 1 reset: pass-through.", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  raw_after_blank <- reactive({
    req(raw_zeroed())
    if (!isTRUE(blank_state$applied) || is.null(blank_state$kept)) return(raw_zeroed())
    df <- raw_zeroed()
    df[df$.FID %in% blank_state$kept, , drop = FALSE]
  })

  output$blank_header_in <- renderUI({ req(raw_zeroed()); h3("Input table") })
  output$blank_table_in  <- renderDT({ req(raw_zeroed()); datatable(raw_zeroed(), options = list(scrollX = TRUE, pageLength = 5)) })
  output$blank_header_out <- renderUI({ req(raw_after_blank()); h3("Output table — after Blank Filters") })
  output$blank_table_out  <- renderDT({ req(raw_after_blank()); datatable(raw_after_blank(), options = list(scrollX = TRUE, pageLength = 5)) })

  output$blank_filter_summary <- renderUI({
    req(blank_state$applied)
    req(isTRUE(blank_state$show_summary))

    before <- blank_state$stats$before %||% 0L
    after  <- blank_state$stats$after %||% before
    removed_blank <- blank_state$stats$removed_blank %||% (before - after)

    summary_table_ui(
      "Filtering results — Step 1 (Blank Filters)",
      tibble::tibble(
        Metric = c("Features before", "Removed by Blank filter", "Features after"),
        Value  = c(before, removed_blank, after)
      )
    )
  }) 

    # --------------------------
    # STEP 2 state: MS filters (REPLACEMENT)
    # --------------------------
    ms_state <- reactiveValues(
      applied = FALSE,
      show_summary = FALSE,
      matrix = NULL,
    
      del_iso = character(0),
      del_add = character(0),
      del_nl  = character(0),
      del_isf = character(0),
    
      iso_table = NULL,
      add_table = NULL,
      nl_table  = NULL,
      isf_table = NULL,
      
      isf_status = NULL,
    
      stats = list(
        before = 0L, after = 0L,
        removed_iso = 0L, removed_add = 0L, removed_nl = 0L, removed_isf = 0L
      )
    )
    
    # input matrix + feature meta remain based on Blank output (keep your existing reactives)
    mat_ms_in <- reactive({
    req(raw_after_blank(), sample_cols0())
    build_matrix_from_raw(raw_after_blank(), sample_cols = sample_cols0())
    })
    
    ft_ms_in <- reactive({
      req(raw_after_blank(), mat_ms_in(), input$mz_col0, input$rt_col0)
      build_feature_meta(raw_after_blank(), input$mz_col0, input$rt_col0, ds_matrix = mat_ms_in())
    })
    
    run_step2_ms <- function(hostess = NULL) {
      req(mat_ms_in(), ft_ms_in())
      ds <- as.data.frame(mat_ms_in())
      ft <- as.data.frame(ft_ms_in())
    
      ms_state$isf_status <- NULL
      ms_state$stats$before <- ncol(ds)
      ms_state$stats$removed_iso <- 0L
      ms_state$stats$removed_add <- 0L
      ms_state$stats$removed_nl  <- 0L
      ms_state$stats$removed_isf <- 0L
    
      ms_state$del_iso <- character(0)
      ms_state$del_add <- character(0)
      ms_state$del_nl  <- character(0)
      ms_state$del_isf <- character(0)
    
      ms_state$iso_table <- NULL
      ms_state$add_table <- NULL
      ms_state$nl_table  <- NULL
      ms_state$isf_table <- NULL
    
      # helper to refresh intensity after each filtering step
      refresh_ft <- function(ds, ft) {
        ints <- suppressWarnings(colMeans(ds, na.rm = TRUE))
        ft2 <- ft %>% dplyr::filter(Feature %in% colnames(ds))
        ft2$intensity <- as.numeric(ints[ft2$Feature])
        ft2
      }
      ft <- refresh_ft(ds, ft)
    
      # ==========================
      # A) Isotopes / dimer-series collapse
      # ==========================
      if (!is.null(hostess)) hostess$set(10)
      if (isTRUE(input$enable_iso2)) {
        z_max <- as.integer(input$iso_zmax2 %||% 3)
        n_iso <- as.integer(input$iso_n2 %||% 1)
        n_d   <- as.integer(input$iso_nd2 %||% 3)
        delta <- 1.00336
    
        shifts_iso <- tidyr::expand_grid(z = 1:z_max, k = 1:n_iso) %>%
          dplyr::mutate(
            name = paste0("C13*", k, " (z=", z, ")"),
            delta_mz = delta * k / z
          ) %>% dplyr::select(name, delta_mz)
    
        shifts_dimer <- tibble::tibble(
          name = paste0("C13*(", 1:n_d, "+0.5)"),
          delta_mz = delta * ((1:n_d) + 0.5)
        )
    
        shifts <- dplyr::bind_rows(shifts_iso, shifts_dimer)
    
        pk <- ft %>%
          dplyr::transmute(Feature, mz, rt, intensity) %>%
          dplyr::filter(is.finite(mz), is.finite(rt), Feature %in% colnames(ds))
    
        tol_fun <- mz_tol_fun_factory(
          type = input$iso_tol_type2 %||% "da",
          mz_tol_da = as.numeric(input$iso_mz_tol2 %||% 0.005),
          ppm = as.numeric(input$iso_ppm2 %||% 10)
        )
    
        edges <- make_mass_shift_edges(
          pk = pk, shifts = shifts,
          rt_tol = as.numeric(input$iso_rt2 %||% 0.01),
          tol_fun = tol_fun
        )
    
        if (nrow(edges) > 0) {
          X <- as.matrix(ds)
          idx1 <- match(edges$Feature1, colnames(X))
          idx2 <- match(edges$Feature2, colnames(X))
          edges$r <- cor_for_pairs(X, idx1, idx2)
          
          rm(X); gc() 
          
          edges <- edges %>% dplyr::filter(is.finite(r), r >= as.numeric(input$iso_cor2 %||% 0.80))
        }
        
        res_iso <- collapse_components_keep_most_intense(edges, pk, group_col = "iso_group")
        
        edges_collapsed <- edges %>%
          dplyr::group_by(Feature1) %>%
          dplyr::summarise(shift = paste(unique(shift), collapse = "; "), .groups = "drop")
        
        iso_tbl <- res_iso$table %>%
          dplyr::left_join(edges_collapsed, by = c("Feature" = "Feature1")) %>%
          dplyr::mutate(shift = tidyr::replace_na(shift, "none"))
        
        ms_state$del_iso <- setdiff(colnames(ds), res_iso$keep)
        ms_state$stats$removed_iso <- length(ms_state$del_iso)
        ms_state$iso_table <- iso_tbl
    
        if (length(ms_state$del_iso) > 0) {
          ds <- ds[, setdiff(colnames(ds), ms_state$del_iso), drop = FALSE]
          ft <- refresh_ft(ds, ft)
        }
        
        suppressWarnings(rm(shifts_iso, shifts_dimer, shifts, pk, edges, res_iso))
        gc()
        
      }
    
      # ==========================
      # B) Adduct families collapse (neutral mass)
      # ==========================
      if (!is.null(hostess)) hostess$set(35)
      if (isTRUE(input$enable_add2) && ncol(ds) > 0) {
        pol <- input$add_pol2 %||% "positive"
    
        adducts_df <- safe_read_csv_any(input$adduct_file2)
        if (is.null(adducts_df)) adducts_df <- built_in_adducts_full()
    
        # Expect columns like: Ion_mode, Name, Charge, Mult, Mass (your file)
        adducts_df <- as.data.frame(adducts_df, stringsAsFactors = FALSE)
        if (!all(c("Name","Charge","Mult","Mass") %in% names(adducts_df))) {
          showNotification("Adduct CSV missing required columns (Name, Charge, Mult, Mass). Using built-in adducts.", type="warning", duration=5)
          adducts_df <- built_in_adducts_full()
        }
    
        if ("Ion_mode" %in% names(adducts_df)) {
          adducts_df <- adducts_df %>% dplyr::filter(tolower(Ion_mode) == tolower(pol))
        }
    
        add_def <- adducts_df %>%
          dplyr::transmute(
            adduct = as.character(Name),
            z      = abs(as.numeric(Charge)),
            mult   = as.numeric(Mult),
            shift  = as.numeric(Mass)
          ) %>%
          dplyr::filter(is.finite(z), is.finite(mult), is.finite(shift), z > 0, mult > 0)
    
        pk <- ft %>%
          dplyr::transmute(Feature, mz = as.numeric(mz), rt = as.numeric(rt), intensity = as.numeric(intensity)) %>%
          dplyr::filter(is.finite(mz), is.finite(rt), Feature %in% colnames(ds)) %>%
          dplyr::mutate(peak_id = as.character(row_number()))
    
        feat_by_pid <- setNames(pk$Feature, pk$peak_id)
    
        tol_fun <- mz_tol_fun_factory(
          type = input$add_tol_type2 %||% "da",
          mz_tol_da = as.numeric(input$add_mz_tol2 %||% 0.005),
          ppm = as.numeric(input$add_ppm2 %||% 10)
        )
    
        min_NM <- as.numeric(input$add_min_nm2 %||% 50)
        rt_tol_add <- as.numeric(input$add_rt2 %||% 0.01)
    
        peaks <- pk %>%
          dplyr::transmute(
            peak_id = as.character(peak_id),
            Feature = as.character(Feature),
            mz      = as.numeric(mz),
            ret     = as.numeric(rt),
            int     = as.numeric(intensity)
          )
    
        # Build neutral mass intervals for each (peak, adduct)
        pad <- tidyr::crossing(peaks, add_def) %>%
          dplyr::mutate(
            tol_mz = tol_fun(mz),
            M      = ((mz - shift) * z) / mult,
            nm_lo  = M - (tol_mz * z / mult),
            nm_hi  = M + (tol_mz * z / mult)
          ) %>%
          dplyr::filter(is.finite(M), is.finite(nm_lo), is.finite(nm_hi), M > min_NM)
    
        if (nrow(pad) > 0) {
          pad_dt <- data.table::as.data.table(pad)
          data.table::setkey(pad_dt, nm_lo, nm_hi)
    
          hits_dt <- data.table::foverlaps(
            x = pad_dt, y = pad_dt,
            by.x = c("nm_lo","nm_hi"), by.y = c("nm_lo","nm_hi"),
            type = "any", nomatch = 0L
          )
    
          hits <- tibble::as_tibble(hits_dt) %>%
            dplyr::filter(
              peak_id < i.peak_id,
              abs(ret - i.ret) <= rt_tol_add
            ) %>%
            dplyr::mutate(
              M_est = (M + i.M) / 2,
              mz_pred_to   = (i.mult * M_est) / i.z + i.shift,
              mz_pred_from = (mult   * M_est) / z   + shift,
              err_to   = abs(i.mz - mz_pred_to),
              err_from = abs(mz   - mz_pred_from)
            ) %>%
            dplyr::filter(err_to <= i.tol_mz, err_from <= tol_mz) %>%
            dplyr::transmute(
              from = peak_id,
              to   = i.peak_id,
              from_adduct = adduct,
              to_adduct   = i.adduct
            ) %>%
            dplyr::distinct()
    
          suppressWarnings(rm(pad_dt, hits_dt, pad)); gc()
          
          if (nrow(hits) > 0) {
            # Correlation filter on edges
            X <- as.matrix(ds)
            f1 <- feat_by_pid[hits$from]
            f2 <- feat_by_pid[hits$to]
            idx1 <- match(f1, colnames(X))
            idx2 <- match(f2, colnames(X))
            hits$r <- cor_for_pairs(X, idx1, idx2)
    
            rm(X); gc()
            
            hits_corr <- hits %>%
              dplyr::filter(is.finite(r), r >= as.numeric(input$add_cor2 %||% 0.80))
    
            # Raw groups (all edges) + refined families (corr edges only)
            g_raw <- igraph::graph_from_data_frame(hits[,c("from","to")], directed = FALSE)
            memb_raw <- igraph::components(g_raw)$membership
            group_map <- tibble::tibble(peak_id = names(memb_raw), group_id = as.integer(memb_raw))
            
            suppressWarnings(rm(g_raw, memb_raw)); gc()
            
            # strict rt control
            if (isTRUE(input$add_strict_rt2)) {
              
              # 1. Assign strict RT group to each node within its broad group
              nodes_rt <- peaks %>%
                dplyr::inner_join(group_map, by = "peak_id") %>%
                dplyr::group_by(group_id) %>%
                dplyr::arrange(ret, .by_group = TRUE) %>%
                dplyr::mutate(rt_group = split_rt_strict(ret, as.numeric(input$add_rt2 %||% 0.01))) %>%
                dplyr::ungroup()
              
              # 2. Filter hits_corr to ONLY keep edges where both nodes share the same strict RT group
              edges_corr_rt <- hits_corr %>%
                dplyr::left_join(nodes_rt %>% dplyr::select(peak_id, group_id, rt_from = rt_group), 
                                 by = c("from" = "peak_id")) %>%
                dplyr::left_join(nodes_rt %>% dplyr::select(peak_id, rt_to = rt_group), 
                                 by = c("to" = "peak_id")) %>%
                dplyr::filter(!is.na(rt_from), !is.na(rt_to), rt_from == rt_to) %>%
                dplyr::mutate(block = paste0(group_id, "_", rt_from))
              
              suppressWarnings(rm(nodes_rt))
              
            } else {
              # Fallback if strict RT control is turned off
              edges_corr_rt <- hits_corr %>%
                dplyr::left_join(group_map, by = c("from" = "peak_id")) %>%
                dplyr::mutate(block = as.character(group_id))
            }
            
            fam_map <- edges_corr_rt %>%
              dplyr::group_by(block) %>%
              dplyr::group_modify(~{
                ed <- .x %>% dplyr::select(from, to) %>% dplyr::distinct()
                if (nrow(ed) == 0) return(tibble::tibble(peak_id = character(), family_id = character()))
                g <- igraph::graph_from_data_frame(ed, directed = FALSE)
                memb <- igraph::components(g)$membership
                tibble::tibble(
                  peak_id   = names(memb),
                  family_id = paste0(.y$block, "_F", as.integer(memb))
                )
              }) %>% dplyr::ungroup()
    
            # adduct annotation per peak (from/to lists)
            add_by_peak <- dplyr::bind_rows(
              hits %>% dplyr::transmute(peak_id = from, adduct = from_adduct, conn = to),
              hits %>% dplyr::transmute(peak_id = to,   adduct = to_adduct, conn = from)
              ) %>%
              # Map internal peak_id to actual Feature name
              dplyr::mutate(conn_feat = feat_by_pid[conn]) %>%
              dplyr::group_by(peak_id, adduct) %>%
              # Group connected features by adduct using commas
              dplyr::summarise(conn_feat = paste0("[", paste(sort(unique(conn_feat)), collapse = ", "), "]"), .groups="drop") %>%
              dplyr::group_by(peak_id) %>%
              # Arrange so adducts and connected_features match 1-to-1 perfectly
              dplyr::arrange(adduct, .by_group = TRUE) %>%
              dplyr::summarise(
                adducts = paste(adduct, collapse = "; "),
                connected_to = paste(conn_feat, collapse = "; "),
                .groups="drop"
              )
    
            # representative per family = max intensity
            reps <- peaks %>%
              dplyr::left_join(fam_map, by = "peak_id") %>%
              dplyr::filter(!is.na(family_id)) %>%
              dplyr::group_by(family_id) %>%
              dplyr::slice_max(order_by = int, n = 1, with_ties = FALSE) %>%
              dplyr::ungroup() %>%
              dplyr::transmute(peak_id, keep_rep = TRUE)
    
            in_family <- unique(fam_map$peak_id)
    
          fam_map_simple <- fam_map %>%
            dplyr::select(peak_id, family_id) %>%
            dplyr::distinct()
          
          add_tbl <- peaks %>%
            dplyr::left_join(group_map, by="peak_id") %>%
            dplyr::left_join(fam_map_simple, by="peak_id") %>%   
            dplyr::left_join(add_by_peak, by="peak_id") %>%
            dplyr::left_join(reps, by="peak_id") %>%
            dplyr::mutate(
              group_id = tidyr::replace_na(group_id, 0L),
              adducts  = tidyr::replace_na(adducts, "none"),
              connected_to = tidyr::replace_na(connected_to, "none"), # <-- ADD THIS LINE
              keep_rep = tidyr::replace_na(keep_rep, FALSE),
              status = dplyr::case_when(
                group_id == 0 ~ "kept (no group)",
                !(peak_id %in% in_family) ~ "kept (no corr-family)",
                keep_rep ~ "keep_rep",
                TRUE ~ "filtered"
              )
            ) %>%
            dplyr::arrange(group_id, family_id, dplyr::desc(int))
          
            ms_state$add_table <- add_tbl
    
            del_pids <- add_tbl %>%
              dplyr::filter(status == "filtered") %>%
              dplyr::pull(peak_id) %>% as.character()
    
            ms_state$del_add <- unname(feat_by_pid[del_pids])
            ms_state$del_add <- ms_state$del_add[!is.na(ms_state$del_add)]
            ms_state$stats$removed_add <- length(ms_state$del_add)
    
            if (ms_state$stats$removed_add > 0) {
              ds <- ds[, setdiff(colnames(ds), ms_state$del_add), drop = FALSE]
              ft <- refresh_ft(ds, ft)
            }
            
            suppressWarnings(rm(hits, hits_corr, group_map, edges_corr_rt, fam_map, fam_map_simple, add_by_peak, reps, add_tbl))
            gc()
            
          }
        }
        
        suppressWarnings(rm(adducts_df, add_def, pk, peaks)); gc()
        
      }
    
      # ==========================
      # C) Neutral losses collapse
      # ==========================
      if (!is.null(hostess)) hostess$set(65)
      if (isTRUE(input$enable_nl2) && ncol(ds) > 0) {
        nl_pol <- input$nl_pol2 %||% "positive"
        nl_df <- safe_read_csv_any(input$nl_file2)
        if (is.null(nl_df)) nl_df <- built_in_neutral_losses_full()
    
        if (is.null(nl_df)) {
          showNotification("Neutral loss table not found (upload or place CSV in app folder). Skipping NL step.", type="warning", duration=5)
        } else {
          # try to standardize expected columns (your file)
          nl_df <- as.data.frame(nl_df, stringsAsFactors = FALSE)
          
          if ("Pos" %in% names(nl_df) && "Neg" %in% names(nl_df)) {
            if (nl_pol == "positive") {
              nl_df <- nl_df %>% dplyr::filter(Pos == "+")
            } else if (nl_pol == "negative") {
              nl_df <- nl_df %>% dplyr::filter(Neg == "+")
            }
          }
          
          has_formula <- "Formula" %in% names(nl_df)
          has_class   <- "substructure or compound class" %in% names(nl_df)
          if (!all(c("Accurate Mass","Neutral Loss") %in% names(nl_df))) {
            showNotification("Neutral loss CSV missing expected columns ('Accurate Mass', 'Neutral Loss'). Skipping NL step.", type="warning", duration=6)
          } else {
            nl_tbl <- nl_df %>%
              dplyr::transmute(
                loss_mass = as.numeric(`Accurate Mass`),
                loss_name = as.character(`Neutral Loss`),
                loss_formula = if (has_formula) as.character(.data[["Formula"]]) else NA_character_,
                loss_class   = if (has_class)   as.character(.data[["substructure or compound class"]]) else NA_character_
              ) %>% dplyr::filter(is.finite(loss_mass), loss_mass > 0)
    
            pk <- ft %>%
              dplyr::transmute(Feature, mz = as.numeric(mz), rt = as.numeric(rt), intensity = as.numeric(intensity)) %>%
              dplyr::filter(is.finite(mz), is.finite(rt), Feature %in% colnames(ds)) %>%
              dplyr::mutate(peak_id = as.character(row_number()))
            feat_by_pid <- setNames(pk$Feature, pk$peak_id)
    
            # candidate pairs by RT proximity
            rt_pairs <- make_rt_pairs(pk, rt_tol = as.numeric(input$nl_rt2 %||% 0.002))
            if (nrow(rt_pairs) > 0) {
              X <- as.matrix(ds)
              idx1 <- match(rt_pairs$Feature1, colnames(X))
              idx2 <- match(rt_pairs$Feature2, colnames(X))
              rt_pairs$r <- cor_for_pairs(X, idx1, idx2)
              
              rm(X); gc()
              
              rt_pairs <- rt_pairs %>% dplyr::filter(is.finite(r), r >= as.numeric(input$nl_cor2 %||% 0.95))
    
              if (nrow(rt_pairs) > 0) {
                # add mz/rt + compute delta_mz
                pkm <- pk %>% dplyr::select(Feature, mz, rt, peak_id)
                pairs2 <- rt_pairs %>%
                  dplyr::left_join(pkm, by = c("Feature1"="Feature")) %>%
                  dplyr::rename(mz1=mz, rt1=rt, pid1=peak_id) %>%
                  dplyr::left_join(pkm, by = c("Feature2"="Feature")) %>%
                  dplyr::rename(mz2=mz, rt2=rt, pid2=peak_id) %>%
                  dplyr::mutate(
                    prec_id = dplyr::if_else(mz1 >= mz2, pid1, pid2),
                    frag_id = dplyr::if_else(mz1 >= mz2, pid2, pid1),
                    prec_mz = pmax(mz1, mz2),
                    frag_mz = pmin(mz1, mz2),
                    delta_mz = prec_mz - frag_mz,
                    delta_rt = abs(rt1 - rt2)
                  ) %>% dplyr::filter(is.finite(delta_mz), delta_mz > 0)
    
                tol_fun <- mz_tol_fun_factory(
                  type = input$nl_tol_type2 %||% "da",
                  mz_tol_da = as.numeric(input$nl_mz_tol2 %||% 0.005),
                  ppm = as.numeric(input$nl_ppm2 %||% 10)
                )
    
                # range match via foverlaps
                dt_pairs <- data.table::as.data.table(pairs2)
                dt_pairs[, `:=`(lo = delta_mz - tol_fun(delta_mz), hi = delta_mz + tol_fun(delta_mz))]
                dt_nl <- data.table::as.data.table(nl_tbl)
                dt_nl[, `:=`(lo = loss_mass, hi = loss_mass)]
    
                data.table::setkey(dt_pairs, lo, hi)
                data.table::setkey(dt_nl, lo, hi)
    
                hits_dt <- data.table::foverlaps(dt_pairs, dt_nl, type = "any", nomatch = 0L)
                
                suppressWarnings(rm(dt_pairs, dt_nl)); gc()
                
                hits <- tibble::as_tibble(hits_dt) %>%
                  dplyr::mutate(diff = abs(delta_mz - loss_mass)) %>%
                  dplyr::arrange(diff, dplyr::desc(r)) %>%
                  dplyr::distinct(prec_id, frag_id, loss_name, .keep_all = TRUE) %>%
                  dplyr::select(prec_id, frag_id, prec_mz, frag_mz, delta_mz, loss_name, loss_formula, loss_class, r, diff, delta_rt)
    
                suppressWarnings(rm(hits_dt)); gc()
    
                if (nrow(hits) > 0) {
                  edges_nl <- hits %>% dplyr::transmute(from = prec_id, to = frag_id) %>% dplyr::distinct()
                  g_nl <- igraph::graph_from_data_frame(edges_nl, directed = FALSE)
                  memb <- igraph::components(g_nl)$membership
                  memb_df <- tibble::tibble(peak_id = names(memb), nl_cluster = as.integer(memb)) %>%
                    dplyr::left_join(pk %>% dplyr::select(peak_id, mz, rt), by = "peak_id")
    
                  suppressWarnings(rm(g_nl, edges_nl, memb)); gc()
                  
                  if (isTRUE(input$nl_strict_rt2)) {
                    memb_df <- memb_df %>%
                      dplyr::group_by(nl_cluster) %>%
                      dplyr::arrange(rt, .by_group = TRUE) %>%
                      dplyr::mutate(rt_group = split_rt_strict(rt, as.numeric(input$nl_rt2 %||% 0.002))) %>%
                      dplyr::ungroup() %>%
                      dplyr::mutate(nl_cluster = as.integer(factor(paste(nl_cluster, rt_group, sep="_")))) %>%
                      dplyr::select(-rt_group)
                  }
    
                  clustered <- memb_df %>%
                    dplyr::group_by(nl_cluster) %>%
                    dplyr::arrange(dplyr::desc(mz), .by_group = TRUE) %>%
                    dplyr::mutate(status = dplyr::if_else(dplyr::row_number() == 1, "kept", "filtered")) %>%
                    dplyr::ungroup() %>%
                    dplyr::arrange(nl_cluster, dplyr::desc(mz))
    
                  ms_state$nl_table <- hits %>%
                    dplyr::left_join(clustered %>% dplyr::select(peak_id, nl_cluster, prec_status = status), by = c("prec_id" = "peak_id")) %>%
                    dplyr::left_join(clustered %>% dplyr::select(peak_id, frag_status = status), by = c("frag_id" = "peak_id")) %>%
                    dplyr::left_join(pk %>% dplyr::select(peak_id, prec_rt = rt), by = c("prec_id" = "peak_id")) %>%
                    dplyr::left_join(pk %>% dplyr::select(peak_id, frag_rt = rt), by = c("frag_id" = "peak_id")) %>%
                    dplyr::mutate(
                      prec_feature = unname(feat_by_pid[prec_id]),
                      frag_feature = unname(feat_by_pid[frag_id])
                    ) %>%
                    dplyr::select(nl_cluster, prec_feature, frag_feature, prec_status, prec_mz, frag_mz, prec_rt, frag_rt, delta_mz, loss_name, r, delta_rt) %>%
                    dplyr::arrange(nl_cluster, dplyr::desc(r))
                  
                  del_pids <- clustered %>% dplyr::filter(status=="filtered") %>% dplyr::pull(peak_id)
                  ms_state$del_nl <- unname(feat_by_pid[as.character(del_pids)])
                  ms_state$del_nl <- ms_state$del_nl[!is.na(ms_state$del_nl)]
                  ms_state$stats$removed_nl <- length(ms_state$del_nl)
    
                  if (ms_state$stats$removed_nl > 0) {
                    ds <- ds[, setdiff(colnames(ds), ms_state$del_nl), drop = FALSE]
                    ft <- refresh_ft(ds, ft)
                  }
                  
                  suppressWarnings(rm(memb_df, clustered, hits))
                  
                }
              }
            }
            
            suppressWarnings(rm(pk, rt_pairs, nl_tbl, nl_df)); gc()
            
          }
        }
      }
    
      # ==========================
      # D) Fragments (ISF) collapse
      # ==========================
      if (!is.null(hostess)) hostess$set(85)
      if (isTRUE(input$enable_isf2) && ncol(ds) > 0) {
        pk <- ft %>%
          dplyr::transmute(Feature, mz = as.numeric(mz), rt = as.numeric(rt), intensity = as.numeric(intensity)) %>%
          dplyr::filter(is.finite(mz), is.finite(rt), Feature %in% colnames(ds)) %>%
          dplyr::mutate(peak_id = as.character(row_number()))
        feat_by_pid <- setNames(pk$Feature, pk$peak_id)
    
        rt_pairs <- make_rt_pairs(pk, rt_tol = as.numeric(input$isf_rt2 %||% 0.002))
        if (nrow(rt_pairs) > 0) {
          X <- as.matrix(ds)
          idx1 <- match(rt_pairs$Feature1, colnames(X))
          idx2 <- match(rt_pairs$Feature2, colnames(X))
          rt_pairs$r <- cor_for_pairs(X, idx1, idx2)
          
          rm(X); gc()
          
          rt_pairs <- rt_pairs %>% dplyr::filter(is.finite(r), r >= as.numeric(input$isf_cor2 %||% 0.95))
    
          if (nrow(rt_pairs) > 0) {
            pkm <- pk %>% dplyr::select(Feature, mz, rt, intensity, peak_id)
            pairs2 <- rt_pairs %>%
              dplyr::left_join(pkm, by = c("Feature1"="Feature")) %>%
              dplyr::rename(mz1=mz, rt1=rt, int1=intensity, pid1=peak_id) %>%
              dplyr::left_join(pkm, by = c("Feature2"="Feature")) %>%
              dplyr::rename(mz2=mz, rt2=rt, int2=intensity, pid2=peak_id) %>%
              dplyr::mutate(
                prec_id = dplyr::if_else(mz1 >= mz2, pid1, pid2),
                frag_id = dplyr::if_else(mz1 >= mz2, pid2, pid1),
                prec_mz = pmax(mz1, mz2),
                frag_mz = pmin(mz1, mz2),
                prec_int = dplyr::if_else(mz1 >= mz2, int1, int2),
                frag_int = dplyr::if_else(mz1 >= mz2, int2, int1),
                frag_to_prec = (frag_int + 1e-12) / (prec_int + 1e-12),
                delta_rt = abs(rt1 - rt2)
              )
    
            if (isTRUE(input$isf_control_int2)) {
              pairs2 <- pairs2 %>%
                dplyr::filter(
                  frag_to_prec >= as.numeric(input$isf_ratio_min2 %||% 0.001),
                  frag_to_prec <= as.numeric(input$isf_ratio_max2 %||% 2.5)
                )
            }
    
            if (nrow(pairs2) > 0) {
              edges <- pairs2 %>% dplyr::transmute(from = prec_id, to = frag_id) %>% dplyr::distinct()
              g <- igraph::graph_from_data_frame(edges, directed = FALSE)
              memb <- igraph::components(g)$membership
              memb_df <- tibble::tibble(peak_id = names(memb), frag_cluster = as.integer(memb)) %>%
              dplyr::left_join(pk %>% dplyr::select(peak_id, mz, rt), by = "peak_id")
            
              suppressWarnings(rm(edges, g, memb)); gc()
              
            # OPTIONAL: strict RT split inside each connected component (prevents RT “bridging”)
            if (isTRUE(input$isf_strict_rt2)) {
              memb_df <- memb_df %>%
                dplyr::group_by(frag_cluster) %>%
                dplyr::arrange(rt, .by_group = TRUE) %>%
                dplyr::mutate(rt_group = split_rt_strict(rt, as.numeric(input$isf_rt2 %||% 0.002))) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(frag_cluster = as.integer(factor(paste(frag_cluster, rt_group, sep = "_")))) %>%
                dplyr::select(-rt_group)
            }
            
            # Now pick representative within each (possibly split) cluster
            memb_df <- memb_df %>%
              dplyr::group_by(frag_cluster) %>%
              dplyr::arrange(dplyr::desc(mz), .by_group = TRUE) %>%
              dplyr::mutate(status = dplyr::if_else(dplyr::row_number() == 1, "kept", "filtered")) %>%
              dplyr::ungroup()

              memb_df <- memb_df %>% 
                dplyr::left_join(pk %>% dplyr::select(peak_id, Feature), by = "peak_id")
              
              ms_state$isf_table <- pairs2 %>%
                dplyr::left_join(memb_df %>% dplyr::select(peak_id, frag_cluster, prec_status = status), by = c("prec_id" = "peak_id")) %>%
                dplyr::left_join(memb_df %>% dplyr::select(peak_id, frag_status = status), by = c("frag_id" = "peak_id")) %>%
                dplyr::left_join(pk %>% dplyr::select(peak_id, prec_rt = rt), by = c("prec_id" = "peak_id")) %>%
                dplyr::left_join(pk %>% dplyr::select(peak_id, frag_rt = rt), by = c("frag_id" = "peak_id")) %>%
                dplyr::mutate(
                  prec_feature = unname(feat_by_pid[prec_id]),
                  frag_feature = unname(feat_by_pid[frag_id])
                ) %>%
                dplyr::select(frag_cluster, prec_feature, frag_feature, prec_status, prec_mz, frag_mz, prec_rt, frag_rt, prec_int, frag_int, frag_to_prec, r, delta_rt) %>%
                dplyr::arrange(frag_cluster, dplyr::desc(r))
              
              ms_state$isf_status <- memb_df %>%
                dplyr::transmute(Feature, isf_status = status) %>%
                dplyr::distinct()
              
              del_pids <- memb_df %>% dplyr::filter(status=="filtered") %>% dplyr::pull(peak_id)
              ms_state$del_isf <- unname(feat_by_pid[as.character(del_pids)])
              ms_state$del_isf <- ms_state$del_isf[!is.na(ms_state$del_isf)]
              ms_state$stats$removed_isf <- length(ms_state$del_isf)
    
              if (is.null(ms_state$isf_status)) {
                  ms_state$isf_status <- tibble::tibble(
                    Feature = colnames(ds),
                    isf_status = "kept (no cluster)"
                  )
                }
      
              if (ms_state$stats$removed_isf > 0) {
                ds <- ds[, setdiff(colnames(ds), ms_state$del_isf), drop = FALSE]
                ft <- refresh_ft(ds, ft)
              }
              
              suppressWarnings(rm(memb_df))
              
            }
          }
        }
        
        suppressWarnings(rm(pk, rt_pairs, pairs2, pkm)); gc()
        
      }
    
      # finalize
      if (!is.null(hostess)) hostess$set(100)
      ms_state$matrix <- ds
      ms_state$applied <- TRUE
      ms_state$show_summary <- TRUE
      ms_state$stats$after <- ncol(ds)
    
      any_ms_enabled <- isTRUE(input$enable_iso2) || isTRUE(input$enable_add2) || isTRUE(input$enable_nl2) || isTRUE(input$enable_isf2)
      
      if (!any_ms_enabled) {
        showNotification("No MS filters enabled. Step 2 frozen as pass-through.", type="message", duration=4)
      } else if (ms_state$stats$after == 0) {
        showNotification("MS filters removed all features. Try relaxing parameters.", type="error", duration=5)
      } else {
        showNotification(sprintf("Step 2 frozen: kept %d of %d features.", ms_state$stats$after, ms_state$stats$before), type="message", duration=4)
      }
    }
    
    observeEvent(input$apply_ms, {
      hostess_ms <- Hostess$new()
      
      w <- Waiter$new(
      id = "ms_table_out",
      html = tagList(
        spin_6(), 
        h4("Applying MS Filters...", style="color:white;"),
        hostess_ms$get_loader(
          preset = "circle", 
          text_color = "white", 
          class = "label-center", stroke_color = "#e74c3c",
          style = "width: 200px; margin: 0 auto;" 
        )
      ),
      color = "rgba(44, 62, 80, 0.8)"
    )
    
    w$show()
    shinyjs::disable("apply_ms")
    
    shinyjs::delay(50, {
      tryCatch({
        run_step2_ms(hostess_ms)
      }, finally = {
        w$hide()
        shinyjs::enable("apply_ms")
      })
    })
    }, ignoreInit = TRUE)
    
    observeEvent(input$reset_ms, {
      ms_state$applied <- FALSE
      ms_state$show_summary <- FALSE
      ms_state$matrix <- NULL
      ms_state$isf_status <- NULL
    
      ms_state$del_iso <- character(0)
      ms_state$del_add <- character(0)
      ms_state$del_nl  <- character(0)
      ms_state$del_isf <- character(0)
    
      ms_state$iso_table <- NULL
      ms_state$add_table <- NULL
      ms_state$nl_table  <- NULL
      ms_state$isf_table <- NULL
    
      ms_state$stats <- list(before=0L, after=0L, removed_iso=0L, removed_add=0L, removed_nl=0L, removed_isf=0L)
    
      showNotification("Step 2 reset: pass-through.", type = "message", duration = 3)
    }, ignoreInit = TRUE)
    
    mat_ms_out <- reactive({
      req(mat_ms_in())
      if (isTRUE(ms_state$applied) && !is.null(ms_state$matrix)) ms_state$matrix else mat_ms_in()
    })
    
    raw_after_ms <- reactive({
    req(raw_after_blank())
    df <- raw_after_blank()
    if (!isTRUE(ms_state$applied)) return(df)
    
    del_ms <- unique(c(
      ms_state$del_iso %||% character(0),
      ms_state$del_add %||% character(0),
      ms_state$del_nl  %||% character(0),
      ms_state$del_isf %||% character(0)
    ))
    if (length(del_ms) > 0) {
      df <- df[!df$.FID %in% del_ms, , drop = FALSE]
    }
    df
    })
    
    output$ms_header_in <- renderUI({ req(raw_after_blank()); h3("Input table") })
    output$ms_table_in  <- renderDT({
    req(raw_after_blank())
    datatable(raw_after_blank(), options = list(scrollX = TRUE, pageLength = 5))
    })
    
    output$ms_header_out <- renderUI({ req(raw_after_ms()); h3("Output table — after MS Filters") })
    output$ms_table_out  <- renderDT({
    req(raw_after_ms())
    datatable(raw_after_ms(), options = list(scrollX = TRUE, pageLength = 5))
    })
    
    # detail tables
    output$iso2_table <- renderDT({
      req(input$show_iso_table2)
      tbl <- ms_state$iso_table
      if (is.null(tbl) || nrow(tbl) == 0)
        return(datatable(data.frame(Note="No isotope/dimer groups found (or skipped).")))
      tbl$iso_group <- suppressWarnings(as.integer(tbl$iso_group))  
      i <- match("iso_group", names(tbl))                       
      datatable(
      tbl, 
      extensions = 'Buttons', 
      options = list(
        pageLength = 10, 
        scrollX = TRUE, 
        order = list(list(i, "desc")),
        dom = 'Bfrtip',       
        buttons = list(        
          list(
            extend = "csvHtml5",
            text   = "Download CSV",
            filename = paste0(tools::file_path_sans_ext(basename(shared$name)), " isotopes-dimers"),
            exportOptions = list(
              modifier = list(page = "all")
            )
          )
        )
      )
    ) %>%
        DT::formatRound(columns = c("mz"), digits = 4) %>%
        DT::formatRound(columns = c("rt"), digits = 3) %>%
        DT::formatRound(columns = c("intensity"), digits = 1) %>% 
        formatStyle('status', 
                    backgroundColor = styleEqual(c("kept", "keep_rep", "filtered"), 
                                                 c("#dff0d8", "#dff0d8", "#f2dede")))
    }, server = F)

    output$add2_table <- renderDT({
      req(input$show_add_table2)
      tbl <- ms_state$add_table 
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(datatable(data.frame(Note="No adduct families found (or skipped).")))
      }
      idx_group <- match("group_id", names(tbl))
      idx_family <- match("family_id", names(tbl))
      datatable(
        tbl, 
        extensions = 'Buttons', 
        options = list(
          pageLength = 10, 
          scrollX = TRUE, 
          order = list(list(idx_group, 'desc'), list(idx_family, 'desc')),
          dom = 'Bfrtip',        
          buttons = list(        
            list(
              extend = "csvHtml5",
              text   = "Download CSV",
              filename = paste0(tools::file_path_sans_ext(basename(shared$name)), " adducts"),
              exportOptions = list(
                modifier = list(page = "all")
              )
            )
          )
        )
      ) %>%
        DT::formatRound(columns = c("mz"), digits = 4) %>%
        DT::formatRound(columns = c("int"), digits = 1) %>%
        DT::formatRound(columns = c("ret"), digits = 3) %>% 
        formatStyle('status', 
                    backgroundColor = styleEqual(c("kept (no group)", "kept (no corr-family)", "keep_rep", "filtered"), 
                                                 c("#dff0d8", "#dff0d8", "#dff0d8", "#f2dede")))
    }, server = F)
    
    output$nl2_table <- renderDT({
      req(input$show_nl_table2)
      if (is.null(ms_state$nl_table) || nrow(ms_state$nl_table) == 0) datatable(data.frame(Note="No neutral loss hits (or skipped)."))
      else datatable(
      ms_state$nl_table, 
      extensions = 'Buttons', 
      options = list(
        pageLength = 10, 
        scrollX = TRUE, 
        dom = 'Bfrtip',       
        buttons = list(        
          list(
            extend = "csvHtml5",
            text   = "Download CSV",
            filename = paste0(tools::file_path_sans_ext(basename(shared$name)), " neutral loses"),
            exportOptions = list(
              modifier = list(page = "all")
            )
          )
        )
      )
    ) %>%
        DT::formatRound(columns = c("prec_mz", "frag_mz", "delta_mz"), digits = 4) %>%
        DT::formatRound(columns = c("prec_rt", "frag_rt", "delta_rt", "r"), digits = 3) %>% 
        formatStyle(c('prec_status'), 
                    backgroundColor = styleEqual(c("kept", "filtered"), c("#dff0d8", "#f2dede")))
    }, server = F)
    
    output$isf2_table <- renderDT({
      req(input$show_isf_table2)
      if (is.null(ms_state$isf_table) || nrow(ms_state$isf_table) == 0) datatable(data.frame(Note="No fragment pairs/clusters (or skipped)."))
      else datatable(
      ms_state$isf_table, 
      extensions = 'Buttons', 
      options = list(
        pageLength = 10, 
        scrollX = TRUE, 
        dom = 'Bfrtip',       
        buttons = list(        
          list(
            extend = "csvHtml5",
            text   = "Download CSV",
            filename = paste0(tools::file_path_sans_ext(basename(shared$name)), " in-source fragments"),
            exportOptions = list(
              modifier = list(page = "all")
            )
          )
        )
      )
    ) %>%
        DT::formatRound(columns = c("prec_mz", "frag_mz"), digits = 4) %>%
        DT::formatRound(columns = c("prec_int", "frag_int"), digits = 1) %>%
        DT::formatRound(columns = c("prec_rt", "frag_rt", "delta_rt", "frag_to_prec", "r"), digits = 3) %>% 
        formatStyle(c('prec_status'), 
                    backgroundColor = styleEqual(c("kept", "filtered"), c("#dff0d8", "#f2dede")))
    }, server = F)
    
    output$ms_filter_summary <- renderUI({
      req(ms_state$applied)
      req(isTRUE(ms_state$show_summary))
    
      before <- ms_state$stats$before %||% 0L
      after  <- ms_state$stats$after  %||% before
    
      removed_iso <- ms_state$stats$removed_iso %||% 0L
      removed_add <- ms_state$stats$removed_add %||% 0L
      removed_nl  <- ms_state$stats$removed_nl  %||% 0L
      removed_isf <- ms_state$stats$removed_isf %||% 0L
      removed_total <- before - after
    
      summary_table_ui(
        "Filtering results — Step 2 (MS Filters)",
        tibble::tibble(
          Metric = c(
            "Features before",
            "Removed by Isotopes/Dimer",
            "Removed by Adduct families",
            "Removed by Neutral losses",
            "Removed by Fragments",
            "Total removed",
            "Features after"
          ),
          Value = c(before, removed_iso, removed_add, removed_nl, removed_isf, removed_total, after)
        )
      )
    })

  # --------------------------
  # QC Labels
  # --------------------------
  qc_custom_labels <- reactive({
    req(input$qc_meta_csv, sample_names())
    ext <- tools::file_ext(input$qc_meta_csv$name)
    validate(need(ext == "csv", "Upload a .csv for QC labels (one column, no header)."))
    vec <- vroom::vroom(input$qc_meta_csv$datapath, col_names = FALSE, delim = ",") |> pull(1)
    validate(need(length(vec) == length(sample_names()),
                  sprintf("QC label count (%d) must match #samples (%d).", length(vec), length(sample_names()))))
    as.character(vec)
  })

  qc_label_vector <- reactive({
    req(sample_names(), label_vector_blank())
    src <- input$qc_label_source %||% "inherit"
    if (identical(src, "inherit")) {
      label_vector_blank()
    } else if (identical(src, "from_custom")) {
      qc_custom_labels()
    } else {
      labels_from_sample_names(sample_names(), token_sep = input$qc_token_sep %||% "_", token_index = input$qc_label_index %||% 2)
    }
  })

  output$qc_labels_table <- renderDT({
    req(sample_names(), qc_label_vector())
    datatable(tibble(Sample = sample_names(), Label = qc_label_vector()),
              options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE)
  })

  # --------------------------
  # STEP 3 state: QC filters
  # --------------------------
  qc_state <- reactiveValues(
    applied = FALSE,
    show_summary = FALSE,
    kept = NULL,
    stats = list(before = 0L, after_final = 0L, 
                 removed_zeros = 0L, removed_mean = 0L, removed_rsd = 0L, removed_min = 0L, removed_total = 0L)
  )

  ds_qc_in <- reactive({
    req(mat_ms_out(), qc_label_vector())
    tibble(Label = qc_label_vector()) %>% bind_cols(as_tibble(mat_ms_out()))
  })

  output$value_groups_ui <- renderUI({
    req(ds_qc_in())
    labs <- sort(unique(ds_qc_in()$Label))
    selectizeInput(
      "value_groups",
      "Group(s) to evaluate:",
      choices = labs, multiple = TRUE,
      options = list(placeholder = "Empty = all groups")
    )
  })

  qc_plot_data <- eventReactive(input$plot_values_qc, {
    req(ds_qc_in())
    ds <- ds_qc_in()
    feats <- numeric_feature_names(ds)
    validate(need(length(feats) > 0, "No numeric features to plot."))

    filters <- input$value_filters %||% character(0)
    validate(need(length(filters) > 0, "Select at least one QC value filter to plot."))

    mode <- input$value_mode %||% "group_any"
    labs_sel <- if (!is.null(input$value_groups) && length(input$value_groups) > 0) input$value_groups else unique(ds$Label)
    res_list <- list()

    if ("zeros" %in% filters) {
      zm <- input$zero_metric %||% "count"
      if (mode %in% c("group_any","group_every")) {
        st <- ds %>%
          group_by(Label) %>%
          summarise(across(all_of(feats),
                           ~ if (zm == "percent") mean(.x == 0, na.rm = TRUE) * 100 else sum(.x == 0, na.rm = TRUE)),
                    .groups="drop") %>%
          filter(Label %in% labs_sel)
        long <- st %>% pivot_longer(-Label, names_to="Feature", values_to="value")
      } else {
        ds_sub <- ds %>% filter(Label %in% labs_sel)
        vec <- vapply(ds_sub[feats], function(x) if (zm=="percent") mean(x==0, na.rm=TRUE)*100 else sum(x==0, na.rm=TRUE), numeric(1))
        long <- tibble(Label = "Pooled", Feature = names(vec), value = as.numeric(vec))
      }
      res_list[["zeros"]] <- list(metric="zeros", mode=mode, zero_metric=zm, data=long)
    }

    if ("mean" %in% filters) {
      if (mode %in% c("group_any","group_every")) {
        st <- ds %>% group_by(Label) %>% summarise(across(all_of(feats), ~ mean(.x, na.rm=TRUE)), .groups="drop") %>% filter(Label %in% labs_sel)
        long <- st %>% pivot_longer(-Label, names_to="Feature", values_to="value")
      } else {
        ds_sub <- ds %>% filter(Label %in% labs_sel)
        vec <- vapply(ds_sub[feats], function(x) mean(x, na.rm=TRUE), numeric(1))
        long <- tibble(Label="Pooled", Feature=names(vec), value=as.numeric(vec))
      }
      res_list[["mean"]] <- list(metric="mean", mode=mode, data=long)
    }

    if ("min" %in% filters) {
      if (mode %in% c("group_any","group_every")) {
        st <- ds %>% group_by(Label) %>% summarise(across(all_of(feats), ~ suppressWarnings(min(.x, na.rm=TRUE))), .groups="drop") %>% filter(Label %in% labs_sel)
        long <- st %>% pivot_longer(-Label, names_to="Feature", values_to="value")
      } else {
        ds_sub <- ds %>% filter(Label %in% labs_sel)
        vec <- vapply(ds_sub[feats], function(x) suppressWarnings(min(x, na.rm=TRUE)), numeric(1))
        long <- tibble(Label="Pooled", Feature=names(vec), value=as.numeric(vec))
      }
      res_list[["min"]] <- list(metric="min", mode=mode, data=long)
    }

    if ("rsd" %in% filters) {
      if (mode %in% c("group_any","group_every")) {
        st <- ds %>% group_by(Label) %>% summarise(across(all_of(feats), ~ calc_rsd(.x)), .groups="drop") %>% filter(Label %in% labs_sel)
        long <- st %>% pivot_longer(-Label, names_to="Feature", values_to="value")
      } else {
        ds_sub <- ds %>% filter(Label %in% labs_sel)
        vec <- vapply(ds_sub[feats], function(x) calc_rsd(x), numeric(1))
        long <- tibble(Label="Pooled", Feature=names(vec), value=as.numeric(vec))
      }
      res_list[["rsd"]] <- list(metric="rsd", mode=mode, data=long)
    }

    res_list
  }, ignoreInit = TRUE)

  output$qcPlotReady <- reactive({ !is.null(qc_plot_data()) })
  outputOptions(output, "qcPlotReady", suspendWhenHidden = FALSE)

  output$qc_value_plot <- renderPlotly({
    md_list <- qc_plot_data()
    
    validate(
      need(!is.null(md_list) && length(md_list) > 0, 
           "Please select at least one QC filter (Zeros, Mean, RSD, or Min) in the sidebar and click 'Plot values distribution'.")
    )

    plots_list <- list()
    
    for (metric_name in names(md_list)) {
      md <- md_list[[metric_name]]
      df <- md$data
      df <- df[is.finite(df$value), , drop = FALSE]
      if (!nrow(df)) next

      mode <- md$mode
      mode_txt <- switch(mode, 
                         group_any = "Groupwise (ANY)", 
                         group_every = "Groupwise (EVERY)", 
                         pooled = "POOLED", 
                         "Unknown")

      # 1. Define specific cutoffs
      if (md$metric == "zeros") {
        xlab_txt <- if (identical(md$zero_metric, "percent")) "Zero values (%)" else "Zero values (count)"
        cutoff <- input$zero_cutoff %||% 0
      } else if (md$metric == "mean") {
        xlab_txt <- "Mean intensity (log10)"
        cutoff <- input$mean_cutoff %||% 5000
      } else if (md$metric == "min") {
        xlab_txt <- "Minimum intensity (log10)"
        cutoff <- input$min_cutoff %||% 1000
      } else {
        xlab_txt <- "RSD (%)"
        cutoff <- input$rsd_cutoff %||% 30
      }

      # 2. EXACT MATH MATCHING
      feat_eval <- df %>%
        dplyr::group_by(Feature) %>%
        dplyr::summarise(
          pass = if (md$metric %in% c("zeros", "rsd")) {
            if (mode == "group_every") all(value <= cutoff, na.rm = TRUE) else any(value <= cutoff, na.rm = TRUE)
          } else {
            if (mode == "group_every") all(value >= cutoff, na.rm = TRUE) else any(value >= cutoff, na.rm = TRUE)
          }
        )
      
      total_feats <- nrow(feat_eval)
      removed_feats <- sum(!feat_eval$pass, na.rm = TRUE)
      perc_filt <- round((removed_feats / total_feats) * 100, 1)

      # 3. DOUBLE SAFETY: Put the xlab_txt (metric type) directly into the header
      df$plot_header <- paste0(toupper(md$metric), " [", xlab_txt, "] - ", mode_txt, 
                               " | Filtered: ", removed_feats, " of ", total_feats, " (", perc_filt, "%)")

      if (mode %in% c("group_any", "group_every")) {
        gg <- ggplot(df, aes(x = value, fill = Label)) +
          geom_histogram(alpha = 0.8, position = "dodge", color = "black", bins = 40) +
          scale_fill_hue(h = c(180, 300))
      } else {
        gg <- ggplot(df, aes(x = value)) +
          geom_histogram(alpha = 0.8, color = "black", fill = "springgreen3", bins = 40)
      }

      gg <- gg +
        geom_vline(xintercept = cutoff, color = "red", linetype = "dashed", linewidth = 1) +
        facet_wrap(~ plot_header) + 
        theme_minimal() +
        theme(strip.text = element_text(size = 12, face = "bold", color = "#2c3e50"),
              strip.background = element_rect(fill = "#ecf0f1", color = NA)) +
        labs(x = NULL, y = "Count") # Native ggplot axis

      if (md$metric %in% c("mean", "min")) {
        gg <- gg + scale_x_continuous(trans = log10_plus_one_trans())
      }
      
      # 4. FORCE PLOTLY AXES: Tell Plotly not to drop the labels
      p <- ggplotly(gg) %>% 
        layout(
          xaxis = list(title = NULL),
          yaxis = list(title = "Count")
        )
      
      plots_list[[length(plots_list) + 1]] <- p
    }

    # 5. Assemble Subplots
    if (length(plots_list) == 1) {
      plots_list[[1]]
    } else {
      subplot(plots_list, nrows = length(plots_list), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.06) %>% 
        layout(showlegend = T)
    }
  })

  run_step3_qc <- function() {
    req(ds_qc_in())
    ds <- ds_qc_in()
    feats <- numeric_feature_names(ds)

    qc_state$stats$before <- length(feats)

    if (!length(input$value_filters %||% character(0))) {
      keep <- feats
      qc_state$stats$removed_zeros <- 0L
      qc_state$stats$removed_mean  <- 0L
      qc_state$stats$removed_rsd   <- 0L
      qc_state$stats$removed_min   <- 0L
      qc_state$stats$after_final   <- length(keep)
      qc_state$stats$removed_total <- qc_state$stats$before - qc_state$stats$after_final
    } else {
      res <- run_qc_value_filters(
        ds_with_label = ds,
        feats_base = feats,
        filters = input$value_filters,
        mode = input$value_mode,
        value_groups = input$value_groups %||% character(0),
        zero_metric = input$zero_metric %||% "count",
        zero_cutoff = input$zero_cutoff,
        mean_cutoff = input$mean_cutoff,
        rsd_cutoff  = input$rsd_cutoff,
        min_cutoff  = input$min_cutoff
      )
      keep <- res$keep
      qc_state$stats$removed_zeros <- res$stats$removed_zeros
      qc_state$stats$removed_mean  <- res$stats$removed_mean
      qc_state$stats$removed_rsd   <- res$stats$removed_rsd
      qc_state$stats$removed_min   <- res$stats$removed_min
      qc_state$stats$after_final   <- res$stats$after_final
      qc_state$stats$removed_total <- res$stats$removed_total
    }

    qc_state$applied <- TRUE
    qc_state$show_summary <- TRUE
    qc_state$kept <- keep

    if (length(input$value_filters %||% character(0)) == 0) {
      showNotification("No QC filters selected. Step 3 frozen as pass-through.", type="message", duration=4)
    } else if (qc_state$stats$after_final == 0) {
      showNotification("QC filters removed all features. Try relaxing cutoffs.", type="error", duration=5)
    } else {
      showNotification(sprintf("Step 3 frozen: kept %d of %d features.", qc_state$stats$after_final, qc_state$stats$before), type="message", duration=4)
    }
  }

  observeEvent(input$apply_qc, {
    w <- Waiter$new(
      id = "qc_table_out", 
      html = tagList(spin_6(), h4("Applying QC Filters...", style="color:white;")),
      color = "rgba(44, 62, 80, 0.8)"
    )
    w$show()
    
    shinyjs::disable("apply_qc")
    
    shinyjs::delay(50, {
      tryCatch({
        run_step3_qc()
      }, finally = {
        w$hide()
        shinyjs::enable("apply_qc")
      })
    })
  }, ignoreInit = TRUE)

  observeEvent(input$reset_qc, {
    qc_state$applied <- FALSE
    qc_state$show_summary <- FALSE
    qc_state$kept <- NULL
    qc_state$stats <- list(before = 0L, after_final = 0L, 
                           removed_zeros = 0L, removed_mean = 0L, removed_rsd = 0L, removed_min = 0L, removed_total = 0L)
    showNotification("Step 3 reset: pass-through.", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  mat_qc_out <- reactive({
    req(mat_ms_out())
    if (isTRUE(qc_state$applied) && !is.null(qc_state$kept)) {
      ds <- as.data.frame(mat_ms_out())
      ds[, intersect(colnames(ds), qc_state$kept), drop = FALSE]
    } else {
      mat_ms_out()
    }
  })

  raw_after_qc <- reactive({
    req(raw_after_ms())
    df <- raw_after_ms()
    if (!isTRUE(qc_state$applied) || is.null(qc_state$kept)) return(df)
    
    df[df$.FID %in% qc_state$kept, , drop = FALSE]
  })
  
  output$qc_header_in <- renderUI({ req(raw_after_ms()); h3("Input table") })
  output$qc_table_in  <- renderDT({
    req(raw_after_ms())
    datatable(raw_after_ms(), options = list(scrollX = TRUE, pageLength = 5))
  })

  output$qc_header_out <- renderUI({ req(raw_after_qc()); h3("Output table — after QC Filters") })
  output$qc_table_out  <- renderDT({
    req(raw_after_qc())
    datatable(raw_after_qc(), options = list(scrollX = TRUE, pageLength = 5))
  })

  output$qc_filter_summary <- renderUI({
    req(qc_state$applied)
    req(isTRUE(qc_state$show_summary))

    before        <- qc_state$stats$before        %||% 0L
    after_final   <- qc_state$stats$after_final   %||% before
    removed_zeros <- qc_state$stats$removed_zeros %||% 0L
    removed_mean  <- qc_state$stats$removed_mean  %||% 0L
    removed_rsd   <- qc_state$stats$removed_rsd   %||% 0L
    removed_min   <- qc_state$stats$removed_min   %||% 0L
    removed_total <- qc_state$stats$removed_total %||% (before - after_final)

    tagList(
    summary_table_ui(
      "Filtering results — Step 3 (QC Filters)",
      tibble::tibble(
        Metric = c(
          "Features before",
          "Removed by Zeros",
          "Removed by Mean",
          "Removed by RSD",
          "Removed by Min",
          "Total removed",
          "Features after"
        ),
        Value  = c(before, removed_zeros, removed_mean, removed_rsd, removed_min, removed_total, after_final)
      )
    ),
    div(style = "margin-top: -10px; margin-bottom: 15px; margin-left: 5px; font-size: 13px; color: white; font-style: italic;",
          "Note: a feature may be counted in multiple filter categories.")
    )
  })

  # --------------------------
  # STEP 4: Peak Filters
  # --------------------------
  peak_state <- reactiveValues(
    applied = FALSE,
    show_summary = FALSE,
    kept = NULL,
    stats = list(before=0L, after=0L,
                 removed_mz=0L, removed_rt=0L, removed_rmd=0L, removed_amd=0L, removed_total=0L)
  )

  # Align raw table to current pipeline input (blank + merge + MS deletions + QC kept)
  peak_table_in_raw <- reactive({
    req(raw_zeroed(), sample_cols0())

    blank_keep <- if (isTRUE(blank_state$applied)) blank_state$kept else NULL
    merge_map  <- NULL

    del_ms <- character(0)
    if (isTRUE(ms_state$applied)) {
      del_ms <- unique(c(
        ms_state$del_iso %||% character(0),
        ms_state$del_add %||% character(0),
        ms_state$del_nl  %||% character(0),
        ms_state$del_isf %||% character(0)
      ))
    }
    final_keep <- if (isTRUE(qc_state$applied)) qc_state$kept else NULL

    build_final_feature_table(
    raw_df_fid  = raw_zeroed(),
    sample_cols = sample_cols0(),
    blank_keep = blank_keep,
    merge_map  = merge_map,
    del_ms     = del_ms,
    final_keep = final_keep
    )
  })

  output$peak_header_in <- renderUI({ req(peak_table_in_raw()); h3("Input table") })
  output$peak_table_in  <- renderDT({
    req(peak_table_in_raw())
    datatable(peak_table_in_raw(), options = list(scrollX = TRUE, pageLength = 5))
  })

  peak_plot_data <- eventReactive(input$plot_peak, {
    req(peak_table_in_raw(), input$mz_col0, input$rt_col0)
    sel <- input$peak_filters %||% character(0)
    validate(need(length(sel) > 0, "Select at least one metric to plot (m/z, rt, RMD, AMD)."))
    
    df <- as.data.frame(peak_table_in_raw(), check.names = FALSE)
    
    validate(
      need(input$mz_col0 %in% names(df), "m/z column (from Upload tab) not found in aligned peak table."),
      need(input$rt_col0 %in% names(df), "rt column (from Upload tab) not found in aligned peak table.")
    )
    
    mz_vals <- suppressWarnings(as.numeric(df[[input$mz_col0]]))
    rt_vals <- suppressWarnings(as.numeric(df[[input$rt_col0]]))
    rmd_vals <- calculate_RMD(mz_vals)
    amd_vals <- calculate_AMD(mz_vals, absolute = TRUE)

    dd <- tibble::tibble(mz = mz_vals, RT = rt_vals, RMD = rmd_vals, AMD = amd_vals)
    dd <- dd[is.finite(dd$mz) & is.finite(dd$RT), , drop = FALSE]
    req(nrow(dd) > 0)

    # Return the raw plotting data and current selection
     list(data = dd, sel = sel)
  }, ignoreInit = TRUE)

  output$peakPlotReady <- reactive({ !is.null(peak_plot_data()) })
  outputOptions(output, "peakPlotReady", suspendWhenHidden = FALSE)

  output$peak_plot <- renderPlotly({
    res <- peak_plot_data()
    dd <- res$data
    sel <- res$sel
    plots <- list()

    # m/z vs RT Scatter (handles vertical RT line and horizontal m/z line)
    if (any(c("mz","rt") %in% sel)) {
      p_mzrt <- ggplot(dd, aes(x = RT, y = mz)) +
        geom_point(alpha = 0.7, size = 3, color = "black", shape = 21, fill = "lightblue") +
        theme_minimal() +
        labs(x = "RT (min)", y = "m/z", title = "m/z vs RT")
      
      if ("rt" %in% sel) {
        if (isTRUE(input$rt_min_enable)) {
          rtmin <- input$peak_rt_min %||% 1
          p_mzrt <- p_mzrt + geom_vline(xintercept = rtmin, color = "red", linetype = "dashed", linewidth = 1)
        }
        if (isTRUE(input$rt_max_enable)) {
          rtmax <- input$peak_rt_max %||% 30
          p_mzrt <- p_mzrt + geom_vline(xintercept = rtmax, color = "red", linetype = "dashed", linewidth = 1)
        }
      }
      if ("mz" %in% sel) {
        if (isTRUE(input$mz_min_enable)) {
          mzmin <- input$peak_mz_min %||% 200
          p_mzrt <- p_mzrt + geom_hline(yintercept = mzmin, color = "red", linetype = "dashed", linewidth = 1)
        }
        if (isTRUE(input$mz_max_enable)) {
          mzmax <- input$peak_mz_max %||% 1500
          p_mzrt <- p_mzrt + geom_hline(yintercept = mzmax, color = "red", linetype = "dashed", linewidth = 1)
        }
      }
      plots[["mzrt"]] <- ggplotly(p_mzrt)
    }

    # RMD Scatter (handles upper and lower horizontal bounds)
    if ("rmd" %in% sel) {
      dd2 <- dd[is.finite(dd$RMD), , drop = FALSE]
      if (nrow(dd2) > 0) {
        p_rmd <- ggplot(dd2, aes(x = mz, y = RMD)) +
          geom_point(alpha = 0.7, size = 3, color = "black", shape = 21, fill = "indianred") +
          theme_minimal() +
          labs(x = "m/z", y = "RMD (ppm)", title = "RMD vs m/z")
        
        if (isTRUE(input$rmd_min_enable)) {
          rmin <- input$peak_rmd_min %||% -2000
          p_rmd <- p_rmd + geom_hline(yintercept = rmin, color = "red", linetype = "dashed", linewidth = 1)
        }
        if (isTRUE(input$rmd_max_enable)) {
          rmax <- input$peak_rmd_max %||% 2000
          p_rmd <- p_rmd + geom_hline(yintercept = rmax, color = "red", linetype = "dashed", linewidth = 1)
        }
        plots[["rmd"]] <- ggplotly(p_rmd)
      }
    }

    # AMD Scatter (handles upper and lower horizontal bounds)
    if ("amd" %in% sel) {
      dd3 <- dd[is.finite(dd$AMD), , drop = FALSE]
      if (nrow(dd3) > 0) {
        p_amd <- ggplot(dd3, aes(x = mz, y = AMD)) +
          geom_point(alpha = 0.7, size = 3, color = "black", shape = 21, fill = "mediumpurple") +
          theme_minimal() +
          labs(x = "m/z", y = "AMD (Da)", title = "AMD vs m/z")
        
        if (isTRUE(input$amd_min_enable)) {
          amin <- input$peak_amd_min %||% 0.00
          p_amd <- p_amd + geom_hline(yintercept = amin, color = "red", linetype = "dashed", linewidth = 1)
        }
        if (isTRUE(input$amd_max_enable)) {
          amax <- input$peak_amd_max %||% 0.50
          p_amd <- p_amd + geom_hline(yintercept = amax, color = "red", linetype = "dashed", linewidth = 1)
        }
        plots[["amd"]] <- ggplotly(p_amd)
      }
    }

    validate(need(length(plots) > 0, "Nothing to plot for the current selection."))
    if (length(plots) == 1) {
      plots[[1]]
    } else {
      subplot(plots, nrows = length(plots), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.08) %>%
        layout(title = "")
    }
  })

  run_step4_peak <- function() {
    req(peak_table_in_raw(), input$mz_col0, input$rt_col0)
  
    df <- as.data.frame(peak_table_in_raw(), check.names = FALSE)
  
    # IMPORTANT: use .FID (internal feature ID) so it matches matrix colnames later
    rid <- if (".FID" %in% names(df)) ".FID" else names(df)[1]
  
    validate(
      need(input$mz_col0 %in% names(df), "m/z column (from Upload tab) not found in aligned peak table."),
      need(input$rt_col0 %in% names(df), "rt column (from Upload tab) not found in aligned peak table.")
    )
  
    sel <- input$peak_filters %||% character(0)
    before <- nrow(df)
    peak_state$stats$before <- before
  
    mzv <- suppressWarnings(as.numeric(df[[input$mz_col0]]))
    rtv <- suppressWarnings(as.numeric(df[[input$rt_col0]]))
  
    # initialize PASS vectors (so missing filters don't break)
    mz_pass  <- rep(TRUE, before)
    rt_pass  <- rep(TRUE, before)
    rmd_pass <- rep(TRUE, before)
    amd_pass <- rep(TRUE, before)
  
    # initialize FAIL vectors
    fail_mz  <- rep(FALSE, before)
    fail_rt  <- rep(FALSE, before)
    fail_rmd <- rep(FALSE, before)
    fail_amd <- rep(FALSE, before)
  
    if ("mz" %in% sel) {
      mz_pass_min <- rep(TRUE, before)
      mz_pass_max <- rep(TRUE, before)
      
      if (isTRUE(input$mz_min_enable)) {
        mzmin <- suppressWarnings(as.numeric(input$peak_mz_min))
        validate(need(is.finite(mzmin), "m/z min cutoff must be numeric."))
        mz_pass_min <- is.finite(mzv) & (mzv >= mzmin)
      }
      if (isTRUE(input$mz_max_enable)) {
        mzmax <- suppressWarnings(as.numeric(input$peak_mz_max))
        validate(need(is.finite(mzmax), "m/z max cutoff must be numeric."))
        mz_pass_max <- is.finite(mzv) & (mzv <= mzmax)
      }
      
      mz_pass <- mz_pass_min & mz_pass_max
      fail_mz <- !mz_pass
    }

    if ("rt" %in% sel) {
      rt_pass_min <- rep(TRUE, before)
      rt_pass_max <- rep(TRUE, before)
      
      if (isTRUE(input$rt_min_enable)) {
        rtmin <- suppressWarnings(as.numeric(input$peak_rt_min))
        validate(need(is.finite(rtmin), "rt min cutoff must be numeric."))
        rt_pass_min <- is.finite(rtv) & (rtv >= rtmin)
      }
      if (isTRUE(input$rt_max_enable)) {
        rtmax <- suppressWarnings(as.numeric(input$peak_rt_max))
        validate(need(is.finite(rtmax), "rt max cutoff must be numeric."))
        rt_pass_max <- is.finite(rtv) & (rtv <= rtmax)
      }
      
      rt_pass <- rt_pass_min & rt_pass_max
      fail_rt <- !rt_pass
    }
  
    if ("rmd" %in% sel) {
      rmd_pass_min <- rep(TRUE, before)
      rmd_pass_max <- rep(TRUE, before)
      rmd_vals <- calculate_RMD(mzv)
      
      if (isTRUE(input$rmd_min_enable)) {
        rmin <- suppressWarnings(as.numeric(input$peak_rmd_min))
        validate(need(is.finite(rmin), "RMD min bound must be numeric."))
        rmd_pass_min <- is.finite(rmd_vals) & (rmd_vals >= rmin)
      }
      if (isTRUE(input$rmd_max_enable)) {
        rmax <- suppressWarnings(as.numeric(input$peak_rmd_max))
        validate(need(is.finite(rmax), "RMD max bound must be numeric."))
        rmd_pass_max <- is.finite(rmd_vals) & (rmd_vals <= rmax)
      }
      
      rmd_pass <- rmd_pass_min & rmd_pass_max
      fail_rmd <- !rmd_pass
    }

    if ("amd" %in% sel) {
      amd_pass_min <- rep(TRUE, before)
      amd_pass_max <- rep(TRUE, before)
      amd_vals <- calculate_AMD(mzv, absolute = TRUE)
      
      if (isTRUE(input$amd_min_enable)) {
        amin <- suppressWarnings(as.numeric(input$peak_amd_min))
        validate(need(is.finite(amin), "AMD min bound must be numeric."))
        amd_pass_min <- is.finite(amd_vals) & (amd_vals >= amin)
      }
      if (isTRUE(input$amd_max_enable)) {
        amax <- suppressWarnings(as.numeric(input$peak_amd_max))
        validate(need(is.finite(amax), "AMD max bound must be numeric."))
        amd_pass_max <- is.finite(amd_vals) & (amd_vals <= amax)
      }
      
      amd_pass <- amd_pass_min & amd_pass_max
      fail_amd <- !amd_pass
    }
  
    keep_idx <- if (length(sel) == 0) rep(TRUE, before) else (mz_pass & rt_pass & rmd_pass & amd_pass)
    ids_kept <- as.character(df[[rid]][keep_idx])
  
    after <- sum(keep_idx)
    peak_state$stats$after <- after
    peak_state$stats$removed_mz  <- sum(fail_mz)
    peak_state$stats$removed_rt  <- sum(fail_rt)
    peak_state$stats$removed_rmd <- sum(fail_rmd)
    peak_state$stats$removed_amd <- sum(fail_amd)
    peak_state$stats$removed_total <- before - after
  
    peak_state$applied <- TRUE
    peak_state$show_summary <- TRUE
    peak_state$kept <- ids_kept
  
    if (length(sel) == 0) {
      showNotification("No Peak filters selected. Step 4 frozen as pass-through.", type="message", duration=4)
    } else if (length(ids_kept) == 0) {
      showNotification("Peak filters removed all features. Try relaxing cutoffs.", type="error", duration=5)
    } else {
      showNotification(sprintf("Step 4 frozen: kept %d of %d features.", after, before), type="message", duration=4)
    }
  }

  observeEvent(input$apply_peak, {
      w <- Waiter$new(
        id = "peak_table_out", 
        html = tagList(spin_6(), h4("Applying Peak Filters...", style="color:white;")),
        color = "rgba(44, 62, 80, 0.8)"
      )
      w$show()
      
      shinyjs::disable("apply_peak")
      
      shinyjs::delay(50, {
        tryCatch({
          run_step4_peak()
        }, finally = {
          w$hide()
          shinyjs::enable("apply_peak")
        })
      })
    }, ignoreInit = TRUE)

  observeEvent(input$reset_peak, {
    peak_state$applied <- FALSE
    peak_state$show_summary <- FALSE
    peak_state$kept <- NULL
    peak_state$stats <- list(before=0L, after=0L,
                             removed_mz=0L, removed_rt=0L, removed_rmd=0L, removed_amd=0L, removed_total=0L)
    showNotification("Step 4 reset: pass-through.", type="message", duration=3)
  }, ignoreInit = TRUE)

  peak_table_out_raw <- reactive({
  req(peak_table_in_raw())
  df <- as.data.frame(peak_table_in_raw(), check.names = FALSE)
  if (!isTRUE(peak_state$applied) || is.null(peak_state$kept)) return(df)

  rid <- if (".FID" %in% names(df)) ".FID" else names(df)[1]
  df[df[[rid]] %in% peak_state$kept, , drop = FALSE]
  })

  output$peak_header_out <- renderUI({ req(peak_table_out_raw()); h3("Output table — after Peak Filters") })
  output$peak_table_out  <- renderDT({
    req(peak_table_out_raw())
    datatable(peak_table_out_raw(), options = list(scrollX = TRUE, pageLength = 5))
  })

  output$peak_filter_summary <- renderUI({
    req(peak_state$applied)
    req(isTRUE(peak_state$show_summary))

    before <- peak_state$stats$before %||% 0L
    after  <- peak_state$stats$after  %||% before

    removed_mz  <- peak_state$stats$removed_mz  %||% 0L
    removed_rt  <- peak_state$stats$removed_rt  %||% 0L
    removed_rmd <- peak_state$stats$removed_rmd %||% 0L
    removed_amd <- peak_state$stats$removed_amd %||% 0L
    removed_total <- peak_state$stats$removed_total %||% (before - after)
    tagList(
    summary_table_ui(
      "Filtering results — Step 4 (Peak Filters)",
      tibble::tibble(
        Metric = c(
          "Features before",
          "Removed by m/z",
          "Removed by rt",
          "Removed by RMD",
          "Removed by AMD",
          "Total removed",
          "Features after"
        ),
        Value = c(before, removed_mz, removed_rt, removed_rmd, removed_amd, removed_total, after)
      )
    ),
    div(style = "margin-top: -10px; margin-bottom: 15px; margin-left: 5px; font-size: 13px; color: white; font-style: italic;",
          "Note: a feature may be counted in multiple filter categories.")
    )
  })

  # --------------------------
  # FINAL compile (aligned)
  # --------------------------
  final_table <- reactiveVal(NULL)

  output$finalReady <- reactive({ !is.null(final_table()) })
  outputOptions(output, "finalReady", suspendWhenHidden = FALSE)

  mat_peak_out <- reactive({
    req(mat_qc_out())
    ds <- as.data.frame(mat_qc_out())
    if (isTRUE(peak_state$applied) && !is.null(peak_state$kept) && length(peak_state$kept)) {
      ds[, intersect(colnames(ds), peak_state$kept), drop = FALSE]
    } else ds
  })

  observeEvent(input$compile_final, {
    
    w <- Waiter$new(
    html = tagList(spin_6(), h4("Compiling Final Output...", style="color:white;")),
    color = "rgba(44, 62, 80, 0.8)"
    )
    w$show()
  
  shinyjs::disable("compile_final")
  shinyjs::delay(50, {
    tryCatch({
      req(raw_fid(), input$sample_keyword0)
    
      blank_keep <- if (isTRUE(blank_state$applied)) blank_state$kept else NULL
      merge_map  <- NULL
    
      del_ms <- character(0)
      if (isTRUE(ms_state$applied)) {
        del_ms <- unique(c(
          ms_state$del_iso %||% character(0),
          ms_state$del_add %||% character(0),
          ms_state$del_nl  %||% character(0),
          ms_state$del_isf %||% character(0)
        ))
      }
    
      # final_keep: QC kept intersect Peak kept (if both exist)
      final_keep <- NULL
      if (isTRUE(qc_state$applied) && !is.null(qc_state$kept) && length(qc_state$kept)) {
        final_keep <- qc_state$kept
      }
      if (isTRUE(peak_state$applied) && !is.null(peak_state$kept) && length(peak_state$kept)) {
        final_keep <- if (is.null(final_keep)) peak_state$kept else intersect(final_keep, peak_state$kept)
      }
    
      # 1) build internal FINAL table (keeps .FID)
      ftbl <- build_final_feature_table(
        raw_df_fid       = raw_zeroed(),
        sample_keywords  = input$sample_keyword0 %||% ".mzML",
        blank_keep       = blank_keep,
        merge_map        = merge_map,
        del_ms           = del_ms,
        final_keep       = final_keep
      )
    
      # 2) make export version that matches original input style
      dtype <- shared$data_type %||% "mzmine"
    
      ftbl_export <- format_final_table_as_input(
        final_df_with_fid = ftbl,
        type              = dtype,
        export_template    = shared$export_template,
        export_colmap      = shared$export_colmap,
        export_rt_factor   = shared$export_rt_factor %||% 1
      )
    
      # 3) store both
      final_table_raw <- reactiveVal(NULL)
      final_table_raw(ftbl)          
      final_table(ftbl_export)       
    
      showNotification("FINAL compiled and aligned.", type = "message", duration = 3)
    }, finally = {
      w$hide()
      shinyjs::enable("compile_final")
    })
  })
    }, ignoreInit = TRUE)

  output$final_report_header <- renderUI({ h3("Filtering summary") })

  output$final_report_body <- renderUI({
    req(raw_fid())
    div(
      style="margin:.5rem 0 1rem; padding:.5rem .75rem; border:1px solid #dfe6e9; border-radius:8px; background:#ffffffaa;",
      tags$ul(
        tags$li(sprintf("Dataset: %s", shared$name %||% "not uploaded")),
        tags$li(sprintf("Step 1 Blank Filters: %s", if (isTRUE(blank_state$applied)) "APPLIED" else "not applied")),
        tags$li(sprintf("Step 2 MS Filters: %s", if (isTRUE(ms_state$applied)) "APPLIED" else "not applied")),
        tags$li(sprintf("Step 3 QC Filters: %s", if (isTRUE(qc_state$applied)) "APPLIED" else "not applied")),
        tags$li(sprintf("Step 4 Peak Filters: %s", if (isTRUE(peak_state$applied)) "APPLIED" else "not applied")),
        tags$li(sprintf("FINAL table rows: %s", if (!is.null(final_table())) nrow(final_table()) else "not compiled"))
      )
    )
  })

  output$final_preview_table <- renderDT({
    req(final_table())
    datatable(final_table(), options = list(scrollX = TRUE, pageLength = 5))
  })

  # --------------------------
  # Downloads
  # --------------------------

  output$dl_final_table <- downloadHandler(
  filename = function() {
      nm <- shared$name %||% "dataset.csv"
      paste0(tools::file_path_sans_ext(basename(nm)), "_filtered.csv")
  },
  content = function(file) {
    req(final_table(), shared$data_type)

    write_final_table_csv(
      path           = file,
      df_export       = final_table(),
      type            = shared$data_type,
      msdial_preamble = shared$msdial_preamble
    )
  }
  )

  output$mgf_toggle_ui <- renderUI({
  req(final_table())
    div(style = "position: relative; margin-bottom: 15px;",
              div(
                style = "margin-bottom: -15px;", 
  materialSwitch(
    inputId = "enable_mgf_filter",
    label = "Filter MGF",
    value = FALSE,
    status = "danger",
    width = "auto"
  )
              ),
              actionButton(
                inputId = "btnmgf", 
                label = "?", 
                class = "btn-primary btn-xs", 
                style = "position: absolute; top: 0px; left: 135px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
              ),
              tags$style(HTML("
                .tooltip-inner {
                  text-align: left; /* Changes alignment from center to left */
                  font-size: 16px;
                  max-width: 550px;  /* Optional: gives the text more room to breathe */
                }
              ")),
              bsTooltip(
                id = "btnmgf", 
                title = "<b>Filtering MGF file based on final filtered table</b><br><br>Feature/Peak ID in MGF should match with selected Feature ID in peak table (see details in About Tab)", 
                placement = "right", 
                trigger = "click", 
                options = list(container = "body", html = TRUE)
              )
            )
  })
  
  filtered_mgf_sps <- reactiveVal(NULL)
  mgf_processing <- reactiveVal(FALSE)
  current_mgf <- reactiveVal(NULL)
  mgf_attempted <- reactiveVal(FALSE) 
  parsed_mgf <- reactiveVal(NULL)
  mgf_true_names <- reactiveVal(NULL)
  
  # 1. READ ONCE ON UPLOAD
  observeEvent(input$mgf_input, {
    req(input$mgf_input)
    mgf_attempted(FALSE)
    ext <- tools::file_ext(input$mgf_input$name)
    if (tolower(ext) != "mgf") {
      showNotification("Error: Please upload a valid .mgf file.", type = "error", duration = 5)
      shinyjs::reset("mgf_input")
      current_mgf(NULL)
      parsed_mgf(NULL)
      mgf_true_names(NULL)
      return()
    }
    
    current_mgf(input$mgf_input$datapath)
    
    w <- Waiter$new(
      html = tagList(spin_6(), h4("Loading MGF into memory...", style="color:white;")),
      color = "rgba(44, 62, 80, 0.8)"
    )
    w$show()
    
    shinyjs::delay(50, {
      tryCatch({
        # Read the file and store it in RAM
        sps <- Spectra::Spectra(input$mgf_input$datapath, source = MsBackendMgf::MsBackendMgf())
        parsed_mgf(sps)
        mgf_true_names(names(Spectra::spectraData(sps)))
        
        showNotification(paste("MGF file", input$mgf_input$name, "loaded successfully!"), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error parsing MGF:", e$message), type = "error", duration = 5)
      }, finally = {
        w$hide()
      })
    })
  })
  
  # 2. GENERATE BOTH DROPDOWNS TOGETHER
  output$mgf_matching_ui <- renderUI({
    # Wait until both the MGF is in memory AND the final table exists
    req(mgf_true_names())
    req(final_table())
    
    # Setup Peak Table column choices
    peak_cols <- names(final_table())
    peak_default <- input$row_id_col0
    colmap <- shared$export_colmap
    if (!is.null(colmap) && peak_default %in% names(colmap)) {
      mapped <- colmap[[peak_default]]
      if (nzchar(mapped) && mapped %in% peak_cols) peak_default <- mapped
    }
    
    # Setup MGF ID choices
    mgf_cols <- mgf_true_names()
    mgf_default <- if ("acquisitionNum" %in% mgf_cols) "acquisitionNum" else mgf_cols[1]
    
    tagList(
      selectInput("mgf_match_col", "Select ID column in Table:", 
                  choices = peak_cols, selected = peak_default),
      selectInput("mgf_id_field", "MGF Feature ID field:", 
                  choices = mgf_cols, selected = mgf_default)
    )
  })
  
  # TRIGGER 1: The Button Click
  # This ONLY starts the animation and resets data
  observeEvent(input$run_mgf_filter, {
    if (is.null(current_mgf())) {
      showNotification("Please upload an MGF file first!", type = "error", duration = 5)
      return()
    }
    req(input$mgf_input, final_table(), input$mgf_match_col, input$mgf_id_field)
    
    filtered_mgf_sps(NULL)    # Clear old data
    mgf_processing(TRUE)      # This is the "Flag" to start the work
    mgf_attempted(TRUE)
    shinyjs::disable("run_mgf_filter")
  })

  # TRIGGER 2: The Actual Calculation (From RAM)
  observe({
    req(mgf_processing() == TRUE)
    withProgress(message = 'Filtering MGF...', value = 0, {
      tryCatch({
        gc() 
        
        # Grab the pre-loaded MGF object directly from RAM
        sps <- parsed_mgf()
        
        # Matching logic
        sel_f <- as.character(final_table()[[input$mgf_match_col]])
        id_field <- input$mgf_id_field 
        mgf_ids <- as.character(Spectra::spectraData(sps)[[id_field]])
        
        idx <- which(trimws(mgf_ids) %in% trimws(sel_f))
        if(length(idx) == 0) idx <- which(as.numeric(mgf_ids) %in% as.numeric(sel_f))
        
        if(length(idx) > 0) {
          res <- sps[idx]
          filtered_mgf_sps(res) # This will stop the spinner
          showNotification(sprintf("Success! %d spectra matched.", length(idx)), type = "message")
        } else {
          showNotification("No matches found.", type = "error")
        }
        
        # Clean up the massive original MGF object
        rm(sps)
        gc()
        
      }, error = function(e) {
        showNotification(paste("MGF Error:", e$message), type = "error")
      })
      
      # Reset flag and re-enable UI
      mgf_processing(FALSE)
      shinyjs::enable("run_mgf_filter")
    })
  })

  output$mgf_status_ui <- renderUI({
  req(isTRUE(mgf_attempted()))  
  if (isTRUE(mgf_processing())) return(NULL)
  req(input$run_mgf_filter > 0)
  req(current_mgf())
  sps <- filtered_mgf_sps()
  if (is.null(sps) || length(sps) == 0) {
    div(style = "margin-top: 10px; padding: 10px; background: #f2dede; border-radius: 5px; color: #a94442;",
        tags$b("Warning: "), 
        "No matching spectra were found in the MGF file."
    )
  } else {
    div(style = "margin-top: 10px; padding: 10px; background: #dff0d8; border-radius: 5px; color: #3c763d;",
        tags$b("Success! "), 
        sprintf("Matched %d spectra from your filtered table.", length(sps))
    )
  }
  })
  
  observeEvent(input$clear_mgf, {
    shinyjs::reset("mgf_input")     
    current_mgf(NULL)
    parsed_mgf(NULL)        
    mgf_true_names(NULL)    
    filtered_mgf_sps(NULL)          
    mgf_processing(FALSE)  
    mgf_attempted(FALSE)
    gc()                            
    showNotification("MGF file and filtered data cleared from memory.", type = "warning", duration = 4)
  }, ignoreInit = TRUE)
  
  output$dl_mgf <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$mgf_input$name), "_filtered.mgf")
    },
    content = function(file) {
      req(filtered_mgf_sps())
      # Export using the Mgf backend
      Spectra::export(filtered_mgf_sps(), backend = MsBackendMgf(), file = file)
    }
  )

  # --------------------------
  # About
  # --------------------------
observeEvent(input$help_go_tab, {
  req(input$help_section)
  # only jump if it's one of the real tabs
  if (input$help_section %in% c("upload","blank","ms","qc","peak", "final")) {
    updateTabsetPanel(session, "tabs", selected = input$help_section)
  }
})

# About content
output$help_body <- renderUI({
  sec <- input$help_section %||% "quick"

  if (sec == "quick") {
    return(div(
      h3("Quick start"),
      tags$ol(
        tags$li(tags$b("Data upload:"), " Upload your CSV, choose correct sample column names keywords (identifiers) + mz & rt columns."),
        tags$li(tags$b("Blank filters:"), " Define Blank group, set Blank filter -> Apply."),
        tags$li(tags$b("MS filters:"), " Enable Deleting Isotopes/Adducts/Neutral Loses/Fragments -> Apply."),
        tags$li(tags$b("QC filters:"), " Define groups, choose Zero/RSD/Mean/Min filters -> Apply."),
        tags$li(tags$b("Peak filters:"), " Choose mz/rt/RMD/AMD cutoffs -> Apply. "),
        tags$li(tags$b("Final summary:"), " Compile summary & export final dataset."),
        tags$li(tags$b("About:"), " Description, Project Details, References.")
      ),
      div(class="highlight", "Rule: The last “Apply” click is saved."),
      br(),
      div(class="highlight", "Rule: Filters are applied sequentially. Changes made on later tabs do not affect the results of previous tabs."),
      br(),
      div(class="highlight", "Rule: Click “Clear” to skip any previous calculation in the tab."),
      br(),
      div(class="highlight", "Rule: Click the question mark icon to toggle the information tooltip."),
      br(),
      div(class="highlight", "Rule: Output Tables reflect the last “Apply” run."),
      br(),
      div(class="highlight", "Rule: Plots are displayed and updated only after clicking the plot buttons, cutoff value on them is updated dynamically."),
      br(),
      div(class="highlight", "Rule: Isotopes-Dimers/Adducts/Neutral Loses/In-Source Fragments tables are displayed after activating the checkbox."),
      br(),
      tags$img(
        src = "https://raw.githubusercontent.com/plyush1993/MetaboCensoR/main/Server_Map.png", 
        width = "800px", 
        height = "420px",
        style = "display: block; margin-bottom: 20px;" # Adds a little space below it
      )
      ))
  }

  if (sec == "upload") {
    return(div(
      h3("Data upload"),
      tags$ul(
        tags$li(tags$b("Upload Peak Table (csv):"), " choose peak table type and then select specified columns."),
        tags$li(tags$b("Check preview:"), " to confirm that sample columns were detected correctly."),
        tags$li(tags$b("Row ID column:"), " pick the column that uniquely identifies each feature."),
        tags$li(tags$b("mz / rt columns:"), " define columns with m/z & rt values."),
        tags$li(tags$b("Sample column keywords:"), " add patterns that match your sample columns (e.g. .mzML, _Area)."),
        tags$li(tags$b("Clear dataset:"), " to delete any calculations and uploaded files.")
      ),
      br(),
      div(class="highlight", 
    HTML("Tip: Try the Example dataset by clicking the button to overview the full App functionality.<br>
    If use this example, run the Blank filter first to save memory for further steps. You can keep all parameters by default.<br>
    It is the LC-MS profiling dataset, described in the study 
    <a href='https://pubs.acs.org/doi/10.1021/acs.analchem.4c05577' target='_blank'>[1]</a>.<br>
    Briefly, methanol extracts from the plant ashwagandha [<i>Withania somnifera</i> (L.) Dunal] together with blanks were analyzed on Thermo Q-Exactive Plus Orbitrap in DDA positive mode. Raw mzML files were then processed in mzMine in default pre-settings for UPLC-DDA.<br>
    Available in the
    <a href='https://github.com/plyush1993/MetaboCensoR/blob/main/Input_Examples/orbi_iimn_gnps_quant.csv' target='_blank'>GitHub</a>.")),
      br(),
      div(class="highlight", "Tip: If “No sample columns” message, your sample keywords don’t match sample column names."),
      br(),
      div(class="highlight", "Note: The `feature_id` column is auto-generated by default. To avoid conflicts, please do not include a column with this exact name in your uploaded dataset."),
      br(),
      div(class="highlight", "Note: We recommend letting the application use its auto-generated `feature_id` to prevent any conflicts during  processing."),
      br(),
      div(class="highlight", "Note: for xcms type rt column is automatically converted to min."),
      br(),
      div(class = "highlight",
      "Tip: examples of accessible Peak Table data format are provided in the ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/tree/main/Input_Examples", "GitHub", target = "_blank"),
      " repository.")
    ))
  }

  if (sec == "blank") {
    return(div(
      h3("Blank filters"),
      tags$ul(
        tags$li(tags$b("Labels:"), " choose label (group) source from sample names (token + separator) or upload labels in CSV (one column no headers)."),
        tags$li(tags$b("Blank filter:"), " select blank group(s) (at least one) -> mode (by cutoff or any peak) -> Apply blank filter."),
        tags$ul(
                  tags$li(tags$b("Cutoff mode:"), " calculates Mean values for Blank group(s) and keeps feature if it is lower than cutoff * maximum mean value among experimental (other) group(s). The default is 0.1, meaning the blank signal should be less than 10% of the signal in the sample group in which it is most abundant"),
                  tags$li(tags$b("Drop any mode:"), "deletes any feature detected in Blank group(s)")
                        ),
        tags$li(tags$b("Plot values distribution:"), " is displayed and updated only after clicking the plot buttons, cutoff value on it is updated dynamically."),
        br(),
        div(class="highlight", "Note: We recommend to perform a Blank filter wherever it is applicable since it removes ghost/background signals and simplifies any further calculations.")
        )
    ))
  }

  if (sec == "ms") {
    return(div(
      h3("MS filters"),
      tags$ul(
        tags$li(tags$b("Isotopes/Dimers:"), " define number of C13 isotopes (n) and possible charges (z_max), and dimer seria for C13*(n_d + 0.5). Uses m/z and RT shifts + correlation threshold to detect isotopes/dimers features by graph and retains most intense isotope/dimer in each family.",
                tags$br(),
                "Note: isotope/dimer family is determined by graph, thus, we recommend to keep the default value of n is 1 and n_d 3."),
        tags$li(tags$b("Adducts:"), " define polarity and minimal neutral mass. By default employs built-in Adducts list (see details below). Uses m/z and RT shifts + correlation threshold to detect adducts features by graph and retains most intense adduct in each family.",
                tags$br(),
                "Note: we recommend to enable `Strict RT split inside clusters` option, that check that adducts always fulfill defined rt tolerance even after grouping by graph."),
        tags$li(tags$b("Neutral Loses:"), " define polarity. By default employs built-in Neutral Losses list (see details below). Uses m/z and RT shifts + correlation threshold to detect neutral loses features by graph and retains ion with highest m/z in each family.", 
                tags$br(),
                "Note: we recommend to enable `Strict RT split inside clusters` option, that check that fragments always fulfill defined rt tolerance even after grouping by graph."),
        tags$li(tags$b("In-Source Fragments:"), " uses RT shift + correlation threshold to detect in-source fragment features by graph and retains ion with highest m/z in each family.", 
                tags$br(),
                "Note: we recommend to enable `Strict RT split inside clusters` option, that check that fragments always fulfill defined rt tolerance even after grouping by graph.",
                tags$br(),
                "Note: we recommend to enable `Control intensity ratio` option, that check precursor / fragment intensity ratio.")
      ),
      div(class="highlight", "Rule: Filters are always applied sequentially. Start with Isotopes, then Adducts, Neutral Loses, and In-Source Fragments. We recommend to keep the order to avoid misannotation."),
      br(),
      div(class="highlight", "Note: You can print and download a table describing any stage of MS filtration. These tables can be used for peak annotation purposes."),
      br(),
      div(class="highlight", "Note: Any MS filter relies on correlation, so maintain a sufficient number of samples and missing values."),
      br(),
      div(class = "highlight",
      "Tip: See Isotopes & Dimers shift table ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/blob/main/Isotopes&Dimers.md", "here", target = "_blank"),
      "."
    ),
      br(),
      div(class = "highlight",
      "Tip: You can upload your custom Adduct list. Default built-in Adducts list is from ",
      tags$a(href = "https://cran.r-project.org/web/packages/nontarget/index.html", "[1]", target = "_blank"),
      " and available ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/blob/main/adducts%20(nontarget).csv", "here", target = "_blank"),
      "."
    ),
    br(),
      div(class = "highlight",
      "Tip: You can upload your custom Neutral Losses list. Default built-in Neutral Losses list is from ",
      tags$a(href = "https://academic.oup.com/bioinformatics/article/41/5/btaf161/8114000", "[2]", target = "_blank"),
      ", ",
      tags$a(href = "https://pubs.acs.org/doi/10.1021/ac502818e", "[3]", target = "_blank"),
      " and available ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/blob/main/neutral%20loss%20data%20(MS1FA%20%26%20Fiehn).csv", "here", target = "_blank"),
      "."
    )
    )
    )
  }
  
  if (sec == "qc") {
    return(div(
      h3("QC filters"),
      tags$ul(
        tags$li(tags$b("Labels:"), " choose label (group) source from sample names (token + separator) or upload labels in CSV (one column no headers)."),
        tags$li(tags$b("Value filters:"), " choose group for removal -> specify value(s): zeros(by counts or %) / mean / rsd / min -> mode of filtering (ANY/EVERY/POOLED) -> Apply value filters.",
                tags$ul(
                        tags$li(tags$b("ANY"), " - satisfies threshold in at least one of the selected groups"),
                        tags$li(tags$b("EVERY"), " - satisfies threshold in all of the selected groups"), 
                        tags$li(tags$b("POOLED"), " - calculates average values among all of the selected groups and then compares"),
                        tags$li(tags$b("Note:"), " If no group is selected, all groups are considered")
                        )),
        tags$li(tags$b("Plot values distribution:"), " is displayed and updated only after clicking the plot buttons, cutoff value on it is updated dynamically."),
        br(),
        div(class="highlight", "Note: We recommend to apply drift/batch correction, normalization, and imputation for large-scale metabolomics study before QC filters, while all other filters should be applied before.")
      )
    ))
  }
  
  if (sec == "peak") {
    return(div(
      h3("Peak filters"),
      tags$ul(
        tags$li(tags$b("Overview:"), " alows to filter peaks by m/z, rt and mass defect (absolute and relative) values."),
        tags$li(tags$b("Pick filters:"), " m/z, rt, RMD, and AMD bounds."),
        tags$li(tags$b("Plot values distribution:"), " is displayed and updated only after clicking the plot buttons, cutoff value on it is updated dynamically.")
      ),
      div(class="highlight", "Note: optional step to filter peaks based on apriori information")
    ))
  }

  if (sec == "final") {
    return(div(
      h3("Final summary"),
      tags$ul(
        tags$li(tags$b("Compile summary & final datasets:"), " final filtered peak table after all applied filters."),
        tags$li(tags$b("MGF filtering:"), " activated after final compile. Filters MGF file based on final filtered table. Feature/Peak ID in MGF should match with selected Feature ID in peak table.")
      ),
      div(class="highlight", "Note: if something looks missing in final output, check tab by tab where it was filtered."),
      br(),
      div(class="highlight", "Important: for xcms peak table all NA values are converted to 0."),
      br(),
      div(class = "highlight",
      "Tip: examples of accessible MGF data format are provided in the ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/tree/main/Input_Examples", "GitHub", target = "_blank"),
      " repository.")
    ))
  }

  # troubleshooting
  if (sec == "trouble") {
    return(div(
    h3("Troubleshooting"),
    tags$ul(
      tags$li(tags$b("No sample columns:"), " your sample keywords don’t match sample column names."),
      tags$li(tags$b("Parsing error:"), " check selected Data table type and compatibility with examples of accessible Peak Table data formats in the ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/tree/main/Input_Examples", "GitHub", target = "_blank", style = "color: #ffcc00; font-weight: bold;"),
      " repository."),
      tags$li(tags$b("Disconnected from the server:"), " could be caused by memory limit, try to compile and run it locally. Instructions in the ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR", "GitHub", target = "_blank", style = "color: #ffcc00; font-weight: bold;"),
      " repository.")
    )
  ))
  }
  
   # project details
   if (sec == "about") {
    return(div(
    h3("Project Details"),
    tags$ul(
      tags$li(HTML('Developed by:&nbsp;   
       <a href="https://github.com/plyush1993/" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          Ivan Plyushchenko
       </a>')),
      tags$li(HTML('Citation:&nbsp;
       <a href="https://www.doi.org/" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          DOI
       </a>')),
      tags$li(HTML('Project Page:&nbsp;
       <a href="https://github.com/plyush1993/MetaboCensoR" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          GitHub
       </a>')),
      tags$li(HTML('Case Studies:&nbsp;
       <a href="https://github.com/plyush1993/MetaboCensoR_Examples" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          GitHub
       </a>')),
      tags$li(HTML('Error Report:&nbsp;
       <a href="https://github.com/plyush1993/MetaboCensoR/issues" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          Issues
       </a>')),
      tags$li(HTML('Feedback:&nbsp;
       <a href="mailto:plyushchenko.ivan@gmail.com" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
         <i class="fa fa-envelope" style="margin-right: 4px;"></i>
       </a>'))
    )
    ))
    }

})
  
}

# ==============================
# Run
# ==============================
shinyApp(ui, server)