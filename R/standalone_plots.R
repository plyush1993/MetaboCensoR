# Plot saved result tables. No filter metric is recalculated here.
.mcs_plot_source <- local({
.mcs_layout <- function(...) NULL # Bound to the real renderer per call.


blank_render <- function() {
    res <- blank_plot_data()
    req(res)
    
    mode <- res$mode
    df <- res$data
    req(nrow(df) > 0)
    
    total_feats <- nrow(df)

        if (mode == "mean_cutoff") {
      current_cutoff <- input$blank_cutoff %||% 0.10

      .validate(
        need(
          length(current_cutoff) == 1L &&
            is.finite(current_cutoff) &&
            current_cutoff >= 0,
          "Enter a finite, non-negative blank cutoff."
        )
      )

      # Match the actual filtering rule, including zero cases.
      keep <- df$keep
      keep[is.na(keep)] <- FALSE

      removed_count <- sum(!keep)
      perc_removed <- round(100 * removed_count / total_feats, 1)

      # Only positive finite ratios can appear on a log axis.
      positive <- df$Ratio[df$Status == "Positive finite ratio"]

      fmt <- function(x) {
        trimws(formatC(x, format = "g", digits = 4))
      }

      bins <- data.frame(
        x = numeric(0),
        count = integer(0),
        hover = character(0)
      )

      # Default display range when all ratios are zero/undefined.
      axis_range <- c(-2, 1)
      bin_width <- 0.1

      if (length(positive)) {
        log_values <- log10(positive)

        lower <- floor(min(log_values))
        upper <- ceiling(max(log_values))

        # Also handle datasets with one unique ratio.
        if (upper <= lower) upper <- lower + 1

        edges <- seq(lower, upper, length.out = 51)

        h <- graphics::hist(
          log_values,
          breaks = edges,
          plot = FALSE,
          include.lowest = TRUE,
          right = FALSE
        )

        left <- head(h$breaks, -1)
        right <- tail(h$breaks, -1)

        bins <- data.frame(
          x = h$mids,
          count = h$counts,
          hover = paste0(
            "Blank/sample ratio: ",
            fmt(10^left),
            " \u2013 ",
            fmt(10^right),
            "<br>Features: ",
            h$counts
          )
        )

        axis_range <- range(h$breaks)
        bin_width <- diff(h$breaks)[1]
      }

      # Include the cutoff in the visible range.
      shapes <- list()

      if (current_cutoff > 0) {
        cutoff_position <- log10(current_cutoff)
        axis_range <- range(c(axis_range, cutoff_position))

        shapes <- list(
          list(
            type = "line",
            xref = "x",
            yref = "paper",
            x0 = cutoff_position,
            x1 = cutoff_position,
            y0 = 0,
            y1 = 1,
            line = list(
              color = "red",
              dash = "dash",
              linewidth = 1
            )
          )
        )
      }

      # Positions are logarithmic; tick labels show original ratios.
      tick_positions <- seq(
        floor(axis_range[1]),
        ceiling(axis_range[2]),
        by = 1
      )

      # Avoid overcrowding for unusually wide distributions.
      if (length(tick_positions) > 12L) {
        tick_positions <- tick_positions[unique(round(seq(1, length(tick_positions), length.out = 12)))]
      }

      count_status <- function(label) {
        sum(df$Status == label)
      }

     other_count <- count_status("Other / undefined")

      summary_text <- paste0(
        "<b>Filtered: ", removed_count, " of ", total_feats,
        " (", perc_removed, "%)</b>",
        " | Cutoff: ", fmt(current_cutoff),

        "<br><b>Not shown on log axis:</b> ",
        "blank = 0, sample &gt; 0: ",
        count_status("Zero blank / positive sample"),

        "; blank &gt; 0, sample = 0: ",
        count_status("Positive blank / zero sample"),

        "; both = 0: ",
        count_status("Both zero"),

        if (other_count > 0) {
          paste0("; other/undefined: ", other_count)
        } else {
          ""
        }
      )

      annotations <- list(
        list(
          x = 0,
          y = 1.02,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          text = summary_text,
          align = "left",
          showarrow = FALSE,
          font = list(size = 12, color = "#2c3e50")
        )
      )

      if (!length(positive)) {
        annotations[[length(annotations) + 1L]] <- list(
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          text = "No positive finite ratios to display.",
          showarrow = FALSE
        )
      }

      p <- plotly::plot_ly(
        data = bins,
        x = ~x,
        y = ~count,
        type = "bar",
        text = ~hover,
        textposition = "none",
        hoverinfo = "text",
        marker = list(
          color = "royalblue",
          line = list(color = "black", width = 0.5)
        ),
        showlegend = FALSE
      ) %>%
        plotly::layout(
          title = list(
            text = "Blank to Sample Ratio Distribution",
            y = 0.99
          ),
          xaxis = list(
            title = paste0(
              "Ratio: Mean(Blank) / Max(Mean(Sample))",
              " (log10)"
            ),
            tickmode = "array",
            tickvals = tick_positions,
            ticktext = fmt(10^tick_positions),
            range = axis_range + c(-0.03, 0.03),
            zeroline = FALSE
          ),
          yaxis = list(
            title = "Feature count",
            rangemode = "tozero"
          ),
          shapes = shapes,
          annotations = annotations,
          margin = list(t = 75, b = 65, l = 65, r = 20),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          bargap = 0
        )

      return(p)

    } else if (mode == "drop_detected") {
      # --- PLOT 2: Bar chart for Drop Detected ---
      removed_count <- sum(df$Detected, na.rm = TRUE)
      perc_removed <- round((removed_count / total_feats) * 100, 1)

      # Create summary table for ggplot
      summary_df <- as.data.frame(table(df$Status))
      names(summary_df) <- c("Status", "Count")

      gg <- ggplot(summary_df, aes(x = Status, y = Count, fill = Status)) +
        geom_bar(stat = "identity", alpha = 0.8, color = "black", width = 0.5) +
        scale_fill_manual(values = c("Filtered" = "indianred", "Kept" = "mediumseagreen")) +
        theme_minimal() +
        labs(x = "", y = "Feature Count", title = "Blank Detection Status") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

      p <- ggplotly(gg, tooltip = c("y", "x")) %>% 
        .mcs_layout(
          annotations = list(
            x = 0.95, y = 0.95, 
            text = paste0("<b>Filtered: ", perc_removed, "%</b>"),
            showarrow = FALSE, xref = 'paper', yref = 'paper',
            font = list(color = "red", size = 16)
          ),
          margin = list(t = 50)
        )
      return(p)
    }
  }



qc_render <- function() {
    req(isTRUE(qc_plot_visible()))
    md_list <- qc_plot_data()
    
    .validate(
      need(!is.null(md_list) && length(md_list) > 0, 
           "Please select at least one QC filter (Zeros, Mean, RSD, D-ratio, or Min) in the sidebar and click 'Plot values distribution'.")
    )

    plots_list <- list()
    
    for (metric_name in names(md_list)) {
      md <- md_list[[metric_name]]
      df_all <- md$data
      if (!nrow(df_all)) next
      
      df <- df_all[is.finite(df_all$value), , drop = FALSE]
      
      # Preserve existing behavior for other metrics.
      if (!nrow(df) && !md$metric %in% c("rsd", "dratio")) next

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
      } else if (md$metric == "dratio") {
        xlab_txt <- "D-ratio (%)"
        cutoff <- input$dratio_cutoff %||% 50
      } else {
        xlab_txt <- "RSD (%)"
        cutoff <- input$rsd_cutoff %||% 30
      }

      # 2. EXACT MATH MATCHING
      eval_df <- if (md$metric %in% c("rsd", "dratio")) df_all else df

      feat_eval <- unique(df_all[, c("Feature", "keep")])
      names(feat_eval)[names(feat_eval)=="keep"] <- "pass"
      total_feats <- nrow(feat_eval)
      removed_feats <- sum(!feat_eval$pass, na.rm = TRUE)
      perc_filt <- round((removed_feats / total_feats) * 100, 1)

      # 3. DOUBLE SAFETY: Put the xlab_txt (metric type) directly into the header
      # If all RSD values are undefined, retain an empty histogram
      # with a header reporting that all features fail.
      if (!nrow(df)) {
        df <- df_all[1, , drop = FALSE]
        df$value <- NA_real_
      }
      metric_title <- if (md$metric == "dratio") {"D-Ratio"
      } else {toupper(md$metric)
      }

      df$plot_header <- paste0(
        metric_title, " [", xlab_txt, "] - ", mode_txt,
        " | Filtered: ", removed_feats, " of ", total_feats,
        " (", perc_filt, "%)"
      )

      if (mode %in% c("group_any", "group_every")) {
        gg <- ggplot(df, aes(x = value, fill = Label)) +
          geom_histogram(alpha = 0.8, position = "dodge", color = "black", bins = 50, boundary = 0) +
          scale_fill_hue(h = c(180, 300))
      } else {
        gg <- ggplot(df, aes(x = value)) +
          geom_histogram(alpha = 0.8, color = "black", fill = "springgreen3", bins = 50, boundary = 0)
      }

      gg <- gg +
        geom_vline(xintercept = cutoff, color = "red", linetype = "dashed", linewidth = 1) +
        facet_wrap(~ plot_header) + 
        theme_minimal() +
        theme(strip.text = element_text(size = 12, face = "bold", color = "#2c3e50"),
              strip.background = element_rect(fill = "#ecf0f1", color = NA)) +
        labs(x = NULL, y = NULL) # Native ggplot axis

      if (md$metric %in% c("mean", "min")) {
        gg <- gg + scale_x_continuous(trans = log10_plus_one_trans(), breaks = function(limits) {
                                                                                      max_val <- max(limits, na.rm = TRUE)
                                                                                      if (!is.finite(max_val) || max_val <= 0) return(0)
                                                                                      c(0, 10^seq(1, ceiling(log10(max_val))))
                                                                                    })
      }
      
      # 4. FORCE PLOTLY AXES: Tell Plotly not to drop the labels
      p <- ggplotly(gg) %>% 
        .mcs_layout(
          xaxis = list(title = NULL),
          yaxis = list(title = "Count")
        )
      
      plots_list[[length(plots_list) + 1]] <- p
    }

    # 5. Assemble Subplots
    if (length(plots_list) == 0) {
      # Catch the empty list before Plotly crashes
      .validate(
        need(FALSE, "Not enough valid data points to generate the plot (e.g., groups may have too few samples to calculate RSD).")
      )
    } else if (length(plots_list) == 1) {
      plots_list[[1]]
    } else {
      subplot(plots_list, nrows = length(plots_list), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.06) %>% 
        .mcs_layout(showlegend = T)
    }
  }



peak_render <- function() {
    res <- peak_plot_data()
    dd <- res$data
    sel <- res$sel
    plots <- list()

    total_feats <- nrow(dd)

    # Helper function to create a consistent, highly visible annotation box
    make_annot <- function(text) {
      list(
        x = 0.98, y = 0.98, # Near top right corner
        text = text,
        showarrow = FALSE, 
        xref = 'paper', yref = 'paper',
        xanchor = 'right', yanchor = 'top',
        font = list(color = "#e74c3c", size = 14),
        bgcolor = "rgba(255,255,255,0.85)", 
        bordercolor = "#e74c3c", 
        borderpad = 5
      )
    }

    # 1. m/z vs RT Scatter (Combined plot)
    if (any(c("mz","rt") %in% sel)) {
      p_mzrt <- ggplot(dd, aes(x = RT, y = mz)) +
        geom_point(alpha = 0.7, size = 3, color = "black", shape = 21, fill = "lightblue") +
        theme_minimal() +
        labs(x = "RT (min)", y = "m/z", title = "m/z vs RT")
      
      fail_mz <- rep(FALSE, total_feats)
      fail_rt <- rep(FALSE, total_feats)

      if ("rt" %in% sel) {
        if (isTRUE(input$rt_min_enable)) {
          rtmin <- input$peak_rt_min %||% 1
          p_mzrt <- p_mzrt + geom_vline(xintercept = rtmin, color = "red", linetype = "dashed", linewidth = 1)
          fail_rt <- fail_rt | !dd$pass_rt
        }
        if (isTRUE(input$rt_max_enable)) {
          rtmax <- input$peak_rt_max %||% 30
          p_mzrt <- p_mzrt + geom_vline(xintercept = rtmax, color = "red", linetype = "dashed", linewidth = 1)
          fail_rt <- fail_rt | !dd$pass_rt
        }
      }
      if ("mz" %in% sel) {
        if (isTRUE(input$mz_min_enable)) {
          mzmin <- input$peak_mz_min %||% 200
          p_mzrt <- p_mzrt + geom_hline(yintercept = mzmin, color = "red", linetype = "dashed", linewidth = 1)
          fail_mz <- fail_mz | !dd$pass_mz
        }
        if (isTRUE(input$mz_max_enable)) {
          mzmax <- input$peak_mz_max %||% 1500
          p_mzrt <- p_mzrt + geom_hline(yintercept = mzmax, color = "red", linetype = "dashed", linewidth = 1)
          fail_mz <- fail_mz | !dd$pass_mz
        }
      }
      
      # Calculate removed counts for the annotation
      rem_mz <- sum(fail_mz, na.rm = TRUE)
      rem_rt <- sum(fail_rt, na.rm = TRUE)
      rem_both <- sum(fail_mz | fail_rt, na.rm = TRUE)
      
      annot_text <- ""
      if ("mz" %in% sel && "rt" %in% sel) {
        annot_text <- paste0("<b>Filtered m/z: ", rem_mz, "<br>Filtered rt: ", rem_rt, "<br>Total Filtered: ", rem_both, "</b>")
      } else if ("mz" %in% sel) {
        annot_text <- paste0("<b>Filtered m/z: ", rem_mz, "</b>")
      } else {
        annot_text <- paste0("<b>Filtered rt: ", rem_rt, "</b>")
      }

      plots[["mzrt"]] <- ggplotly(p_mzrt) %>% toWebGL() %>% .mcs_layout(annotations = list(make_annot(annot_text)))
    }

    # 2. RMD Scatter
    if ("rmd" %in% sel) {
      dd2 <- dd[is.finite(dd$RMD), , drop = FALSE]
      if (nrow(dd2) > 0) {
        p_rmd <- ggplot(dd2, aes(x = mz, y = RMD)) +
          geom_point(alpha = 0.7, size = 3, color = "black", shape = 21, fill = "indianred") +
          theme_minimal() +
          labs(x = "m/z", y = "RMD (ppm)", title = "RMD vs m/z")
        
        fail_rmd <- rep(FALSE, nrow(dd2))
        
        if (isTRUE(input$rmd_min_enable)) {
          rmin <- input$peak_rmd_min %||% -2000
          p_rmd <- p_rmd + geom_hline(yintercept = rmin, color = "red", linetype = "dashed", linewidth = 1)
          fail_rmd <- fail_rmd | !dd2$pass_rmd
        }
        if (isTRUE(input$rmd_max_enable)) {
          rmax <- input$peak_rmd_max %||% 2000
          p_rmd <- p_rmd + geom_hline(yintercept = rmax, color = "red", linetype = "dashed", linewidth = 1)
          fail_rmd <- fail_rmd | !dd2$pass_rmd
        }
        
        rem_rmd <- sum(fail_rmd, na.rm = TRUE)
        annot_text <- paste0("<b>Filtered RMD: ", rem_rmd, "</b>")

        plots[["rmd"]] <- ggplotly(p_rmd) %>%  toWebGL() %>% .mcs_layout(annotations = list(make_annot(annot_text)))
      }
    }

    # 3. AMD Scatter
    if ("amd" %in% sel) {
      dd3 <- dd[is.finite(dd$AMD), , drop = FALSE]
      if (nrow(dd3) > 0) {
        p_amd <- ggplot(dd3, aes(x = mz, y = AMD)) +
          geom_point(alpha = 0.7, size = 3, color = "black", shape = 21, fill = "mediumpurple") +
          theme_minimal() +
          labs(x = "m/z", y = "AMD (Da)", title = "AMD vs m/z")
        
        fail_amd <- rep(FALSE, nrow(dd3))
        
        if (isTRUE(input$amd_min_enable)) {
          amin <- input$peak_amd_min %||% 0.00
          p_amd <- p_amd + geom_hline(yintercept = amin, color = "red", linetype = "dashed", linewidth = 1)
          fail_amd <- fail_amd | !dd3$pass_amd
        }
        if (isTRUE(input$amd_max_enable)) {
          amax <- input$peak_amd_max %||% 0.50
          p_amd <- p_amd + geom_hline(yintercept = amax, color = "red", linetype = "dashed", linewidth = 1)
          fail_amd <- fail_amd | !dd3$pass_amd
        }
        
        rem_amd <- sum(fail_amd, na.rm = TRUE)
        annot_text <- paste0("<b>Filtered AMD: ", rem_amd, "</b>")

        plots[["amd"]] <- ggplotly(p_amd) %>% toWebGL() %>% .mcs_layout(annotations = list(make_annot(annot_text)))
      }
    }

    .validate(need(length(plots) > 0, "Nothing to plot for the current selection."))
    
    if (length(plots) == 1) {
      plots[[1]]
    } else {
      # Use Plotly's subplot feature to combine the graphs dynamically
      subplot(plots, nrows = length(plots), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.08) %>%
        .mcs_layout(title = "")
    }
  }

log10_plus_one_trans <- function() {
  scales::trans_new(
    "log10_plus_one",
    function(x) log10(x + 1),
    function(x) 10^x - 1,
    domain = c(0, Inf)
  )
}
 environment()
})

.mcs_plot_environment <- function(x, input, interactive, webgl = TRUE) {
  .mcs_check(x)
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE))
    stop("Install ggplot2 and scales for plotting.")
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) stop("Install plotly for interactive plots.")
  e <- new.env(parent = .mcs_core)
  for (nm in c("ggplot", "aes", "geom_bar", "geom_histogram", "geom_vline", "geom_hline",
               "geom_point", "scale_fill_manual", "scale_fill_hue", "theme_minimal",
               "labs", "theme", "element_text", "element_rect", "facet_wrap", "scale_x_continuous"))
    e[[nm]] <- getExportedValue("ggplot2", nm)
  e$input <- input
  e$need <- function(test, message) { if (!isTRUE(test)) stop(message, call. = FALSE); invisible(NULL) }
  e$.validate <- function(...) { force(list(...)); invisible(NULL) }
  e$req <- function(...) {
    for (v in list(...)) if (is.null(v) || !length(v) || identical(v, FALSE))
      stop("No data available to plot; use the analysis object before filtering.", call. = FALSE)
    invisible(NULL)
  }
  e$captured <- list()
  e$ggplotly <- function(p, ...) {
    e$captured[[length(e$captured)+1L]] <- p
    if (interactive) plotly::ggplotly(p, ...) else structure(list(), class="mc_plot_stub")
  }
  e$.mcs_layout <- function(p, ...) {
    force(p)  # Capture this panel before applying its annotation (R arguments are lazy).
    args <- list(...)
    if (!interactive && length(args$annotations) && length(e$captured)) {
      a <- args$annotations
      if (!is.null(a$text)) a <- list(a)
      texts <- vapply(a, function(z) if(is.null(z$text)) "" else z$text, character(1))
      txt <- gsub("<[^>]+>", "", gsub("<br>", "; ", paste(texts, collapse="; "), fixed=TRUE))
      k <- length(e$captured)
      e$captured[[k]] <- e$captured[[k]] + ggplot2::labs(subtitle=txt)
    }
    if (interactive) plotly::layout(p, ...) else p
  }
  e$toWebGL <- function(p) {
    if (!interactive || !webgl) return(p)
    p <- plotly::plotly_build(p)
    for (i in seq_along(p$x$data)) p$x$data[[i]]$hoveron <- NULL
    plotly::toWebGL(p)
  }
  e$subplot <- function(...) if (interactive) plotly::subplot(...) else NULL
  e$qc_plot_visible <- function() TRUE
  for (nm in ls(.mcs_plot_source)) {
    f <- .mcs_plot_source[[nm]]; environment(f) <- e; e[[nm]] <- f
  }
  e
}
.mcs_plot_result <- function(p, e, data, interactive, panel_names=NULL) {
  if (!interactive) {
    p <- e$captured
    if (length(panel_names)==length(p)) names(p) <- panel_names
    if (length(p)==1L) p <- p[[1]]
  }
  if (interactive) p <- { p$height <- max(480, 320*length(panel_names)); p }
  attr(p, "mc_plot_data") <- data
  p
}
#' Plot blank-filter results
#'
#' @description Visualize the saved blank comparisons and filtering threshold.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param interactive \code{TRUE} returns an interactive Plotly figure. \code{FALSE} returns static ggplot output.
#' @details Use an object after blank filtering. Ratio mode plots positive finite ratios in logarithmic bins with the cutoff indicated; zero and undefined cases are summarized separately because they cannot appear on a logarithmic axis. Detection mode plots detected and not-detected counts. Values come from \code{blank_tbl}, including features removed at that stage.
#' @return A Plotly object when \code{interactive = TRUE}; otherwise a ggplot object.
#' @seealso \code{\link{mc_filter_blank}}
#' @examples
#' \dontrun{
#' # Requires an internet connection.
#' base_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/"
#' )
#' peak_table <- read.csv(
#'   paste0(base_url, "orbi_iimn_gnps_quant.csv"),
#'   check.names = FALSE
#' )
#' imported <- mc_prepare(
#'   peak_table, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- tempfile(fileext = ".csv")
#' download.file(paste0(base_url, "metadata.csv"), metadata_file, mode = "wb")
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' mc_plot_blank(after_blank, interactive = FALSE)
#' mc_plot_blank(after_blank, interactive = TRUE)
#' }
#' @export
mc_plot_blank <- function(x,interactive=TRUE) {
  mid <- count <- NULL
  .mcs_check(x)
  if(is.null(x$blank_tbl)||!nrow(x$blank_tbl))stop("No saved Blank values. Run mc_filter_blank() first on a nonempty table.")
  data<-list(mode=x$blank$mode,data=x$blank_tbl)
  e<-.mcs_plot_environment(x,list(blank_cutoff=x$blank$cutoff),interactive)
  e$blank_plot_data<-function()data
  cutoff<-x$blank$cutoff
  if(!interactive && x$blank$mode=="mean_cutoff") {
    # Static counterpart to the app's manually binned log-ratio Plotly chart.
    df <- data$data; v <- df$Ratio[df$Status=="Positive finite ratio"]
    edges <- c(-2,1); bins <- data.frame(mid=numeric(), count=integer())
    if (length(v)) {
      lo <- floor(min(log10(v))); hi <- ceiling(max(log10(v))); if(hi<=lo) hi<-lo+1
      h <- graphics::hist(log10(v), breaks=seq(lo,hi,length.out=51), plot=FALSE, include.lowest=TRUE,right=FALSE)
      edges <- range(h$breaks); bins <- data.frame(mid=h$mids,count=h$counts)
    }
    if (cutoff>0) edges <- range(c(edges,log10(cutoff)))
    ticks <- seq(floor(edges[1]),ceiling(edges[2])); if(length(ticks)>12) ticks<-ticks[unique(round(seq(1,length(ticks),length.out=12)))]
    keep <- df$keep; keep[is.na(keep)]<-FALSE
    counts <- table(factor(df$Status, levels=c("Zero blank / positive sample","Positive blank / zero sample","Both zero","Other / undefined")))
    sub <- sprintf("Filtered: %d of %d (%.1f%%) | Cutoff: %g",sum(!keep),nrow(df),100*mean(!keep),cutoff)
    caption <- sprintf("Not shown on log axis: blank = 0, sample > 0: %d;\nblank > 0, sample = 0: %d; both = 0: %d",counts[1],counts[2],counts[3])
    if(counts[4]>0) caption<-paste0(caption,"; other/undefined: ",counts[4])
    p <- ggplot2::ggplot(bins,ggplot2::aes(x=mid,y=count)) +
      ggplot2::geom_col(fill="royalblue",colour="black",linewidth=.3,width=if(length(v))diff(h$breaks)[1] else .1) +
      ggplot2::scale_x_continuous(breaks=ticks,labels=trimws(formatC(10^ticks,format="g",digits=4)),limits=edges+c(-.03,.03)) +
      ggplot2::theme_minimal() + ggplot2::theme(plot.title=ggplot2::element_text(hjust=.5)) +
      ggplot2::labs(title="Blank to Sample Ratio Distribution",subtitle=sub,caption=caption,
                    x="Ratio: Mean(Blank) / Max(Mean(Sample)) (log10)",y="Feature count")
    if(cutoff>0) p<-p+ggplot2::geom_vline(xintercept=log10(cutoff),colour="red",linetype="dashed")
    if(!length(v)) p<-p+ggplot2::annotate("text",x=mean(edges),y=0,label="No positive finite ratios to display.")
    attr(p,"mc_plot_data")<-data
    return(p)
  }
  p<-e$blank_render()
  .mcs_plot_result(p,e,data,interactive)
}
#' Plot QC metric distributions
#'
#' @description Visualize the saved QC values and their filtering thresholds.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param filters QC metrics to display: zeros, mean, rsd, dratio, or min. Only metrics already stored by \code{mc_filter_qc()} can be selected. NULL displays all stored metrics.
#' @param interactive \code{TRUE} returns an interactive Plotly figure. \code{FALSE} returns static ggplot output.
#' @details Use an object after QC filtering with at least one selected metric. Values and thresholds come from \code{qc_tbl} and \code{qc}. The plots include features entering that stage, including those subsequently removed. Group modes display group-specific values; pooled mode displays pooled values. Mean and minimum intensity axes use a log10(value + 1) display transformation; the stored values and filtering thresholds remain on the original scale. Metrics are not recalculated. For multiple static metrics, access a plot by name, for example \code{plots$rsd}.
#' @return A Plotly object when \code{interactive = TRUE}; otherwise a ggplot object. When several static panels are needed, returns a named list of ggplot objects instead.
#' @seealso \code{\link{mc_filter_qc}}
#' @examples
#' \dontrun{
#' # Requires an internet connection.
#' base_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/"
#' )
#' peak_table <- read.csv(
#'   paste0(base_url, "orbi_iimn_gnps_quant.csv"),
#'   check.names = FALSE
#' )
#' imported <- mc_prepare(
#'   peak_table, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- tempfile(fileext = ".csv")
#' download.file(paste0(base_url, "metadata.csv"), metadata_file, mode = "wb")
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' # Illustration of QC filtering, not the published Orbi settings.
#' # Select biological groups explicitly; exclude blanks and QC samples.
#' biological_groups <- setdiff(unique(imported$labels), c("Blank", "QC"))
#' after_qc <- mc_filter_qc(
#'   imported, filters = c("zeros", "rsd"),
#'   groups = biological_groups, mode = "group_every",
#'   zero_metric = "percent", zero_cutoff = 50, rsd_cutoff = 30
#' )
#' plots <- mc_plot_qc(after_qc, interactive = FALSE)
#' plots$rsd
#' }
#' @export
mc_plot_qc <- function(x,filters=NULL,interactive=TRUE) {
  .mcs_check(x)
  if(is.null(x$qc_tbl)||!nrow(x$qc_tbl))stop("No saved QC values. Run mc_filter_qc() with selected filters first.")
  available<-unique(x$qc_tbl$metric)
  if(is.null(filters))filters<-available
  if(!length(filters)||any(!filters %in% available))stop("Select metrics already stored in qc_tbl.")
  filters<-intersect(c("zeros","mean","rsd","dratio","min"),filters)
  input<-as.list(stats::setNames(unname(x$qc$cutoffs),paste0(names(x$qc$cutoffs),"_cutoff")))
  input$zero_cutoff<-unname(x$qc$cutoffs["zeros"])
  data<-stats::setNames(lapply(filters,function(metric)list(metric=metric,mode=x$qc$mode,
    zero_metric=x$qc$zero_metric,data=x$qc_tbl[x$qc_tbl$metric==metric,,drop=FALSE])),filters)
  e<-.mcs_plot_environment(x,input,interactive);e$qc_plot_data<-function()data
  p<-e$qc_render();.mcs_plot_result(p,e,data,interactive,names(data))
}
#' Plot peak properties and filtering bounds
#'
#' @description Visualize the saved m/z, retention times, and mass defects.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param filters Properties to display: mz, rt, rmd, or amd. NULL selects the corresponding filters used in the peak-filtering step. Target matching has no scatter plot.
#' @param interactive \code{TRUE} returns an interactive Plotly figure. \code{FALSE} returns static ggplot output.
#' @param webgl Use WebGL for interactive scatter traces. Ignored for static output.
#' @details Use an object after peak filtering. The m/z and RT selections share an m/z-versus-RT panel; RMD and AMD use additional panels. Saved values include removed features. Properties not used for filtering can still be displayed, without their cutoff lines. Static multi-panel output is named \code{mzrt}, \code{rmd}, and/or \code{amd}.
#' @return A Plotly object when \code{interactive = TRUE}; otherwise a ggplot object. When several static panels are needed, returns a named list of ggplot objects instead.
#' @seealso \code{\link{mc_filter_peak}}
#' @examples
#' \dontrun{
#' # Requires an internet connection.
#' base_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/"
#' )
#' peak_table <- read.csv(
#'   paste0(base_url, "orbi_iimn_gnps_quant.csv"),
#'   check.names = FALSE
#' )
#' imported <- mc_prepare(
#'   peak_table, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' # Illustration of peak filtering, not the published Orbi settings.
#' peak_options <- mc_peak_parameters(
#'   mz_min_enable = TRUE, peak_mz_min = 200,
#'   rt_min_enable = TRUE, peak_rt_min = 1
#' )
#' after_peak <- mc_filter_peak(
#'   imported, filters = c("mz", "rt"), parameters = peak_options
#' )
#' mc_plot_peak(after_peak, filters = c("mz", "rt"), interactive = FALSE)
#' }
#' @export
mc_plot_peak <- function(x,filters=NULL,interactive=TRUE,webgl=TRUE) {
  .mcs_check(x)
  if(is.null(x$peak_tbl)||!nrow(x$peak_tbl))stop("No saved Peak values. Run mc_filter_peak() first.")
  if(is.null(filters))filters<-intersect(x$peak$peak_filters,c("mz","rt","rmd","amd"))
  if(!length(filters)||any(!filters %in% c("mz","rt","rmd","amd")))stop("Choose mz, rt, rmd or amd; target matching has no scatter plot.")
  # Unselected metrics may be viewed as distributions, without filter bounds.
  input<-x$peak
  for(metric in setdiff(filters,input$peak_filters)) {
    input[[paste0(metric,"_min_enable")]]<-FALSE
    input[[paste0(metric,"_max_enable")]]<-FALSE
  }
  dd<-x$peak_tbl;dd<-dd[is.finite(dd$mz)&is.finite(dd$RT),,drop=FALSE]
  if(!nrow(dd))stop("No finite m/z and RT values to plot.")
  data<-list(data=dd,sel=filters)
  e<-.mcs_plot_environment(x,input,interactive,webgl);e$peak_plot_data<-function()data
  p<-e$peak_render()
  panels<-c(if(any(c("mz","rt") %in% filters))"mzrt",if("rmd" %in% filters&&any(is.finite(dd$RMD)))"rmd",if("amd" %in% filters&&any(is.finite(dd$AMD)))"amd")
  .mcs_plot_result(p,e,data,interactive,panels)
}
#' Plot MS subfilter removal counts
#'
#' @description Visualize the saved number of features removed by each MS subfilter.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param interactive \code{TRUE} returns an interactive Plotly figure. \code{FALSE} returns static ggplot output.
#' @details Use an object after MS filtering. Bars show the number of features removed by each enabled MS subfilter, in processing order. Counts come from \code{ms$counts}; ion relationships are not recalculated.
#' @return A Plotly object when \code{interactive = TRUE}; otherwise a ggplot object.
#' @seealso \code{\link{mc_filter_ms}}
#' @examples
#' \dontrun{
#' # Requires an internet connection.
#' base_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/"
#' )
#' peak_table <- read.csv(
#'   paste0(base_url, "orbi_iimn_gnps_quant.csv"),
#'   check.names = FALSE
#' )
#' imported <- mc_prepare(
#'   peak_table, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- tempfile(fileext = ".csv")
#' download.file(paste0(base_url, "metadata.csv"), metadata_file, mode = "wb")
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' after_ms <- mc_filter_ms(after_blank, filters = c("isotopes", "adducts"))
#' mc_plot_ms(after_ms, interactive = FALSE)
#' }
#' @export
mc_plot_ms <- function(x,interactive=TRUE) {
  Filter <- Removed <- NULL
  .mcs_check(x);if(is.null(x$ms))stop("Run mc_filter_ms() first.")
  .mcs_plot_environment(x,list(),interactive)
  s<-x$ms$counts;keys<-c("removed_iso","removed_add","removed_nl","removed_isf","removed_mis","removed_ring")
  labels<-c("Isotopes","Adducts","Neutral losses","ISF","Mispicked","Ringing")
  df<-data.frame(Filter=factor(labels,levels=labels),Removed=vapply(keys,function(k)as.numeric(s[[k]]),numeric(1)))
  p<-ggplot2::ggplot(df,ggplot2::aes(x=Filter,y=Removed))+ggplot2::geom_col(fill="royalblue",colour="black",alpha=.8)+
    ggplot2::theme_minimal()+ggplot2::labs(title="MS filtering results",x=NULL,y="Features removed",
      subtitle=sprintf("Kept: %d of %d",s$after,s$before))
  if(interactive)p<-plotly::ggplotly(p,tooltip=c("x","y"))
  p
}
#' Plot feature counts across filtering stages
#'
#' @description Visualize the saved number of retained features after each filtering stage.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param interactive \code{TRUE} returns an interactive Plotly figure. \code{FALSE} returns static ggplot output.
#' @details Bars show the starting feature count and the number retained after each recorded stage. Counts come from \code{counts}, in execution order. No filtering is rerun.
#' @return A Plotly object when \code{interactive = TRUE}; otherwise a ggplot object.
#' @seealso \code{\link{mc_summary}}
#' @examples
#' \dontrun{
#' # Requires an internet connection.
#' base_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/"
#' )
#' peak_table <- read.csv(
#'   paste0(base_url, "orbi_iimn_gnps_quant.csv"),
#'   check.names = FALSE
#' )
#' imported <- mc_prepare(
#'   peak_table, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- tempfile(fileext = ".csv")
#' download.file(paste0(base_url, "metadata.csv"), metadata_file, mode = "wb")
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' after_ms <- mc_filter_ms(after_blank, filters = c("isotopes", "adducts"))
#' mc_plot_summary(after_ms, interactive = FALSE)
#' }
#' @export
mc_plot_summary <- function(x,interactive=TRUE) {
  Stage <- Features <- NULL
  .mcs_check(x);if(!nrow(x$counts))stop("Run a filter first.")
  .mcs_plot_environment(x,list(),interactive)
  df<-data.frame(Stage=c("Input",x$counts$step),Features=c(x$counts$before[1],x$counts$after))
  df$Stage<-factor(df$Stage,levels=df$Stage)
  p<-ggplot2::ggplot(df,ggplot2::aes(x=Stage,y=Features))+ggplot2::geom_col(fill="mediumseagreen",colour="black",alpha=.8)+
    ggplot2::theme_minimal()+ggplot2::labs(title="Processing summary",x=NULL,y="Features retained")
  if(interactive)p<-plotly::ggplotly(p,tooltip=c("x","y"))
  p
}
