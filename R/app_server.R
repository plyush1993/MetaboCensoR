# ==============================
# Server ----
# ==============================
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import plotly
#' @import vroom
#' @import DT
#' @import data.table
#' @import tibble
#' @import igraph
#' @import scales
#' @import Spectra
#' @import MsBackendMgf
#' @import shinyjs
#' @import shinyWidgets
#' @import shinycssloaders
#' @import waiter
#' @import shinyBS
#' @import jsonlite
app_server <- function(input, output, session) {

  # -----------------------------------------------------------------------
  # Analysis settings: state and export
  # -----------------------------------------------------------------------

  settings_records <- reactiveValues(
    blank = NULL,
    ms = NULL,
    qc = NULL,
    peak = NULL,
    final = NULL,
    mgf = NULL
  )

  settings_embedded <- reactiveVal(list())

  settings_import <- reactiveValues(
    values = NULL,
    token = 0L,
    round = 0L,
    stable = 0L,
    busy = FALSE,
    issues = list(),
    notes = character(0),
    manual = NULL,
    manual_done = FALSE,
    files = NULL,
    final_only = FALSE,
    pending = character(0)
  )

  settings_file <- function(id) {
    embedded <- settings_embedded()

    if (!is.null(embedded[[id]])) {
      embedded[[id]]
    } else {
      input[[id]]
    }
  }

  # A newly uploaded CSV replaces the corresponding embedded CSV.
  for (id in settings_aux_ids) local({
    key <- id

    observeEvent(input[[key]], {
      x <- settings_embedded()
      x[[key]] <- NULL
      settings_embedded(x)
    }, ignoreInit = TRUE, ignoreNULL = TRUE, priority = 100)
  })

  settings_safe <- function(expr) {
    tryCatch(expr, error = function(e) NULL)
  }

  settings_inputs <- function() {
    ids <- names(settings_registry)

    values <- lapply(ids, function(id) input[[id]])
    names(values) <- ids

    for (id in ids) {
      if (is.null(values[[id]]) &&
          isTRUE(settings_registry[[id]]$multiple)) {
        values[id] <- list(character(0))
      }
    }

    values[!vapply(values, is.null, logical(1))]
  }

  settings_files <- function() {
    ans <- lapply(settings_file_ids, function(id) {
      settings_file_record(
        settings_file(id),
        embed = id %in% settings_aux_ids
      )
    })

    names(ans) <- settings_file_ids

    # Both MGF uploaders share one active spectra object.
    if (!is.null(current_mgf()) && file.exists(current_mgf())) {
      ans$active_mgf <- settings_file_record(
        data.frame(
          name = current_mgf_name(),
          datapath = current_mgf(),
          stringsAsFactors = FALSE
        )
      )
    }

    ans
  }

  settings_snapshot <- function(stats = NULL) {
    list(
      at_utc = settings_utc(),
      parameters = settings_inputs(),
      files = settings_files(),
      stats = stats,

      context = list(
        dataset_name = shared$name,

        sample_columns = settings_safe(sample_cols0()),

        blank_labels = settings_safe(
          make_label_table(
            sample_names(),
            label_vector_blank()
          )
        ),

        qc_labels = settings_safe(
          make_label_table(
            sample_names(),
            qc_label_vector()
          )
        )
      )
    )
  }

  settings_capture <- function(stage, stats = NULL) {
    settings_records[[stage]] <- settings_snapshot(stats)
  }

  settings_active <- function() {
    list(
      blank = isTRUE(blank_state$applied),
      ms = isTRUE(ms_state$applied),
      qc = isTRUE(qc_state$applied),
      peak = isTRUE(peak_state$applied)
    )
  }

  observe({
  shinyjs::toggleState(
    "download_analysis_settings",
    condition = !is.null(shared$raw)
  )
})

  output$download_analysis_settings <- downloadHandler(
    filename = function() {
      paste0(
        "MetaboCensoR_settings_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".json"
      )
    },

    content = function(file) {
      req(shared$raw)

      current <- settings_snapshot()
      active <- settings_active()

      stages <- lapply(names(active), function(stage) {
        list(
          applied = active[[stage]],

          snapshot = if (active[[stage]]) {
            settings_record_metadata(settings_records[[stage]])
          } else {
            NULL
          }
        )
      })

      names(stages) <- names(active)

      # Applied settings override later, unapplied edits.
      recipe <- current$parameters
      files <- current$files

      manual <- list(
        blank_labels = current$context$blank_labels,
        qc_labels = current$context$qc_labels
      )

      first <- TRUE

      for (stage in names(active)) {
        if (!active[[stage]]) next

        record <- settings_records[[stage]]

        if (is.null(record)) {
          stop(
            "Applied step has no settings record; reapply ",
            stage,
            "."
          )
        }

        ids <- names(settings_registry)[
          vapply(
            settings_registry,
            function(z) z$stage == stage,
            logical(1)
          )
        ]

        if (first) {
          upload_ids <- names(settings_registry)[
            vapply(
              settings_registry,
              function(z) z$stage == "upload",
              logical(1)
            )
          ]

          ids <- union(ids, upload_ids)
          files$file0 <- record$files$file0
          first <- FALSE
        }

        ids <- intersect(ids, names(record$parameters))
        recipe[ids] <- record$parameters[ids]

        asset_ids <- switch(
          stage,
          blank = c("meta_csv", "blank_metadata_csv"),
          ms = c("adduct_file2", "nl_file2", "active_mgf"),
          qc = c("qc_meta_csv", "qc_metadata_csv"),
          peak = "target_peak_csv"
        )

        files[asset_ids] <- record$files[asset_ids]

        if (stage == "blank") {
          manual$blank_labels <- record$context$blank_labels
        }

        if (stage == "qc") {
          manual$qc_labels <- record$context$qc_labels
        }
      }

      payload <- list(
        format = "MetaboCensoR-analysis-settings",
        schema_version = 1L,
        exported_at_utc = settings_utc(),
        app_version = settings_version(),
        r_version = R.version.string,

        note = paste(
          "Applied-step snapshots are authoritative.",
          "Current controls may differ.",
          "Import restores settings only;",
          "it does not run filters or restore results.",
          "Unapplied steps in the transfer recipe use current controls."
        ),

        pipeline_order = c("blank", "ms", "qc", "peak"),
        steps = stages,

        compiled_output = if (!is.null(final_table())) {
          settings_record_metadata(settings_records$final)
        } else {
          NULL
        },

        filtered_mgf = if (!is.null(filtered_mgf_sps())) {
          settings_record_metadata(settings_records$mgf)
        } else {
          NULL
        },

        current_controls = settings_record_metadata(current),

        transfer = list(
          parameters = recipe,
          files = files,
          labels = manual,
          originally_applied = active
        )
      )

      jsonlite::write_json(
        payload,
        file,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null",
        na = "null",
        digits = NA,
        dataframe = "rows"
      )
    }
  )

  # -----------------------------------------------------------------------
  # Automatic restoration of pending settings
  # -----------------------------------------------------------------------

  settings_finish_items <- function(ids) {
    settings_import$pending <- setdiff(
      settings_import$pending,
      ids
    )

    x <- settings_import$issues
    x[intersect(names(x), ids)] <- NULL
    settings_import$issues <- x
  }

  settings_probe <- function() isolate({
    if (!length(settings_import$pending)) {
      return(invisible(NULL))
    }

    ids <- intersect(
      settings_import$pending,
      names(settings_registry)
    )

    # Watch label-source changes while manual labels remain unresolved.
    if ("blank_manual_labels" %in% settings_import$pending) {
      ids <- union(ids, "label_source")
    }

    if ("qc_manual_labels" %in% settings_import$pending) {
      ids <- union(ids, "qc_label_source")
    }

    session$sendCustomMessage(
      "settings-probe",
      list(
        ids = unname(as.list(ids)),
        token = settings_import$token,
        delay = if (isTRUE(settings_import$busy)) 200 else 1000
      )
    )
  })

  settings_begin_restore <- function() {
    if (is.null(settings_import$values)) {
      return(invisible(NULL))
    }

    # Invalidate results once, when a new JSON is imported.
    invalidate_all_pipeline()

    settings_import$token <- settings_import$token + 1L
    settings_import$pending <- names(settings_import$values)

    for (stage in c("blank", "qc")) {
      source_id <- if (stage == "blank") {
        "label_source"
      } else {
        "qc_label_source"
      }

      if (identical(
        settings_import$values[[source_id]],
        "manual"
      )) {
        settings_import$pending <- c(
          settings_import$pending,
          paste0(stage, "_manual_labels")
        )
      }
    }

    settings_import$issues <- list()
    settings_import$stable <- 0L
    settings_import$busy <- TRUE

    session$onFlushed(settings_probe, once = TRUE)
  }

  # A manual change takes precedence over the imported value.
  observeEvent(input$settings_user_override, {
    event <- input$settings_user_override

    if (!identical(
      as.integer(event$token),
      settings_import$token
    )) {
      return()
    }

    id <- event$id

    if (!is.character(id) ||
        length(id) != 1L ||
        !id %in% names(settings_registry)) {
      return()
    }

    settings_finish_items(id)

    if (id == "label_source") {
      settings_finish_items("blank_manual_labels")
    }

    if (id == "qc_label_source") {
      settings_finish_items("qc_manual_labels")
    }
  }, ignoreInit = TRUE, priority = 100)

  # Keep your existing label-edit observers as well.
  observeEvent(input$labels_table_cell_edit, {
    settings_finish_items("blank_manual_labels")
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(input$qc_labels_table_cell_edit, {
    settings_finish_items("qc_manual_labels")
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(input$settings_probe_result, {
    msg <- input$settings_probe_result

    if (!identical(
      as.integer(msg$token),
      settings_import$token
    ) || !length(settings_import$pending)) {
      return()
    }

    ids <- intersect(
      settings_import$pending,
      names(settings_registry)
    )

    values <- settings_import$values[ids]
    issues <- list()
    changed <- FALSE

    for (id in ids) {
      spec <- settings_registry[[id]]
      wanted <- values[[id]]
      control <- msg$controls[[id]]

      if (is.null(control)) {
        issues[[id]] <- paste(
          "Waiting for the required data/control;",
          "restoration is automatic."
        )
        next
      }

      if (spec$type %in% c(
        "selectInput",
        "selectizeInput",
        "radioButtons",
        "checkboxGroupButtons"
      )) {
        problem <- settings_choice_problem(
          id,
          wanted,
          unlist(control$choices, use.names = FALSE)
        )

        if (!is.null(problem)) {
          issues[[id]] <- paste(
            problem,
            "Choose a replacement manually or load matching data."
          )
          next
        }
      }

      actual <- input[[id]]

      equal <- if (is.numeric(wanted)) {
        identical(
          as.numeric(actual),
          as.numeric(wanted)
        )
      } else {
        identical(
          unname(actual %||% character(0)),
          unname(wanted)
        )
      }

      if (!equal) {
        if (id == "sample_cols0") {
          manual_range_state$updating <- TRUE
        }

        if (id %in% c(
          "sample_keyword0",
          "blank_metadata_remove_suffixes",
          "qc_metadata_remove_suffixes",
          "add_preferred2"
        )) {
          updateSelectizeInput(
            session,
            id,
            choices = unique(c(
              unlist(control$choices),
              wanted
            )),
            selected = wanted
          )
        } else {
          settings_update_input(
            session,
            id,
            wanted,
            spec$type
          )
        }

        changed <- TRUE
      }
    }

    # Restore manual labels after column and label controls settle.
    if (!changed) {
      for (stage in c("blank", "qc")) {
        key <- paste0(stage, "_manual_labels")

        if (!key %in% settings_import$pending) next

        source_id <- if (stage == "blank") {
          "label_source"
        } else {
          "qc_label_source"
        }

        samples <- settings_safe(sample_names())

        if (!identical(input[[source_id]], "manual") ||
            !length(samples)) {
          issues[[key]] <- paste(
            "Waiting for sample names and manual label mode."
          )
          next
        }

        rows <- settings_import$manual[[paste0(stage, "_labels")]]

        tbl <- if (length(rows)) {
          dplyr::bind_rows(rows)
        } else {
          NULL
        }

        if (is.null(tbl) ||
            !all(samples %in% tbl$Sample)) {
          issues[[key]] <- paste(
            "Saved labels do not cover current samples;",
            "edit the label table or choose another label source."
          )
          next
        }

        tbl <- as.data.frame(
          tbl[
            match(samples, tbl$Sample),
            c("Sample", "Label")
          ]
        )

        if (stage == "blank") {
          manual_labels_blank(tbl)
        } else {
          manual_labels_qc(tbl)
        }

        settings_finish_items(key)
        changed <- TRUE
      }
    }

    settings_import$stable <- if (changed) {
      0L
    } else {
      settings_import$stable + 1L
    }

    settings_import$issues <- issues

    # Confirm updates before retiring successfully restored settings.
    if (!changed && settings_import$stable >= 3L) {
      settings_finish_items(
        setdiff(
          settings_import$pending,
          names(issues)
        )
      )

      settings_import$busy <- FALSE
    } else {
      settings_import$busy <- changed
    }

    # Check only unresolved items. Completed items are never reapplied.
    if (length(settings_import$pending)) {
      session$onFlushed(settings_probe, once = TRUE)
    }
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Analysis settings: import and restoration
  # -----------------------------------------------------------------------

  observeEvent(input$settings_json, {
    req(input$settings_json)

    tryCatch({
      if (input$settings_json$size > 256 * 1024^2) {
        stop("Settings JSON exceeds 256 MB.")
      }

      obj <- jsonlite::fromJSON(
        input$settings_json$datapath,
        simplifyVector = FALSE
      )

      if (!identical(obj$format, "MetaboCensoR-analysis-settings") ||
          !identical(as.integer(obj$schema_version), 1L)) {
        stop("Unsupported settings format/version.")
      }

      values <- settings_validate_values(
        obj$transfer$parameters,
        settings_registry
      )

      labels <- obj$transfer$labels

      if (!is.null(labels) && !is.list(labels)) {
        stop("Invalid saved labels.")
      }

      for (rows in labels) {
        if (!length(rows)) next

        tbl <- dplyr::bind_rows(rows)

        if (!all(c("Sample", "Label") %in% names(tbl)) ||
            !is.character(tbl$Sample) ||
            !is.character(tbl$Label) ||
            anyNA(tbl$Sample) ||
            anyNA(tbl$Label) ||
            anyDuplicated(tbl$Sample)) {
          stop("Invalid sample-label mapping.")
        }
      }

      files <- obj$transfer$files
      if (!is.list(files)) stop("Invalid file records.")

      embedded <- list()

      for (id in intersect(names(files), settings_aux_ids)) {
        rec <- files[[id]]

        if (is.null(rec$content_base64)) next

        if (!is.character(rec$content_base64) ||
            length(rec$content_base64) != 1L ||
            nchar(rec$content_base64) > 30 * 1024^2) {
          stop("Invalid embedded CSV: ", id)
        }

        bytes <- jsonlite::base64_dec(rec$content_base64)

        if (length(bytes) > 20 * 1024^2) {
          stop("Embedded CSV exceeds 20 MB: ", id)
        }

        path <- tempfile(
          pattern = "metabocensor_settings_",
          fileext = ".csv"
        )

        writeBin(bytes, path)

        if (!identical(
          unname(tools::md5sum(path)),
          rec$md5
        )) {
          stop("CSV checksum mismatch: ", id)
        }

        embedded[[id]] <- data.frame(
          name = basename(rec$name),
          size = length(bytes),
          type = "text/csv",
          datapath = path,
          stringsAsFactors = FALSE
        )
      }

      # Only change the analysis state after validation succeeds.
      target_peak_csv_cache(empty_peak_targets())
      target_peak_cleared(FALSE)

      for (id in settings_aux_ids) {
        shinyjs::reset(id)
      }

      settings_embedded(embedded)

      settings_import$values <- values
      settings_import$files <- files
      settings_import$manual <- obj$transfer$labels
      settings_import$manual_done <- FALSE

      settings_import$notes <- c(
        paste("Source app:", obj$app_version),

        paste(
          "Previously applied steps:",
          paste(
            names(Filter(isTRUE, obj$transfer$originally_applied)),
            collapse = ", "
          )
        ),

        paste(
          "No filters have been run.",
          "Review the report and then apply the required steps."
        ),

        paste(
          "Selections requiring additional files are restored automatically",
          "when those files are uploaded.",
          "Final MGF selections are restored after compiling the output."
        ),

        if (length(embedded)) {
          paste(
            "Embedded CSVs restored:",
            paste(names(embedded), collapse = ", ")
          )
        }
      )

      settings_begin_restore()

    }, error = function(e) {
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = 10
      )
    })
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Analysis settings: report, processing guard, and resets
  # -----------------------------------------------------------------------

  output$settings_import_report <- renderUI({
    if (is.null(settings_import$values)) return(NULL)

    files <- settings_import$files

    file_notes <- vapply(
      intersect(
        names(files),
        c("file0", "active_mgf", settings_aux_ids)
      ),
      function(id) {
        rec <- files[[id]]
        if (is.null(rec)) return("")

        if (!is.null(settings_embedded()[[id]])) {
          return(
            paste(id, ": restored embedded", rec$name)
          )
        }

        current <- if (id == "active_mgf") {
          if (is.null(current_mgf())) {
            NULL
          } else {
            data.frame(
              name = current_mgf_name(),
              datapath = current_mgf()
            )
          }
        } else {
          settings_file(id)
        }

        actual <- settings_file_record(current)

        if (is.null(actual)) {
          paste(id, ": upload", rec$name)
        } else if (identical(actual$md5, rec$md5)) {
          paste(id, ": matches original", rec$name)
        } else {
          paste(
            id,
            ": different file loaded (allowed for a new analysis);",
            "original:",
            rec$name
          )
        }
      },
      character(1)
    )

    issue_notes <- vapply(
      names(settings_import$issues),
      function(id) {
        paste(
          id,
          ":",
          settings_import$issues[[id]]
        )
      },
      character(1)
    )

    div(
      class = "well",
      style = "color:#2c3e50;",

      tags$b(
        if (isTRUE(settings_import$busy)) {
          "Restoring settings..."
        } else {
          "Settings import report"
        }
      ),

      tags$ul(
        lapply(
          c(
            settings_import$notes,
            file_notes[nzchar(file_notes)],
            issue_notes
          ),
          tags$li
        )
      ),

      helpText(
        paste(
          "Missing inputs are checked automatically.",
          "If saved group or column names do not match your dataset,",
          "choose replacements manually; your choices are preserved."
        )
      )
    )
  })

    settings_ready <- function(include_final = FALSE, stage = NULL) {
    pending <- settings_import$pending %||% character(0)

    if (!length(pending)) return(TRUE)

    order <- c(
      "upload", "blank", "ms", "qc", "peak", "final"
    )

    last <- if (include_final) {
      "final"
    } else {
      stage %||% "peak"
    }

    stages <- order[seq_len(match(last, order))]

    ids <- names(settings_registry)[
      vapply(
        settings_registry,
        function(x) x$stage %in% stages,
        logical(1)
      )
    ]

    if ("blank" %in% stages) {
      ids <- c(ids, "blank_manual_labels")
    }

    if ("qc" %in% stages) {
      ids <- c(ids, "qc_manual_labels")
    }

    # Unused optional selections do not block processing.
    if (!isTRUE(input$enable_blank)) {
      ids <- setdiff(ids, "groups_to_remove")
    }

    if (!isTRUE(input$enable_isf2) ||
        !isTRUE(input$isf_ms2_check2)) {
      ids <- setdiff(ids, "isf_mgf_id_field")
    }

    if (!length(input$value_filters)) {
      ids <- setdiff(ids, "value_groups")
    }

    if (!"dratio" %in% (input$value_filters %||% character(0))) {
      ids <- setdiff(ids, "dratio_qc_group")
    }

    if (!identical(input$sample_mode0, "manual")) {
      ids <- setdiff(ids, "sample_cols0")
    }

    for (prefix in c("blank", "qc")) {
      src <- if (prefix == "blank") {
        input$label_source
      } else {
        input$qc_label_source
      }

      if (!identical(src, "from_metadata")) {
        ids <- setdiff(
          ids,
          paste0(
            prefix,
            c("_metadata_sample_col", "_metadata_label_col")
          )
        )
      }

      if (!identical(src, "manual")) {
        ids <- setdiff(
          ids,
          paste0(prefix, "_manual_labels")
        )
      }
    }

    blocked <- intersect(pending, ids)

    if (length(blocked)) {
      showNotification(
        paste(
          "Settings still waiting:",
          paste(blocked, collapse = ", "),
          "— wait briefly, upload the required file,",
          "or choose replacements manually."
        ),
        type = "warning",
        duration = 8
      )

      return(FALSE)
    }

    TRUE
  }

  observeEvent(input$clear_shared, {
    settings_embedded(list())
    settings_import$values <- NULL
    settings_import$pending <- character(0)
    settings_import$busy <- FALSE
    settings_import$issues <- list()
    settings_import$token <- settings_import$token + 1L

    for (stage in c("blank", "ms", "qc", "peak", "final", "mgf")) {
      settings_records[[stage]] <- NULL
    }

    shinyjs::reset("settings_json")
  }, ignoreInit = TRUE, priority = 100)

  for (stage in c("blank", "ms", "qc", "peak")) local({
    key <- stage

    observeEvent(input[[paste0("reset_", key)]], {
      assets <- switch(
        key,
        blank = c("meta_csv", "blank_metadata_csv"),
        ms = c("adduct_file2", "nl_file2"),
        qc = c("qc_meta_csv", "qc_metadata_csv"),
        peak = "target_peak_csv"
      )

      x <- settings_embedded()
      x[assets] <- NULL
      settings_embedded(x)

      settings_records[[key]] <- NULL
            reset_ids <- names(settings_registry)[
        vapply(
          settings_registry,
          function(x) x$stage == key,
          logical(1)
        )
      ]

      settings_finish_items(
        c(
          reset_ids,
          paste0(key, "_manual_labels")
        )
      )
    }, ignoreInit = TRUE, priority = 100)
  })

  add_previous_polarity <- reactiveVal(NULL)

observeEvent(
  list(input$add_pol2, settings_file("adduct_file2")),
  {
    pol <- input$add_pol2 %||% "positive"

    definitions <- safe_read_csv_any(settings_file("adduct_file2"))

    if (is.null(definitions) ||
        !all(c("Name", "Charge", "Mult", "Mass") %in%
             names(definitions))) {
      definitions <- built_in_adducts_full()
    }

    if ("Ion_mode" %in% names(definitions)) {
      definitions <- definitions[
        tolower(as.character(definitions$Ion_mode)) == tolower(pol),
        ,
        drop = FALSE
      ]
    }

    available <- trimws(as.character(definitions$Name))
    available <- sort(unique(
      available[!is.na(available) & nzchar(available)]
    ))

    default_ion <- if (pol == "negative") "M-H" else "M+H"

    previous <- isolate(add_previous_polarity())
    selected <- isolate(input$add_preferred2)
    restoring <- isTRUE(isolate(settings_import$busy))

    if (is.null(previous) ||
        (!identical(previous, pol) && !restoring)) {
      selected <- default_ion
    }

    # Preserve custom selections and an intentionally empty selection.
    selected <- as.character(selected)

    updateSelectizeInput(
      session,
      "add_preferred2",
      choices = unique(c(available, selected)),
      selected = selected
    )

    add_previous_polarity(pol)
  },
  ignoreInit = FALSE
)

observeEvent(
  list(input$reset_ms, input$clear_shared),
  {
    shinyWidgets::updatePrettyCheckbox(
      session,
      "add_prefer2",
      value = FALSE
    )

    shiny::updateNumericInput(
      session,
      "add_prefer_pct2",
      value = 10
    )

    pol <- input$add_pol2 %||% "positive"
    default_ion <- if (pol == "negative") "M-H" else "M+H"

    definitions <- built_in_adducts_full()

    available <- as.character(
      definitions$Name[
        tolower(as.character(definitions$Ion_mode)) ==
          tolower(pol)
      ]
    )

    available <- available[
      !is.na(available) & nzchar(available)
    ]

    shiny::updateSelectizeInput(
      session,
      "add_preferred2",
      choices = sort(unique(c(available, default_ion))),
      selected = default_ion
    )
  },
  ignoreInit = TRUE,
  priority = -100
)

#options(shiny.maxRequestSize = 1024 * 1024^2)
  # --------------------------
  # Shared upload hub (Tab 0)
  # --------------------------
  file_state <- reactiveValues(valid = FALSE)
  shared <- reactiveValues(raw = NULL, raw_orig = NULL, name = NULL, data_type = NULL,
                           export_template = NULL, export_colmap = NULL, export_rt_factor = 1,
                           msdial_preamble = NULL, msdial_export_names = NULL)

  # mgf
  filtered_mgf_sps <- reactiveVal(NULL)
  mgf_processing <- reactiveVal(FALSE)
  mgf_attempted <- reactiveVal(FALSE)

  current_mgf <- reactiveVal(NULL)
  current_mgf_name <- reactiveVal(NULL)
  parsed_mgf <- reactiveVal(NULL)
  mgf_true_names <- reactiveVal(NULL)

  load_mgf_file <- function(upload, input_id) {
  req(upload)
  ext <- tolower(tools::file_ext(upload$name))

  if (ext != "mgf") {
  showNotification("Error: Please upload a valid .mgf file.",
                   type = "error", duration = 5)
  shinyjs::reset(input_id)
  return(invisible(FALSE))
  }

  w <- Waiter$new(
    html = tagList(
      spin_6(),
      h4("Loading MGF into memory...",
         style = "color:#ffffff !important; text-shadow:none !important;")
    ),
    color = "rgba(44, 62, 80, 0.8)"
  )

  w$show()

  shinyjs::delay(50, {

    tryCatch({

      sps <- Spectra::Spectra(
        upload$datapath,
        source = MsBackendMgf::MsBackendMgf()
      )

      current_mgf(upload$datapath)
      current_mgf_name(upload$name)
      parsed_mgf(sps)
      mgf_true_names(names(Spectra::spectraData(sps)))

      filtered_mgf_sps(NULL)
      mgf_attempted(FALSE)

      showNotification(
        paste("MGF file", upload$name, "loaded successfully!"),
        type = "message",
        duration = 3
      )

    }, error = function(e) {

    shinyjs::reset(input_id)

    showNotification(
      paste("Error parsing MGF:", e$message),
      type = "error",
      duration = 5
    )

    }, finally = {
      w$hide()
    })
  })

  invisible(TRUE)
}

  observeEvent(input$isf_mgf_input, {
  req(input$isf_mgf_input)

  load_mgf_file(
    upload = input$isf_mgf_input,
    input_id = "isf_mgf_input"
  )
  }, ignoreInit = TRUE)

  output$isf_mgf_status_ui <- renderUI({
  req(input$isf_ms2_check2)
  sps <- parsed_mgf()
  if (is.null(sps) || length(sps) == 0) {

    div(
      style = paste0(
        "padding:8px; margin-bottom:10px;",
        "background:#f2dede; color:#a94442;",
        "border-radius:5px;"
      ),
      "No MGF loaded."
    )

  } else {

    div(
      style = paste0(
        "padding:8px; margin-bottom:10px;",
        "background:#dff0d8; color:#3c763d;",
        "border-radius:5px;"
      ),
      tags$b("MGF loaded: "),
      current_mgf_name() %||% "MGF file",
      sprintf(" (%d spectra)", length(sps))
    )
  }
})

  output$isf_mgf_field_ui <- renderUI({

  req(input$isf_ms2_check2)

  sps <- parsed_mgf()
  cols <- mgf_true_names()

  if (is.null(sps) || length(sps) == 0 || is.null(cols))
    return(NULL)

  mgf_default <- if ("acquisitionNum" %in% cols) {
    "acquisitionNum"
  } else {
    cols[1]
  }

  selectInput(
    "isf_mgf_id_field",
    "MGF Feature ID field:",
    choices = cols,
    selected = mgf_default
  )
})

  # raw table, etc
  observeEvent(list(input$file0, input$data_type0), {

    # 1. Check if file exists AND is marked as valid (not cleared)
    req(input$file0)
    file_state$valid <- TRUE

    w_up <- Waiter$new(
    html = tagList(spin_6(), h4("Reading Table...", style="color:#ffffff !important; text-shadow:none !important;")),
    color = "rgba(44, 62, 80, 0.8)"
    )
    w_up$show()

    shinyjs::delay(100, {
    tryCatch({

    ext <- tools::file_ext(input$file0$name)
    shiny::validate(need(ext == "csv", "Error: Please upload a .csv file."))

    type <- input$data_type0 %||% "mzmine"

    if (type == "msdial") {
      df0 <- as.data.frame(vroom::vroom(input$file0$datapath,
                delim = ",", col_names = FALSE, col_types = vroom::cols(.default = "c"), na = ""))
    } else {
      df0 <- as.data.frame(vroom::vroom(input$file0$datapath, delim = ",", show_col_types = FALSE))
    }

    # Wrap standardization
    df_std <- standardize_peak_table(df0, type = type)

    req(df_std) # Stop if parsing failed

    # If successful, clear any previous error
    upload_error(NULL)
    invalidate_all_pipeline()

    shared$raw_orig  <- df0
    shared$raw       <- df_std
    shared$export_template  <- attr(df_std, "export_template")
    shared$export_colmap    <- attr(df_std, "export_colmap")
    shared$export_rt_factor <- attr(df_std, "export_rt_factor") %||% 1

    shared$msdial_preamble  <- attr(df_std, "msdial_preamble")
    shared$msdial_export_names <- attr(df_std, "msdial_export_names")

    shared$name      <- input$file0$name
    shared$data_type <- type

    shinyjs::reset("mgf_input")
    shinyjs::reset("isf_mgf_input")

    current_mgf(NULL)
    current_mgf_name(NULL)
    parsed_mgf(NULL)
    mgf_true_names(NULL)
    filtered_mgf_sps(NULL)
    mgf_processing(FALSE)
    mgf_attempted(FALSE)

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
    shinyjs::reset("target_peak_csv")
    shinyjs::reset("adduct_file2")
    shinyjs::reset("nl_file2")
    shinyjs::reset("mgf_input")
    shinyjs::reset("isf_mgf_input")

    # Reset metadata CSV inputs for Blank and QC labels
    shinyjs::reset("blank_metadata_csv")
    shinyjs::reset("blank_metadata_sample_col")
    shinyjs::reset("blank_metadata_label_col")
    shinyjs::reset("blank_metadata_remove_suffixes")

    shinyjs::reset("qc_metadata_csv")
    shinyjs::reset("qc_metadata_sample_col")
    shinyjs::reset("qc_metadata_label_col")
    shinyjs::reset("qc_metadata_remove_suffixes")

    updateRadioButtons(session, "label_source", selected = "from_rows")
    updateRadioButtons(session, "qc_label_source", selected = "inherit")

    updateSelectizeInput(session, "dratio_qc_group", selected = "")
    updateNumericInput(session, "dratio_cutoff", value = 50)
    shinyWidgets::updateCheckboxGroupButtons(
      session, "value_filters", selected = setdiff(input$value_filters %||% character(0), "dratio"))

    updatePrettyCheckbox(session, "blank_clean_metadata_names", value = FALSE)
    updatePrettyCheckbox(session, "qc_clean_metadata_names", value = FALSE)

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
    ms_state$del_mis  <- character(0)
    ms_state$del_ring <- character(0)

    ms_state$mis_merge_map <- NULL
    ms_state$iso_table <- NULL
    ms_state$add_table <- NULL
    ms_state$nl_table  <- NULL
    ms_state$isf_table <- NULL
    ms_state$mis_table  <- NULL
    ms_state$ring_table <- NULL

    ms_state$stats <- list(before=0L, after=0L, removed_iso=0L, removed_add=0L, removed_nl=0L, removed_isf=0L, removed_mis = 0L, removed_ring = 0L)

    # 6. Reset Step 3 (QC)
    qc_state$applied <- FALSE
    qc_state$show_summary <- FALSE
    qc_state$kept <- NULL
    qc_state$stats <- list(before = 0L, after_final = 0L,
                           removed_zeros = 0L, removed_mean = 0L, removed_rsd = 0L, removed_dratio = 0L, removed_min = 0L, removed_total = 0L)

    # 7. Reset Step 4 (Peak)
    peak_state$applied <- FALSE
    peak_state$show_summary <- FALSE
    peak_state$kept <- NULL
    peak_state$target_matches <- empty_peak_matches()
    updateTextAreaInput(session, "target_peak_text", value = "")
    target_peak_csv_cache(empty_peak_targets())
    target_peak_cleared(TRUE)
    peak_state$stats <- list(
        before = 0L,
        after = 0L,
        removed_mz = 0L,
        removed_rt = 0L,
        removed_rmd = 0L,
        removed_amd = 0L,
        removed_target = 0L,
        matched_target = 0L,
        target_action = "remove",
        removed_total = 0L
      )

    # 8. Reset Final Results
    final_table(NULL)

    # Reset mgf processing
    current_mgf(NULL)
    current_mgf_name(NULL)
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
      html = tagList(spin_6(), h4("Loading Example Dataset...", style="color:#ffffff !important; text-shadow:none !important;")),
      color = "rgba(44, 62, 80, 0.8)"
    )
    w_up$show()

    shinyjs::delay(100, {
      tryCatch({
        # 1. Update the dropdown visually so the user knows it's an mzMine file
        updateSelectInput(session, "data_type0", selected = "mzmine")

        # 2. Use the RAW GitHub URL (vroom needs the raw CSV, not the HTML page)
        example_url <- system.file("extdata", "orbi_iimn_gnps_quant.csv", package = "MetaboCensoR")

        # 3. Read data directly from GitHub
        df0 <- as.data.frame(vroom::vroom(example_url, delim = ",", show_col_types = FALSE))

        # 4. Standardize
        type <- "mzmine"
        df_std <- standardize_peak_table(df0, type = type)

        req(df_std) # Stop if parsing failed

        # 5. Clear any previous errors
        upload_error(NULL)
        invalidate_all_pipeline()

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

        shinyjs::reset("mgf_input")
        shinyjs::reset("isf_mgf_input")

        current_mgf(NULL)
        current_mgf_name(NULL)
        parsed_mgf(NULL)
        mgf_true_names(NULL)
        filtered_mgf_sps(NULL)
        mgf_processing(FALSE)
        mgf_attempted(FALSE)

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

  df_show <- limit_table_preview(
    shared$raw,
    input$limit_table_preview,
    max_rows = 50,
    max_cols = 50
  )

  df_show <- clean_mzmine_export(df_show)

  colmap <- shared$export_colmap
  if (!is.null(colmap)) {
    for (std_nm in names(colmap)) {
      orig_nm <- colmap[[std_nm]]
      if (std_nm %in% names(df_show) && nzchar(orig_nm)) {
        names(df_show)[names(df_show) == std_nm] <- orig_nm
      }
    }
  }

  datatable(df_show, options = list(scrollX = TRUE, pageLength = 5))
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
            h3(n_samples, class = "quick-stat-samples", style = "margin: 0; color: #EE2C2C; font-weight: 800; font-size: 28px;")
        ),
        div(style = "flex: 1; background-color: #f8f9fa; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #3498db; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
            h5("Total Features (Peaks)", style = "margin: 0 0 5px 0; color: #2c3e50; font-weight: bold; text-transform: uppercase; font-size: 14px;"),
            h3(n_features, class = "quick-stat-features", style = "margin: 0; color: #3498db; font-weight: 800; font-size: 28px;")
        ),
        tags$details(
 style = "flex: 1; background-color: #f8f9fa; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #FF8C00; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",

  tags$summary(
    style = "cursor:pointer; font-weight:bold; font-size: 14px; text-transform: uppercase;",
    icon("circle-info"),
    " Large datasets / memory limits"
  ),

  tags$p(
    style = "margin-top:10px;  margin-bottom:8px;",
    "Web version memory limit: 1 GB. ",
    "For large datasets or memory-intensive analyses, we recommend activating the “Limit table previews” option and running MetaboCensoR locally. "
  ),

  tags$a(
    "Local installation instructions",
    href = "https://github.com/plyush1993/MetaboCensoR#launch-the-app-rocket",
    target = "_blank",
    style = "font-weight:bold; color:#FF8C00;"
  )
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

    for (std_nm in names(colmap)) {
      orig_nm <- colmap[[std_nm]]
      idx <- match(std_nm, cols_std)

      if (!is.na(idx) && nzchar(orig_nm)) {
        display_names[idx] <- orig_nm
      }
    }
  }

  # XCMS: first column is the native feature identifier.
  choices_list <- setNames(cols_std, display_names)

    norm_id <- function(x) gsub("[^a-z0-9]", "", tolower(x))
      default_rid <- switch(
        shared$data_type %||% "default",
        mzmine = {
          ii <- which(norm_id(cols_std) == "rowid")[1]
          if (!is.na(ii)) cols_std[ii] else cols_std[1]
        },
        msdial = {
          ii <- which(norm_id(cols_std) == "alignmentid")[1]
          if (!is.na(ii)) cols_std[ii] else cols_std[1]
        },
        xcms = cols_std[1],
        cols_std[1]
      )
    default_mz  <- if ("mz" %in% cols_std) "mz" else cols_std[1]
    default_rt  <- if ("rt" %in% cols_std) "rt" else cols_std[1]

    tagList(
      selectInput("row_id_col0", "Feature ID:", choices = choices_list, selected = default_rid),
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
          choices  = c(".mzML", ".mzXML", ".raw", ".d", ".wiff", ".lcd", "Peak area", "Area", "_Area"),
          selected = c(".mzML", ".mzXML"),
          multiple = TRUE,
          options  = list(create = TRUE, createOnBlur = TRUE,
                          placeholder = "Type to add (e.g. Area) and press Enter")
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
          options = list(placeholder = "Select first and last sample columns")
        )
      ),

    )
  })

  manual_range_state <- reactiveValues(
  first = NULL,
  selected = character(0),
  updating = FALSE
)

  observeEvent(shared$raw, {
  manual_range_state$first <- NULL
  manual_range_state$selected <- character(0)
  manual_range_state$updating <- FALSE
}, ignoreInit = TRUE)

observeEvent(input$sample_cols0, {

  req(shared$raw)

  sel  <- input$sample_cols0 %||% character(0)
  cols <- names(shared$raw)

  # Ignore the observer call caused by updateSelectizeInput()
  if (isTRUE(manual_range_state$updating)) {
    manual_range_state$updating <- FALSE
    manual_range_state$selected <- sel
    return()
  }

  # Detect newly clicked column(s)
  added <- setdiff(sel, manual_range_state$selected)

  # Allow manual removal of selected columns
  removed <- setdiff(manual_range_state$selected, sel)
  if (length(removed)) {
    manual_range_state$selected <- sel
    if (!is.null(manual_range_state$first) &&
        manual_range_state$first %in% removed) {
      manual_range_state$first <- NULL
    }
    return()
  }

  if (!length(added)) {
    manual_range_state$selected <- sel
    return()
  }

  new_col <- tail(added, 1)

  # First click = start of a new range
  if (is.null(manual_range_state$first)) {

    manual_range_state$first <- new_col
    manual_range_state$selected <- sel
    return()
  }

  # Second click = end of the range
  first_col <- manual_range_state$first

  p1 <- match(first_col, cols)
  p2 <- match(new_col, cols)

  if (!is.na(p1) && !is.na(p2)) {

    range_cols <- cols[min(p1, p2):max(p1, p2)]

    # Add new range to already selected ranges
    all_selected <- union(manual_range_state$selected, range_cols)

    # Keep original peak-table column order
    all_selected <- cols[cols %in% all_selected]

    manual_range_state$first <- NULL
    manual_range_state$selected <- all_selected
    manual_range_state$updating <- TRUE

    updateSelectizeInput(
      session,
      "sample_cols0",
      selected = all_selected
    )
  }

}, ignoreInit = TRUE)

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

    req(raw_fid())
    upload_error(NULL)
    df <- raw_fid()
    cols <- names(df)

    mode <- input$sample_mode0 %||% "auto"

    # always exclude known meta columns
    meta <- unique(c(".FID", input$row_id_col0, input$mz_col0, input$rt_col0, "mz", "rt"))
    meta <- meta[!is.na(meta) & nzchar(meta)]

    if (mode == "manual") {
      shiny::validate(need(!is.null(input$sample_cols0) && length(input$sample_cols0) > 0,
                    "Pick sample columns (Upload tab)."))
      sc <- intersect(input$sample_cols0, cols)
      shiny::validate(need(length(sc) > 0, "Manual sample columns not found in table."))
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
        shiny::validate(need(FALSE, msg))
      }
      # ---------------------------

      sc <- cols[idx]
      sc <- setdiff(sc, meta)
      shiny::validate(need(length(sc) > 0, "Keyword hits were only meta columns. Use Manual/Auto."))
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

    shiny::validate(need(length(sc) > 0,
                  "Auto-detect found no numeric sample columns. Switch to Manual or Keywords."))
    sc
  })

  sample_names <- reactive({
  req(sample_cols0())
  sample_cols0()
  })

  output$sample_list_table <- renderDT({

  req(sample_names())

  tbl <- data.frame(
    Sample = sample_names(),
    stringsAsFactors = FALSE
  )

  datatable(
    tbl,
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      ordering = FALSE,
      searching = FALSE
    ),
    rownames = FALSE
  )

}, server = FALSE)

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
  output$blank_label_upload_warning <- renderUI({
  src <- input$label_source %||% "from_rows"

  need_file <- FALSE

  if (identical(src, "from_custom") && is.null(settings_file("meta_csv"))) {
    need_file <- TRUE
  }

  if (identical(src, "from_metadata") && is.null(settings_file("blank_metadata_csv"))) {
    need_file <- TRUE
  }

  if (!need_file) return(NULL)

  div(class="alert alert-warning text-center",
            style="font-size: 18px; font-weight: bold; margin-top: 15px;",
            icon("exclamation-triangle"), " Please upload a CSV file.")
})

  custom_labels_blank <- reactive({
  req(settings_file("meta_csv"), sample_names())

  ext <- tools::file_ext(settings_file("meta_csv")$name)
  shiny::validate(need(ext == "csv", "Upload a .csv for labels (one column, no header)."))

  vec <- vroom::vroom(settings_file("meta_csv")$datapath, col_names = FALSE, delim = ",") |> pull(1)

  shiny::validate(
    need(
      length(vec) == length(sample_names()),
      sprintf("Label count (%d) must match #samples (%d).", length(vec), length(sample_names()))
    )
  )

  as.character(vec)
})

manual_labels_blank <- reactiveVal(NULL)

observeEvent(sample_names(), {
  req(sample_names())

  auto_labs <- labels_from_sample_names(
    sample_names(),
    token_sep = input$token_sep %||% "_",
    token_index = input$label_index %||% 2,
    show_notification = identical(input$tabs, "blank")
  )

  manual_labels_blank(
    make_label_table(sample_names(), auto_labs)
  )
}, ignoreInit = FALSE)

observeEvent(input$apply_auto_labels_blank, {
  req(sample_names())

  auto_labs <- labels_from_sample_names(
    sample_names(),
    token_sep = input$token_sep %||% "_",
    token_index = input$label_index %||% 2,
    show_notification = identical(input$tabs, "blank")
  )

  manual_labels_blank(
    make_label_table(sample_names(), auto_labs)
  )

  showNotification(
    "Editable Blank label table was filled from current token labels.",
    type = "message",
    duration = 3
  )
}, ignoreInit = TRUE)

observeEvent(input$labels_table_cell_edit, {
  info <- input$labels_table_cell_edit

  tbl <- manual_labels_blank()
  req(tbl)

  row_i <- as.integer(info$row)

  shiny::validate(
    need(row_i >= 1 && row_i <= nrow(tbl), "Edited row is outside label table.")
  )

  # Only Label column should be editable in the UI,
  # so always write the edited value into Label.
  tbl$Label[row_i] <- trimws(as.character(info$value))

  manual_labels_blank(tbl)

  showNotification(
    paste0("Label updated: ", tbl$Sample[row_i], " -> ", tbl$Label[row_i]),
    type = "message",
    duration = 2
  )
}, ignoreInit = TRUE)

blank_metadata_raw <- reactive({
  req(settings_file("blank_metadata_csv"))
  read_metadata_csv(settings_file("blank_metadata_csv"), context = "Blank metadata labels")
})


output$blank_metadata_sample_col_ui <- renderUI({
  req(blank_metadata_raw())

  cols <- names(blank_metadata_raw())

  selectInput(
    "blank_metadata_sample_col",
    "Metadata sample-name column:",
    choices = cols,
    selected = guess_metadata_sample_col(cols)
  )
})


output$blank_metadata_label_col_ui <- renderUI({
  req(blank_metadata_raw())

  cols <- names(blank_metadata_raw())
  sample_col <- input$blank_metadata_sample_col %||% guess_metadata_sample_col(cols)
  choices <- setdiff(cols, sample_col)

  shiny::validate(
    need(length(choices) > 0, "Metadata file has no column available for labels.")
  )

  selectizeInput(
    "blank_metadata_label_col",
    "Metadata column to use as Label:",
    choices = choices,
    selected = guess_metadata_label_col(cols, sample_col),
    multiple = FALSE
  )
})


blank_metadata_labels <- reactive({
  req(
    settings_file("blank_metadata_csv"),
    input$blank_metadata_sample_col,
    input$blank_metadata_label_col,
    sample_names()
  )

  metadata_labels_by_sample(
    upload = settings_file("blank_metadata_csv"),
    sample_names = sample_names(),
    sample_col = input$blank_metadata_sample_col,
    label_col = input$blank_metadata_label_col,
    clean_enabled = isTRUE(input$blank_clean_metadata_names),
    remove_suffixes = input$blank_metadata_remove_suffixes %||% character(0),
    context = "Blank metadata labels"
  )
})

label_vector_blank <- reactive({
  req(sample_names())

  src <- input$label_source %||% "from_rows"

  if (identical(src, "from_custom")) {

    custom_labels_blank()

  } else if (identical(src, "from_metadata")) {

    blank_metadata_labels()

  } else if (identical(src, "manual")) {

    tbl <- manual_labels_blank()
    req(tbl)

    shiny::validate(
      need(nrow(tbl) == length(sample_names()),
           "Manual label table must match the number of samples."),
      need(!any(is.na(tbl$Label) | tbl$Label == ""),
           "All samples must have labels.")
    )

    as.character(tbl$Label)

  } else {

    labels_from_sample_names(
      sample_names(),
      token_sep = input$token_sep %||% "_",
      token_index = input$label_index %||% 2
    )
  }
})

output$labels_table <- renderDT({
  req(sample_names(), label_vector_blank())

  src <- input$label_source %||% "from_rows"

  tbl <- if (identical(src, "manual")) {
    manual_labels_blank()
  } else {
    make_label_table(sample_names(), label_vector_blank())
  }

  req(tbl)

  datatable(
    tbl,
    editable = if (identical(src, "manual")) {
      list(
        target = "cell",
        disable = list(columns = c(0)) # do not edit Sample column
      )
    } else {
      FALSE
    },
    options = list(
      pageLength = 5,
      scrollX = TRUE,
  ordering = FALSE,
  searching = FALSE
    ),
    rownames = FALSE
  )
}, server = FALSE)

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
        choices = labs,
        multiple = TRUE,
        selected = intersect(
          isolate(input$groups_to_remove) %||% character(0),
          labs
        ),
        options = list(
          placeholder = "Pick one or more (e.g., Media, Blank)"
        )
      ),
      radioButtons(
        "blank_mode", "Filtering mode:",
        choices = c(
          "Blank < Cutoff * Sample Max" = "mean_cutoff",
          "Drop any feature detected in Blank" = "drop_detected"
        ),
        selected = isolate(input$blank_mode) %||% "mean_cutoff"
      ),
      conditionalPanel(
        condition = "input.blank_mode == 'mean_cutoff'",
        numericInput(
            "blank_cutoff",
            "Cutoff (fraction of max experimental mean):",
            value = isolate(input$blank_cutoff) %||% 0.10,
            min = 0,
            step = 0.01
          )
      )
    )
  })

  blank_plot_data <- eventReactive(input$plot_blank, {
    req(ds0_with_label(), input$groups_to_remove, input$blank_mode)
    ds <- ds0_with_label()
    groups <- input$groups_to_remove
    feats <- numeric_feature_names(ds)

    shiny::validate(need(length(feats) > 0, "No numeric features to plot."))

    mode <- input$blank_mode

    if (mode == "mean_cutoff") {
      # --- MODE 1: Ratio Cutoff ---
      # Calculate means for all groups
      mean_table <- ds %>%
        group_by(Label) %>%
        summarise(across(all_of(feats), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

      # Get Blank means
      blanks <- mean_table %>% filter(Label %in% groups) %>% select(-Label)
      shiny::validate(need(nrow(blanks) > 0, "Selected Blank groups not found in data."))
      blank_means <- blanks %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
      blank_vec <- as.numeric(blank_means[1, ])

      # Get Experimental maximum means
      exps <- mean_table %>% filter(!Label %in% groups) %>% select(-Label)
      shiny::validate(need(nrow(exps) > 0, "No experimental groups remaining to compare against."))
      max_exp <- vapply(exps, function(x) max(x, na.rm = TRUE), numeric(1))

      # Calculate Ratio
      ratio <- blank_vec / max_exp

      valid <- is.finite(blank_vec) &
        is.finite(max_exp) &
        blank_vec >= 0 &
        max_exp >= 0

      status <- rep("Other / undefined", length(feats))

      status[valid & blank_vec > 0 & max_exp > 0 & is.finite(ratio)] <- "Positive finite ratio"

      status[valid & blank_vec == 0 & max_exp > 0] <- "Zero blank / positive sample"

      status[valid & blank_vec > 0 & max_exp == 0] <- "Positive blank / zero sample"

      status[valid & blank_vec == 0 & max_exp == 0] <- "Both zero"

      df <- tibble::tibble(
        Feature = feats,
        Blank = blank_vec,
        Sample = as.numeric(max_exp),
        Ratio = ratio,
        Status = status
      )

      return(list(mode = "mean_cutoff", data = df))

    } else if (mode == "drop_detected") {
      # --- MODE 2: Drop Detected ---
      blanks <- ds %>% filter(Label %in% groups)
      shiny::validate(need(nrow(blanks) > 0, "Selected Blank groups not found in data."))

      # Check if feature has ANY signal > 0 in the blank groups
      detected <- vapply(feats, function(f) any(blanks[[f]] > 0, na.rm = TRUE), logical(1))

      df <- tibble::tibble(
        Feature = feats,
        Detected = detected,
        Status = ifelse(detected, "Filtered", "Kept")
      )

      return(list(mode = "drop_detected", data = df))
    }
  }, ignoreInit = TRUE)

  # Flag to trigger the UI ConditionalPanel
  output$blankPlotReady <- reactive({ !is.null(blank_plot_data()) })
  outputOptions(output, "blankPlotReady", suspendWhenHidden = FALSE)

  # Render the Plotly histogram
  output$blank_plot <- renderPlotly({
    res <- blank_plot_data()
    req(res)

    mode <- res$mode
    df <- res$data
    req(nrow(df) > 0)

    total_feats <- nrow(df)

        if (mode == "mean_cutoff") {
      current_cutoff <- input$blank_cutoff %||% 0.10

      shiny::validate(
        need(
          length(current_cutoff) == 1L &&
            is.finite(current_cutoff) &&
            current_cutoff >= 0,
          "Enter a finite, non-negative blank cutoff."
        )
      )

      # Match the actual filtering rule, including zero cases.
      keep <- df$Blank < current_cutoff * pmax(df$Sample, 0)
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

        h <- hist(
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
            " – ",
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
        layout(
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
    settings_capture("blank", blank_state$stats)

    if (!isTRUE(input$enable_blank)) {
      showNotification("Blank Filters disabled. Step 1 frozen as pass-through.", type="message", duration=4)
    } else if (blank_state$stats$after == 0) {
      showNotification("Blank Filters removed all features. Try relaxing cutoffs.", type="error", duration=5)
    } else {
      showNotification(sprintf("Step 1 frozen: kept %d of %d features.", blank_state$stats$after, blank_state$stats$before), type="message", duration=4)
    }
  }

  observeEvent(input$apply_blank, {
    if (!settings_ready(stage = "blank")) return()
    if (isTRUE(input$enable_blank) && length(input$groups_to_remove) == 0) {
      showNotification("Error: Please select at least one Blank/Media group before applying.", type = "error", duration = 5)
      return() # Stop execution immediately
    }

    w <- Waiter$new(
      id = "blank_table_out",
      html = tagList(spin_6(), h4("Applying Blank Filters...", style="color:#ffffff !important; text-shadow:none !important;")),
      color = "rgba(44, 62, 80, 0.8)"
    )
    w$show()

    shinyjs::disable("apply_blank")

    shinyjs::delay(50, {
      tryCatch({
        run_step1_blank()
        invalidate_after_blank()
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
    blank_state$stats <- list(before = 0L, after = 0L, removed_blank = 0L)

    # Clear Blank labels uploads
    shinyjs::reset("meta_csv")
    shinyjs::reset("blank_metadata_csv")
    shinyjs::reset("blank_metadata_sample_col")
    shinyjs::reset("blank_metadata_label_col")
    shinyjs::reset("blank_metadata_remove_suffixes")

    updateRadioButtons(session, "label_source", selected = "from_rows")
    updatePrettyCheckbox(session, "blank_clean_metadata_names", value = FALSE)

    invalidate_after_blank()
    showNotification("Step 1 reset: pass-through.", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  raw_after_blank <- reactive({
    req(raw_zeroed())
    if (!isTRUE(blank_state$applied) || is.null(blank_state$kept)) return(raw_zeroed())
    df <- raw_zeroed()
    df[df$.FID %in% blank_state$kept, , drop = FALSE]
  })

  output$blank_header_in <- renderUI({ req(raw_zeroed()); h3("Input table") })
  output$blank_table_in  <- renderDT({
  req(raw_zeroed())
  datatable(limit_table_preview(raw_zeroed(), input$limit_table_preview, 50, 50),
            options = list(scrollX = TRUE, pageLength = 5))
})
  output$blank_header_out <- renderUI({ req(raw_after_blank()); h3("Output table — after Blank Filters") })
  output$blank_table_out <- renderDT({
  req(raw_after_blank())
  datatable(limit_table_preview(raw_after_blank(), input$limit_table_preview, 50, 50),
            options = list(scrollX = TRUE, pageLength = 5))
})

  output$blank_filter_summary <- renderUI({
    req(blank_state$applied)
    req(isTRUE(blank_state$show_summary))

    before <- blank_state$stats$before %||% 0L
    after  <- blank_state$stats$after %||% before
    removed_blank <- blank_state$stats$removed_blank %||% (before - after)

    summary_table_ui(
      "Filtering results — Step 1 (Blank Filters)",
      tibble::tibble(
        Metric = c("Features before", "Removed by Blank Filters", "Features after"),
        Value  = c(before, removed_blank, after)
      )
    )
  })

    # --------------------------
    # STEP 2 state: MS Filters (REPLACEMENT)
    # --------------------------
    ms_state <- reactiveValues(
      applied = FALSE,
      show_summary = FALSE,
      matrix = NULL,

      del_iso = character(0),
      del_add = character(0),
      del_nl  = character(0),
      del_isf = character(0),
      del_mis = character(0),
      del_ring = character(0),

      mis_merge_map = NULL,
      iso_table = NULL,
      add_table = NULL,
      nl_table  = NULL,
      isf_table = NULL,
      mis_table = NULL,
      ring_table = NULL,

      isf_status = NULL,

      stats = list(
        before = 0L, after = 0L,
        removed_iso = 0L, removed_add = 0L, removed_nl = 0L, removed_isf = 0L, removed_mis = 0L, removed_ring = 0L
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
      ms_state$stats$removed_iso  <- 0L
      ms_state$stats$removed_add  <- 0L
      ms_state$stats$removed_nl   <- 0L
      ms_state$stats$removed_isf  <- 0L
      ms_state$stats$removed_mis  <- 0L
      ms_state$stats$removed_ring <- 0L

      ms_state$del_iso  <- character(0)
      ms_state$del_add  <- character(0)
      ms_state$del_nl   <- character(0)
      ms_state$del_isf  <- character(0)
      ms_state$del_mis  <- character(0)
      ms_state$del_ring <- character(0)

      ms_state$mis_merge_map <- NULL
      ms_state$iso_table  <- NULL
      ms_state$add_table  <- NULL
      ms_state$nl_table   <- NULL
      ms_state$isf_table  <- NULL
      ms_state$mis_table  <- NULL
      ms_state$ring_table <- NULL

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
        edges$r <- NA_real_

        if (nrow(edges) > 0) {
          X <- as.matrix(ds)
          idx1 <- match(edges$Feature1, colnames(X))
          idx2 <- match(edges$Feature2, colnames(X))
          edges$r <- cor_for_pairs(X, idx1, idx2)

          rm(X); gc()

          edges <- edges %>% dplyr::filter(is.finite(r), r >= as.numeric(input$iso_cor2 %||% 0.80))
        }

        res_iso <- collapse_components_keep_most_intense(edges, pk, group_col = "iso_group")

        # Isotope/dimer annotation per feature — include both directions
        iso_by_peak <- dplyr::bind_rows(
          edges %>%
            dplyr::transmute(
              Feature = Feature1,
              connected_to = Feature2,
              shift = as.character(shift),
              r = as.numeric(r)
            ),
          edges %>%
            dplyr::transmute(
              Feature = Feature2,
              connected_to = Feature1,
              shift = as.character(shift),
              r = as.numeric(r)
            )
        ) %>%
          dplyr::filter(!is.na(Feature), !is.na(connected_to)) %>%
          dplyr::distinct(Feature, connected_to, shift, .keep_all = TRUE) %>%
          dplyr::arrange(Feature, connected_to, shift) %>%
          dplyr::group_by(Feature) %>%
          dplyr::summarise(
            connected_to = paste(connected_to, collapse = "; "),
            shift = paste(shift, collapse = "; "),
            r = paste(formatC(r, format = "f", digits = 3), collapse = "; "),
            .groups = "drop"
          )

        iso_tbl <- res_iso$table %>%
          dplyr::left_join(iso_by_peak, by = "Feature") %>%
          dplyr::mutate(
            connected_to = tidyr::replace_na(connected_to, "none"),
            shift = tidyr::replace_na(shift, "none"),
            r = tidyr::replace_na(r, "none")
          ) %>%
          dplyr::relocate(connected_to, .after = Feature)

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
      if (!is.null(hostess)) hostess$set(25)
      if (isTRUE(input$enable_add2) && ncol(ds) > 0) {
        pol <- input$add_pol2 %||% "positive"

        adducts_df <- safe_read_csv_any(settings_file("adduct_file2"))
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
        rt_tol_add <- as.numeric(input$add_rt2 %||% 0.005)

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
                dplyr::mutate(rt_group = split_rt_strict(ret, as.numeric(input$add_rt2 %||% 0.005))) %>%
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
            # adduct annotation per peak, based only on final correlated / strict-RT edges
              add_by_peak <- dplyr::bind_rows(
                edges_corr_rt %>%
                  dplyr::transmute(
                    peak_id = as.character(from),
                    pair = paste0(from_adduct, " -> ", to_adduct, " [", feat_by_pid[to], "]"),
                    r_value = as.numeric(r)
                  ),

                edges_corr_rt %>%
                  dplyr::transmute(
                    peak_id = as.character(to),
                    pair = paste0(to_adduct, " -> ", from_adduct, " [", feat_by_pid[from], "]"),
                    r_value = as.numeric(r)
                  )
              ) %>%
                dplyr::filter(!is.na(pair), nzchar(pair)) %>%
                dplyr::group_by(peak_id) %>%
                dplyr::summarise(
                  adducts = paste(sort(unique(pair)), collapse = "; "),
                  connected_to = paste(
                    sort(unique(gsub("^.*\\[|\\]$", "", pair))), collapse = "; "),
                  r = paste(formatC(r_value[match(sort(unique(pair)), pair)], format = "f", digits = 3), collapse = "; "),
                  .groups = "drop"
                )

            # representative per family = max intensity
            reps <- peaks %>%
              dplyr::left_join(fam_map, by = "peak_id") %>%
              dplyr::filter(!is.na(family_id)) %>%
              dplyr::group_by(family_id) %>%
              dplyr::slice_max(order_by = int, n = 1, with_ties = FALSE) %>%
              dplyr::ungroup() %>%
              dplyr::transmute(peak_id, keep_rep = TRUE)

            if (isTRUE(input$add_prefer2)) {
            reps <- .mc_preferred_adduct_reps(
              peaks = peaks,
              fam_map = fam_map,
              edges = edges_corr_rt,
              preferred = input$add_preferred2,
              max_drop = as.numeric(input$add_prefer_pct2 %||% 10)
            )
          }

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
      if (!is.null(hostess)) hostess$set(55)
      if (isTRUE(input$enable_nl2) && ncol(ds) > 0) {
        nl_pol <- input$nl_pol2 %||% "positive"
        nl_df <- safe_read_csv_any(settings_file("nl_file2"))
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

  # Pairwise directional filtering:
  # higher m/z = precursor; lower m/z = putative neutral-loss product
  del_pids <- unique(as.character(hits$frag_id))

  ms_state$del_nl <- unique(unname(feat_by_pid[del_pids]))
  ms_state$del_nl <- ms_state$del_nl[!is.na(ms_state$del_nl)]
  ms_state$stats$removed_nl <- length(ms_state$del_nl)

  ms_state$nl_table <- hits %>%
    dplyr::mutate(
      prec_feature = unname(feat_by_pid[as.character(prec_id)]),
      frag_feature = unname(feat_by_pid[as.character(frag_id)]),
      prec_status = dplyr::if_else(
        as.character(prec_id) %in% del_pids, "filtered", "kept"
      ),
      frag_status = "filtered"
    ) %>%
    dplyr::left_join(
      pk %>% dplyr::select(peak_id, prec_rt = rt, prec_int = intensity),
      by = c("prec_id" = "peak_id")
    ) %>%
    dplyr::left_join(
      pk %>% dplyr::select(peak_id, frag_rt = rt, frag_int = intensity),
      by = c("frag_id" = "peak_id")
    ) %>%
    dplyr::select(
      prec_feature, frag_feature, prec_status, frag_status,
      prec_mz, frag_mz, prec_rt, frag_rt, prec_int, frag_int,
      delta_mz, loss_name, r, delta_rt
    ) %>%
    dplyr::arrange(prec_feature, dplyr::desc(r))

  if (ms_state$stats$removed_nl > 0) {
    ds <- ds[, setdiff(colnames(ds), ms_state$del_nl), drop = FALSE]
    ft <- refresh_ft(ds, ft)
  }

  suppressWarnings(rm(hits, del_pids))
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
      if (!is.null(hostess)) hostess$set(65)
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
                delta_mz = prec_mz - frag_mz,
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

            # Add canonical feature IDs to candidate precursor/fragment pairs.
            # These IDs are used for MGF matching.
            pairs2 <- pairs2 %>%
              dplyr::mutate(
                prec_feature = unname(feat_by_pid[as.character(prec_id)]),
                frag_feature = unname(feat_by_pid[as.character(frag_id)])
              )

            # OPTIONAL: MS2 confirmation of ISF relationships
            if (isTRUE(input$isf_ms2_check2) && nrow(pairs2) > 0) {

              pairs2 <- check_isf_pairs_ms2(
                pairs = pairs2,
                sps = parsed_mgf(),
                id_field = input$isf_mgf_id_field,
                min_rel_pct = as.numeric(input$isf_ms2_rel2 %||% 1),
                mz_tol_da = as.numeric(input$isf_ms2_mz_tol2 %||% 0.01)
              )

              if (!any(pairs2$ms2_available)) {
              showNotification(
                "No precursor Feature IDs matched the selected MGF ID field. Check that the Feature ID selected during upload corresponds to the MGF spectrum IDs.",
                type = "warning", duration = 8
              )
            }

              n_all <- nrow(pairs2)
              n_ms2 <- sum(pairs2$ms2_available)
              n_supported <- sum(pairs2$ms2_available & pairs2$ms2_match)

              showNotification(
                sprintf("MS2 confirmation: %d/%d candidate pairs had precursor MS2; %d were supported.",
                        n_ms2, n_all, n_supported),
                type = "message", duration = 6
              )

              # Keep only candidate edges supported by MS2
              pairs2 <- pairs2 %>%
                dplyr::filter(ms2_available, ms2_match)
            }

            if (nrow(pairs2) > 0) {

  # Pairwise directional filtering:
  # higher m/z = precursor; lower m/z = putative in-source fragment
  del_pids <- unique(as.character(pairs2$frag_id))

  ms_state$del_isf <- unique(unname(feat_by_pid[del_pids]))
  ms_state$del_isf <- ms_state$del_isf[!is.na(ms_state$del_isf)]
  ms_state$stats$removed_isf <- length(ms_state$del_isf)

  ms_state$isf_table <- pairs2 %>%
    dplyr::mutate(
      prec_status = dplyr::if_else(
        as.character(prec_id) %in% del_pids, "filtered", "kept"
      ),
      frag_status = "filtered"
    ) %>%
    dplyr::left_join(
      pk %>% dplyr::select(peak_id, prec_rt = rt),
      by = c("prec_id" = "peak_id")
    ) %>%
    dplyr::left_join(
      pk %>% dplyr::select(peak_id, frag_rt = rt),
      by = c("frag_id" = "peak_id")
    ) %>%
    dplyr::select(
      prec_feature, frag_feature, prec_status, frag_status,
      prec_mz, frag_mz, delta_mz,
      dplyr::any_of(c(
        "ms2_match", "ms2_n_spectra", "ms2_match_mz",
        "ms2_error_da", "ms2_rel_int"
      )),
      prec_rt, frag_rt, prec_int, frag_int,
      frag_to_prec, r, delta_rt
    ) %>%
    dplyr::arrange(prec_feature, dplyr::desc(r))

  ms_state$isf_status <- pk %>%
    dplyr::transmute(
      Feature,
      isf_status = dplyr::if_else(
        as.character(peak_id) %in% del_pids,
        "filtered", "kept"
      )
    )

  if (ms_state$stats$removed_isf > 0) {
    ds <- ds[, setdiff(colnames(ds), ms_state$del_isf), drop = FALSE]
    ft <- refresh_ft(ds, ft)
  }

  suppressWarnings(rm(del_pids))
}
          }
        }

        suppressWarnings(rm(pk, rt_pairs, pairs2, pkm)); gc()

      }

      # ==========================
      # E) Mispicked ions collapse
      # ==========================
      if (!is.null(hostess)) hostess$set(75)
      if (isTRUE(input$enable_mis2) && ncol(ds) > 0) {
        pk <- ft %>%
          dplyr::transmute(Feature, mz = as.numeric(mz), rt = as.numeric(rt), intensity = as.numeric(intensity)) %>%
          dplyr::filter(is.finite(mz), is.finite(rt), Feature %in% colnames(ds))

        tol_fun <- mz_tol_fun_factory(
          type = input$mis_tol_type2 %||% "ppm",
          mz_tol_da = as.numeric(input$mis_mz_tol2 %||% 0.005),
          ppm = as.numeric(input$mis_ppm2 %||% 10)
        )

        edges <- make_mispicked_edges(pk = pk, rt_tol = as.numeric(input$mis_rt2 %||% 0.002), tol_fun = tol_fun)
        edges$r <- NA_real_

        if (nrow(edges) > 0) {
          X <- as.matrix(ds)
          idx1 <- match(edges$Feature1, colnames(X))
          idx2 <- match(edges$Feature2, colnames(X))
          edges$r <- cor_for_pairs(X, idx1, idx2)
          rm(X); gc()

          edges <- edges %>% dplyr::filter(is.finite(r), r >= as.numeric(input$mis_cor2 %||% 0.80))
        }

        res_mis <- collapse_components_keep_most_intense(edges, pk, group_col = "mis_group")
        mis_r <- dplyr::bind_rows(
          edges %>%
            dplyr::transmute(
              Feature = Feature1,
              r_value = r
            ),

          edges %>%
            dplyr::transmute(
              Feature = Feature2,
              r_value = r
            )

        ) %>%
          dplyr::group_by(Feature) %>%
          dplyr::summarise(
            r = paste(
              formatC(
                r_value,
                format = "f",
                digits = 3
              ),
              collapse = "; "
            ),
            .groups = "drop"
          )

        ms_state$mis_table <- res_mis$table %>%
          dplyr::left_join(
            mis_r,
            by = "Feature"
          )
        ms_state$mis_merge_map <- res_mis$merge_map

        # Sum mispicked ion intensities into representative ion
        if (!is.null(ms_state$mis_merge_map) && nrow(ms_state$mis_merge_map) > 0) {
          ds <- apply_merge_map_to_matrix(ds, ms_state$mis_merge_map)
        }

        # Delete non-representative features after summing
        ms_state$del_mis <- setdiff(colnames(ds), res_mis$keep)
        ms_state$stats$removed_mis <- length(ms_state$del_mis)

        if (length(ms_state$del_mis) > 0) {
          ds <- ds[, setdiff(colnames(ds), ms_state$del_mis), drop = FALSE]
          ft <- refresh_ft(ds, ft)
        }

        suppressWarnings(rm(edges, res_mis)); gc()
      }

      # ==========================
      # F) Saturated ions (Ringing) collapse
      # ==========================
      if (!is.null(hostess)) hostess$set(85)
      if (isTRUE(input$enable_ring2) && ncol(ds) > 0) {
        pk <- ft %>%
          dplyr::transmute(Feature, mz = as.numeric(mz), rt = as.numeric(rt), intensity = as.numeric(intensity)) %>%
          dplyr::filter(is.finite(mz), is.finite(rt), Feature %in% colnames(ds))

        tol_fun_ring <- mz_tol_fun_factory(
          type = input$ring_tol_type2 %||% "da",
          mz_tol_da = as.numeric(input$ring_mz_tol2 %||% 0.95),
          ppm = as.numeric(input$ring_ppm2 %||% 500)
        )

        edges <- make_ringing_edges(
          pk = pk,
          rt_tol = as.numeric(input$ring_rt2 %||% 0.05),
          tol_fun = tol_fun_ring,
          min_anchor_int = as.numeric(input$ring_min_anch2 %||% 1e5),
          max_ratio = as.numeric(input$ring_ratio2 %||% 10),
          bidirectional = isTRUE(input$ring_bidirectional2)
        )
        edges$r <- NA_real_

        if (nrow(edges) > 0) {
          X <- as.matrix(ds)
          idx1 <- match(edges$Feature1, colnames(X))
          idx2 <- match(edges$Feature2, colnames(X))
          edges$r <- cor_for_pairs(X, idx1, idx2)
          rm(X); gc()

          edges <- edges %>% dplyr::filter(is.finite(r), r >= as.numeric(input$ring_cor2 %||% 0.80))
        }

        res_ring <- collapse_components_keep_most_intense(edges, pk, group_col = "ring_group")

        ring_r <- dplyr::bind_rows(

          edges %>%
            dplyr::transmute(
              Feature = Feature1,
              r_value = r
            ),

          edges %>%
            dplyr::transmute(
              Feature = Feature2,
              r_value = r)
          ) %>%
          dplyr::group_by(Feature) %>%
          dplyr::summarise(
            r = paste(
              formatC(
                r_value,
                format = "f",
                digits = 3
              ),
              collapse = "; "
            ),
            .groups = "drop")

        ms_state$ring_table <- res_ring$table %>%
          dplyr::left_join(
            ring_r,
            by = "Feature"
          )
        ms_state$del_ring <- setdiff(colnames(ds), res_ring$keep)
        ms_state$stats$removed_ring <- length(ms_state$del_ring)

        if (length(ms_state$del_ring) > 0) {
          ds <- ds[, setdiff(colnames(ds), ms_state$del_ring), drop = FALSE]
          ft <- refresh_ft(ds, ft)
        }
        suppressWarnings(rm(edges, res_ring)); gc()
      }

      # finalize
      if (!is.null(hostess)) hostess$set(100)
      ms_state$matrix <- ds
      ms_state$applied <- TRUE
      ms_state$show_summary <- TRUE
      ms_state$stats$after <- ncol(ds)
      settings_capture("ms", ms_state$stats)

      any_ms_enabled <- isTRUE(input$enable_iso2) ||
      isTRUE(input$enable_add2) ||
      isTRUE(input$enable_nl2) ||
      isTRUE(input$enable_isf2) ||
      isTRUE(input$enable_mis2) ||
      isTRUE(input$enable_ring2)

      if (!any_ms_enabled) {
        showNotification("No MS Filters enabled. Step 2 frozen as pass-through.", type="message", duration=4)
      } else if (ms_state$stats$after == 0) {
        showNotification("MS Filters removed all features. Try relaxing parameters.", type="error", duration=5)
      } else {
        showNotification(sprintf("Step 2 frozen: kept %d of %d features.", ms_state$stats$after, ms_state$stats$before), type="message", duration=4)
      }
    }

    observeEvent(input$apply_ms, {
  if (!settings_ready(stage = "ms")) return()

  # Validate preferred-ion intensity threshold
  if (isTRUE(input$enable_add2) && isTRUE(input$add_prefer2)) {
    pct <- input$add_prefer_pct2

    if (!is.numeric(pct) ||
        length(pct) != 1L ||
        !is.finite(pct) ||
        pct < 0 ||
        pct > 100) {
      showNotification(
        paste(
          "Preferred ions: enter a maximum intensity decrease",
          "between 0 and 100%."
        ),
        type = "error",
        duration = 6
      )
      return()
    }
  }

  # MS2-supported ISF requires an uploaded MGF
  if (isTRUE(input$enable_isf2) && isTRUE(input$isf_ms2_check2)) {

    sps <- parsed_mgf()

    if (is.null(sps) || length(sps) == 0) {
      showNotification(
        "MS2-supported ISF filtering is enabled. Please upload an MGF file.",
        type = "error",
        duration = 6
      )
      return()
    }

    if (is.null(input$isf_mgf_id_field) ||
        !nzchar(input$isf_mgf_id_field) ||
        !input$isf_mgf_id_field %in% mgf_true_names()) {

      showNotification(
        "Please select a valid MGF Feature ID field.",
        type = "error",
        duration = 6
      )
      return()
    }
  }
      hostess_ms <- Hostess$new()

      w <- Waiter$new(
      id = "ms_table_out",
      html = tagList(
        spin_6(),
        h4("Applying MS Filters...", style="color:#ffffff !important; text-shadow:none !important;"),
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
        invalidate_after_ms()
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

      ms_state$del_iso  <- character(0)
      ms_state$del_add  <- character(0)
      ms_state$del_nl   <- character(0)
      ms_state$del_isf  <- character(0)
      ms_state$del_mis  <- character(0)
      ms_state$del_ring <- character(0)

      ms_state$mis_merge_map <- NULL
      ms_state$iso_table  <- NULL
      ms_state$add_table  <- NULL
      ms_state$nl_table   <- NULL
      ms_state$isf_table  <- NULL
      ms_state$mis_table  <- NULL
      ms_state$ring_table <- NULL

      shinyjs::reset("adduct_file2")
      shinyjs::reset("nl_file2")

      ms_state$stats <- list(
        before = 0L,
        after = 0L,
        removed_iso = 0L,
        removed_add = 0L,
        removed_nl = 0L,
        removed_isf = 0L,
        removed_mis = 0L,
        removed_ring = 0L
      )

      invalidate_after_ms()
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

    if (!is.null(ms_state$mis_merge_map) && nrow(ms_state$mis_merge_map) > 0) {
    df <- apply_merge_map_to_raw(
      raw_df_fid = df,
      merge_map  = ms_state$mis_merge_map,
      sample_cols = sample_cols0()
    )
    }

    del_ms <- unique(c(
      ms_state$del_iso %||% character(0),
      ms_state$del_add %||% character(0),
      ms_state$del_nl  %||% character(0),
      ms_state$del_isf %||% character(0),
      ms_state$del_mis %||% character(0),
      ms_state$del_ring %||% character(0)
    ))
    if (length(del_ms) > 0) {
      df <- df[!df$.FID %in% del_ms, , drop = FALSE]
    }
    df
    })

    output$ms_header_in <- renderUI({ req(raw_after_blank()); h3("Input table") })
    output$ms_table_in <- renderDT({
  req(raw_after_blank())
  datatable(
    limit_table_preview(raw_after_blank(), input$limit_table_preview, 50, 50),
    options = list(scrollX = TRUE, pageLength = 5)
  )
})

    output$ms_header_out <- renderUI({ req(raw_after_ms()); h3("Output table — after MS Filters") })
    output$ms_table_out <- renderDT({
  req(raw_after_ms())
  datatable(
    limit_table_preview(raw_after_ms(), input$limit_table_preview, 50, 50),
    options = list(scrollX = TRUE, pageLength = 5)
  )
})

    # detail tables
    output$iso2_table <- renderDT({
  req(input$show_iso_table2)

  tbl <- ms_state$iso_table
  if (is.null(tbl) || nrow(tbl) == 0)
    return(datatable(data.frame(Note = "No isotope/dimer groups found (or skipped).")))

  tbl <- tbl[, setdiff(names(tbl), "Rep"), drop = FALSE]
  tbl$iso_group <- suppressWarnings(as.integer(tbl$iso_group))

  tbl_show <- limit_table_preview(
    tbl,
    input$limit_table_preview,
    max_rows = 100,
    max_cols = Inf
  )

  i <- match("iso_group", names(tbl_show))

  datatable(
    tbl_show,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      order = list(list(i, "desc"))
    )
  ) %>%
    DT::formatRound(columns = c("mz"), digits = 4) %>%
    DT::formatRound(columns = c("rt"), digits = 3) %>%
    DT::formatRound(columns = c("intensity"), digits = 1) %>%
    formatStyle(
      "status",
      backgroundColor = styleEqual(
        c("kept", "keep_rep", "filtered"),
        c("#dff0d8", "#dff0d8", "#f2dede")
      )
    )
}, server = FALSE)

    output$dl_iso2_table <- downloadHandler(
  filename = function() {
    paste0(tools::file_path_sans_ext(basename(shared$name)), " isotopes-dimers.csv")
  },
  content = function(file) {
    tbl <- ms_state$iso_table
    req(tbl)

    tbl <- tbl[, setdiff(names(tbl), "Rep"), drop = FALSE]
    write.csv(tbl, file, row.names = FALSE)
  }
)

    output$add2_table <- renderDT({
  req(input$show_add_table2)

  tbl <- ms_state$add_table
  if (is.null(tbl) || nrow(tbl) == 0)
    return(datatable(data.frame(Note = "No adduct families found (or skipped).")))

  tbl <- tbl[, setdiff(names(tbl), c("keep_rep", "peak_id")), drop = FALSE]

  tbl <- tbl %>%
    dplyr::rename(
      intensity = int,
      rt = ret
    ) %>%
    dplyr::relocate(
      r,
      .after = status
    )

  tbl_show <- limit_table_preview(
    tbl,
    input$limit_table_preview,
    max_rows = 100,
    max_cols = Inf
  )

  idx_group <- match("group_id", names(tbl_show))
  idx_family <- match("family_id", names(tbl_show))

  datatable(
    tbl_show,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      order = list(
        list(idx_group, "desc"),
        list(idx_family, "desc")
      )
    )
  ) %>%
    DT::formatRound(columns = c("mz"), digits = 4) %>%
    DT::formatRound(columns = c("intensity"), digits = 1) %>%
    DT::formatRound(columns = c("rt"), digits = 3) %>%
    formatStyle(
      "status",
      backgroundColor = styleEqual(
        c("kept (no group)", "kept (no corr-family)", "keep_rep", "filtered"),
        c("#dff0d8", "#dff0d8", "#dff0d8", "#f2dede")
      )
    )
}, server = FALSE)

    output$dl_add2_table <- downloadHandler(
  filename = function() {
    paste0(tools::file_path_sans_ext(basename(shared$name)), " adducts.csv")
  },
  content = function(file) {
    tbl <- ms_state$add_table
    req(tbl)

    tbl <- tbl[, setdiff(names(tbl), c("keep_rep", "peak_id")), drop = FALSE]

    tbl <- tbl %>%
      dplyr::rename(
        intensity = int,
        rt = ret
      ) %>%
      dplyr::relocate(
        r,
        .after = status
      )

    write.csv(tbl, file, row.names = FALSE)
  }
)

    output$add2_stats_table <- renderDT({
  req(input$show_add_stats2)

  tbl <- ms_state$add_table

  if (is.null(tbl) || nrow(tbl) == 0) {
    return(datatable(data.frame(Note = "No adduct families found (or skipped).")))
  }

  if (!"adducts" %in% names(tbl)) {
    return(datatable(data.frame(Note = "No adduct annotation column found.")))
  }

  tbl_sub <- tbl[
    !is.na(tbl$adducts) &
      tbl$adducts != "none" &
      nzchar(tbl$adducts),
    ,
    drop = FALSE
  ]

  if (nrow(tbl_sub) == 0) {
    return(datatable(data.frame(Note = "No specific adducts annotated.")))
  }

  add_long <- tbl_sub %>%
    dplyr::select(peak_id, adducts) %>%
    tidyr::separate_rows(adducts, sep = ";\\s*") %>%
    dplyr::mutate(
      adduct_type = trimws(sub("\\s*->.*$", "", adducts))
    ) %>%
    dplyr::filter(
      !is.na(adduct_type),
      nzchar(adduct_type),
      adduct_type != "none"
    ) %>%
    dplyr::distinct(peak_id, adduct_type)

  if (nrow(add_long) == 0) {
    return(datatable(data.frame(Note = "No specific adducts annotated.")))
  }

  add_freq <- add_long %>%
    dplyr::count(adduct_type, name = "Total Count") %>%
    dplyr::arrange(dplyr::desc(`Total Count`)) %>%
    dplyr::rename(`Adduct Type` = adduct_type)

  add_freq_show <- limit_table_preview(
    add_freq,
    input$limit_table_preview,
    max_rows = 100,
    max_cols = Inf
  )

  datatable(
    add_freq_show,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      order = list(list(1, "desc"))
    )
  )

}, server = FALSE)

    output$dl_add2_stats_table <- downloadHandler(
  filename = function() {
    paste0(
      tools::file_path_sans_ext(basename(shared$name %||% "dataset")),
      " adduct_stats.csv"
    )
  },

  content = function(file) {

    tbl <- ms_state$add_table
    req(tbl)

    if (!"adducts" %in% names(tbl)) {
      write.csv(
        data.frame(Note = "No adduct annotation column found."),
        file,
        row.names = FALSE
      )
      return()
    }

    tbl_sub <- tbl[
      !is.na(tbl$adducts) &
        tbl$adducts != "none" &
        nzchar(tbl$adducts),
      ,
      drop = FALSE
    ]

    add_long <- tbl_sub %>%
      dplyr::select(peak_id, adducts) %>%
      tidyr::separate_rows(adducts, sep = ";\\s*") %>%
      dplyr::mutate(
        adduct_type = trimws(sub("\\s*->.*$", "", adducts))
      ) %>%
      dplyr::filter(
        !is.na(adduct_type),
        nzchar(adduct_type),
        adduct_type != "none"
      ) %>%
      dplyr::distinct(peak_id, adduct_type)

    add_freq <- add_long %>%
      dplyr::count(adduct_type, name = "Total Count") %>%
      dplyr::arrange(dplyr::desc(`Total Count`)) %>%
      dplyr::rename(`Adduct Type` = adduct_type)

    write.csv(add_freq, file, row.names = FALSE)
  }
)

    output$nl2_table <- renderDT({
  req(input$show_nl_table2)

  if (is.null(ms_state$nl_table) || nrow(ms_state$nl_table) == 0) {
    return(datatable(data.frame(Note = "No neutral loss hits (or skipped).")))
  }

  tbl_show <- limit_table_preview(
    ms_state$nl_table,
    input$limit_table_preview,
    max_rows = 100,
    max_cols = Inf
  )

  datatable(
    tbl_show,
    options = list(
      pageLength = 10,
      scrollX = TRUE
    )
  ) %>%
    DT::formatRound(columns = c("prec_mz", "frag_mz", "delta_mz"), digits = 4) %>%
    DT::formatRound(columns = c("prec_int", "frag_int"), digits = 1) %>%
    DT::formatRound(columns = c("prec_rt", "frag_rt", "delta_rt", "r"), digits = 3) %>%
    formatStyle(
      "prec_status",
      backgroundColor = styleEqual(
        c("kept", "filtered"),
        c("#dff0d8", "#f2dede")
      )
    )
}, server = FALSE)

    output$dl_nl2_table <- downloadHandler(
  filename = function() {
    paste0(tools::file_path_sans_ext(basename(shared$name)), " neutral loses.csv")
  },
  content = function(file) {
    req(ms_state$nl_table)
    write.csv(ms_state$nl_table, file, row.names = FALSE)
  }
)

    output$isf2_table <- renderDT({
  req(input$show_isf_table2)

  if (is.null(ms_state$isf_table) || nrow(ms_state$isf_table) == 0) {
    return(datatable(data.frame(Note = "No fragment pairs/clusters (or skipped).")))
  }

  tbl_show <- limit_table_preview(
  ms_state$isf_table,
  input$limit_table_preview,
  max_rows = 100,
  max_cols = Inf
)

if ("ms2_match_mz" %in% names(tbl_show))
  tbl_show$ms2_match_mz <- round(tbl_show$ms2_match_mz, 4)

if ("ms2_error_da" %in% names(tbl_show))
  tbl_show$ms2_error_da <- round(tbl_show$ms2_error_da, 4)

if ("ms2_rel_int" %in% names(tbl_show))
  tbl_show$ms2_rel_int <- round(tbl_show$ms2_rel_int, 1)

  datatable(
    tbl_show,
    options = list(
      pageLength = 10,
      scrollX = TRUE
    )
  ) %>%
    DT::formatRound(columns = c("prec_mz", "frag_mz", "delta_mz"), digits = 4) %>%
    DT::formatRound(columns = c("prec_int", "frag_int"), digits = 1) %>%
    DT::formatRound(columns = c("prec_rt", "frag_rt", "delta_rt", "frag_to_prec", "r"), digits = 3) %>%
    formatStyle(
      "prec_status",
      backgroundColor = styleEqual(
        c("kept", "filtered"),
        c("#dff0d8", "#f2dede")
      )
    )
}, server = FALSE)

    output$dl_isf2_table <- downloadHandler(
  filename = function() {
    paste0(tools::file_path_sans_ext(basename(shared$name)), " in-source fragments.csv")
  },
  content = function(file) {
    req(ms_state$isf_table)
    write.csv(ms_state$isf_table, file, row.names = FALSE)
  }
)

    output$mis2_table <- renderDT({
  req(input$show_mis_table2)

  tbl <- ms_state$mis_table
  if (is.null(tbl) || nrow(tbl) == 0)
    return(datatable(data.frame(Note = "No mispicked ions clusters identified.")))

  tbl <- tbl[, setdiff(names(tbl), "Rep"), drop = FALSE]
  tbl$mis_group <- suppressWarnings(as.integer(tbl$mis_group))

  tbl_show <- limit_table_preview(
    tbl,
    input$limit_table_preview,
    max_rows = 100,
    max_cols = Inf
  )

  i <- match("mis_group", names(tbl_show))

  datatable(
    tbl_show,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      order = list(list(i, "desc"))
    )
  ) %>%
    DT::formatRound(columns = c("mz"), digits = 4) %>%
    DT::formatRound(columns = c("rt"), digits = 3) %>%
    DT::formatRound(columns = c("intensity"), digits = 1) %>%
    formatStyle(
      "status",
      backgroundColor = styleEqual(
        c("kept", "keep_rep", "filtered"),
        c("#dff0d8", "#dff0d8", "#f2dede")
      )
    )
}, server = FALSE)

    output$dl_mis2_table <- downloadHandler(
  filename = function() {
    paste0(tools::file_path_sans_ext(basename(shared$name)), " mispicked ions.csv")
  },
  content = function(file) {
    tbl <- ms_state$mis_table
    req(tbl)

    tbl <- tbl[, setdiff(names(tbl), "Rep"), drop = FALSE]
    tbl$mis_group <- suppressWarnings(as.integer(tbl$mis_group))

    write.csv(tbl, file, row.names = FALSE)
  }
)

    output$ring2_table <- renderDT({
  req(input$show_ring_table2)

  tbl <- ms_state$ring_table
  if (is.null(tbl) || nrow(tbl) == 0)
    return(datatable(data.frame(Note = "No saturated (ringing) ions identified.")))

  tbl <- tbl[, setdiff(names(tbl), "Rep"), drop = FALSE]
  tbl$ring_group <- suppressWarnings(as.integer(tbl$ring_group))

  tbl_show <- limit_table_preview(
    tbl,
    input$limit_table_preview,
    max_rows = 100,
    max_cols = Inf
  )

  i <- match("ring_group", names(tbl_show))

  datatable(
    tbl_show,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      order = list(list(i, "desc"))
    )
  ) %>%
    DT::formatRound(columns = c("mz"), digits = 4) %>%
    DT::formatRound(columns = c("rt"), digits = 3) %>%
    DT::formatRound(columns = c("intensity"), digits = 1) %>%
    formatStyle(
      "status",
      backgroundColor = styleEqual(
        c("kept", "keep_rep", "filtered"),
        c("#dff0d8", "#dff0d8", "#f2dede")
      )
    )
}, server = FALSE)

    output$dl_ring2_table <- downloadHandler(
  filename = function() {
    paste0(tools::file_path_sans_ext(basename(shared$name)), " saturated ions.csv")
  },
  content = function(file) {
    tbl <- ms_state$ring_table
    req(tbl)

    tbl <- tbl[, setdiff(names(tbl), "Rep"), drop = FALSE]
    tbl$ring_group <- suppressWarnings(as.integer(tbl$ring_group))

    write.csv(tbl, file, row.names = FALSE)
  }
)

    output$ms_filter_summary <- renderUI({
      req(ms_state$applied)
      req(isTRUE(ms_state$show_summary))

      before <- ms_state$stats$before %||% 0L
      after  <- ms_state$stats$after  %||% before

      removed_iso <- ms_state$stats$removed_iso %||% 0L
      removed_add <- ms_state$stats$removed_add %||% 0L
      removed_nl  <- ms_state$stats$removed_nl  %||% 0L
      removed_isf <- ms_state$stats$removed_isf %||% 0L
      removed_mis  <- ms_state$stats$removed_mis  %||% 0L
      removed_ring <- ms_state$stats$removed_ring %||% 0L
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
            "Removed by Mispicked Ions",
            "Removed by Saturated Ions",
            "Total removed",
            "Features after"
          ),
          Value = c(before, removed_iso, removed_add, removed_nl, removed_isf, removed_mis, removed_ring, removed_total, after)
        )
      )
    })

  # --------------------------
  # QC Labels
  # --------------------------
  output$qc_label_upload_warning <- renderUI({
  src <- input$qc_label_source %||% "inherit"

  need_file <- FALSE

  if (identical(src, "from_custom") && is.null(settings_file("qc_meta_csv"))) {
    need_file <- TRUE
  }

  if (identical(src, "from_metadata") && is.null(settings_file("qc_metadata_csv"))) {
    need_file <- TRUE
  }

  if (!need_file) return(NULL)

  div(class="alert alert-warning text-center",
            style="font-size: 18px; font-weight: bold; margin-top: 15px;",
            icon("exclamation-triangle"), " Please upload a CSV file.")
})

  qc_custom_labels <- reactive({
  req(settings_file("qc_meta_csv"), sample_names())

  ext <- tools::file_ext(settings_file("qc_meta_csv")$name)
  shiny::validate(need(ext == "csv", "Upload a .csv for QC labels (one column, no header)."))

  vec <- vroom::vroom(settings_file("qc_meta_csv")$datapath, col_names = FALSE, delim = ",") |> pull(1)

  shiny::validate(
    need(
      length(vec) == length(sample_names()),
      sprintf("QC label count (%d) must match #samples (%d).", length(vec), length(sample_names()))
    )
  )

  as.character(vec)
})

manual_labels_qc <- reactiveVal(NULL)

observeEvent(sample_names(), {
  req(sample_names())

  auto_labs <- labels_from_sample_names(
    sample_names(),
    token_sep = input$qc_token_sep %||% "_",
    token_index = input$qc_label_index %||% 2,
    show_notification = identical(input$tabs, "qc")
  )

  manual_labels_qc(
    make_label_table(sample_names(), auto_labs)
  )
}, ignoreInit = FALSE)

observeEvent(input$apply_auto_labels_qc, {
  req(sample_names())

  auto_labs <- labels_from_sample_names(
    sample_names(),
    token_sep = input$qc_token_sep %||% "_",
    token_index = input$qc_label_index %||% 2,
    show_notification = identical(input$tabs, "qc")
  )

  manual_labels_qc(
    make_label_table(sample_names(), auto_labs)
  )

  showNotification(
    "Editable QC label table was filled from current token labels.",
    type = "message",
    duration = 3
  )
}, ignoreInit = TRUE)

observeEvent(input$qc_labels_table_cell_edit, {
  info <- input$qc_labels_table_cell_edit

  tbl <- manual_labels_qc()
  req(tbl)

  row_i <- as.integer(info$row)

  shiny::validate(
    need(row_i >= 1 && row_i <= nrow(tbl), "Edited row is outside QC label table.")
  )

  tbl$Label[row_i] <- trimws(as.character(info$value))

  manual_labels_qc(tbl)

  showNotification(
    paste0("QC label updated: ", tbl$Sample[row_i], " -> ", tbl$Label[row_i]),
    type = "message",
    duration = 2
  )
}, ignoreInit = TRUE)

qc_metadata_raw <- reactive({
  req(settings_file("qc_metadata_csv"))
  read_metadata_csv(settings_file("qc_metadata_csv"), context = "QC metadata labels")
})


output$qc_metadata_sample_col_ui <- renderUI({
  req(qc_metadata_raw())

  cols <- names(qc_metadata_raw())

  selectInput(
    "qc_metadata_sample_col",
    "Metadata sample-name column:",
    choices = cols,
    selected = guess_metadata_sample_col(cols)
  )
})


output$qc_metadata_label_col_ui <- renderUI({
  req(qc_metadata_raw())

  cols <- names(qc_metadata_raw())
  sample_col <- input$qc_metadata_sample_col %||% guess_metadata_sample_col(cols)
  choices <- setdiff(cols, sample_col)

  shiny::validate(
    need(length(choices) > 0, "Metadata file has no column available for labels.")
  )

  selectizeInput(
    "qc_metadata_label_col",
    "Metadata column to use as Label:",
    choices = choices,
    selected = guess_metadata_label_col(cols, sample_col),
    multiple = FALSE
  )
})


qc_metadata_labels <- reactive({
  req(
    settings_file("qc_metadata_csv"),
    input$qc_metadata_sample_col,
    input$qc_metadata_label_col,
    sample_names()
  )

  metadata_labels_by_sample(
    upload = settings_file("qc_metadata_csv"),
    sample_names = sample_names(),
    sample_col = input$qc_metadata_sample_col,
    label_col = input$qc_metadata_label_col,
    clean_enabled = isTRUE(input$qc_clean_metadata_names),
    remove_suffixes = input$qc_metadata_remove_suffixes %||% character(0),
    context = "QC metadata labels"
  )
})

qc_label_vector <- reactive({
  req(sample_names())

  src <- input$qc_label_source %||% "inherit"

  if (identical(src, "inherit")) {

    req(label_vector_blank())
    label_vector_blank()

  } else if (identical(src, "from_custom")) {

    qc_custom_labels()

  } else if (identical(src, "from_metadata")) {

    qc_metadata_labels()

  } else if (identical(src, "manual")) {

    tbl <- manual_labels_qc()
    req(tbl)

    shiny::validate(
      need(nrow(tbl) == length(sample_names()),
           "Manual QC label table must match the number of samples."),
      need(!any(is.na(tbl$Label) | tbl$Label == ""),
           "All QC samples must have labels.")
    )

    as.character(tbl$Label)

  } else {

    labels_from_sample_names(
      sample_names(),
      token_sep = input$qc_token_sep %||% "_",
      token_index = input$qc_label_index %||% 2
    )
  }
})

output$qc_labels_table <- renderDT({
  req(sample_names(), qc_label_vector())

  src <- input$qc_label_source %||% "inherit"

  tbl <- if (identical(src, "manual")) {
    manual_labels_qc()
  } else {
    make_label_table(sample_names(), qc_label_vector())
  }

  req(tbl)

  datatable(
    tbl,
    editable = if (identical(src, "manual")) {
      list(
        target = "cell",
        disable = list(columns = c(0)) # do not edit Sample column
      )
    } else {
      FALSE
    },
    options = list(
      pageLength = 5,
      scrollX = TRUE,
  ordering = FALSE,
  searching = FALSE
    ),
    rownames = FALSE
  )
}, server = FALSE)

  # --------------------------
  # STEP 3 state: QC Filters
  # --------------------------
  qc_state <- reactiveValues(
    applied = FALSE,
    show_summary = FALSE,
    kept = NULL,
    stats = list(before = 0L, after_final = 0L,
                 removed_zeros = 0L, removed_mean = 0L, removed_rsd = 0L, removed_dratio = 0L, removed_min = 0L, removed_total = 0L)
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
      choices = labs,
      multiple = TRUE,
      selected = intersect(
        isolate(input$value_groups) %||% character(0),
        labs
      ),
      options = list(
        placeholder = "Empty = all groups"
      )
    )
  })

    output$dratio_qc_group_ui <- renderUI({
    req(ds_qc_in())

    labs <- sort(unique(as.character(ds_qc_in()$Label)))
    labs <- labs[!is.na(labs) & nzchar(labs)]

    selected <- isolate(input$dratio_qc_group)

    if (length(selected) != 1L ||
        is.na(selected) ||
        !selected %in% labs) {
      selected <- ""
    }

    selectizeInput(
      "dratio_qc_group",
      "QC group for D-ratio (required):",
      choices = c(
        "Choose a QC group..." = "",
        stats::setNames(labs, labs)
      ),
      selected = selected,
      multiple = FALSE,
      options = list(
        placeholder = "Choose a QC group..."
      )
    )
  })

  qc_plot_visible <- reactiveVal(FALSE)

  observeEvent(input$plot_values_qc, {
    qc_plot_visible(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(
    list(input$reset_qc, input$clear_shared),
    {
      qc_plot_visible(FALSE)
    },
    ignoreInit = TRUE,
    priority = 100
  )

  qc_plot_data <- eventReactive(input$plot_values_qc, {
    req(ds_qc_in())
    ds <- ds_qc_in()
    feats <- numeric_feature_names(ds)
    shiny::validate(need(length(feats) > 0, "No numeric features to plot."))

    filters <- input$value_filters %||% character(0)
    shiny::validate(need(length(filters) > 0, "Select at least one QC value filter to plot."))

    mode <- input$value_mode %||% "group_any"
    labs_sel <- if (!is.null(input$value_groups) && length(input$value_groups) > 0) input$value_groups else unique(ds$Label)
    res_list <- list()

    if ("dratio" %in% filters) {
      problem <- dratio_problem(
        ds,
        input$value_groups,
        input$dratio_qc_group,
        input$dratio_cutoff
      )

      shiny::validate(need(
        is.null(problem),
        if (is.null(problem)) "" else problem
      ))

      res_list[["dratio"]] <- list(
        metric = "dratio",
        mode = mode,
        data = dratio_values(
          ds = ds,
          feats = feats,
          mode = mode,
          value_groups = input$value_groups,
          qc_group = input$dratio_qc_group
        )
      )
    }

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
    res_list[intersect(c("zeros", "mean", "rsd", "dratio", "min"), names(res_list))]
  }, ignoreInit = FALSE, ignoreNULL = TRUE)

  output$qcPlotReady <- reactive({
  if (!isTRUE(qc_plot_visible())) return(FALSE)
  !is.null(qc_plot_data())
})
  outputOptions(output, "qcPlotReady", suspendWhenHidden = FALSE)

  output$qc_value_plot <- renderPlotly({
    req(isTRUE(qc_plot_visible()))
    md_list <- qc_plot_data()

    shiny::validate(
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

      feat_eval <- eval_df %>%
        dplyr::group_by(Feature) %>%
        dplyr::summarise(
          pass = {
            if (md$metric %in% c("rsd", "dratio")) {
              passes <- is.finite(value) & (value <= cutoff)
            } else if (md$metric == "zeros") {
              passes <- value < cutoff
            } else {
              passes <- value >= cutoff
            }

            if (mode == "group_every") {
              all(passes, na.rm = TRUE)
            } else {
              any(passes, na.rm = TRUE)
            }
          },
          .groups = "drop"
        )

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
        layout(
          xaxis = list(title = NULL),
          yaxis = list(title = "Count")
        )

      plots_list[[length(plots_list) + 1]] <- p
    }

    # 5. Assemble Subplots
    if (length(plots_list) == 0) {
      # Catch the empty list before Plotly crashes
      shiny::validate(
        need(FALSE, "Not enough valid data points to generate the plot (e.g., groups may have too few samples to calculate RSD).")
      )
    } else if (length(plots_list) == 1) {
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

    # Validate D-ratio settings before changing results
    if ("dratio" %in% (input$value_filters %||% character(0))) {
      problem <- dratio_problem(
        ds,
        input$value_groups,
        input$dratio_qc_group,
        input$dratio_cutoff
      )

      if (!is.null(problem)) {
        showNotification(
          problem,
          type = "error",
          duration = 6
        )
        return(invisible(FALSE))
      }
    }

    qc_state$stats$before <- length(feats)

    if (!length(input$value_filters %||% character(0))) {
      keep <- feats
      qc_state$stats$removed_zeros <- 0L
      qc_state$stats$removed_mean  <- 0L
      qc_state$stats$removed_rsd   <- 0L
      qc_state$stats$removed_dratio <- 0L
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
        min_cutoff = input$min_cutoff,
        dratio_qc_group = input$dratio_qc_group,
        dratio_cutoff = input$dratio_cutoff
      )
      keep <- res$keep
      qc_state$stats$removed_zeros <- res$stats$removed_zeros
      qc_state$stats$removed_mean  <- res$stats$removed_mean
      qc_state$stats$removed_rsd   <- res$stats$removed_rsd
      qc_state$stats$removed_dratio <- res$stats$removed_dratio
      qc_state$stats$removed_min   <- res$stats$removed_min
      qc_state$stats$after_final   <- res$stats$after_final
      qc_state$stats$removed_total <- res$stats$removed_total
    }

    qc_state$applied <- TRUE
    qc_state$show_summary <- TRUE
    qc_state$kept <- keep
    settings_capture("qc", qc_state$stats)

    if (length(input$value_filters %||% character(0)) == 0) {
      showNotification("No QC Filters selected. Step 3 frozen as pass-through.", type="message", duration=4)
    } else if (qc_state$stats$after_final == 0) {
      showNotification("QC Filters removed all features. Try relaxing cutoffs.", type="error", duration=5)
    } else {
      showNotification(sprintf("Step 3 frozen: kept %d of %d features.", qc_state$stats$after_final, qc_state$stats$before), type="message", duration=4)
    }
  invisible(TRUE)}

  observeEvent(input$apply_qc, {
    if (!settings_ready(stage = "qc")) return()
    w <- Waiter$new(
      id = "qc_table_out",
      html = tagList(spin_6(), h4("Applying QC Filters...", style="color:#ffffff !important; text-shadow:none !important;")),
      color = "rgba(44, 62, 80, 0.8)"
    )
    w$show()

    shinyjs::disable("apply_qc")

        shinyjs::delay(50, {
      tryCatch(
        {
          if (isTRUE(run_step3_qc())) {
            invalidate_after_qc()
          }
        },
        error = function(e) {
          showNotification(
            conditionMessage(e),
            type = "error",
            duration = 8
          )
        },
        finally = {
          w$hide()
          shinyjs::enable("apply_qc")
        }
      )
    })
  }, ignoreInit = TRUE)

  observeEvent(input$reset_qc, {
    qc_state$applied <- FALSE
    qc_state$show_summary <- FALSE
    qc_state$kept <- NULL
    qc_state$stats <- list(before = 0L, after_final = 0L,
                           removed_zeros = 0L, removed_mean = 0L, removed_rsd = 0L, removed_dratio = 0L, removed_min = 0L, removed_total = 0L)

    # Clear QC labels uploads
  shinyjs::reset("qc_meta_csv")
  shinyjs::reset("qc_metadata_csv")
  shinyjs::reset("qc_metadata_sample_col")
  shinyjs::reset("qc_metadata_label_col")
  shinyjs::reset("qc_metadata_remove_suffixes")

  updateRadioButtons(session, "qc_label_source", selected = "inherit")
  updatePrettyCheckbox(session, "qc_clean_metadata_names", value = FALSE)
  updateSelectizeInput(session, "dratio_qc_group", selected = "")
  updateNumericInput(session, "dratio_cutoff", value = 50)
  shinyWidgets::updateCheckboxGroupButtons(
      session, "value_filters", selected = setdiff(input$value_filters %||% character(0), "dratio"))

  invalidate_after_qc()
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
  output$qc_table_in <- renderDT({
  req(raw_after_ms())
  datatable(
    limit_table_preview(raw_after_ms(), input$limit_table_preview, 50, 50),
    options = list(scrollX = TRUE, pageLength = 5)
  )
})

  output$qc_header_out <- renderUI({ req(raw_after_qc()); h3("Output table — after QC Filters") })
  output$qc_table_out <- renderDT({
  req(raw_after_qc())
  datatable(
    limit_table_preview(raw_after_qc(), input$limit_table_preview, 50, 50),
    options = list(scrollX = TRUE, pageLength = 5)
  )
})

  output$qc_filter_summary <- renderUI({
    req(qc_state$applied)
    req(isTRUE(qc_state$show_summary))

    before        <- qc_state$stats$before        %||% 0L
    after_final   <- qc_state$stats$after_final   %||% before
    removed_zeros <- qc_state$stats$removed_zeros %||% 0L
    removed_mean  <- qc_state$stats$removed_mean  %||% 0L
    removed_rsd   <- qc_state$stats$removed_rsd   %||% 0L
    removed_dratio <- qc_state$stats$removed_dratio %||% 0L
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
          "Removed by D-ratio",
          "Removed by Min",
          "Total removed",
          "Features after"
        ),
        Value  = c(before, removed_zeros, removed_mean, removed_rsd, removed_dratio, removed_min, removed_total, after_final)
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
  target_matches = NULL,
  stats = list(
    before = 0L,
    after = 0L,
    removed_mz = 0L,
    removed_rt = 0L,
    removed_rmd = 0L,
    removed_amd = 0L,
    removed_target = 0L,
    matched_target = 0L,
    target_action = "remove",
    removed_total = 0L
  )
)

  target_peak_cleared <- reactiveVal(FALSE)
  target_peak_csv_cache <- reactiveVal(empty_peak_targets())

  # Align raw table to current pipeline input (blank + MS deletions + QC kept)
  peak_table_in_raw <- reactive({
    req(raw_zeroed(), sample_cols0())

    blank_keep <- if (isTRUE(blank_state$applied)) blank_state$kept else NULL

    del_ms <- character(0)
    if (isTRUE(ms_state$applied)) {
      del_ms <- unique(c(
        ms_state$del_iso %||% character(0),
        ms_state$del_add %||% character(0),
        ms_state$del_nl  %||% character(0),
        ms_state$del_isf %||% character(0),
        ms_state$del_mis  %||% character(0),
        ms_state$del_ring %||% character(0)
      ))
    }
    final_keep <- if (isTRUE(qc_state$applied)) qc_state$kept else NULL

    build_final_feature_table(
    raw_df_fid  = raw_zeroed(),
    sample_cols = sample_cols0(),
    blank_keep = blank_keep,
    merge_map   = ms_state$mis_merge_map,
    del_ms     = del_ms,
    final_keep = final_keep
    )
  })

  output$peak_header_in <- renderUI({ req(peak_table_in_raw()); h3("Input table") })
  output$peak_table_in <- renderDT({
  req(peak_table_in_raw())
  datatable(
    limit_table_preview(peak_table_in_raw(), input$limit_table_preview, 50, 50),
    options = list(scrollX = TRUE, pageLength = 5)
  )
})

  peak_plot_data <- eventReactive(input$plot_peak, {
    req(peak_table_in_raw(), input$mz_col0, input$rt_col0)
    sel <- input$peak_filters %||% character(0)
    shiny::validate(need(length(sel) > 0, "Select at least one metric to plot (m/z, rt, RMD, AMD)."))

    df <- as.data.frame(peak_table_in_raw(), check.names = FALSE)

    shiny::validate(
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
          fail_rt <- fail_rt | (dd$RT < rtmin)
        }
        if (isTRUE(input$rt_max_enable)) {
          rtmax <- input$peak_rt_max %||% 30
          p_mzrt <- p_mzrt + geom_vline(xintercept = rtmax, color = "red", linetype = "dashed", linewidth = 1)
          fail_rt <- fail_rt | (dd$RT > rtmax)
        }
      }
      if ("mz" %in% sel) {
        if (isTRUE(input$mz_min_enable)) {
          mzmin <- input$peak_mz_min %||% 200
          p_mzrt <- p_mzrt + geom_hline(yintercept = mzmin, color = "red", linetype = "dashed", linewidth = 1)
          fail_mz <- fail_mz | (dd$mz < mzmin)
        }
        if (isTRUE(input$mz_max_enable)) {
          mzmax <- input$peak_mz_max %||% 1500
          p_mzrt <- p_mzrt + geom_hline(yintercept = mzmax, color = "red", linetype = "dashed", linewidth = 1)
          fail_mz <- fail_mz | (dd$mz > mzmax)
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

      plots[["mzrt"]] <- ggplotly(p_mzrt) %>% plotly::toWebGL() %>% layout(annotations = list(make_annot(annot_text)))
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
          fail_rmd <- fail_rmd | (dd2$RMD < rmin)
        }
        if (isTRUE(input$rmd_max_enable)) {
          rmax <- input$peak_rmd_max %||% 2000
          p_rmd <- p_rmd + geom_hline(yintercept = rmax, color = "red", linetype = "dashed", linewidth = 1)
          fail_rmd <- fail_rmd | (dd2$RMD > rmax)
        }

        rem_rmd <- sum(fail_rmd, na.rm = TRUE)
        annot_text <- paste0("<b>Filtered RMD: ", rem_rmd, "</b>")

        plots[["rmd"]] <- ggplotly(p_rmd) %>%  plotly::toWebGL() %>% layout(annotations = list(make_annot(annot_text)))
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
          fail_amd <- fail_amd | (dd3$AMD < amin)
        }
        if (isTRUE(input$amd_max_enable)) {
          amax <- input$peak_amd_max %||% 0.50
          p_amd <- p_amd + geom_hline(yintercept = amax, color = "red", linetype = "dashed", linewidth = 1)
          fail_amd <- fail_amd | (dd3$AMD > amax)
        }

        rem_amd <- sum(fail_amd, na.rm = TRUE)
        annot_text <- paste0("<b>Filtered AMD: ", rem_amd, "</b>")

        plots[["amd"]] <- ggplotly(p_amd) %>% plotly::toWebGL() %>% layout(annotations = list(make_annot(annot_text)))
      }
    }

    shiny::validate(need(length(plots) > 0, "Nothing to plot for the current selection."))

    if (length(plots) == 1) {
      plots[[1]]
    } else {
      # Use Plotly's subplot feature to combine the graphs dynamically
      subplot(plots, nrows = length(plots), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.08) %>%
        layout(title = "")
    }
  })

 target_peak_list <- reactive({
  if (isTRUE(target_peak_cleared())) {
    return(empty_peak_targets())
  }

  typed_targets <- read_peak_targets(
    text_input = input$target_peak_text,
    upload_input = NULL
  )

  csv_targets <- target_peak_csv_cache()

  out <- dplyr::bind_rows(
    typed_targets,
    csv_targets
  )

  if (!nrow(out)) return(empty_peak_targets())

  out %>%
    dplyr::distinct(target_mz, target_rt, .keep_all = TRUE)
})

 observeEvent(input$target_peak_text, {
  if (nzchar(trimws(input$target_peak_text %||% ""))) {
    target_peak_cleared(FALSE)
  }
}, ignoreInit = TRUE)

observeEvent(settings_file("target_peak_csv"), {
  if (!is.null(settings_file("target_peak_csv")) && !is.null(settings_file("target_peak_csv")$datapath)) {

    csv_df <- read_target_csv_smart(settings_file("target_peak_csv")$datapath)
    target_peak_csv_cache(
      extract_peak_targets(csv_df, source = "csv")
    )

    target_peak_cleared(FALSE)
  }
}, ignoreInit = TRUE)

target_peak_matches_live <- reactive({
  req(peak_table_in_raw(), input$mz_col0, input$rt_col0)

  df <- as.data.frame(peak_table_in_raw(), check.names = FALSE)
  rid <- if (".FID" %in% names(df)) ".FID" else names(df)[1]

  find_target_peak_matches(
    df = df,
    rid_col = rid,
    mz_col = input$mz_col0,
    rt_col = input$rt_col0,
    targets = target_peak_list(),
    mz_tol_type = input$target_mz_tol_type %||% "da",
    mz_tol_da = input$target_mz_tol_da %||% 0.005,
    mz_tol_ppm = input$target_mz_tol_ppm %||% 10,
    rt_tol = input$target_rt_tol %||% 0.05
  )
})

output$target_peak_match_table <- renderDT({
  req("target" %in% (input$peak_filters %||% character(0)))

  mm <- target_peak_matches_live()

  shiny::validate(
    need(nrow(target_peak_list()) > 0, "No target m/z and rt values provided."),
    need(nrow(mm) > 0, "No matching peaks found with current tolerances.")
  )

  tbl <- datatable(
    mm,
    rownames = FALSE,
    width = "100%",
    options = list(
      scrollX = F,
      #scrollY = "170px",
      scrollCollapse = FALSE,
      pageLength = 5,
      autoWidth = F
    )
  )

  mz_cols <- intersect(
    c("target_mz", "mz", "delta_mz", "mz_tol_used"),
    names(mm)
  )

  rt_cols <- intersect(
    c("target_rt", "rt", "delta_rt", "rt_tol_used"),
    names(mm)
  )

  ppm_cols <- intersect(
    c("delta_mz_ppm"),
    names(mm)
  )

  if (length(mz_cols)) {
    tbl <- DT::formatRound(tbl, columns = mz_cols, digits = 4)
  }

  if (length(rt_cols)) {
    tbl <- DT::formatRound(tbl, columns = rt_cols, digits = 3)
  }

  if (length(ppm_cols)) {
    tbl <- DT::formatRound(tbl, columns = ppm_cols, digits = 2)
  }

  tbl
})

  run_step4_peak <- function() {
    req(peak_table_in_raw(), input$mz_col0, input$rt_col0)

    df <- as.data.frame(peak_table_in_raw(), check.names = FALSE)

    # IMPORTANT: use .FID (internal feature ID) so it matches matrix colnames later
    rid <- if (".FID" %in% names(df)) ".FID" else names(df)[1]

    shiny::validate(
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
    target_hit <- rep(FALSE, before)
    fail_target <- rep(FALSE, before)
    target_matches <- empty_peak_matches()
    target_action <- input$target_peak_action %||% "remove"
    target_skipped_empty <- FALSE

    if ("mz" %in% sel) {
      mz_pass_min <- rep(TRUE, before)
      mz_pass_max <- rep(TRUE, before)

      if (isTRUE(input$mz_min_enable)) {
        mzmin <- suppressWarnings(as.numeric(input$peak_mz_min))
        shiny::validate(need(is.finite(mzmin), "m/z min cutoff must be numeric."))
        mz_pass_min <- is.finite(mzv) & (mzv >= mzmin)
      }
      if (isTRUE(input$mz_max_enable)) {
        mzmax <- suppressWarnings(as.numeric(input$peak_mz_max))
        shiny::validate(need(is.finite(mzmax), "m/z max cutoff must be numeric."))
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
        shiny::validate(need(is.finite(rtmin), "rt min cutoff must be numeric."))
        rt_pass_min <- is.finite(rtv) & (rtv >= rtmin)
      }
      if (isTRUE(input$rt_max_enable)) {
        rtmax <- suppressWarnings(as.numeric(input$peak_rt_max))
        shiny::validate(need(is.finite(rtmax), "rt max cutoff must be numeric."))
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
        shiny::validate(need(is.finite(rmin), "RMD min bound must be numeric."))
        rmd_pass_min <- is.finite(rmd_vals) & (rmd_vals >= rmin)
      }
      if (isTRUE(input$rmd_max_enable)) {
        rmax <- suppressWarnings(as.numeric(input$peak_rmd_max))
        shiny::validate(need(is.finite(rmax), "RMD max bound must be numeric."))
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
        shiny::validate(need(is.finite(amin), "AMD min bound must be numeric."))
        amd_pass_min <- is.finite(amd_vals) & (amd_vals >= amin)
      }
      if (isTRUE(input$amd_max_enable)) {
        amax <- suppressWarnings(as.numeric(input$peak_amd_max))
        shiny::validate(need(is.finite(amax), "AMD max bound must be numeric."))
        amd_pass_max <- is.finite(amd_vals) & (amd_vals <= amax)
      }

      amd_pass <- amd_pass_min & amd_pass_max
      fail_amd <- !amd_pass
    }

if ("target" %in% sel) {

  targets_now <- target_peak_list()

  if (nrow(targets_now) == 0) {

    # Target filter was selected, but no list was provided.
    # Treat it as no-op / pass-through, not as an error.
    target_skipped_empty <- TRUE
    target_matches <- empty_peak_matches()
    target_hit <- rep(FALSE, before)
    fail_target <- rep(FALSE, before)

  } else {

    target_matches <- target_peak_matches_live()
    target_hit <- as.character(df[[rid]]) %in% unique(target_matches$Feature)
    target_action <- input$target_peak_action %||% "remove"

    if (identical(target_action, "keep")) {

      shiny::validate(
        need(
          nrow(target_matches) > 0,
          "Target peak keep-only mode selected, but no matching peaks were found with current tolerances."
        )
      )

      # Keep matched target peaks, remove everything else
      fail_target <- !target_hit

    } else {

      # Remove matched target peaks
      fail_target <- target_hit

      if (nrow(target_matches) == 0) {
        showNotification(
          "Target peak removal selected, but no matching peaks were found with current tolerances.",
          type = "warning",
          duration = 5
        )
      }
    }
  }
}

    keep_idx <- if (length(sel) == 0) {
        rep(TRUE, before)
      } else {
        (mz_pass & rt_pass & rmd_pass & amd_pass) & !fail_target
      }
    ids_kept <- as.character(df[[rid]][keep_idx])

    after <- sum(keep_idx)
    peak_state$stats$after <- after
    peak_state$stats$removed_mz  <- sum(fail_mz)
    peak_state$stats$removed_rt  <- sum(fail_rt)
    peak_state$stats$removed_rmd <- sum(fail_rmd)
    peak_state$stats$removed_amd <- sum(fail_amd)
    peak_state$stats$removed_target <- sum(fail_target)
    peak_state$stats$matched_target <- sum(target_hit)
    peak_state$stats$target_action <- target_action
    peak_state$target_matches <- target_matches
    peak_state$stats$removed_total <- before - after

    peak_state$applied <- TRUE
    peak_state$show_summary <- TRUE
    peak_state$kept <- ids_kept
    settings_capture("peak", peak_state$stats)

    effective_sel <- sel
      if (isTRUE(target_skipped_empty)) {
        effective_sel <- setdiff(effective_sel, "target")
      }

      if (length(effective_sel) == 0) {
        showNotification("No effective Peak Filters selected. Step 4 frozen as pass-through.", type="message", duration=4)
      } else if (length(ids_kept) == 0) {
        showNotification("Peak Filters removed all features. Try relaxing cutoffs.", type="error", duration=5)
      } else {
        showNotification(sprintf("Step 4 frozen: kept %d of %d features.", after, before), type="message", duration=4)
      }
  }

  observeEvent(input$apply_peak, {
    if (!settings_ready(stage = "peak")) return()
      w <- Waiter$new(
        id = "peak_table_out",
        html = tagList(spin_6(), h4("Applying Peak Filters...", style="color:#ffffff !important; text-shadow:none !important;")),
        color = "rgba(44, 62, 80, 0.8)"
      )
      w$show()

      shinyjs::disable("apply_peak")

      shinyjs::delay(50, {
        tryCatch({
          run_step4_peak()
          invalidate_after_peak()
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
    peak_state$target_matches <- empty_peak_matches()
    target_peak_csv_cache(empty_peak_targets())
    target_peak_cleared(TRUE)

    shinyjs::reset("target_peak_csv")
    updateTextAreaInput(session, "target_peak_text", value = "")
    updateRadioButtons(session, "target_peak_action", selected = "remove")

    peak_state$stats <- list(before=0L, after=0L,
                     removed_mz=0L, removed_rt=0L,
                     removed_rmd=0L, removed_amd=0L,
                     removed_target=0L,
                     matched_target=0L,
                     target_action="remove",
                     removed_total=0L)
    invalidate_after_peak()
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
  output$peak_table_out <- renderDT({
  req(peak_table_out_raw())
  datatable(
    limit_table_preview(peak_table_out_raw(), input$limit_table_preview, 50, 50),
    options = list(scrollX = TRUE, pageLength = 5)
  )
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
    removed_target <- peak_state$stats$removed_target %||% 0L
    matched_target <- peak_state$stats$matched_target %||% 0L
    target_action <- peak_state$stats$target_action %||% "remove"

    target_metric_label <- if (identical(target_action, "keep")) {
      "Removed because not in target list"
    } else {
      "Removed by target m/z + rt"
    }
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
            "Matched target peaks",
            target_metric_label,
            "Total removed",
            "Features after"
          ),
          Value = c(
            before,
            removed_mz,
            removed_rt,
            removed_rmd,
            removed_amd,
            matched_target,
            removed_target,
            removed_total,
            after
          )
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

  # --------------------------
# Pipeline invalidation helpers
# --------------------------

invalidate_final <- function() {
  final_table(NULL)

  # A previously filtered final MGF is also no longer valid
  filtered_mgf_sps(NULL)
  mgf_processing(FALSE)
  mgf_attempted(FALSE)
}

invalidate_after_peak <- function() {
  invalidate_final()
}

invalidate_after_qc <- function() {
  peak_state$applied <- FALSE
  peak_state$show_summary <- FALSE
  peak_state$kept <- NULL
  peak_state$target_matches <- empty_peak_matches()

  peak_state$stats <- list(
    before = 0L, after = 0L,
    removed_mz = 0L, removed_rt = 0L,
    removed_rmd = 0L, removed_amd = 0L,
    removed_target = 0L, matched_target = 0L,
    target_action = "remove", removed_total = 0L
  )

  invalidate_final()
}

invalidate_after_ms <- function() {
  qc_state$applied <- FALSE
  qc_state$show_summary <- FALSE
  qc_state$kept <- NULL
  qc_state$stats <- list(
    before = 0L, after_final = 0L,
    removed_zeros = 0L, removed_mean = 0L,
    removed_rsd = 0L, removed_dratio = 0L, removed_min = 0L,
    removed_total = 0L
  )

  invalidate_after_qc()
}

invalidate_after_blank <- function() {
  ms_state$applied <- FALSE
  ms_state$show_summary <- FALSE
  ms_state$matrix <- NULL
  ms_state$isf_status <- NULL

  ms_state$del_iso  <- character(0)
  ms_state$del_add  <- character(0)
  ms_state$del_nl   <- character(0)
  ms_state$del_isf  <- character(0)
  ms_state$del_mis  <- character(0)
  ms_state$del_ring <- character(0)

  ms_state$mis_merge_map <- NULL
  ms_state$iso_table  <- NULL
  ms_state$add_table  <- NULL
  ms_state$nl_table   <- NULL
  ms_state$isf_table  <- NULL
  ms_state$mis_table  <- NULL
  ms_state$ring_table <- NULL

  ms_state$stats <- list(
    before = 0L, after = 0L,
    removed_iso = 0L, removed_add = 0L,
    removed_nl = 0L, removed_isf = 0L,
    removed_mis = 0L, removed_ring = 0L
  )

  invalidate_after_ms()
}

invalidate_all_pipeline <- function() {
  blank_state$applied <- FALSE
  blank_state$show_summary <- FALSE
  blank_state$kept <- NULL
  blank_state$stats <- list(
    before = 0L, after = 0L, removed_blank = 0L
  )

  invalidate_after_blank()
}

  output$finalReady <- reactive({ !is.null(final_table()) })
  outputOptions(output, "finalReady", suspendWhenHidden = FALSE)

  mat_peak_out <- reactive({
  req(mat_qc_out())
  ds <- as.data.frame(mat_qc_out())

  if (isTRUE(peak_state$applied) && !is.null(peak_state$kept)) {
    keep <- intersect(colnames(ds), peak_state$kept)
    ds[, keep, drop = FALSE]
  } else {
    ds
  }
})

  observeEvent(input$compile_final, {
    if (!settings_ready(stage = "peak")) return()
    w <- Waiter$new(
    html = tagList(spin_6(), h4("Compiling Final Output...", style="color:#ffffff !important; text-shadow:none !important;")),
    color = "rgba(44, 62, 80, 0.8)"
    )
    w$show()

  shinyjs::disable("compile_final")
  shinyjs::delay(50, {
    tryCatch({
      req(raw_zeroed(), sample_cols0())

      blank_keep <- if (isTRUE(blank_state$applied)) blank_state$kept else NULL

      del_ms <- character(0)
      if (isTRUE(ms_state$applied)) {
        del_ms <- unique(c(
          ms_state$del_iso %||% character(0),
          ms_state$del_add %||% character(0),
          ms_state$del_nl  %||% character(0),
          ms_state$del_isf %||% character(0),
          ms_state$del_mis  %||% character(0),
          ms_state$del_ring %||% character(0)
        ))
      }

      # final_keep: QC kept intersect Peak kept (if both exist)
      final_keep <- NULL
      if (isTRUE(qc_state$applied) && !is.null(qc_state$kept)) {
        final_keep <- qc_state$kept
      }

      if (isTRUE(peak_state$applied) && !is.null(peak_state$kept)) {
        final_keep <- if (is.null(final_keep)) {
          peak_state$kept
        } else {
          intersect(final_keep, peak_state$kept)
        }
      }

      # 1) build internal FINAL table (keeps .FID)
      ftbl <- build_final_feature_table(
        raw_df_fid       = raw_zeroed(),
        sample_cols = sample_cols0(),
        blank_keep       = blank_keep,
        merge_map   = ms_state$mis_merge_map,
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

      # 3) store
      settings_capture("final", list(features = nrow(ftbl_export)))
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

  initial_n <- tryCatch({
    df0 <- raw_zeroed()
    if (is.null(df0)) {
      NA_integer_
    } else if (".FID" %in% names(df0)) {
      length(unique(df0$.FID))
    } else {
      nrow(df0)
    }
  }, error = function(e) NA_integer_)

  final_n <- if (!is.null(final_table())) {
    nrow(final_table())
  } else {
    NA_integer_
  }

  diff_n <- if (is.finite(initial_n) && is.finite(final_n)) {
    initial_n - final_n
  } else {
    NA_integer_
  }

  diff_pct <- if (is.finite(initial_n) && initial_n > 0 && is.finite(diff_n)) {
    round(100 * diff_n / initial_n, 2)
  } else {
    NA_real_
  }

  final_txt <- if (is.finite(final_n)) final_n else "not compiled"
  diff_txt <- if (is.finite(diff_n)) {
    paste0(diff_n, " (", diff_pct, "%)")
  } else {
    "not compiled"
  }

  div(
    style = "margin:.5rem 0 1rem; padding:.5rem .75rem; border:1px solid #dfe6e9; border-radius:8px; background:#ffffffaa;",
    tags$ul(
      tags$li(sprintf("Dataset: %s", shared$name %||% "not uploaded")),
      tags$li(sprintf("Step 1 Blank Filters: %s", if (isTRUE(blank_state$applied)) "APPLIED" else "not applied")),
      tags$li(sprintf("Step 2 MS Filters: %s", if (isTRUE(ms_state$applied)) "APPLIED" else "not applied")),
      tags$li(sprintf("Step 3 QC Filters: %s", if (isTRUE(qc_state$applied)) "APPLIED" else "not applied")),
      tags$li(sprintf("Step 4 Peak Filters: %s", if (isTRUE(peak_state$applied)) "APPLIED" else "not applied")),
      tags$li(sprintf("Raw features: %s", initial_n)),
      tags$li(sprintf("Filtered features: %s", diff_txt)),
      tags$li(sprintf("Final features: %s", final_txt))
    )
  )
})

  output$final_preview_table <- renderDT({
  req(final_table())
  datatable(
    limit_table_preview(final_table(), input$limit_table_preview, 50, 50),
    options = list(scrollX = TRUE, pageLength = 5)
  )
})

  # --------------------------
  # Downloads
  # --------------------------

  observe({
    shinyjs::toggleState("dl_final_table", condition = !is.null(final_table()))
  })

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
      msdial_preamble = shared$msdial_preamble,
      msdial_export_names  = shared$msdial_export_names
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
    label = "Enable Filter MGF",
    value = FALSE,
    status = "danger",
    width = "auto"
  )
              ),
              actionButton(
                inputId = "btnmgf",
                label = "?",
                class = "btn-primary btn-xs",
                style = "position: absolute; top: 0px; left: 185px; border-radius: 50%; width: 22px; height: 22px; padding: 0; line-height: 1; font-size: 12px;"
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
                title = "<b>Filtering MGF file based on final filtered table</b><br><br>Upload the MGF file corresponding to the peak table, then specify the matching Feature ID fields<br><br>Feature/Peak ID in MGF should match with selected Feature ID in peak table. We recommend using the default columns (see details in About Tab)",
                placement = "right",
                trigger = "click",
                options = list(container = "body", html = TRUE)
              )
            )
  })

  # 1. READ ONCE ON UPLOAD
  observeEvent(input$mgf_input, {
  req(input$mgf_input)

  load_mgf_file(
    upload = input$mgf_input,
    input_id = "mgf_input"
  )
}, ignoreInit = TRUE)

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
  if (!settings_ready(TRUE)) return()
  sps <- parsed_mgf()

  if (is.null(sps) || length(sps) == 0) {
    showNotification(
      "Please upload an MGF file first!",
      type = "error",
      duration = 5
    )
    return()
  }

  req(
    final_table(),
    input$mgf_match_col,
    input$mgf_id_field
  )

  filtered_mgf_sps(NULL)
  mgf_processing(TRUE)
  mgf_attempted(TRUE)

  shinyjs::disable("run_mgf_filter")

}, ignoreInit = TRUE)

  output$final_mgf_loaded_ui <- renderUI({
  sps <- parsed_mgf()
  if (is.null(sps) || length(sps) == 0) return(NULL)

  div(
    style = "padding:8px; margin-bottom:10px; background:#dff0d8; color:#3c763d; border-radius:5px;",
    tags$b("MGF already loaded: "),
    current_mgf_name() %||% "MGF file",
    sprintf(" (%d spectra). Re-upload only to replace it.", length(sps))
  )
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
        sel_f <- trimws(as.character(final_table()[[input$mgf_match_col]]))
        id_field <- input$mgf_id_field
        mgf_ids <- trimws(as.character(Spectra::spectraData(sps)[[id_field]]))

        sel_ok <- !is.na(sel_f) & nzchar(sel_f)
        mgf_ok <- !is.na(mgf_ids) & nzchar(mgf_ids)

        # Exact ID matching
        idx_exact <- which(mgf_ok & mgf_ids %in% sel_f[sel_ok])

        # Numeric-equivalent matching, e.g. "12" vs "12.0"
        mgf_num <- suppressWarnings(as.numeric(mgf_ids))
        sel_num <- suppressWarnings(as.numeric(sel_f[sel_ok]))
        sel_num <- sel_num[is.finite(sel_num)]

        idx_num <- if (length(sel_num)) {
          which(is.finite(mgf_num) & mgf_num %in% sel_num)
        } else {
          integer(0)
        }

        # Keep both exact and numeric-equivalent matches
        idx <- sort(unique(c(idx_exact, idx_num)))

        if(length(idx) > 0) {
          res <- sps[idx]
          settings_capture("mgf", list(spectra = length(res)))
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
  shinyjs::reset("isf_mgf_input")

  current_mgf(NULL)
  current_mgf_name(NULL)
  parsed_mgf(NULL)
  mgf_true_names(NULL)

  filtered_mgf_sps(NULL)
  mgf_processing(FALSE)
  mgf_attempted(FALSE)

  gc()

  showNotification(
    "MGF file and filtered data cleared from memory.",
    type = "warning",
    duration = 4
  )

}, ignoreInit = TRUE)

  observe({
    sps <- filtered_mgf_sps()
    is_ready <- !is.null(sps) && length(sps) > 0
    shinyjs::toggleState("dl_mgf", condition = is_ready)
  })

  output$dl_mgf <- downloadHandler(
    filename = function() {
    nm <- current_mgf_name() %||% "spectra.mgf"
    paste0(tools::file_path_sans_ext(basename(nm)), "_filtered.mgf")},
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
      h3("Quick Start"),
      tags$ol(
        tags$li(tags$b("Upload Data:"), " Upload your CSV, choose correct sample column names keywords (identifiers) + mz & rt columns."),
        tags$li(tags$b("Blank Filters:"), " Define Blank group, set Blank filter -> Apply."),
        tags$li(tags$b("MS Filters:"), " Enable Deleting Isotopes/Adducts/Neutral Loses/Fragments/Mispicked/Saturated -> Apply."),
        tags$li(tags$b("QC Filters:"), " Define groups, choose Zero/RSD/D-ratio/Mean/Min filters -> Apply."),
        tags$li(tags$b("Peak Filters:"), " Choose mz/rt/RMD/AMD cutoffs -> Apply. "),
        tags$li(tags$b("Final Summary:"), " Compile summary & export final dataset."),
        tags$li(tags$b("About:"), " Description, Project Details, References.")
      ),
      div(
  style = "background-color: #fff3e0; border-left: 5px solid #FF8C00; padding: 10px 12px; margin-top: 8px; margin-bottom: 8px; border-radius: 6px; font-size: 18px; line-height: 1.45;",
  HTML("
    <span style='color:#d96f00; font-weight:700; font-size:18px;'>
      <i class='fa fa-server'></i> Memory limit
    </span><br>
    The Web version of <i>MetaboCensoR</i> is limited to <b>1 GB</b> of memory.
    For large datasets or memory-intensive analyses, we recommend activating the <b>Limit table previews</b> option and running <i>MetaboCensoR</i> locally.<br>
    <a href='https://github.com/plyush1993/MetaboCensoR#launch-the-app-rocket'
       target='_blank' style='font-weight:700; color:#d96f00;'>
       Local installation instructions
    </a>
  ")
),
br(),
      div(class = "highlight",
      "Quick Start Guide: ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/blob/main/MetaboCensoR_tutorial.pdf", "Tutorial", target = "_blank"),
      "."
      ),
      br(),
      div(class="highlight", "Rule & Tip: Filters are applied sequentially. Changes made on later tabs do not affect the results of previous tabs. To change the order of applied filters (e.g., 'Peak Filters' before 'Blank Filters'), run the application sequentially several times, using the output from one run as the input for the next."),
      br(),
      div(class="highlight", "Rule: The last 'Apply' click is saved."),
      br(),
      div(class="highlight", "Rule: Output Tables reflect the last 'Apply' run."),
      br(),
      div(class="highlight", "Rule: Click 'Clear' to skip any previous calculation in the tab."),
      br(),
      div(class="highlight", "Rule: Click the question mark icon to toggle the information tooltip."),
      br(),
      div(class="highlight", "Note: We recommend to apply QC Filters (except zeros) after drift/batch correction, normalization, and imputation for large-scale metabolomics study, while it might make sense to apply all other filters before them."),
      br(),
      div(class="highlight", "Rule: Plots are displayed and updated only after clicking the plot buttons, cutoff value on them is updated dynamically."),
      br(),
      div(class="highlight", "Rule: MS filters are always applied sequentially. Start with Isotopes, then Adducts, Neutral Loses, In-Source Fragments, Mispicked Ions, and Saturated Ions. We recommend to keep the order to avoid misannotation."),
      br(),
      div(class="highlight", "Rule: Isotopes-Dimers/Adducts/Neutral Loses/In-Source Fragments/Mispicked/Saturated ions tables are displayed after activating the checkbox."),
      br(),
      tags$img(
        src = "www/Server_Map.png",
        width = "1000px",
        height = "520px",
        style = "display: block; margin: 0 auto 20px auto;"
      )
      ))
  }

  if (sec == "upload") {
    return(div(
      h3("Upload Data"),
      tags$ul(
        tags$li(tags$b("Upload Peak Table (csv):"), " choose peak table type and then select specified columns."),
        tags$li(tags$b("Check preview:"), " to confirm that sample columns were detected correctly."),
        tags$li(tags$b("Row ID column:"), " pick the column that uniquely identifies each feature."),
        tags$li(tags$b("mz / rt columns:"), " define columns with m/z & rt values."),
        tags$li(tags$b("Sample column keywords:"), " add patterns that match your sample columns (e.g. .mzML, _Area)."),
        tags$li(tags$b("Clear dataset:"), " to delete any calculations and uploaded files."),
        tags$li(tags$b("Import analysis setting (.json):"), " upload .json file with settings to restore filter switches, thresholds, evaluation modes, compatible column/group selections, manual labels, and embedded auxiliary CSV files.")
      ),
      div(
  style = "background-color: #fff3e0; border-left: 5px solid #FF8C00; padding: 10px 12px; margin-top: 8px; margin-bottom: 8px; border-radius: 6px; font-size: 18px; line-height: 1.45;",
  HTML("
    <span style='color:#d96f00; font-weight:700; font-size:18px;'>
      <i class='fa fa-server'></i> Memory limit
    </span><br>
    The Web version of <i>MetaboCensoR</i> is limited to <b>1 GB</b> of memory.
    For large datasets or memory-intensive analyses, we recommend activating the <b>Limit table previews</b> option and running <i>MetaboCensoR</i> locally.<br>
    <a href='https://github.com/plyush1993/MetaboCensoR#launch-the-app-rocket'
       target='_blank' style='font-weight:700; color:#d96f00;'>
       Local installation instructions
    </a>
  ")
),
br(),
      div(class="highlight",
    HTML("Tip: Try the Example dataset by clicking the button to overview the full App functionality.<br>
    If use this example, run the Blank filter first to save memory for further steps. You can keep all parameters by default.<br>
    It is the LC-MS profiling dataset, described in the study
    <a href='https://pubs.acs.org/doi/10.1021/acs.analchem.4c05577' target='_blank'>[1]</a>, and associated peak table available at
    <a href='https://github.com/plyush1993/MetaboCensoR/blob/main/inst/extdata/orbi_iimn_gnps_quant.csv' target='_blank'>Github</a>.<br>
    Briefly, methanol extracts from the plant ashwagandha [<i>Withania somnifera</i> (L.) Dunal] together with blanks were analyzed on Thermo Q-Exactive Plus Orbitrap in DDA positive mode. Raw mzML files were then processed in mzMine in default pre-settings for UPLC-DDA.<br>")),
      br(),
      div(class="highlight", "Tip: If 'No sample columns' message, your sample keywords don’t match sample column names."),
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
      h3("Blank Filters"),
      tags$ul(
        tags$li(
  tags$b("Labels:"),
  " choose how sample groups are assigned before Blank filtering. You can check sample-label pairs matching by 'labels table'.",
  tags$br(),
  tags$b("From sample names:"),
  " labels are extracted from sample column names using the selected token index and token separator. ",
  "For example: Sample name -> Orbi_Sample_A.mzML; separator: _; token index: 2; resulting label: Sample.",
  tags$br(),
  tags$b("From metadata CSV:"),
  " upload a metadata table with column names, choose the sample-name column and the column to use as Label. Rows are matched by sample name. You can also clean sample name to remove file extension, suffixes, etc.",
  tags$br(),
  tags$b("From custom CSV:"),
  " upload a one-column CSV file without a header. The number and order of labels must match the detected sample columns in the uploaded peak table.",
  tags$br(),
  tags$b("Manual editable table:"),
  " fills the label table from the current token-based labels and allows manual correction.",
  "Double-click a cell in the Label column to edit the group name.",
  tags$br(),
  "Click the button 'Fill editable ...' to update the cuttent labels after changing.",
  tags$br(),
  "To start from full raw sample names, use a token index larger than the number of available tokens; the app will fall back to the full sample name. "
),
        tags$li(tags$b("Blank filter:"), " select blank group(s) (at least one) -> mode (by cutoff or any peak) -> Apply blank filter."),
        tags$ul(
                  tags$li(tags$b("Cutoff mode:"), " calculates Mean values for Blank group(s) and keeps feature if it is lower than cutoff * maximum mean value among experimental (other) group(s). The default is 0.1, meaning the blank signal should be less than 10% of the signal in the sample group in which it is the most abundant"),
                  tags$li(tags$b("Drop any mode:"), "deletes any feature detected in Blank group(s)")
                        ),
        tags$li(tags$b("Plot values distribution:"), " is displayed and updated only after clicking the plot buttons, cutoff value on it is updated dynamically."),
        br(),
        div(class="highlight", "Note: We recommend to perform a Blank filter wherever it is applicable since it removes ghost/background signals and simplifies any further calculations."),
        br(),
        div(class="highlight", "Tip: Blank filtering can be used for media samples, matrix/substrate controls, and other condition-specific background groups.")
        )
    ))
  }

  if (sec == "ms") {
    return(div(
      h3("MS Filters"),
      tags$ul(
        tags$li(tags$b("Isotopes/Dimers:"), " define number of C13 isotopes (n) and possible charges (z_max), and dimer seria for C13*(n_d + 0.5). Uses m/z and RT shifts + correlation threshold to detect isotopes/dimers features by graph and retains the most intense isotope/dimer in each family.",
                tags$br(),
                tags$b("Note: "),"isotope/dimer family is determined by graph, thus, we recommend to keep the default value of n is 1, if you do not expect isotope patterns other than the regular 13C ladder."),
        tags$li(tags$b("Adducts:"), " define polarity and minimal neutral mass. By default employs built-in Adducts list (see details below). Uses m/z and RT shifts + correlation threshold to detect adducts features by graph and retains the most intense adduct in each family by default. When Preferred ions is enabled, a preferred ion within the allowed intensity difference can be retained instead.",
                tags$br(),
                tags$b("Note: "),"we recommend to enable `Strict RT split inside clusters` option, that check that adducts always fulfill defined rt tolerance even after grouping by graph.",
                tags$br(),
                tags$b("Note: "),"we recommend to enable `Preferred ions` option only if you have a specific reason to prioritize certain ion forms."),
        tags$li(tags$b("Neutral Loses:"), " define polarity. By default employs built-in Neutral Losses list (see details below). Uses m/z and RT shifts + correlation threshold to detect neutral losses features and retains ion with the highest m/z in each pair."),
        tags$li(tags$b("In-Source Fragments:"), " uses RT shifts + correlation threshold to detect in-source fragment features and retains ion with the highest m/z in each pair.",
                tags$br(),
                tags$b("Note: "),"we recommend to enable `Control intensity ratio` option, that check precursor / fragment intensity ratio.",
                tags$br(),
                tags$b("Note: "),"we recommend, if possible, to enable `Require MS2 fragment match` option, that check if potential fragment is presented in associated precursor MS2 spectra"),
        tags$li(tags$b("Mispicked Ions:"), " uses m/z and RT shifts + correlation threshold to detect mispicked ions by graph and retains the most intense ion in each family and merges it with others.",
                tags$br(),
                tags$b("Note: "),"we recommend using this filter with caution, and only if you expect poor integration. Be aware of incorrectly merging isomeric peaks."),
        tags$li(tags$b("Saturated Ions:"), " uses m/z and RT shifts + correlation threshold to detect saturated ions by graph and retains the most intense ion in each family.",
                tags$br(),
                tags$b("Note: "),"we recommend to disable `Bidirectional search` option, and remaining saturated artifacts to be strictly higher than anchor ion; also consider min intensity for the saturated (anchor) ion and its ratio with artifacts.",
                tags$br(),
                tags$b("Note: "),"we recommend using this filter with caution, and only if you expect poor integration or supersaturated signals with TOF instruments.")
      ),
      div(class="highlight", "Rule: Filters are always applied sequentially. Start with Isotopes, then Adducts, Neutral Loses, In-Source Fragments, Mispicked Ions, and Saturated Ions. We recommend to keep the order to avoid misannotation."),
      br(),
      div(class="highlight", "Rule: Isotopes-Dimers/Adducts/Neutral Loses/In-Source Fragments/Mispicked/Saturated ions tables are displayed after activating the checkbox."),
      br(),
      div(class = "highlight",
      "Tip: See Isotopes & Dimers shift table ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/blob/main/Isotopes&Dimers.md", "here", target = "_blank"),
      "."
    ),
      br(),
      div(class = "highlight",
      "Tip: You can upload your custom Adduct list. Default built-in Adducts list used in ",
      tags$a(href = "https://cran.r-project.org/web/packages/nontarget/index.html", "[1]", target = "_blank"),
      " and available ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/blob/main/adducts%20(nontarget).csv", "here", target = "_blank"),
      ". For an extended list of adducts, please refer to the ",
      tags$a(href = "https://github.com/plyush1993/MetaboCensoR/blob/main/AdductLists.md", "Adduct Lists", target = "_blank"),
      "and 'adducts*.csv' files located in the root GitHub directory."
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
    ),
    br(),
    div(class="highlight", HTML("Tip: <b>MS2-supported in-source fragment matching and filtering</b><br><br>",
                                "<u>Provides the most specific ISF filtering by requiring MS2 support, reducing the risk of removing true signals. Requires MS2 spectra linked to peak IDs and therefore applies only to features with acquired MS2 data, making it potentially less sensitive than MS1-only filtering.</u><br><br>",
                  "Upload the MGF file corresponding to the peak table, then specify the Feature ID field, m/z tolerance, and relative abundance threshold.<br>",
                  "Candidate ISF relationships are retained only when the putative fragment m/z (at specified tolerance) is detected in an MS2 spectrum (at specified minimum relative abundance) of the precursor feature.<br>",
                  "<b>Important:</b> The <b>Feature ID</b> selected in the Upload tab must correspond to the values in the selected <b>MGF Feature ID field</b>.")),
    br(),
    div(
  style = "
    background-color: #f1e1ff;
    border-left: 5px solid #9112f3;
    padding: 10px 12px;
    margin-top: 8px;
    margin-bottom: 8px;
    border-radius: 6px;
    font-size: 18px;
    line-height: 1.45;
  ",
  HTML("
    <span style='
      color:#6f00b8;
      font-weight:700;
      font-size:18px;
    '>
    <i class='fa fa-lightbulb-o'></i> Pro tip: Fragment search strategies
    </span><br><br>

    <b>Neutral Losses</b> and <b>In-Source Fragments</b> target two related but distinct types of fragment relationships. ISF is broad enough to cover both fragment types, whereas NL relies on specific mass differences, making it highly specific.<br><br>
    <b>If only MS1 data is available:</b><br>
    We recommend running only the <b>Neutral Losses</b> search with default settings. This ensures accuracy and minimizes the risk of accidentally deleting closely co-eluting peaks. <i>Note: While using both NL and ISF on MS1 data can clean the peak table more aggressively, it increases the risk of removing valid co-eluting peaks.</i><br><br>
    <b>If MS2 data is available:</b><br>
    We recommend running only the <b>In-Source Fragments</b> search, as it inherently covers neutral losses. Because MS2 fragmentation spectra provide much higher confidence (using the <b>Require MS2 fragment match</b> option), you can safely use an increased RT tolerance (for example, to <b>0.01 min</b>).
")),
  br(),
      div(
  style = "
    background-color: #f1e1ff;
    border-left: 5px solid #9112f3;
    padding: 10px 12px;
    margin-top: 8px;
    margin-bottom: 8px;
    border-radius: 6px;
    font-size: 18px;
    line-height: 1.45;
  ",
  HTML("
    <span style='
      color:#6f00b8;
      font-weight:700;
      font-size:18px;
    '>
    <i class='fa fa-lightbulb-o'></i> Pro tip: Additional isotope refinement
    </span><br><br>

    In the default isotope search, using <b>n = 1</b> is conservative: the algorithm first searches for the first <sup>13</sup>C isotope before grouping higher isotope peaks such as <b>M+2</b> or <b>M+3</b>. This reduces the risk of incorrectly clustering two independent compounds that are separated by approximately <b>2.0067 Da</b> or <b>3.0101 Da</b>, especially when the intermediate <b>M+1</b> peak is absent.<br><br>
This assumption usually works well because isotope peaks detected in peak tables are most commonly dominated by the <sup>13</sup>C isotope ladder. However, depending on the sample type and metabolite class, additional isotope patterns may be relevant. For example, heteroatom-containing metabolites or natural products, particularly compounds containing S, Cl, Br, Si, or multiple O atoms, may show clear <b>M+2</b> isotope peaks, while combined isotope contributions can also produce visible <b>M+3</b> peaks.<br><br>
For these cases, a more permissive isotope search can be performed by increasing the isotope order to <b>n = 2</b> and using a wider m/z tolerance, for example <b>0.015 Da</b>. A broader tolerance such as <b>0.05 Da</b>, <b>n = 3</b>, <b>0.05 min</b> should be treated as a diagnostic/refinement search and interpreted carefully.<br><br>
Both <b>Mispicked Ion Merging</b> and <b>Saturated Ion Cleaning</b> can potentially be used to detect and remove additional isotope-like signals, heteroatom isotope patterns, or misaligned peaks. For refined isotope-like artifact search, <b>Saturated Ion Cleaning</b> can be used in <b>bidirectional mode</b> with a controlled intensity ratio and adjusted parameters, for example <b>m/z tolerance = 0.05 Da</b> and <b>minimum intensity = 10,000</b>.<br><br>
Be aware that wide m/z tolerances increase the risk of incorrectly grouping unrelated coeluting compounds, such as lipid species differing only by the number of double bonds. Results from permissive isotope refinement should therefore be manually inspected.")),
  br(),
      div(
  style = "
    background-color: #f1e1ff;
    border-left: 5px solid #9112f3;
    padding: 10px 12px;
    margin-top: 8px;
    margin-bottom: 8px;
    border-radius: 6px;
    font-size: 18px;
    line-height: 1.45;
  ",
  HTML("
    <span style='
      color:#6f00b8;
      font-weight:700;
      font-size:18px;
    '>
    <i class='fa fa-lightbulb-o'></i> Pro tip: Peak table annotation
    </span><br><br>
                                  You can print and download a table describing any stage of MS filtration. These tables can be used for peak annotation purposes. <br><br>
                                  Adduct statistics table shows the frequency distribution for each determined adduct type and can be used to refine Adduct list.<br><br>
                                  Adduct relationships are determined by neutral-mass calculation. Therefore, the algorithm evaluates all defined adduct, charge-state, and multimer combinations. However, neutral-mass matching alone does not always define one unique ion type. Different ion forms can produce compatible neutral masses and may therefore be grouped into the same ion family. For example, the algorithm may not always distinguish between a multiply charged ion, a multimer, or another compatible adduct relationship (e.g. with AcN). These assignments should be considered putative and manually checked when needed, using MS1 isotope spacing to confirm charge state and MS2 spectra to support multimer- or other adduct-related assignments.")),

    )
    )
  }

  if (sec == "qc") {
    return(div(
      h3("QC Filters"),
      tags$ul(
                tags$li(
  tags$b("Labels:"),
  " choose how sample groups are assigned before QC filtering. You can check sample-label pairs matching by 'labels table'.",
  tags$br(),
  tags$b("Use labels from Blank Tab:"),
  " QC filtering inherits the labels already defined in the Blank Filters tab. ",
  tags$br(),
  tags$b("From sample names:"),
  " labels are extracted from sample column names using the selected token index and token separator. ",
  "For example: Sample name -> Orbi_Sample_A.mzML; separator: _; token index: 2; resulting label: Sample.",
  tags$br(),
  tags$b("From metadata CSV:"),
  " upload a metadata table with column names, choose the sample-name column and the column to use as Label. Rows are matched by sample name. You can also clean sample name to remove file extension, suffixes, etc.",
  tags$br(),
  tags$b("From custom CSV:"),
  " upload a one-column CSV file without a header. The number and order of labels must match the detected sample columns in the uploaded peak table.",
  tags$br(),
  tags$b("Manual editable table:"),
  " fills the label table from the current token-based labels and allows manual correction.",
  "Double-click a cell in the Label column to edit the group name.",
  tags$br(),
  "Click the button 'Fill editable ...' to update the cuttent labels after changing.",
  tags$br(),
  "To start from full raw sample names, use a token index larger than the number of available tokens; the app will fall back to the full sample name. "
),
        tags$li(tags$b("Value filters:"), " choose group for removal -> specify value(s): zeros(by counts or %) / mean / RSD / D-ratio / min -> mode of filtering (ANY/EVERY/POOLED) -> Apply value filters.",
                tags$ul(
                        tags$li(tags$b("ANY"), " - satisfies threshold in at least one of the selected groups"),
                        tags$li(tags$b("EVERY"), " - satisfies threshold in all of the selected groups"),
                        tags$li(tags$b("POOLED"), " - calculates average values among all of the selected groups and then compares"),
                        tags$li(tags$b("Note:"), " If no group is selected, all groups are considered")
                        )),
        tags$li(tags$b("Plot values distribution:"), " is displayed and updated only after clicking the plot buttons, cutoff value on it is updated dynamically."),
        br(),
        div(class="highlight", "Note: We recommend to apply QC Filters (except zeros) after drift/batch correction, normalization, and imputation for large-scale metabolomics study, while it might make sense to apply all other filters before them."),
        br(),
        div(class="highlight", "Note: RSD calculation requires at least 2 samples per group. All-zero or all-missing features produce NA RSD and fail the RSD filter."),
        br(),
        div(class="highlight", "Note: D-ratio (%) = 100 × SD(QC) / SD(biological samples). A QC group is required and is excluded from the biological groups. At least 2 finite values are required in each SD calculation. Zero biological SD or undefined ratios fail.")
      )
    ))
  }

  if (sec == "peak") {
    return(div(
      h3("Peak Filters"),
      tags$ul(
        tags$li(tags$b("Overview:"), " alows to filter peaks by m/z, rt, mass defect (absolute and relative) values, or a target peak list to remove/keep."),
        tags$li(tags$b("Peak filters:"), " m/z, rt, RMD, AMD bounds, and a target peak list to remove/keep."),
        tags$li(tags$b("Filtering values:"), " filter peaks by m/z, rt, mass defect (absolute or relative) values by specifying threshold ranges for corresponding filtering value."),
        tags$li(tags$b("Target peaks to remove/keep:"), " for making a target peak list to remove or keep enter comma-separated m/z and RT pairs manually, one pair per line, or upload a CSV file. Also specify m/z and RT tolerance for matching. 'Matched target peaks' table shows features from the current peak table that match the provided target m/z and RT pairs within the selected tolerances."),
        tags$li(tags$b("Plot values distribution:"), " is displayed and updated only after clicking the plot buttons, cutoff value on it is updated dynamically.")
      ),
      div(class="highlight", "Note: optional step to filter peaks based on a priori information."),
      br(),
div(
  class = "highlight",
  tags$b("Tip: "),
  tags$span(
    style = "color:#e74c3c; font-weight:900;",
    "Target peak list; REMOVE mode — "
  ),
  "if blank samples were not processed together with experimental samples, or if the peak-picking software struggles to process blanks and experimental samples together, we recommend generating a separate peak table for blanks only. Then, m/z-RT pairs of peaks detected in blanks with reasonable intensity can be added to the target peak list and ",
  tags$b("removed"),
  " using the target peak removal mode."
),
br(),
div(
  class = "highlight",
  tags$b("Tip: "),
  tags$span(
    style = "color:#18bc9c; font-weight:900;",
    "Target peak list; KEEP mode — "
  ),
  "if you want to focus only on a specific group of features, such as target ions, with MS1/MS2 annotations, library matches, molecular networking features, or manually selected compounds, these peaks can be added to the target peak list and ",
  tags$b("retained"),
  " using the keep-only target peak mode. It also may be useful to subset from MGF file."
)
    ))
  }

  if (sec == "final") {
    return(div(
      h3("Final Summary"),
      tags$ul(
        tags$li(tags$b("Compile summary & final datasets:"), " final filtered peak table after all applied filters."),
        tags$li(tags$b("MGF filtering:"), " activated after final compile. Filters MGF file based on final filtered table. Upload the MGF file corresponding to the peak table, then specify the matching Feature ID fields. Feature/Peak ID in MGF should match with selected Feature ID in peak table. We recommend using the default columns unless you have specific requirements."),
        tags$li(tags$b("Download settings (.json):"), " export analysis settings in .json file for re-using."),
      ),
      div(class="highlight", "Note: if something looks missing in final output, check tab by tab where it was filtered."),
      br(),
      div(class="highlight", "Note: if no IDs match during MGF filtering, the app reports: 'No matching spectra were found in the MGF file'."),
      br(),
      div(class="highlight", "Note: for xcms peak table all NA values are converted to 0."),
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
      " repository."),

tags$li(
  style = "text-align: left !important;",
  tags$b("Error 'subscript out of bounds' for any plot:"),
  " this can occur on local machine due to a known version mismatch between ", tags$code("ggplot2"), " and ", tags$code("plotly"), ". ",
  "You can fix this by explicitly installing matched stable versions:",
tags$div(
  style = "position: relative; background-color: #f6f8fa; border-radius: 6px; padding: 2px 2px; margin-top: 6px; margin-bottom: 6px; border: 1px solid #d0d7de; text-align: left; direction: ltr;",
  tags$button(
    "Copy",
    style = "position: absolute; top: 4px; right: 4px; font-size: 15px; padding: 2px 2px; cursor: pointer; border-radius: 4px; border: 1px solid #d0d7de; background-color: #ffffff; color: #24292f;",
    onclick = "var codeText = document.getElementById('plotly-fix-code').innerText; var textArea = document.createElement('textarea'); textArea.value = codeText; document.body.appendChild(textArea); textArea.select(); document.execCommand('copy'); document.body.removeChild(textArea); this.innerText = 'Copied!'; setTimeout(() => this.innerText = 'Copy', 2000);"
  ),

  tags$pre(
    style = "margin: 0; background: transparent; border: none; overflow-x: auto; text-align: left; direction: ltr; line-height: 1.25;",
    tags$code(
      id = "plotly-fix-code",
      style = "font-family: monospace; font-size: 14px; color: #24292f; text-align: left; direction: ltr; display: block; white-space: pre; padding: 0; margin: 0;",
      "if (!requireNamespace(\"devtools\", quietly = TRUE)) install.packages(\"devtools\")\ndevtools::install_version(\"ggplot2\", version = \"3.4.4\", repos = \"http://cran.us.r-project.org\")\ndevtools::install_version(\"plotly\", version = \"4.10.4\", repos = \"http://cran.us.r-project.org\")"
    )
  )
)),
br(),
div(
  style = "background-color: #fff3e0; border-left: 5px solid #FF8C00; padding: 10px 12px; margin-top: 8px; margin-bottom: 8px; border-radius: 6px; font-size: 18px; line-height: 1.45;",
  HTML("
    <span style='color:#d96f00; font-weight:700; font-size:18px;'>
      <i class='fa fa-server'></i> Memory limit
    </span><br>
    The Web version of <i>MetaboCensoR</i> is limited to <b>1 GB</b> of memory.
    For large datasets or memory-intensive analyses, we recommend activating the <b>Limit table previews</b> option and running <i>MetaboCensoR</i> locally.<br>
    <a href='https://github.com/plyush1993/MetaboCensoR#launch-the-app-rocket'
       target='_blank' style='font-weight:700; color:#d96f00;'>
       Local installation instructions
    </a>
  ")
)
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
      tags$li(HTML('Publication:&nbsp;
       <a href="https://doi.org/10.64898/2026.07.02.735197" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          bioRxiv
       </a>')),
      tags$li(HTML('Tutorial:&nbsp;
       <a href="https://github.com/plyush1993/MetaboCensoR/blob/main/MetaboCensoR_tutorial.pdf" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          PDF
       </a>')),
      tags$li(HTML('Project Page:&nbsp;
       <a href="https://github.com/plyush1993/MetaboCensoR" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          GitHub
       </a>')),
      tags$li(HTML('Examples & Benchmarking:&nbsp;
       <a href="https://github.com/plyush1993/MetaboCensoR_Examples" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          GitHub
       </a>')),
      tags$li(HTML('Error Report:&nbsp;
       <a href="https://github.com/plyush1993/MetaboCensoR/issues" target="_blank"
          style="color:#ffcc00; font-weight:700; text-decoration:none;">
          Issues
       </a>')),
      tags$li(
        "Contact: ",
        tags$input(
          id = "feedback_email_copy",
          type = "text",
          value = "plyushchenko.ivan@gmail.com",
          readonly = "readonly",
          style = "position:absolute; left:-9999px;"
        ),
        tags$button(
          type = "button",
          onclick = "
            var emailInput = document.getElementById('feedback_email_copy');
            emailInput.select();
            emailInput.setSelectionRange(0, 99999);

            try {
              var ok = document.execCommand('copy');
              if (ok) {
                this.innerHTML = '<i class=\"fa fa-check\"></i> Copied';
                var btn = this;
                setTimeout(function() {
                  btn.innerHTML = '<i class=\"fa fa-envelope\"></i>';
                }, 1500);
              } else {
                window.prompt('Copy email address:', emailInput.value);
              }
            } catch(e) {
              window.prompt('Copy email address:', emailInput.value);
            }
          ",
          style = "
            margin-left: 8px;
            border: 1px solid #ffcc00;
            border-radius: 5px;
            background: transparent;
            color: #ffcc00;
            cursor: pointer;
            font-weight: 700;
            padding: 2px 7px;
            font-size: 12px;
          ",
          tags$i(class = "fa fa-envelope"),
          " "
        )
      )
    )
    ))
    }

})

  for (id in c(
    "global_controls0",
    "blank_controls1",
    "value_groups_ui",
    "dratio_qc_group_ui",
    "blank_metadata_sample_col_ui",
    "blank_metadata_label_col_ui",
    "qc_metadata_sample_col_ui",
    "qc_metadata_label_col_ui",
    "isf_mgf_field_ui",
    "mgf_toggle_ui",
    "mgf_matching_ui"
  )) {
    outputOptions(
      output,
      id,
      suspendWhenHidden = FALSE
    )
  }

}
