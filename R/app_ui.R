# ==============================
# UI ----
# ==============================
#' @import shiny
#' @import shinythemes
#' @import shinyjs
#' @import waiter
#' @import DT
#' @import shinyWidgets
#' @import shinyBS
#' @import waiter
#' @import shinycssloaders
#' @import htmltools
#' @import plotly
app_ui <- function() {
shiny::fluidPage(
  use_waiter(),
  use_hostess(),
  theme = shinythemes::shinytheme("flatly"),
  setBackgroundColor(color = c("#43cea2", "#185a9d"), gradient = "linear", direction = "bottom"),
  useShinyjs(),

  tags$head(
    tags$title("MetaboCensoR"),
    tags$link(
      rel  = "icon",
      type = "image/png",
      href = "www/metabocensor_logo.png"
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
        src = "www/metabocensor_logo.png",
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
            div(class="highlight", "No dataset loaded. Go to 'Upload Data' tab.")
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
            div(class="highlight", "No dataset loaded. Go to 'Upload Data' tab.")
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
            div(class="highlight", "No dataset loaded. Go to 'Upload Data' tab.")
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
            div(class="highlight", "No dataset loaded. Go to 'Upload Data' tab.")
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
            div(class="highlight", "No dataset loaded. Go to 'Upload Data' tab.")
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
}
