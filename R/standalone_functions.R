# Public standalone interface. Internal helpers are in standalone_core.R.
.mcs_scalar <- function(x, name, min = -Inf, max = Inf) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < min || x > max)
    stop(name, " must be one finite number in [", min, ", ", max, "].", call. = FALSE)
  invisible(x)
}

.mcs_labels <- function(x, labels = NULL) {
  if (is.null(labels)) return(x$labels)
  if (!is.null(names(labels))) {
    if (anyDuplicated(names(labels)) || !all(x$sample_cols %in% names(labels)))
      stop("Named labels must uniquely cover all sample column names.", call. = FALSE)
    labels <- labels[x$sample_cols]
  }
  labels <- as.character(labels)
  if (length(labels) != length(x$sample_cols) || anyNA(labels) || any(!nzchar(labels)))
    stop("Supply one nonempty label per sample, in sample_cols order, or a named vector.", call. = FALSE)
  stats::setNames(labels, x$sample_cols)
}

.mcs_override <- function(defaults, overrides) {
  if (length(overrides) && (is.null(names(overrides)) || any(!nzchar(names(overrides))) ||
                            anyDuplicated(names(overrides)))) stop("Use unique named parameter overrides.")
  bad <- setdiff(names(overrides), names(defaults))
  if (length(bad)) stop("Unknown parameters: ", paste(bad, collapse = ", "))
  for (nm in names(overrides)) {
    value <- overrides[[nm]]; default <- defaults[[nm]]
    if (is.numeric(default)) .mcs_scalar(value, nm)
    if (is.logical(default) && (!is.logical(value) || length(value) != 1L || is.na(value)))
      stop(nm, " must be TRUE or FALSE.")
    if (is.character(default) && length(default) == 1L &&
        (!is.character(value) || length(value) != 1L || is.na(value))) stop(nm, " must be a string.")
    defaults[[nm]] <- value
  }
  for (nm in grep("_tol_type", names(defaults), value = TRUE))
    if (!defaults[[nm]] %in% c("da", "ppm")) stop(nm, " must be da or ppm.")
  for (nm in grep("_pol2$", names(defaults), value = TRUE))
    if (!defaults[[nm]] %in% c("positive", "negative")) stop(nm, " must be positive or negative.")
  for (nm in names(defaults)[vapply(defaults, is.numeric, logical(1))]) {
    v <- defaults[[nm]]
    if (!grepl("peak_(rmd|amd)_", nm) && v < 0) stop(nm, " must be non-negative.")
    if (grepl("_cor2$", nm) && v > 1) stop(nm, " must be <= 1.")
  }
  for (nm in intersect(c("iso_zmax2", "iso_n2", "iso_nd2"), names(defaults)))
    if (defaults[[nm]] < 1 || defaults[[nm]] != as.integer(defaults[[nm]])) stop(nm, " must be a positive integer.")
  if ("ring_ratio2" %in% names(defaults) && defaults$ring_ratio2 < 1) stop("ring_ratio2 must be >= 1.")
  if ("isf_ms2_rel2" %in% names(defaults) && defaults$isf_ms2_rel2 > 100) stop("isf_ms2_rel2 must be <= 100.")
  defaults
}

.mcs_aux_table <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.data.frame(value)) return(value)
  if (!is.character(value) || length(value) != 1L || !file.exists(value))
    stop("Auxiliary table must be a data.frame or an existing CSV path.")
  .mcs_core$safe_read_csv_any(value)
}

#' Extract sample groups from column names
#'
#' @noMd
#' @description Split each sample name at a chosen separator and use one part as its group label.
#' @param sample_names Character vector of sample-column names, in the required output order.
#' @param separator Literal separator, not a regular expression. Default: \code{"_"}.
#' @param token Positive integer giving the one-based position of the group name. Default: 2.
#' @details If the requested part is missing or empty, the full sample name is used instead. Missing parts generate a warning. Inspect the returned labels before using them for filtering.
#' @return A character vector named by \code{sample_names}, in the same order.
#' @seealso \code{\link{mc_labels_from_metadata}}, \code{\link{mc_labels_from_csv}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' # The bundled names are Orbi_Blank_A..., Orbi_Sample_A..., etc.
#' imported$labels <- mc_labels_from_names(
#'   imported$sample_cols, separator = "_", token = 2
#' )
#' data.frame(
#'   sample = names(imported$labels),
#'   group = unname(imported$labels)
#' )
#'
#' @export
mc_labels_from_names <- function(sample_names, separator = "_", token = 2) {
  .mcs_scalar(token, "token", 1)
  if (token != as.integer(token)) stop("token must be an integer.")
  stats::setNames(.mcs_core$labels_from_sample_names(sample_names, separator, token,
                                                  show_notification = FALSE), sample_names)
}

#' Match sample groups from a metadata CSV
#'
#' @noMd
#' @description Read sample names and groups from two metadata columns and align the group labels to the peak-table sample order.
#' @param path Local metadata CSV filename. The first row must contain column names. Download remote files before calling this function.
#' @param sample_names Sample-column names to match, in the required output order.
#' @param sample_col Exact metadata column name containing sample identifiers.
#' @param label_col Exact metadata column name containing group labels.
#' @param clean_names If \code{TRUE}, also remove the suffixes specified in \code{remove_suffixes} from both sets of names before matching. Default: \code{FALSE}.
#' @param remove_suffixes Optional trailing strings to remove, for example \code{c(" Peak area", ".mzML")}. Suffix removal is case-insensitive and is used only when \code{clean_names = TRUE}.
#' @details Whitespace and surrounding double quotes are normalized before matching. With suffix removal enabled, suffixes are stripped repeatedly, allowing a name ending in both a file extension and an intensity suffix to match a shorter metadata name. The remaining name matching is case-sensitive. Missing samples, duplicate matching keys, and missing or empty group labels raise an error. Extra metadata samples are allowed.
#' @return A character vector named by \code{sample_names}, aligned to that order regardless of metadata row order.
#' @seealso \code{\link{mc_prepare}}, \code{\link{mc_labels_from_csv}}, \code{\link{mc_labels_from_names}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' table(imported$labels)
#'
#' @export
mc_labels_from_metadata <- function(path, sample_names, sample_col, label_col,
                                    clean_names = FALSE, remove_suffixes = NULL) {
  if (!file.exists(path)) stop("Metadata file does not exist: ", path)
  labs <- .mcs_core$metadata_labels_by_sample(
    list(name = basename(path), datapath = path), sample_names, sample_col,
    label_col, clean_enabled = clean_names, remove_suffixes = remove_suffixes)
  stats::setNames(labs, sample_names)
}

#' Read sample groups from an ordered label CSV
#'
#' @noMd
#' @description Read a headerless CSV containing one sample-group label per row.
#' @param path Filename of a comma-separated, headerless label file. The first column is read as the label vector.
#' @param sample_names Sample-column names in the same order as the labels in the file.
#' @details The file must have exactly one nonempty label for each sample. Labels are assigned by row position, not by matching sample names. Use metadata matching when sample order may differ.
#' @return A character vector named by \code{sample_names}.
#' @seealso \code{\link{mc_labels_from_metadata}}, \code{\link{mc_prepare}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' label_file <- system.file(
#'   "extdata", "Label.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_csv(label_file, imported$sample_cols)
#' table(imported$labels)
#'
#' @export
mc_labels_from_csv <- function(path, sample_names) {
  dat <- vroom::vroom(path, col_names = FALSE, delim = ",", show_col_types = FALSE)
  labs <- as.character(dat[[1]])
  if (length(labs) != length(sample_names) || anyNA(labs) || any(!nzchar(labs)))
    stop("Label CSV must have one nonempty row per sample (no header).")
  stats::setNames(labs, sample_names)
}

#' Set tolerances and switches for MS filtering
#'
#' @noMd
#' @description Create a complete named list of MS-filter settings, replacing selected defaults with your named arguments.
#' @param ... Named settings to replace. Supported names, meanings, units, and defaults are listed below. Unknown names raise an error.
#' @details Pass the returned list to \code{mc_filter_ms(parameters = ...)}. Merely creating the list does not change any analysis. All RT tolerances are in minutes. Da and ppm tolerances are alternatives selected separately for each subfilter. Explicit filter names in \code{mc_filter_ms()} override the enable flags.
#'
#' \describe{
#' \item{\code{enable_iso2}}{Enable isotope filtering when mc_filter_ms(filters = NULL). An explicit filters vector overrides this flag; use "isotopes" to select isotope filtering explicitly. Default: \code{TRUE}.}
#' \item{\code{iso_cor2}}{Minimum Pearson correlation between the sample-intensity profiles of a candidate isotope and its parent feature. Values range from 0 to 1; higher cutoffs require more similar profiles. Default: \code{0.8}.}
#' \item{\code{iso_rt2}}{Maximum retention-time difference, in minutes, between a candidate isotope and its parent feature. Default: \code{0.01}.}
#' \item{\code{iso_tol_type2}}{Mass-tolerance unit for isotope matching: "da" uses iso_mz_tol2; "ppm" uses iso_ppm2. Only the selected tolerance is used. Default: \code{da}.}
#' \item{\code{iso_mz_tol2}}{Absolute mass tolerance in Da for isotope matching. Used only when iso_tol_type2 = "da". Default: \code{0.005}.}
#' \item{\code{iso_ppm2}}{Relative mass tolerance in ppm for isotope matching. Used only when iso_tol_type2 = "ppm". Default: \code{10}.}
#' \item{\code{iso_zmax2}}{Maximum charge state searched for isotope relationships. For example, 3 searches charge states 1, 2 and 3. Default: \code{3}.}
#' \item{\code{iso_n2}}{Maximum isotope order k searched. Expected isotope spacing is 1.00336 * k / z Da, for orders 1 through iso_n2 and charges 1 through iso_zmax2. Default: \code{1}.}
#' \item{\code{iso_nd2}}{Number of additional dimer-pattern isotope shifts searched: 1.00336 * (j + 0.5) Da for j from 1 through iso_nd2. Default: \code{3}.}
#' \item{\code{enable_add2}}{Enable adduct filtering when mc_filter_ms(filters = NULL). An explicit filters vector overrides this flag; use "adducts" to select adduct filtering explicitly. Default: \code{TRUE}.}
#' \item{\code{add_pol2}}{Ionization polarity used to select adduct definitions: "positive" or "negative". Set nl_pol2 separately if neutral-loss filtering is also used. Default: \code{positive}.}
#' \item{\code{add_cor2}}{Minimum Pearson correlation between the sample-intensity profiles of features proposed to belong to the same adduct family. Values range from 0 to 1; higher cutoffs require more similar profiles. Default: \code{0.8}.}
#' \item{\code{add_rt2}}{Maximum retention-time difference, in minutes, between features proposed to belong to the same adduct family. Default: \code{0.005}.}
#' \item{\code{add_tol_type2}}{Mass-tolerance unit for adduct matching: "da" uses add_mz_tol2; "ppm" uses add_ppm2. Only the selected tolerance is used. Default: \code{da}.}
#' \item{\code{add_mz_tol2}}{Absolute mass tolerance in Da for adduct matching. Used only when add_tol_type2 = "da". Default: \code{0.005}.}
#' \item{\code{add_ppm2}}{Relative mass tolerance in ppm for adduct matching. Used only when add_tol_type2 = "ppm". Default: \code{10}.}
#' \item{\code{add_min_nm2}}{Minimum candidate neutral mass in Da for adduct annotation. Candidate neutral masses must be strictly greater than this threshold. Default: \code{50}.}
#' \item{\code{add_strict_rt2}}{If TRUE, restrict the total retention-time spread of an adduct family to add_rt2 minutes, in addition to pairwise matching. Default: \code{TRUE}.}
#' \item{\code{enable_nl2}}{Enable neutral-loss filtering when mc_filter_ms(filters = NULL). An explicit filters vector overrides this flag; use "neutral_losses" to select neutral-loss filtering explicitly. Default: \code{FALSE}.}
#' \item{\code{nl_pol2}}{Ionization polarity used to select neutral-loss definitions: "positive" or "negative". Set add_pol2 separately if adduct filtering is also used. Default: \code{positive}.}
#' \item{\code{nl_cor2}}{Minimum Pearson correlation between the sample-intensity profiles of features proposed to differ by a listed neutral loss. Values range from 0 to 1; higher cutoffs require more similar profiles. Default: \code{0.95}.}
#' \item{\code{nl_rt2}}{Maximum retention-time difference, in minutes, between features proposed to differ by a listed neutral loss. Default: \code{0.002}.}
#' \item{\code{nl_tol_type2}}{Mass-tolerance unit for neutral-loss matching: "da" uses nl_mz_tol2; "ppm" uses nl_ppm2. Only the selected tolerance is used. Default: \code{da}.}
#' \item{\code{nl_mz_tol2}}{Absolute mass tolerance in Da for neutral-loss matching. Used only when nl_tol_type2 = "da". Default: \code{0.005}.}
#' \item{\code{nl_ppm2}}{Relative mass tolerance in ppm for neutral-loss matching. Used only when nl_tol_type2 = "ppm". Default: \code{10}.}
#' \item{\code{enable_isf2}}{Enable in-source fragment filtering when mc_filter_ms(filters = NULL). An explicit filters vector overrides this flag; use "isf" to select in-source fragment filtering explicitly. Default: \code{FALSE}.}
#' \item{\code{isf_cor2}}{Minimum Pearson correlation between the sample-intensity profiles of a candidate in-source fragment and its precursor feature. Values range from 0 to 1; higher cutoffs require more similar profiles. Default: \code{0.95}.}
#' \item{\code{isf_rt2}}{Maximum retention-time difference, in minutes, between a candidate in-source fragment and its precursor feature. Default: \code{0.01}.}
#' \item{\code{isf_control_int2}}{If TRUE, require the mean fragment/precursor intensity ratio to fall between isf_ratio_min2 and isf_ratio_max2. If FALSE, omit this intensity-ratio condition. Default: \code{TRUE}.}
#' \item{\code{isf_ratio_min2}}{Lower allowed ratio of mean candidate fragment intensity to mean precursor intensity. Used when isf_control_int2 = TRUE. Default: \code{0.001}.}
#' \item{\code{isf_ratio_max2}}{Upper allowed ratio of mean candidate fragment intensity to mean precursor intensity. Used when isf_control_int2 = TRUE. Default: \code{2.5}.}
#' \item{\code{isf_ms2_check2}}{If TRUE, require the candidate fragment m/z in an eligible MS2 spectrum of the precursor; supply spectra to mc_filter_ms(). Support in any eligible spectrum is sufficient. FALSE uses MS1 evidence only. Default: \code{TRUE}.}
#' \item{\code{isf_ms2_rel2}}{Minimum intensity of a supporting MS2 peak, expressed as a percentage of that spectrum's base peak. For example, 1 means 1 percent. Used when isf_ms2_check2 = TRUE. Default: \code{1}.}
#' \item{\code{isf_ms2_mz_tol2}}{Absolute tolerance in Da between the candidate in-source fragment m/z and a supporting MS2 peak. Used when isf_ms2_check2 = TRUE. Default: \code{0.01}.}
#' \item{\code{isf_mgf_id_field}}{Spectrum metadata field used to match MGF spectra to precursor feature IDs for in-source fragment checking; typically "acquisitionNum". Default: \code{acquisitionNum}.}
#' \item{\code{enable_mis2}}{Enable mispicked-ion filtering when mc_filter_ms(filters = NULL). An explicit filters vector overrides this flag; use "mispicked" to select mispicked-ion filtering explicitly. Default: \code{FALSE}.}
#' \item{\code{mis_cor2}}{Minimum Pearson correlation between the sample-intensity profiles of features proposed to be duplicate peak detections. Values range from 0 to 1; higher cutoffs require more similar profiles. Default: \code{0.8}.}
#' \item{\code{mis_rt2}}{Maximum retention-time difference, in minutes, between features proposed to be duplicate peak detections. Default: \code{0.002}.}
#' \item{\code{mis_tol_type2}}{Mass-tolerance unit for mispicked-ion matching: "da" uses mis_mz_tol2; "ppm" uses mis_ppm2. Only the selected tolerance is used. Default: \code{ppm}.}
#' \item{\code{mis_mz_tol2}}{Absolute mass tolerance in Da for mispicked-ion matching. Used only when mis_tol_type2 = "da". Default: \code{0.005}.}
#' \item{\code{mis_ppm2}}{Relative mass tolerance in ppm for mispicked-ion matching. Used only when mis_tol_type2 = "ppm". Default: \code{10}.}
#' \item{\code{enable_ring2}}{Enable ringing/saturated-ion filtering when mc_filter_ms(filters = NULL). An explicit filters vector overrides this flag; use "ringing" to select ringing/saturated-ion filtering explicitly. Default: \code{FALSE}.}
#' \item{\code{ring_cor2}}{Minimum Pearson correlation between the sample-intensity profiles of a candidate ringing artifact and its high-intensity anchor. Values range from 0 to 1; higher cutoffs require more similar profiles. Default: \code{0.8}.}
#' \item{\code{ring_rt2}}{Maximum retention-time difference, in minutes, between a candidate ringing artifact and its high-intensity anchor. Default: \code{0.05}.}
#' \item{\code{ring_tol_type2}}{Mass-tolerance unit for ringing/saturated-ion matching: "da" uses ring_mz_tol2; "ppm" uses ring_ppm2. Only the selected tolerance is used. Default: \code{da}.}
#' \item{\code{ring_mz_tol2}}{Absolute mass tolerance in Da for ringing/saturated-ion matching. Used only when ring_tol_type2 = "da". Default: \code{0.95}.}
#' \item{\code{ring_ppm2}}{Relative mass tolerance in ppm for ringing/saturated-ion matching. Used only when ring_tol_type2 = "ppm". Default: \code{500}.}
#' \item{\code{ring_min_anch2}}{Minimum mean sample intensity for a feature to act as the high-intensity anchor in ringing/saturated-ion filtering. Default: \code{1e+05}.}
#' \item{\code{ring_ratio2}}{Minimum ratio of anchor mean intensity to candidate artifact mean intensity in ringing/saturated-ion filtering. For example, 10 requires the anchor to be at least ten times as intense; must be at least 1. Default: \code{10}.}
#' \item{\code{ring_bidirectional2}}{If TRUE, search for ringing artifacts below and above the anchor m/z. If FALSE, search only above the anchor m/z. Default: \code{FALSE}.}
#' }
#' @return A named list containing every MS parameter, including unchanged defaults.
#' @seealso \code{\link{mc_filter_ms}}, \code{\link{mc_read_mgf}}
#' @examples
#' ms_options <- mc_ms_parameters(
#'   iso_rt2 = 0.01, iso_cor2 = 0.80,
#'   iso_tol_type2 = "da", iso_mz_tol2 = 0.005,
#'   add_rt2 = 0.005, add_cor2 = 0.80,
#'   add_tol_type2 = "da", add_mz_tol2 = 0.005,
#'   add_pol2 = "positive",
#'   isf_rt2 = 0.01, isf_cor2 = 0.95,
#'   isf_ms2_check2 = TRUE, isf_ms2_mz_tol2 = 0.01,
#'   isf_ms2_rel2 = 1, isf_mgf_id_field = "acquisitionNum"
#' )
#' ms_options
#' # Supply with: mc_filter_ms(imported, parameters = ms_options, ...)
#'
#' @export
mc_ms_parameters <- function(...) {
  defaults <- list(
    enable_iso2 = TRUE, iso_cor2 = .80, iso_rt2 = .01, iso_tol_type2 = "da",
    iso_mz_tol2 = .005, iso_ppm2 = 10, iso_zmax2 = 3, iso_n2 = 1, iso_nd2 = 3,
    enable_add2 = TRUE, add_pol2 = "positive", add_cor2 = .80, add_rt2 = .005,
    add_tol_type2 = "da", add_mz_tol2 = .005, add_ppm2 = 10, add_min_nm2 = 50,
    add_strict_rt2 = TRUE,
    enable_nl2 = FALSE, nl_pol2 = "positive", nl_cor2 = .95, nl_rt2 = .002,
    nl_tol_type2 = "da", nl_mz_tol2 = .005, nl_ppm2 = 10,
    enable_isf2 = FALSE, isf_cor2 = .95, isf_rt2 = .01, isf_control_int2 = TRUE,
    isf_ratio_min2 = .001, isf_ratio_max2 = 2.5, isf_ms2_check2 = TRUE,
    isf_ms2_rel2 = 1, isf_ms2_mz_tol2 = .01, isf_mgf_id_field = "acquisitionNum",
    enable_mis2 = FALSE, mis_cor2 = .80, mis_rt2 = .002, mis_tol_type2 = "ppm",
    mis_mz_tol2 = .005, mis_ppm2 = 10,
    enable_ring2 = FALSE, ring_cor2 = .80, ring_rt2 = .05, ring_tol_type2 = "da",
    ring_mz_tol2 = .95, ring_ppm2 = 500, ring_min_anch2 = 1e5, ring_ratio2 = 10,
    ring_bidirectional2 = FALSE)
  .mcs_override(defaults, list(...))
}

#' Set bounds and target tolerances for peak filtering
#'
#' @noMd
#' @description Create a complete named list of peak-filter settings with selected defaults replaced.
#' @param ... Named settings to replace. Supported names, meanings, units, and defaults are listed below. Unknown names raise an error.
#' @details Bounds have an effect only when their corresponding filter is selected. No peak filters are selected by default. Pass the returned list to \code{mc_filter_peak(parameters = ...)}.
#'
#' \describe{
#' \item{\code{peak_filters}}{Peak filters enabled when mc_filter_peak(filters = NULL): "mz", "rt", "rmd", "amd" and/or "target". An empty vector selects none. An explicit filters vector overrides this setting. Default: \code{character(0)}.}
#' \item{\code{mz_min_enable}}{Enable the lower m/z limit, peak_mz_min, when the "mz" peak filter is selected. FALSE disables this bound only. Default: \code{TRUE}.}
#' \item{\code{mz_max_enable}}{Enable the upper m/z limit, peak_mz_max, when the "mz" peak filter is selected. FALSE disables this bound only. Default: \code{FALSE}.}
#' \item{\code{peak_mz_min}}{Lower m/z limit in m/z units. Keep values greater than or equal to this limit when mz_min_enable = TRUE and the "mz" filter is selected. Default: \code{200}.}
#' \item{\code{peak_mz_max}}{Upper m/z limit in m/z units. Keep values less than or equal to this limit when mz_max_enable = TRUE and the "mz" filter is selected. Default: \code{1500}.}
#' \item{\code{rt_min_enable}}{Enable the lower retention time limit, peak_rt_min, when the "rt" peak filter is selected. FALSE disables this bound only. Default: \code{TRUE}.}
#' \item{\code{rt_max_enable}}{Enable the upper retention time limit, peak_rt_max, when the "rt" peak filter is selected. FALSE disables this bound only. Default: \code{FALSE}.}
#' \item{\code{peak_rt_min}}{Lower retention time limit in minutes. Keep values greater than or equal to this limit when rt_min_enable = TRUE and the "rt" filter is selected. Default: \code{1}.}
#' \item{\code{peak_rt_max}}{Upper retention time limit in minutes. Keep values less than or equal to this limit when rt_max_enable = TRUE and the "rt" filter is selected. Default: \code{30}.}
#' \item{\code{rmd_min_enable}}{Enable the lower relative mass defect (RMD) limit, peak_rmd_min, when the "rmd" peak filter is selected. FALSE disables this bound only. Default: \code{TRUE}.}
#' \item{\code{rmd_max_enable}}{Enable the upper relative mass defect (RMD) limit, peak_rmd_max, when the "rmd" peak filter is selected. FALSE disables this bound only. Default: \code{TRUE}.}
#' \item{\code{peak_rmd_min}}{Lower relative mass defect (RMD) limit in ppm. Keep values greater than or equal to this limit when rmd_min_enable = TRUE and the "rmd" filter is selected. Default: \code{-2000}.}
#' \item{\code{peak_rmd_max}}{Upper relative mass defect (RMD) limit in ppm. Keep values less than or equal to this limit when rmd_max_enable = TRUE and the "rmd" filter is selected. Default: \code{2000}.}
#' \item{\code{amd_min_enable}}{Enable the lower absolute mass defect (AMD) limit, peak_amd_min, when the "amd" peak filter is selected. FALSE disables this bound only. Default: \code{TRUE}.}
#' \item{\code{amd_max_enable}}{Enable the upper absolute mass defect (AMD) limit, peak_amd_max, when the "amd" peak filter is selected. FALSE disables this bound only. Default: \code{TRUE}.}
#' \item{\code{peak_amd_min}}{Lower absolute mass defect (AMD) limit in Da. Keep values greater than or equal to this limit when amd_min_enable = TRUE and the "amd" filter is selected. Default: \code{0}.}
#' \item{\code{peak_amd_max}}{Upper absolute mass defect (AMD) limit in Da. Keep values less than or equal to this limit when amd_max_enable = TRUE and the "amd" filter is selected. Default: \code{0.5}.}
#' \item{\code{target_peak_action}}{Action for features matching a target by both m/z and retention time: "remove" discards matches; "keep" retains only matches. Used when the "target" peak filter is selected. Default: \code{remove}.}
#' \item{\code{target_mz_tol_type}}{Mass-tolerance unit for target matching: "da" uses target_mz_tol_da; "ppm" uses target_mz_tol_ppm. Default: \code{da}.}
#' \item{\code{target_mz_tol_da}}{Absolute m/z tolerance in Da for matching features to targets. Used only when target_mz_tol_type = "da". Default: \code{0.005}.}
#' \item{\code{target_mz_tol_ppm}}{Relative m/z tolerance in ppm for matching features to targets. Used only when target_mz_tol_type = "ppm". Default: \code{10}.}
#' \item{\code{target_rt_tol}}{Maximum retention-time difference in minutes between a feature and a target. Both the m/z and retention-time conditions must pass for a target match. Default: \code{0.05}.}
#' }
#' @return A named list containing every peak-filter parameter, including unchanged defaults.
#' @seealso \code{\link{mc_filter_peak}}, \code{\link{mc_plot_peak}}
#' @examples
#' peak_options <- mc_peak_parameters(
#'   peak_filters = c("mz", "rt"),
#'   mz_min_enable = TRUE, peak_mz_min = 200,
#'   mz_max_enable = TRUE, peak_mz_max = 1500,
#'   rt_min_enable = TRUE, peak_rt_min = 1,
#'   rt_max_enable = FALSE
#' )
#' peak_options
#'
#' @export
mc_peak_parameters <- function(...) {
  .mcs_override(list(peak_filters = character(),
    mz_min_enable = TRUE, mz_max_enable = FALSE, peak_mz_min = 200, peak_mz_max = 1500,
    rt_min_enable = TRUE, rt_max_enable = FALSE, peak_rt_min = 1, peak_rt_max = 30,
    rmd_min_enable = TRUE, rmd_max_enable = TRUE, peak_rmd_min = -2000, peak_rmd_max = 2000,
    amd_min_enable = TRUE, amd_max_enable = TRUE, peak_amd_min = 0, peak_amd_max = .5,
    target_peak_action = "remove", target_mz_tol_type = "da", target_mz_tol_da = .005,
    target_mz_tol_ppm = 10, target_rt_tol = .05), list(...))
}

#' Read mass spectra from an MGF file
#'
#' @noMd
#' @description Import an MGF file as a Spectra object for fragment confirmation or synchronized peak-table and spectral export.
#' @param path Local MGF filename. Download a remote MGF to a local file before reading it.
#' @details Requires the Bioconductor packages \pkg{Spectra} and \pkg{MsBackendMgf}. The file is read using the MsBackendMgf backend. Keep the local input file available while using the spectra. Reading does not match spectra to a peak table, merge duplicate feature spectra, or apply feature filters.
#' @return A \code{Spectra} object containing the imported spectra and their metadata.
#' @seealso \code{\link{mc_filter_ms}}, \code{\link{mc_filter_mgf}}, \code{\link{mc_export}}
#' @examples
#' \dontrun{
#' # The peak table and labels are bundled; only MGF requires internet.
#' mgf_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/orbi_iimn_gnps.mgf"
#' )
#' mgf_file <- tempfile(fileext = ".mgf")
#' download.file(mgf_url, mgf_file, mode = "wb")
#' spectra <- mc_read_mgf(mgf_file)
#' head(Spectra::spectraData(spectra))
#' }
#' @export
mc_read_mgf <- function(path) {
  if (!requireNamespace("Spectra", quietly = TRUE) || !requireNamespace("MsBackendMgf", quietly = TRUE))
    stop("Install Bioconductor packages Spectra and MsBackendMgf for MGF support.")
  Spectra::Spectra(path, source = MsBackendMgf::MsBackendMgf())
}

#' Keep spectra matching retained feature identifiers
#'
#' @noMd
#' @description Subset a Spectra object using the feature IDs in a filtered result or a supplied table.
#' @param spectra A \code{Spectra} object to subset.
#' @param table A \code{metabocensor_result} or a data frame identifying the features to retain.
#' @param match_col Feature-ID column. With a result object, \code{NULL} uses its selected ID column; with a data frame, \code{NULL} uses the first column. An explicit name for a result object refers to the input-format exported table.
#' @param id_field Spectrum metadata field containing feature IDs. Default: \code{"acquisitionNum"}.
#' @details IDs are trimmed and matched as exact strings or finite numeric equivalents; for example, 12 and 12.0 match. Every spectrum with a matching ID is retained, including multiple spectra per feature. No MS-level restriction or spectral merging is performed. If no IDs match, an empty Spectra object is returned with a warning.
#' @return A subset of the supplied \code{Spectra} object.
#' @seealso \code{\link{mc_read_mgf}}, \code{\link{mc_write_mgf}}, \code{\link{mc_export}}
#' @examples
#' \dontrun{
#' # The peak table and labels are bundled; only MGF requires internet.
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' mgf_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/orbi_iimn_gnps.mgf"
#' )
#' mgf_file <- tempfile(fileext = ".mgf")
#' download.file(mgf_url, mgf_file, mode = "wb")
#' spectra <- mc_read_mgf(mgf_file)
#' filtered_spectra <- mc_filter_mgf(spectra, after_blank)
#' length(filtered_spectra)
#' }
#' @export
mc_filter_mgf <- function(spectra, table, match_col = NULL, id_field = "acquisitionNum") {
  if (!requireNamespace("Spectra", quietly = TRUE)) stop("Install Spectra for MGF support.")
  if (inherits(table, "metabocensor_result")) {
    if (is.null(match_col)) {
      table <- data.frame(id=table$table[[table$id_col]], stringsAsFactors=FALSE)
      match_col <- "id"
    } else table <- mc_final_table(table)
  }
  if (is.null(match_col)) match_col <- names(table)[1]
  if (!match_col %in% names(table)) stop("Table ID column not found.")
  sd <- Spectra::spectraData(spectra)
  if (!id_field %in% names(sd)) stop("Spectrum ID field not found.")
  sel_f <- trimws(as.character(table[[match_col]]))
  mgf_ids <- trimws(as.character(sd[[id_field]]))
  sel_ok <- !is.na(sel_f) & nzchar(sel_f)
  mgf_ok <- !is.na(mgf_ids) & nzchar(mgf_ids)
  idx_exact <- which(mgf_ok & mgf_ids %in% sel_f[sel_ok])
  mgf_num <- suppressWarnings(as.numeric(mgf_ids))
  sel_num <- suppressWarnings(as.numeric(sel_f[sel_ok]))
  sel_num <- sel_num[is.finite(sel_num)]
  idx_num <- if (length(sel_num)) which(is.finite(mgf_num) & mgf_num %in% sel_num) else integer(0)
  idx <- sort(unique(c(idx_exact, idx_num)))
  if (!length(idx)) warning("No matching spectra found; returning an empty Spectra object.", call. = FALSE)
  spectra[idx]
}

#' Write spectra to an MGF file
#'
#' @noMd
#' @description Export a Spectra object to a specified MGF file.
#' @param spectra A \code{Spectra} object to export.
#' @param path Output MGF filename, including the desired directory. An existing file may be overwritten.
#' @details Requires \pkg{Spectra} and \pkg{MsBackendMgf}. No additional feature filtering, spectral filtering, or merging is performed. Use \code{mc_filter_mgf()} first when exporting only retained features.
#' @return The output filename, returned invisibly.
#' @seealso \code{\link{mc_filter_mgf}}, \code{\link{mc_export}}
#' @examples
#' \dontrun{
#' # The peak table and labels are bundled; only MGF requires internet.
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' mgf_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/orbi_iimn_gnps.mgf"
#' )
#' mgf_file <- tempfile(fileext = ".mgf")
#' download.file(mgf_url, mgf_file, mode = "wb")
#' spectra <- mc_read_mgf(mgf_file)
#' filtered_spectra <- mc_filter_mgf(spectra, after_blank)
#' output_mgf <- file.path(tempdir(), "orbi_blank_filtered.mgf")
#' mc_write_mgf(filtered_spectra, output_mgf)
#' }
#' @export
mc_write_mgf <- function(spectra, path) {
  if (!requireNamespace("Spectra", quietly = TRUE) || !requireNamespace("MsBackendMgf", quietly = TRUE))
    stop("Install Spectra and MsBackendMgf for MGF export.")
  Spectra::export(spectra, backend = MsBackendMgf::MsBackendMgf(), file = path)
  invisible(path)
}

.mcs_check <- function(x) {
  if(!inherits(x,"metabocensor_result"))stop("Use mc_prepare() from this bundle first.",call.=FALSE)
  invisible(x)
}
.mcs_check_step <- function(x,step) {
  .mcs_check(x)
  if(step %in% names(x$removed_ids))stop("This object already contains a ",step,
    " result. Rerun from the object before that step; results are not stored as history.",call.=FALSE)
}
.mcs_matrix <- function(x) {
  if(nrow(x$table))return(.mcs_core$build_matrix_from_raw(x$table,sample_cols=x$sample_cols))
  as.data.frame(matrix(numeric(),nrow=length(x$sample_cols),ncol=0,dimnames=list(x$sample_cols,NULL)))
}
.mcs_finish <- function(x,table,step) {
  before<-nrow(x$table)
  x$removed_ids[[step]]<-setdiff(x$table$.FID,table$.FID)
  x$counts<-rbind(x$counts,data.frame(step=step,before=before,after=nrow(table),removed=before-nrow(table)))
  x$table<-table
  x
}

#' Remove features associated with blank samples
#'
#' @noMd
#' @description Compare feature intensities in blank and experimental groups, or remove every feature detected in a blank sample. Save the values and decisions for inspection and plotting.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param groups One or more existing group labels identifying blanks, for example \code{"Blank"}. All remaining groups are treated as experimental groups in ratio mode.
#' @param mode \code{"mean_cutoff"} (default) compares blank and experimental group means. \code{"drop_detected"} removes any feature with intensity greater than zero in any selected blank sample.
#' @param cutoff Nonnegative blank-to-sample ratio threshold, default 0.10. Used only in \code{"mean_cutoff"} mode. A ratio equal to the cutoff does not pass.
#' @param labels Optional sample-group labels. An unnamed vector must follow \code{x$sample_cols}; a named vector must cover those sample names exactly. \code{NULL} uses \code{x$labels}.
#' @details For each feature in \code{"mean_cutoff"} mode, calculate its mean intensity within each group. Let B be the mean of the selected blank-group means, and S the largest mean among the remaining groups. Retain the feature only when \code{B < cutoff * S}. Blank groups have equal weight even if their sample counts differ.
#'
#' For a positive cutoff, zero blank signal with positive sample signal passes. Positive blank signal with zero sample signal fails; zero signal in both also fails. The saved ratio is B/S, so zero denominators can produce infinite or undefined ratios. No artificial small denominator is added.
#'
#' Each filtering stage can be used once per result object. To change its settings, start again from the object saved before that stage. Separate stages may be called in any order; each receives only the features retained so far.
#' @return The updated result. \code{table} contains retained features, \code{removed_ids$blank} contains removed internal IDs, and \code{counts} gains a step summary. \code{blank} stores the mode and cutoff.
#'
#' \code{blank_tbl} includes every feature entering this step, including removed features. Ratio mode provides \code{Feature}, \code{Blank}, \code{Sample}, \code{Ratio}, \code{Status}, and \code{keep}. Detection mode provides \code{Feature}, \code{Detected}, \code{Status}, and \code{keep}. \code{Feature} corresponds to \code{.FID}; \code{keep} is the filtering decision.
#' @seealso \code{\link{mc_prepare}}, \code{\link{mc_plot_blank}}, \code{\link{mc_run}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' head(after_blank$blank_tbl)
#' mc_summary(after_blank)
#' mc_plot_blank(after_blank, interactive = FALSE)
#'
#' @export
mc_filter_blank <- function(x,groups,mode=c("mean_cutoff","drop_detected"),cutoff=.10,labels=NULL) {
  Label <- NULL
  .mcs_check_step(x,"blank");mode<-match.arg(mode);.mcs_scalar(cutoff,"cutoff",0)
  labs<-.mcs_labels(x,labels)
  if(!length(groups)||!all(groups %in% labs))stop("Select existing blank groups.")
  if(mode=="mean_cutoff"&&!any(!labs %in% groups))stop("At least one experimental group is required.")
  mat<-.mcs_matrix(x);feats<-x$table$.FID
  if(mode=="mean_cutoff") {
    if(length(feats)) {
      ds<-mat;ds$Label<-unname(labs)
      mt<-dplyr::summarise(dplyr::group_by(ds,Label),dplyr::across(dplyr::all_of(feats),~mean(.x,na.rm=TRUE)),.groups="drop")
      blanks<-mt[mt$Label %in% groups,feats,drop=FALSE]
      bv<-as.numeric(dplyr::summarise(blanks,dplyr::across(dplyr::everything(),~mean(.x,na.rm=TRUE)))[1,])
      exps<-mt[!mt$Label %in% groups,feats,drop=FALSE]
      ev<-vapply(exps,function(v)max(v,na.rm=TRUE),numeric(1))
    } else { bv<-ev<-numeric() }
    ratio<-bv/ev
    valid<-is.finite(bv)&is.finite(ev)&bv>=0&ev>=0
    status<-rep("Other / undefined",length(feats))
    status[valid&bv>0&ev>0&is.finite(ratio)]<-"Positive finite ratio"
    status[valid&bv==0&ev>0]<-"Zero blank / positive sample"
    status[valid&bv>0&ev==0]<-"Positive blank / zero sample"
    status[valid&bv==0&ev==0]<-"Both zero"
    keep<-bv<cutoff*pmax(ev,0);keep[is.na(keep)]<-FALSE
    tbl<-data.frame(Feature=feats,Blank=unname(bv),Sample=unname(ev),Ratio=unname(ratio),Status=status,keep=unname(keep))
  } else {
    detected<-vapply(mat[labs %in% groups,,drop=FALSE],function(v)any(v>0,na.rm=TRUE),logical(1))
    keep<-!detected
    tbl<-data.frame(Feature=feats,Detected=unname(detected),Status=ifelse(detected,"Filtered","Kept"),keep=unname(keep))
  }
  x$blank_tbl<-tbl;x$blank<-list(mode=mode,cutoff=cutoff)
  x$labels<-labs
  .mcs_finish(x,x$table[keep,,drop=FALSE],"blank")
}

#' Filter features by reproducibility and signal intensity
#'
#' @noMd
#' @description Calculate zero abundance, mean intensity, relative standard deviation, D-ratio, or minimum intensity for selected sample groups, then retain features that pass every selected metric.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param filters Character vector containing any of \code{"zeros"}, \code{"mean"}, \code{"rsd"}, \code{"dratio"}, and \code{"min"}. The default empty vector removes nothing.
#' @param mode How to combine samples or group-level filtering decisions.
#'   Default: \code{"group_any"}.
#'   \describe{
#'     \item{\code{"group_any"}}{For each selected metric, a feature must pass in at least one evaluated group. Different groups may satisfy different metrics.}
#'     \item{\code{"group_every"}}{For each selected metric, a feature must pass in every evaluated group.}
#'     \item{\code{"pooled"}}{Combine samples from the evaluated groups before calculating each metric.}
#'   }
#'   When multiple metrics are selected, a feature must pass all of them.
#' @param groups Group labels to evaluate. \code{NULL} or an empty vector selects all groups. For D-ratio, the group specified by \code{qc_group} is excluded from the biological denominator. Select biological groups explicitly to avoid including blanks.
#' @param zero_metric How to express zero intensities: \code{"count"} (default) counts samples with zero intensity; \code{"percent"} expresses these samples as a percentage of the evaluated samples. Used only when \code{"zeros"} is selected in \code{filters}.
#' @param zero_cutoff Zero-count or zero-percentage threshold. Default: \code{1}. Features pass only when their zero count or percentage is strictly less than this threshold. With \code{zero_metric = "count"}, a cutoff of 1 allows no zeros. With \code{zero_metric = "percent"}, a cutoff of 50 rejects features with exactly 50 percent zeros.
#' @param mean_cutoff Minimum arithmetic mean intensity. Equality passes. Default: 5000.
#' @param rsd_cutoff Maximum RSD in percent. Equality passes. Default: 30.
#' @param min_cutoff Minimum observed intensity. Equality passes. Default: 1000.
#' @param qc_group Exactly one existing pooled-QC group label for D-ratio, containing at least two samples. Required only when dratio is selected.
#' @param dratio_cutoff Maximum D-ratio in percent. Equality passes. Default: 50.
#' @param labels Optional sample-group labels. An unnamed vector must follow \code{x$sample_cols}; a named vector must cover those sample names exactly. \code{NULL} uses \code{x$labels}.
#' @details RSD is \code{100 * sd(intensity) / mean(intensity)} using the sample standard deviation. A zero or nonfinite mean gives an undefined RSD; undefined and infinite RSD values fail.
#'
#' D-ratio is \code{100 * sd(QC) / sd(biological)}. In group modes, each biological group has its own denominator; pooled mode uses all selected biological samples together. At least two finite observations are needed to calculate each standard deviation. A zero biological standard deviation gives an undefined ratio and fails, including when the QC standard deviation is also zero. A zero QC standard deviation with a positive biological standard deviation gives zero and passes.
#'
#' All metrics use the same input features and original intensity scale. In group_any mode, different metrics may pass in different groups. A feature must still pass every selected metric after group aggregation.
#'
#' Missing values are omitted from summary statistics. In group modes, missing comparison results are omitted by any/all; consequently, a feature with only missing comparisons for a metric fails group_any but passes group_every. Pooled mode requires finite metric values. Import normally converts missing intensity entries to zero.
#'
#' Each filtering stage can be used once per result object. To change its settings, start again from the object saved before that stage. Separate stages may be called in any order; each receives only the features retained so far.
#' @return The updated result with retained \code{table}, removed IDs in \code{removed_ids$qc}, and updated \code{counts}. \code{qc} stores mode, zero units, and cutoffs.
#'
#' \code{qc_tbl} contains \code{Feature}, \code{Label}, \code{metric}, \code{value}, \code{pass}, and \code{keep} for features entering the step. \code{pass} is the group-level comparison; \code{keep} is the group-aggregated decision for that metric, not the final decision across all metrics. Failures can overlap between metrics, so their counts should not be added.
#' @seealso \code{\link{mc_plot_qc}}, \code{\link{mc_filter_blank}}, \code{\link{mc_run}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
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
#' head(after_qc$qc_tbl)
#' mc_plot_qc(after_qc, filters = "rsd", interactive = FALSE)
#'
#' @export
mc_filter_qc <- function(x,filters=character(),mode=c("group_any","group_every","pooled"),
  groups=NULL,zero_metric=c("count","percent"),zero_cutoff=1,mean_cutoff=5000,
  rsd_cutoff=30,min_cutoff=1000,qc_group=NULL,dratio_cutoff=50,labels=NULL) {
  .mcs_check_step(x,"qc");mode<-match.arg(mode);zero_metric<-match.arg(zero_metric)
  if(any(!filters %in% c("zeros","mean","rsd","dratio","min")))stop("Unknown QC filter.")
  for(nm in c("zero_cutoff","mean_cutoff","rsd_cutoff","min_cutoff","dratio_cutoff")) .mcs_scalar(get(nm),nm,0)
  labs<-.mcs_labels(x,labels)
  if(length(groups)&&!all(groups %in% labs))stop("QC groups not found.")
  ds<-.mcs_matrix(x);ds$Label<-unname(labs);feats<-x$table$.FID
  if("dratio" %in% filters) {
    problem<-.mcs_core$dratio_problem(ds,groups,qc_group,dratio_cutoff)
    if(!is.null(problem))stop(problem,call.=FALSE)
  }
  input<-list(value_filters=filters,value_mode=mode,value_groups=groups,zero_metric=zero_metric,
    zero_cutoff=zero_cutoff,mean_cutoff=mean_cutoff,rsd_cutoff=rsd_cutoff,min_cutoff=min_cutoff,
    dratio_qc_group=qc_group,dratio_cutoff=dratio_cutoff)
  values<-if(length(filters)&&length(feats)).mcs_qc_values(ds,input) else list()
  cuts<-c(zeros=zero_cutoff,mean=mean_cutoff,rsd=rsd_cutoff,dratio=dratio_cutoff,min=min_cutoff)
  kept<-feats;rows<-list()
  for(metric in names(values)) {
    d<-as.data.frame(values[[metric]]$data);v<-d$value
    pass<-if(metric %in% c("rsd","dratio")) is.finite(v)&v<=cuts[metric] else if(metric=="zeros") v<cuts[metric] else v>=cuts[metric]
    if(mode=="pooled")pass<-is.finite(v)&pass
    pred<-if(mode=="group_every")all else any
    byfeat<-vapply(split(pass,d$Feature),pred,logical(1),na.rm=TRUE)
    metric_kept<-names(byfeat)[byfeat]
    kept<-intersect(kept,metric_kept)
    d$metric<-metric;d$pass<-pass;d$keep<-unname(byfeat[d$Feature])
    rows[[metric]]<-d[,c("Feature","Label","metric","value","pass","keep")]
  }
  x$qc_tbl<-if(length(rows))do.call(rbind,unname(rows)) else data.frame(Feature=character(),Label=character(),metric=character(),value=numeric(),pass=logical(),keep=logical())
  x$qc<-list(mode=mode,zero_metric=zero_metric,cutoffs=cuts[intersect(names(cuts),filters)])
  x$labels<-labs
  .mcs_finish(x,x$table[x$table$.FID %in% kept,,drop=FALSE],"qc")
}

#' Export the filtered peak table and matching spectra
#'
#' @noMd
#' @description Collect the current filtered peak table and optionally retain matching MGF spectra. Write either or both outputs when filenames are supplied.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param csv Optional output CSV filename. NULL returns the table without writing a CSV.
#' @param mgf Optional local MGF filename or \code{Spectra} object. Spectra are matched to retained feature IDs.
#' @param mgf_file Optional output MGF filename. Requires \code{mgf}.
#' @param id_field Spectrum metadata field containing feature IDs. Default: \code{"acquisitionNum"}.
#' @param match_col Optional ID column in the exported table. \code{NULL} uses the ID column selected at import, regardless of output layout.
#' @param format Output layout: \code{"input"} restores original column names and order, metadata columns, and XCMS retention times in seconds; \code{"standard"} returns Feature ID, mz, rt in minutes, and sample intensities; \code{"internal"} returns the current table including \code{.FID}.
#' @details All previously applied filters are already reflected in the current table. Export does not rerun filtering or restore removed rows. Matching retains all spectra associated with each retained feature, including duplicate spectra. No additional MS-level restriction is applied. Annotation and metric tables remain in the result object; they are not automatically written into the peak-table CSV.
#' @return A list with \code{table}, the final data frame, and \code{spectra}, the matching Spectra object or NULL. Requested CSV/MGF files are also written.
#' @seealso \code{\link{mc_final_table}}, \code{\link{mc_filter_mgf}}, \code{\link{mc_write_table}}
#' @examples
#' \dontrun{
#' # The peak table and labels are bundled; only MGF requires internet.
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' mgf_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/orbi_iimn_gnps.mgf"
#' )
#' mgf_file <- tempfile(fileext = ".mgf")
#' download.file(mgf_url, mgf_file, mode = "wb")
#' spectra <- mc_read_mgf(mgf_file)
#' ms_options <- mc_ms_parameters(
#'   iso_rt2 = 0.01, iso_cor2 = 0.80,
#'   iso_tol_type2 = "da", iso_mz_tol2 = 0.005,
#'   add_rt2 = 0.005, add_cor2 = 0.80,
#'   add_tol_type2 = "da", add_mz_tol2 = 0.005,
#'   add_pol2 = "positive",
#'   isf_rt2 = 0.01, isf_cor2 = 0.95,
#'   isf_ms2_check2 = TRUE, isf_mgf_id_field = "acquisitionNum",
#'   isf_ms2_mz_tol2 = 0.01, isf_ms2_rel2 = 1
#' )
#' after_ms <- mc_filter_ms(
#'   after_blank, filters = c("isotopes", "adducts", "isf"),
#'   parameters = ms_options, spectra = spectra
#' )
#' output <- mc_export(
#'   after_ms,
#'   csv = file.path(tempdir(), "orbi_filtered.csv"),
#'   mgf = spectra,
#'   mgf_file = file.path(tempdir(), "orbi_filtered.mgf")
#' )
#' head(output$table)
#' # Save annotation tables separately when needed:
#' write.csv(after_ms$annot_tbls$add_table,
#'           file.path(tempdir(), "orbi_adduct_annotations.csv"),
#'           row.names = FALSE)
#' }
#' @export
mc_export <- function(x,csv=NULL,mgf=NULL,mgf_file=NULL,id_field="acquisitionNum",match_col=NULL,
                      format=c("input","standard","internal")) {
  .mcs_check(x);format<-match.arg(format)
  table<-mc_final_table(x,format)
  spectra<-NULL
  if(!is.null(mgf_file)&&is.null(mgf))stop("Supply mgf to write filtered spectra.")
  if(!is.null(mgf)) {
    if(is.character(mgf)&&length(mgf)==1L)mgf<-mc_read_mgf(mgf)
    # Use the actual selected ID column rather than assuming the first column is ID.
    ids<-data.frame(id=x$table[[x$id_col]],stringsAsFactors=FALSE)
    spectra<-if(is.null(match_col))mc_filter_mgf(mgf,ids,"id",id_field) else mc_filter_mgf(mgf,table,match_col,id_field)
  }
  if(!is.null(csv))mc_write_table(x,csv,format)
  if(!is.null(mgf_file))mc_write_mgf(spectra,mgf_file)
  list(table=table,spectra=spectra)
}

#' Run several filtering stages in one call
#'
#' @noMd
#' @description Apply selected filtering stages in the fixed order blank, MS, QC, and peak filtering.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param blank \code{NULL} to skip blank filtering, or a named list of arguments to \code{mc_filter_blank()} other than x.
#' @param ms \code{NULL} to skip MS filtering, or a named list of arguments to \code{mc_filter_ms()} other than x.
#' @param qc \code{NULL} to skip QC filtering, or a named list of arguments to \code{mc_filter_qc()} other than x.
#' @param peak \code{NULL} to skip peak filtering, or a named list of arguments to \code{mc_filter_peak()} other than x.
#' @details The output of each stage is the input to the next. Use individual filter functions when a different stage order is required. Each stage can be applied once per object. To compare workflows or thresholds, start each run from a saved imported or upstream object. Export is a separate call to \code{mc_export()}.
#' @return A \code{metabocensor_result} containing the retained table, step counts, removed IDs, and calculated result tables for the stages run.
#' @seealso \code{\link{mc_filter_blank}}, \code{\link{mc_filter_ms}}, \code{\link{mc_filter_qc}}, \code{\link{mc_filter_peak}}, \code{\link{mc_export}}
#' @examples
#' \dontrun{
#' # The peak table and labels are bundled; only MGF requires internet.
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' mgf_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/orbi_iimn_gnps.mgf"
#' )
#' mgf_file <- tempfile(fileext = ".mgf")
#' download.file(mgf_url, mgf_file, mode = "wb")
#' spectra <- mc_read_mgf(mgf_file)
#' ms_options <- mc_ms_parameters(
#'   iso_rt2 = 0.01, iso_cor2 = 0.80,
#'   iso_tol_type2 = "da", iso_mz_tol2 = 0.005,
#'   add_rt2 = 0.005, add_cor2 = 0.80,
#'   add_tol_type2 = "da", add_mz_tol2 = 0.005,
#'   add_pol2 = "positive",
#'   isf_rt2 = 0.01, isf_cor2 = 0.95,
#'   isf_ms2_check2 = TRUE, isf_mgf_id_field = "acquisitionNum",
#'   isf_ms2_mz_tol2 = 0.01, isf_ms2_rel2 = 1
#' )
#' after_run <- mc_run(
#'   imported,
#'   blank = list(groups = "Blank", mode = "mean_cutoff", cutoff = 0.10),
#'   ms = list(
#'     filters = c("isotopes", "adducts", "isf"),
#'     parameters = ms_options, spectra = spectra
#'   ),
#'   qc = NULL,
#'   peak = NULL
#' )
#' mc_summary(after_run)
#' }
#' @export
mc_run <- function(x,blank=NULL,ms=NULL,qc=NULL,peak=NULL) {
  steps<-list(blank=blank,ms=ms,qc=qc,peak=peak)
  funs<-list(blank=mc_filter_blank,ms=mc_filter_ms,qc=mc_filter_qc,peak=mc_filter_peak)
  for(step in names(steps))if(!is.null(steps[[step]]))x<-do.call(funs[[step]],c(list(x=x),steps[[step]]))
  x
}

#' Summarize feature counts after filtering
#'
#' @noMd
#' @description Return feature counts before and after each completed filtering stage.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @details Counts are reported in execution order. For individual MS subfilters, inspect \code{x$ms$counts}. Metric and annotation tables remain accessible in the object and are not printed automatically.
#' @return A data frame with \code{step}, \code{before}, \code{after}, and \code{removed}.
#' @seealso \code{\link{mc_prepare}}, \code{\link{mc_plot_summary}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' mc_summary(after_blank)
#'
#' @export
mc_summary <- function(x) { .mcs_check(x);x$counts }
#' Print a filtering-result summary
#'
#' @noMd
#' @description Display the current numbers of features and samples, followed by stage-level filtering counts.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param ... Additional arguments; currently unused.
#' @details Counts are reported in execution order. For individual MS subfilters, inspect \code{x$ms$counts}. Metric and annotation tables remain accessible in the object and are not printed automatically.
#' @return The supplied object \code{x}, invisibly.
#' @seealso \code{\link{mc_prepare}}, \code{\link{mc_plot_summary}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' print(after_blank)
#'
#' @export
print.metabocensor_result <- function(x,...) {
  cat("MetaboCensoR:",nrow(x$table),"features;",length(x$sample_cols),"samples\n")
  if(nrow(x$counts))print(x$counts,row.names=FALSE)
  invisible(x)
}


.mcs_qc_values <- function(dataset,input) {
    Label <- NULL
    ds <- dataset
    feats <- numeric_feature_names(ds)
    .mc_validate(.mc_need(length(feats) > 0, "No numeric features to plot."))

    filters <- input$value_filters %||% character(0)
    .mc_validate(.mc_need(length(filters) > 0, "Select at least one QC value filter to plot."))

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

      .mc_validate(.mc_need(
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
        st <- ds %>% group_by(Label) %>% summarise(across(all_of(feats), ~ { v <- suppressWarnings(min(.x, na.rm=TRUE)); if(is.finite(v)) v else NA_real_ }), .groups="drop") %>% filter(Label %in% labs_sel)
        long <- st %>% pivot_longer(-Label, names_to="Feature", values_to="value")
      } else {
        ds_sub <- ds %>% filter(Label %in% labs_sel)
        vec <- vapply(ds_sub[feats], function(x) { v <- suppressWarnings(min(x, na.rm=TRUE)); if(is.finite(v)) v else NA_real_ }, numeric(1))
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
  }
environment(.mcs_qc_values) <- .mcs_core


#' Prepare an LC-MS peak table for filtering
#'
#' @noMd
#' @description Read a feature-by-sample peak table, identify intensity columns, and assign sample groups. Feature metadata are retained alongside the intensity values.
#' @param data A local CSV filename or a data frame with one feature per row. Bundled files can be located with \code{system.file()}. For a web-hosted CSV, first read it with \code{vroom::vroom(url, delim = ",")} and pass the resulting table.
#' @param type Input format: \code{"mzmine"} (default), \code{"generic"}, \code{"xcms"}, or \code{"msdial"}. XCMS retention times are converted from seconds to minutes. Other formats require retention times in minutes. See \href{https://github.com/plyush1993/MetaboCensoR/tree/main/Input_Examples}{compatible peak-table examples} for the supported input layouts.
#' @param sample_cols Intensity-column names or integer column positions after format conversion and column cleanup. Explicit selections take precedence over \code{sample_keywords}.
#' @param sample_keywords Case-sensitive literal substrings used to identify intensity columns. A column is selected if its name contains at least one substring. If neither selector is supplied, columns with at least 70\% finite numeric entries are considered, excluding identified feature columns and names beginning with row.
#' @param id_col Feature-ID column name. If omitted, searches for row ID, Feature ID, or Alignment ID, then uses the first column. Original names recognized during format conversion may also be supplied.
#' @param mz_col Feature m/z column name. Defaults to the standardized \code{mz} column; recognized original column names are accepted.
#' @param rt_col Retention-time column name. Defaults to the standardized \code{rt} column; recognized original column names are accepted.
#' @param labels One nonempty group label per sample. An unnamed vector must follow the selected sample columns; a named vector is matched to their names. If omitted, groups are extracted from sample names using \code{token_sep} and \code{token_index}.
#' @param token_sep Literal separator used to split sample names when labels are omitted. Default: \code{"_"}.
#' @param token_index One-based position of the group name in each split sample name. Default: 2.
#' @details Missing or nonnumeric intensity entries become zero. Values are not normalized or log-transformed. A unique internal identifier, \code{.FID}, is added to every feature; original IDs remain available for export. Explicit sample-column selection avoids accidentally treating numeric metadata as intensities.
#'
#' Group labels can be replaced before filtering with \code{mc_labels_from_metadata()}, \code{mc_labels_from_csv()}, or \code{mc_labels_from_names()}.
#'
#' The object stores the current peak table rather than a second copy of the original table. Save the imported object if you want to compare alternative settings.
#' @return A \code{metabocensor_result} list containing:
#' \describe{
#' \item{table}{Current peak table, including metadata, sample intensities, and \code{.FID}.}
#' \item{sample_cols, labels}{Intensity-column names and a named sample-group vector.}
#' \item{id_col, mz_col, rt_col}{Selected identifier, m/z, and retention-time column names.}
#' \item{format}{Column and format information needed to restore the input layout on export.}
#' \item{removed_ids, counts}{Removed internal IDs and feature counts, populated by subsequent filtering.}
#' }
#' @seealso \code{\link{mc_labels_from_metadata}}, \code{\link{mc_filter_blank}}, \code{\link{mc_export}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' table(imported$labels)
#' head(imported$table)
#'
#' @export
mc_prepare <- function(data, type = c("mzmine", "generic", "xcms", "msdial"),
                       sample_cols = NULL, sample_keywords = NULL,
                       id_col = NULL, mz_col = NULL, rt_col = NULL,
                       labels = NULL, token_sep = "_", token_index = 2) {
  type <- match.arg(type)
  if (is.character(data) && length(data) == 1L) {
    if (!file.exists(data)) stop("Peak table does not exist: ", data)
    data <- if (type == "msdial") {
      as.data.frame(vroom::vroom(data, delim = ",", col_names = FALSE,
                                col_types = vroom::cols(.default = "c"), na = ""))
    } else as.data.frame(vroom::vroom(data, delim = ",", show_col_types = FALSE))
  }
  if (!is.data.frame(data) || !nrow(data)) stop("Supply a nonempty peak table.")
  df <- .mcs_core$standardize_peak_table(data, type)
  if (anyDuplicated(names(df))) stop("Peak-table column names must be unique.")
  export <- list(type = type, export_template = attr(df, "export_template"),
                 export_colmap = attr(df, "export_colmap"),
                 export_rt_factor = attr(df, "export_rt_factor"),
                 msdial_preamble = attr(df, "msdial_preamble"),
                 msdial_export_names = attr(df, "msdial_export_names"))
  df <- .mcs_core$clean_mzmine_export(df)
  if (is.null(id_col)) {
    choices <- c("row ID", "Feature ID", "Alignment ID", names(df)[1])
    id_col <- choices[choices %in% names(df)][1]
  }
  if (is.null(mz_col)) mz_col <- "mz"
  if (is.null(rt_col)) rt_col <- "rt"
  resolve_col <- function(nm) {
    if (nm %in% names(df)) return(nm)
    map <- export$export_colmap
    if (length(map) && nm %in% unname(map)) return(names(map)[match(nm, unname(map))])
    nm
  }
  id_col <- resolve_col(id_col); mz_col <- resolve_col(mz_col); rt_col <- resolve_col(rt_col)
  for (nm in c(id_col, mz_col, rt_col))
    if (!nm %in% names(df)) stop("Column not found after standardization: ", nm)
  if (is.numeric(sample_cols)) {
    if(anyNA(sample_cols) || any(sample_cols != as.integer(sample_cols)) ||
       any(sample_cols < 1 | sample_cols > ncol(df))) stop("Invalid sample column positions.")
    sample_cols <- names(df)[sample_cols]
  }
  if (is.null(sample_cols)) {
    meta <- unique(c(".FID", id_col, mz_col, rt_col, "mz", "rt"))
    if (length(sample_keywords)) {
      sample_cols <- setdiff(names(df)[.mcs_core$multi_sample_idx(names(df), sample_keywords)], meta)
    } else {
      cand <- setdiff(names(df), meta)
      cand <- cand[!grepl("^row\\b", cand, ignore.case = TRUE)]
      prop <- vapply(df[cand], function(v) mean(is.finite(suppressWarnings(
        as.numeric(as.character(v)))), na.rm = TRUE), numeric(1))
      sample_cols <- cand[prop >= 0.7]
    }
  }
  if (!length(sample_cols) || anyDuplicated(sample_cols) ||
      !all(sample_cols %in% names(df))) stop("Specify valid, unique sample_cols.")
  if (any(sample_cols %in% c(id_col, mz_col, rt_col, ".FID")))
    stop("Sample columns cannot include ID, m/z or RT columns.")
  ids <- .mcs_core$make_internal_ids(df, id_col, mz_col, rt_col)
  df$.FID <- ids$ids_unique
  if ("Label" %in% df$.FID) stop("Feature ID 'Label' conflicts with the app's group column; rename this ID.")
  for (nm in sample_cols) {
    v <- suppressWarnings(as.numeric(df[[nm]])); v[is.na(v)] <- 0; df[[nm]] <- v
  }
  x <- structure(list(table = df, sample_cols = sample_cols,
                       id_col = id_col, mz_col = mz_col, rt_col = rt_col,
                       format = export, removed_ids = list(),
                       counts = data.frame(step=character(), before=integer(), after=integer(), removed=integer())),
                 class = "metabocensor_result")
  if (is.null(labels)) labels <- mc_labels_from_names(sample_cols, token_sep, token_index)
  x$labels <- .mcs_labels(x, labels)
  x
}

#' Reduce redundant ion features and annotate their relationships
#'
#' @noMd
#' @description Apply isotope, adduct, neutral-loss, in-source-fragment, mispicked-ion, and ringing-artifact filters. Retain representative features and expose the annotation tables for inspection.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param filters Any of \code{"isotopes"}, \code{"adducts"}, \code{"neutral_losses"}, \code{"isf"}, \code{"mispicked"}, and \code{"ringing"}. An explicit vector overrides enable flags in parameters. \code{NULL} uses those flags; isotopes and adducts are enabled by default. \code{character(0)} disables all subfilters.
#' @param parameters Named list from \code{mc_ms_parameters()}. Pass your list explicitly with \code{parameters = ms_options}.
#' @param adducts Custom adduct data frame or local CSV filename. NULL uses built-in definitions. Required columns are described below.
#' @param neutral_losses Custom neutral-loss data frame or local CSV filename. NULL uses built-in definitions. Required columns are described below.
#' @param spectra A \code{Spectra} object, for example from \code{mc_read_mgf()}. Required for ISF when MS2 checking is enabled.
#' @details Filters run in this order, irrespective of their order in \code{filters}:
#' \describe{
#' \item{isotopes}{Find candidate isotope/dimer-pattern mass shifts within retention-time and Pearson-correlation thresholds. Connected families retain their highest-mean-intensity feature.}
#' \item{adducts}{Convert candidate ion forms to neutral-mass intervals and connect compatible features within retention-time and correlation tolerances. Optional strict retention-time control limits family-wide RT spread. Each family retains its highest-mean-intensity feature.}
#' \item{neutral_losses}{Find correlated, coeluting pairs separated by a listed neutral-loss mass. Remove the lower-m/z member of accepted pairs.}
#' \item{isf}{Find correlated, coeluting lower-m/z candidate fragments, optionally constrained by fragment-to-precursor intensity ratio. With MS2 checking enabled, require the candidate fragment mass in a matching precursor spectrum above the relative-intensity threshold. A match in any eligible spectrum for that precursor is sufficient. Remove accepted lower-m/z fragments.}
#' \item{mispicked}{Group nearly coincident, correlated features. Keep the highest-mean-intensity representative and sum family intensities into it for each sample.}
#' \item{ringing}{Identify lower-intensity, correlated signals near a sufficiently intense anchor, subject to m/z, RT, and intensity-ratio limits; remove accepted artifacts.}
#' }
#'
#' Pearson correlations and mean intensities are computed across all sample columns in the input object. The MS step does not select samples by group label. Each subfilter receives the features remaining after the preceding subfilter.
#'
#' Adduct tables require \code{Name}, \code{Charge}, \code{Mult}, and \code{Mass}; optional \code{Ion_mode} selects positive or negative entries. Neutral mass is calculated as \code{(mz - Mass) * abs(Charge) / Mult}. If required adduct columns are missing, a warning is issued and built-in definitions are used.
#'
#' Neutral-loss tables require \code{Accurate Mass} and \code{Neutral Loss}. Optional \code{Pos} and \code{Neg} columns restrict definitions by polarity; \code{Formula} and \code{substructure or compound class} provide extra annotations.
#'
#' Use \code{isf_ms2_check2 = FALSE} explicitly for MS1-only ISF filtering. Otherwise provide spectra with IDs matching the peak table. See \code{mc_ms_parameters()} for all tolerances and switches.
#'
#' Each filtering stage can be used once per result object. To change its settings, start again from the object saved before that stage. Separate stages may be called in any order; each receives only the features retained so far.
#' @return The updated result with retained \code{table}, \code{removed_ids$ms}, and updated \code{counts}. \code{ms$counts} reports sequential removal counts by subfilter; \code{ms$isf_status} records ISF processing status.
#'
#' \code{annot_tbls} contains \code{iso_table}, \code{add_table}, \code{nl_table}, \code{isf_table}, \code{mis_table}, and \code{ring_table}. These tables describe feature relationships, masses/retention times, correlation evidence, and retention status where applicable. Tables may be NULL when a step is disabled or yields no annotation table; inspect names before subsetting columns. Annotation rows are not necessarily one row per removed feature.
#'
#' \code{merge_map} records mispicked-feature merges when produced. Spectra are not stored in the result; keep the supplied Spectra object for later export.
#' @seealso \code{\link{mc_ms_parameters}}, \code{\link{mc_read_mgf}}, \code{\link{mc_plot_ms}}, \code{\link{mc_export}}
#' @examples
#' \dontrun{
#' # The peak table and labels are bundled; only MGF requires internet.
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' mgf_url <- paste0(
#'   "https://raw.githubusercontent.com/plyush1993/",
#'   "MetaboCensoR_Examples/main/orbi/orbi_iimn_gnps.mgf"
#' )
#' mgf_file <- tempfile(fileext = ".mgf")
#' download.file(mgf_url, mgf_file, mode = "wb")
#' spectra <- mc_read_mgf(mgf_file)
#' ms_options <- mc_ms_parameters(
#'   iso_rt2 = 0.01, iso_cor2 = 0.80,
#'   iso_tol_type2 = "da", iso_mz_tol2 = 0.005,
#'   add_rt2 = 0.005, add_cor2 = 0.80,
#'   add_tol_type2 = "da", add_mz_tol2 = 0.005,
#'   add_pol2 = "positive",
#'   isf_rt2 = 0.01, isf_cor2 = 0.95,
#'   isf_ms2_check2 = TRUE, isf_mgf_id_field = "acquisitionNum",
#'   isf_ms2_mz_tol2 = 0.01, isf_ms2_rel2 = 1
#' )
#' after_ms <- mc_filter_ms(
#'   after_blank, filters = c("isotopes", "adducts", "isf"),
#'   parameters = ms_options, spectra = spectra
#' )
#' after_ms$ms$counts
#' head(after_ms$annot_tbls$add_table)
#' head(after_ms$annot_tbls$isf_table)
#' }
#' @export
mc_filter_ms <- function(x, filters = NULL, parameters = mc_ms_parameters(),
                         adducts = NULL, neutral_losses = NULL, spectra = NULL) {
  .mcs_check_step(x,"ms")
  mat <- .mcs_matrix(x)
  ft <- if(ncol(mat)) .mcs_core$build_feature_meta(x$table,x$mz_col,x$rt_col,mat) else data.frame()
  parameters <- do.call(mc_ms_parameters, parameters)
  switches <- c(isotopes = "enable_iso2", adducts = "enable_add2",
                 neutral_losses = "enable_nl2", isf = "enable_isf2",
                 mispicked = "enable_mis2", ringing = "enable_ring2")
  if (!is.null(filters)) {
    if (any(!filters %in% names(switches))) stop("Unknown MS filter.")
    for (nm in names(switches)) parameters[[switches[[nm]]]] <- nm %in% filters
  }
  if (isTRUE(parameters$enable_isf2) && isTRUE(parameters$isf_ms2_check2)) {
    if (is.null(spectra) || !length(spectra)) stop("MS2-confirmed ISF requires spectra.")
    if (!parameters$isf_mgf_id_field %in% names(Spectra::spectraData(spectra)))
      stop("ISF spectrum ID field not found.")
  }
  adduct_table <- .mcs_aux_table(adducts); nl_table <- .mcs_aux_table(neutral_losses)
  if (!ncol(mat)) {
    res <- list(stats = list(before = 0L, after = 0L, removed_iso = 0L,
      removed_add = 0L, removed_nl = 0L, removed_isf = 0L, removed_mis = 0L, removed_ring = 0L))
    res$matrix <- mat
    res$mis_merge_map <- NULL
  }
  if (ncol(mat)) res <- .mcs_core$ms_engine(mat, ft, parameters,
                            adduct_table, nl_table, spectra)
  raw <- x$table
  if (!is.null(res$mis_merge_map) && nrow(res$mis_merge_map))
    raw <- .mcs_core$apply_merge_map_to_raw(raw, res$mis_merge_map, x$sample_cols)
  raw <- raw[raw$.FID %in% colnames(res$matrix), , drop = FALSE]
  keys <- c("iso_table","add_table","nl_table","isf_table","mis_table","ring_table")
  x$annot_tbls <- stats::setNames(lapply(keys,function(k)res[[k]]),keys)
  x$merge_map <- res$mis_merge_map
  x$ms <- list(enabled=names(switches)[vapply(switches,function(k)isTRUE(parameters[[k]]),logical(1))],
               counts=res$stats, isf_status=res$isf_status)
  .mcs_finish(x,raw,"ms")
}

#' Filter features by m/z, retention time, mass defect, or target list
#'
#' @noMd
#' @description Apply selected feature-property bounds or match a list of m/z and retention-time targets. Store values and filtering decisions for plotting.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param filters Any of \code{"mz"}, \code{"rt"}, \code{"rmd"}, \code{"amd"}, and \code{"target"}. \code{NULL} uses \code{parameters$peak_filters}; the default selects no filters.
#' @param parameters Named list from \code{mc_peak_parameters()}, specifying enabled bounds and target-matching tolerances.
#' @param targets Target data frame or local CSV filename with m/z and retention time in minutes. Use columns named \code{mz} and \code{rt}; if recognizable names are absent, the first two columns are used.
#' @param target_text Optional comma-separated target rows as one string, with or without a header. These rows are combined with targets and duplicate m/z-RT pairs are removed.
#' @details All selected filters must pass. Minimum and maximum bounds are inclusive and enabled independently. Nonfinite property values fail an active bound.
#'
#' Nominal mass is \code{floor(mz + 0.5)}. Relative mass defect (RMD) is \code{1e6 * (mz - nominal_mass) / mz}, in ppm; absolute mass defect (AMD) is \code{abs(mz - nominal_mass)}, in Da. These calculations use the observed m/z, not a reconstructed neutral mass.
#'
#' Target matching requires both m/z and RT to fall within their tolerances. \code{target_peak_action = "remove"} removes matches; \code{"keep"} retains only matches. Nonfinite target rows are discarded. An empty usable target list does not filter features; a nonempty keep-only list with no matches raises an error.
#'
#' Each filtering stage can be used once per result object. To change its settings, start again from the object saved before that stage. Separate stages may be called in any order; each receives only the features retained so far.
#' @return The updated result with retained \code{table}, \code{removed_ids$peak}, and updated \code{counts}. \code{peak} stores the settings and \code{target_matches} records accepted target-feature matches and mass/RT differences.
#'
#' \code{peak_tbl} includes all input features with \code{Feature}, \code{mz}, \code{RT}, \code{RMD}, \code{AMD}, per-filter \code{pass_*} columns, \code{target_hit}, and final \code{keep}.
#' @seealso \code{\link{mc_peak_parameters}}, \code{\link{mc_plot_peak}}, \code{\link{mc_run}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
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
#' head(after_peak$peak_tbl)
#' mc_plot_peak(after_peak, interactive = FALSE)
#'
#' @export
mc_filter_peak <- function(x, filters = NULL, parameters = mc_peak_parameters(),
                           targets = NULL, target_text = NULL) {
  target_mz <- target_rt <- NULL
  .mcs_check_step(x,"peak"); parameters <- do.call(mc_peak_parameters, parameters)
  if (!is.null(filters)) parameters$peak_filters <- filters
  if (any(!parameters$peak_filters %in% c("mz", "rt", "rmd", "amd", "target"))) stop("Unknown Peak filter.")
  if (!parameters$target_peak_action %in% c("remove", "keep")) stop("Target action must be remove or keep.")
  parameters$mz_col0 <- x$mz_col; parameters$rt_col0 <- x$rt_col
  target_df <- if (is.data.frame(targets)) {
    .mcs_core$extract_peak_targets(targets, source = "csv")
  } else {
    if (!is.null(targets) && (!is.character(targets) || length(targets) != 1 || !file.exists(targets)))
      stop("targets must be a data.frame or an existing CSV path.")
    .mcs_core$read_peak_targets(upload_input = if (!is.null(targets)) list(datapath = targets) else NULL)
  }
  # Typed entries precede CSV entries, as in the app.
  target_df <- dplyr::distinct(dplyr::bind_rows(
    .mcs_core$read_peak_targets(text_input = target_text), target_df), target_mz, target_rt, .keep_all = TRUE)
  matches <- .mcs_core$find_target_peak_matches(x$table, ".FID", x$mz_col, x$rt_col, target_df,
    parameters$target_mz_tol_type, parameters$target_mz_tol_da,
    parameters$target_mz_tol_ppm, parameters$target_rt_tol)
  res <- .mcs_core$peak_engine(x$table, parameters, target_df, matches)
  raw <- x$table[x$table$.FID %in% res$keep, , drop = FALSE]
  x$peak_tbl <- res$values
  x$target_matches <- res$target_matches
  x$peak <- parameters
  .mcs_finish(x,raw,"peak")
}

#' Extract the current filtered peak table
#'
#' @noMd
#' @description Return retained features as a data frame in the original, standardized, or internal layout.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param format Output layout: \code{"input"} restores original column names and order, metadata columns, and XCMS retention times in seconds; \code{"standard"} returns Feature ID, mz, rt in minutes, and sample intensities; \code{"internal"} returns the current table including \code{.FID}.
#' @details Uses the current result without rerunning filters. Removed features are not restored. Summed intensities from mispicked-feature merging are included. In input format, retained metadata columns accompany the intensities. An MS-DIAL preamble is written by \code{mc_write_table()} rather than included as rows in this data frame.
#' @return A data frame with one row per retained feature.
#' @seealso \code{\link{mc_write_table}}, \code{\link{mc_export}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' final_table <- mc_final_table(after_blank, format = "input")
#' head(final_table)
#' standard_table <- mc_final_table(after_blank, format = "standard")
#'
#' @export
mc_final_table <- function(x, format = c("input", "standard", "internal")) {
  .mcs_check(x); format <- match.arg(format)
  raw <- .mcs_core$clean_mzmine_export(x$table)
  if (format == "internal") return(raw)
  if (format == "standard") {
    out <- raw[, c(x$id_col, x$mz_col, x$rt_col, x$sample_cols), drop = FALSE]
    names(out)[1:3] <- c("Feature ID", "mz", "rt")
    return(out)
  }
  .mcs_core$format_final_table_as_input(raw, x$format$type, x$format$export_template,
    x$format$export_colmap, x$format$export_rt_factor)
}

#' Write the current filtered peak table to CSV
#'
#' @noMd
#' @description Export retained features to a CSV file with a chosen column layout.
#' @param x A \code{metabocensor_result} returned by \code{mc_prepare()} or a filtering function.
#' @param path Output CSV filename, including the desired directory. Existing output is overwritten.
#' @param format Output layout: \code{"input"} restores original column names and order, metadata columns, and XCMS retention times in seconds; \code{"standard"} returns Feature ID, mz, rt in minutes, and sample intensities; \code{"internal"} returns the current table including \code{.FID}.
#' @details Input-format export restores retained metadata, column names and order, and XCMS retention-time units. For MS-DIAL input, the saved preamble is written before the feature table. Use \code{mc_final_table()} to obtain a data frame without writing a file.
#' @return The output filename, returned invisibly.
#' @seealso \code{\link{mc_final_table}}, \code{\link{mc_export}}
#' @examples
#' peak_file <- system.file(
#'   "extdata", "orbi_iimn_gnps_quant.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported <- mc_prepare(
#'   peak_file, type = "mzmine",
#'   sample_keywords = c(".mzML", "Peak area")
#' )
#' metadata_file <- system.file(
#'   "extdata", "metadata.csv",
#'   package = "MetaboCensoR", mustWork = TRUE
#' )
#' imported$labels <- mc_labels_from_metadata(
#'   metadata_file, sample_names = imported$sample_cols,
#'   sample_col = "Sample name", label_col = "Label"
#' )
#' after_blank <- mc_filter_blank(
#'   imported, groups = "Blank", mode = "mean_cutoff", cutoff = 0.10
#' )
#' output_csv <- file.path(tempdir(), "orbi_blank_filtered.csv")
#' mc_write_table(after_blank, output_csv, format = "input")
#'
#' @export
mc_write_table <- function(x, path, format = c("input", "standard", "internal")) {
  format <- match.arg(format); df <- mc_final_table(x, format)
  if (format == "input") {
    .mcs_core$write_final_table_csv(path, df, x$format$type,
      x$format$msdial_preamble, x$format$msdial_export_names)
  } else utils::write.csv(df, path, row.names = FALSE, na = "")
  invisible(path)
}
