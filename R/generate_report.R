# =============================================================================
# generate_report.R
# =============================================================================

#' Generate an automatic HTML or PDF pest risk report
#'
#' @description
#' Compiles all analysis results (data summary, model performance, risk maps,
#' variable importance, regional statistics) into a reproducible, shareable
#' HTML or PDF report using R Markdown.
#'
#' @param species_name `character`. Common or scientific name of the pest.
#' @param occurrences_sf `sf` object. Cleaned occurrence data from
#'   [clean_occurrences()].
#' @param clean_report `data.frame`. Cleaning report from [clean_occurrences()].
#' @param eval_results `list`. Output of [evaluate_models()].
#' @param risk_map_obj `list`. Output of [predict_risk_map()].
#' @param country_summary `data.frame`. Output of [summarize_country_risk()].
#' @param model_obj `list`. Fitted model (RF or MaxEnt) for variable importance.
#' @param output_file `character`. Path to the output report file. Extension
#'   determines format: `.html` (default, recommended) or `.pdf`.
#' @param title `character`. Report title. Default auto-generated.
#' @param author `character`. Author name for the report.
#' @param verbose `logical`. Print progress (default `TRUE`).
#'
#' @return Invisibly returns `output_file`. The report is written to disk.
#'
#' @details
#' The report is rendered from the package's built-in R Markdown template
#' (`inst/extdata/report_template.Rmd`) using [rmarkdown::render()].
#'
#' ## Report sections
#' 1. **Executive Summary** — key findings in plain language
#' 2. **Data Sources** — GBIF records, WorldClim variables used
#' 3. **Data Cleaning** — step-by-step cleaning report table
#' 4. **Model Performance** — AUC, Sensitivity, Specificity table + ROC curves
#' 5. **Suitability Map** — continuous habitat suitability map
#' 6. **Risk Classification Map** — Low / Medium / High map
#' 7. **Variable Importance** — which climate drivers matter most
#' 8. **Regional Risk Ranking** — top vulnerable countries/regions
#' 9. **Ecological Interpretation** — agronomic implications
#' 10. **Methods** — reproducible methodology description
#'
#' @examples
#' \dontrun{
#' generate_report(
#'   species_name    = "Spodoptera frugiperda",
#'   occurrences_sf  = clean_occ$data,
#'   clean_report    = clean_occ$report,
#'   eval_results    = eval_out,
#'   risk_map_obj    = risk_out,
#'   country_summary = country_tbl,
#'   model_obj       = rf_out,
#'   output_file     = "pestRisk_Sf_report.html",
#'   author          = "Meriem Benali"
#' )
#' }
#'
#' @importFrom rmarkdown render
#' @export
generate_report <- function(species_name,
                             occurrences_sf,
                             clean_report,
                             eval_results,
                             risk_map_obj,
                             country_summary,
                             model_obj,
                             output_file  = "pestRisk_report.html",
                             title        = NULL,
                             author       = "pestRisk package",
                             verbose      = TRUE) {

  .check_pkg("rmarkdown")
  .check_pkg("knitr")

  if (is.null(title))
    title <- paste0("Pest Invasion Risk Report – ", species_name)

  # ---- Locate template -------------------------------------------------------
  template <- system.file("extdata", "report_template.Rmd",
                           package = "pestRisk")
  if (!file.exists(template))
    stop("[pestRisk] Report template not found. Reinstall the package.",
         call. = FALSE)

  # ---- Set output format from extension -------------------------------------
  ext    <- tolower(tools::file_ext(output_file))
  format <- switch(ext,
                   html = "html_document",
                   pdf  = "pdf_document",
                   {
                     warning("[pestRisk] Unknown extension '", ext,
                             "', defaulting to HTML.", call. = FALSE)
                     "html_document"
                   })

  if (verbose) message("[pestRisk] Rendering ", toupper(ext), " report for: ",
                       species_name, " …")

  # ---- Prepare output path --------------------------------------------------
  output_dir  <- dirname(normalizePath(output_file, mustWork = FALSE))
  output_base <- basename(output_file)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # ---- Save plots to temp files for embedding --------------------------------
  tmp_dir <- tempdir()

  suit_map_file <- file.path(tmp_dir, "suit_map.png")
  risk_map_file <- file.path(tmp_dir, "risk_map.png")
  roc_file      <- file.path(tmp_dir, "roc.png")
  imp_file      <- file.path(tmp_dir, "importance.png")

  plot_risk_map(risk_map_obj, type = "continuous",
                occurrences_sf = occurrences_sf,
                species_name   = species_name,
                output_file    = suit_map_file, verbose = FALSE)
  plot_risk_map(risk_map_obj, type = "classified",
                species_name   = species_name,
                output_file    = risk_map_file, verbose = FALSE)
  if (!is.null(eval_results$roc_plot))
    ggplot2::ggsave(roc_file, eval_results$roc_plot,
                    width = 7, height = 5, dpi = 200)
  tryCatch(
    plot_variable_importance(model_obj, output_file = imp_file, verbose = FALSE),
    error = function(e) NULL
  )

  # ---- Pass parameters to Rmd template --------------------------------------
  params <- list(
    species_name    = species_name,
    title           = title,
    author          = author,
    n_occ           = nrow(occurrences_sf),
    clean_report    = clean_report,
    metrics_df      = eval_results$metrics,
    country_summary = country_summary,
    suit_map_file   = suit_map_file,
    risk_map_file   = risk_map_file,
    roc_file        = if (file.exists(roc_file)) roc_file else NULL,
    imp_file        = if (file.exists(imp_file)) imp_file else NULL,
    model_type      = model_obj$type,
    selected_vars   = model_obj$predictors,
    breaks          = risk_map_obj$breaks
  )

  # ---- Render ----------------------------------------------------------------
  rmarkdown::render(
    input         = template,
    output_format = format,
    output_file   = output_base,
    output_dir    = output_dir,
    params        = params,
    quiet         = !verbose
  )

  if (verbose) message("[pestRisk] Report saved to: ", output_file)
  invisible(output_file)
}
