# =============================================================================
# plot_risk_map.R
# =============================================================================

#' Visualise pest invasion risk maps
#'
#' @description
#' Produces publication-ready risk maps from a [predict_risk_map()] object.
#' Supports both continuous suitability and classified (Low / Medium / High)
#' risk visualisation using `ggplot2`.
#'
#' @param risk_map_obj `list`. Output of [predict_risk_map()].
#' @param type `character`. Map type: `"continuous"` (suitability gradient,
#'   default) or `"classified"` (three-class risk).
#' @param occurrences_sf `sf` object or `NULL`. If provided, presence points
#'   are overlaid on the map.
#' @param species_name `character`. Species name for the map title.
#' @param country_borders `logical`. Overlay country borders (default `TRUE`).
#' @param output_file `character` or `NULL`. File path to export the map
#'   (e.g. `"map.png"`, `"map.pdf"`). `NULL` returns the ggplot object without
#'   saving.
#' @param width `numeric`. Export width in inches (default `10`).
#' @param height `numeric`. Export height in inches (default `7`).
#' @param verbose `logical`. Print messages (default `TRUE`).
#'
#' @return A `ggplot` object (invisibly if `output_file` is specified).
#'
#' @examples
#' \dontrun{
#' risk <- predict_risk_map(rf_out, bio_africa)
#' p <- plot_risk_map(risk, type = "continuous",
#'                    species_name = "Spodoptera frugiperda",
#'                    output_file  = "sf_suitability.png")
#' }
#'
#' @importFrom terra as.data.frame
#' @importFrom ggplot2 ggplot aes geom_raster geom_sf scale_fill_gradientn
#'   scale_fill_manual labs theme_minimal theme element_text coord_sf ggsave
#' @importFrom ggplot2 scale_fill_viridis_c
#' @export
plot_risk_map <- function(risk_map_obj,
                           type           = c("continuous", "classified"),
                           occurrences_sf = NULL,
                           species_name   = "Agricultural Pest",
                           country_borders = TRUE,
                           output_file    = NULL,
                           width          = 10,
                           height         = 7,
                           verbose        = TRUE) {

  .check_pkg("ggplot2")
  .check_pkg("terra")

  type <- match.arg(type)

  # ---- Convert raster to data.frame -----------------------------------------
  if (type == "continuous") {
    rast_obj <- risk_map_obj$suitability
    df       <- as.data.frame(rast_obj, xy = TRUE, na.rm = TRUE)
    names(df)[3] <- "value"
  } else {
    rast_obj <- risk_map_obj$risk_class
    df       <- as.data.frame(rast_obj, xy = TRUE, na.rm = TRUE)
    names(df)[3] <- "value"
    df$value <- factor(df$value,
                       levels = 1:3,
                       labels = c("Low", "Medium", "High"))
  }

  # ---- Base map --------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = value))

  if (type == "continuous") {
    p <- p + ggplot2::scale_fill_viridis_c(
      option    = "inferno",
      name      = "Suitability",
      limits    = c(0, 1),
      direction = -1
    )
  } else {
    p <- p + ggplot2::scale_fill_manual(
      values = c("Low"    = "#2ECC71",
                 "Medium" = "#F39C12",
                 "High"   = "#C0392B"),
      name   = "Risk level"
    )
  }

  # ---- Country borders -------------------------------------------------------
  if (country_borders) {
    tryCatch({
      .check_pkg("geodata")
      world_sf <- sf::st_as_sf(
        geodata::world(resolution = 3, path = tempdir())
      )
      p <- p + ggplot2::geom_sf(
        data        = world_sf,
        fill        = NA,
        colour      = "grey30",
        linewidth   = 0.2,
        inherit.aes = FALSE
      )
    }, error = function(e) NULL)  # silently skip if borders can't be loaded
  }

  # ---- Overlay occurrences ---------------------------------------------------
  if (!is.null(occurrences_sf)) {
    p <- p + ggplot2::geom_sf(
      data        = occurrences_sf,
      colour      = "white",
      fill        = "black",
      shape       = 21,
      size        = 1.2,
      stroke      = 0.3,
      alpha       = 0.8,
      inherit.aes = FALSE
    )
  }

  # ---- Labels & theme --------------------------------------------------------
  subtitle <- switch(type,
                     continuous  = "Predicted habitat suitability [0–1]",
                     classified  = "Invasion risk classification (Low / Medium / High)")

  p <- p +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(
      title    = paste0("Pest invasion risk – ", species_name),
      subtitle = subtitle,
      x        = NULL, y = NULL,
      caption  = "Data: GBIF | WorldClim v2.1 | Model: pestRisk"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(colour = "grey40"),
      legend.position = "right"
    )

  # ---- Export ----------------------------------------------------------------
  if (!is.null(output_file)) {
    if (verbose) message("[pestRisk] Saving map to: ", output_file)
    ggplot2::ggsave(output_file, plot = p, width = width, height = height,
                    dpi = 300)
    invisible(p)
  } else {
    p
  }
}


# =============================================================================
# plot_variable_importance.R
# =============================================================================

#' Plot variable importance and response curves
#'
#' @description
#' Generates a barplot of variable importance from a Random Forest model and /
#' or response curves from a MaxEnt model. Helps interpret which bioclimatic
#' drivers most strongly explain pest distribution.
#'
#' @param model_obj `list`. Output of [train_rf_model()] or [train_maxent()].
#' @param type `character`. Plot type: `"importance"` (RF barplot, default),
#'   `"response"` (MaxEnt response curves), or `"both"` (both on a grid).
#' @param top_n `integer`. Number of top variables to show in importance plot
#'   (default all).
#' @param output_file `character` or `NULL`. File path to export (e.g.
#'   `"importance.png"`). Returns the ggplot invisibly if provided.
#' @param verbose `logical`. Print messages (default `TRUE`).
#'
#' @return A `ggplot` object.
#'
#' @examples
#' data(spodoptera_model_data)
#' rf_out <- train_rf_model(spodoptera_model_data)
#' plot_variable_importance(rf_out)
#'
#' mx_out <- train_maxent(spodoptera_model_data)
#' plot_variable_importance(mx_out, type = "response")
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_line coord_flip facet_wrap
#'   labs theme_minimal theme element_text reorder scale_fill_viridis_c ggsave
#' @importFrom dplyr mutate
#' @export
plot_variable_importance <- function(model_obj,
                                      type        = c("importance", "response", "both"),
                                      top_n       = Inf,
                                      output_file = NULL,
                                      verbose     = TRUE) {

  .check_pkg("ggplot2")
  type <- match.arg(type)

  p <- NULL

  # ---- Variable importance barplot (Random Forest) --------------------------
  if ((type == "importance" || type == "both") && model_obj$type == "rf") {
    imp_df <- model_obj$importance
    if (is.finite(top_n)) imp_df <- imp_df[seq_len(min(top_n, nrow(imp_df))), ]

    p_imp <- ggplot2::ggplot(imp_df,
                              ggplot2::aes(
                                x    = reorder(variable, importance),
                                y    = importance,
                                fill = importance
                              )) +
      ggplot2::geom_bar(stat = "identity", colour = "white", linewidth = 0.3) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_viridis_c(option = "plasma", direction = -1,
                                    guide  = "none") +
      ggplot2::labs(
        title = "Variable Importance – Random Forest",
        x     = "Bioclimatic variable",
        y     = "Mean Decrease Gini",
        caption = "Higher values = stronger predictor of pest presence"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    p <- p_imp
  }

  # ---- MaxEnt response curves -----------------------------------------------
  if ((type == "response" || type == "both") && model_obj$type == "maxent") {
    resp_df <- model_obj$response_curves

    p_resp <- ggplot2::ggplot(resp_df,
                               ggplot2::aes(x = value, y = suitability)) +
      ggplot2::geom_line(colour = "#C0392B", size = 0.9) +
      ggplot2::facet_wrap(~ variable, scales = "free_x") +
      ggplot2::labs(
        title    = "MaxEnt Response Curves",
        subtitle = "Marginal suitability response to each climatic variable",
        x        = "Variable value",
        y        = "Predicted suitability"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold"),
        strip.text    = ggplot2::element_text(face = "bold", size = 9),
        plot.subtitle = ggplot2::element_text(colour = "grey40")
      )

    p <- p_resp
  }

  if (is.null(p))
    stop("[pestRisk] Cannot generate '", type, "' plot for model type '",
         model_obj$type, "'.", call. = FALSE)

  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = 10, height = 6, dpi = 300)
    if (verbose) message("[pestRisk] Plot saved to: ", output_file)
    invisible(p)
  } else {
    p
  }
}
