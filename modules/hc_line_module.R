# ================= UI HELPERS ==================
linePlotModuleUI <- function(id) {
  ns <- NS(id)
  highcharter::highchartOutput(ns("line_plot"), height = "90vh")
}

linePlotLegendUI <- function(id, width = NULL) {
  ns <- NS(id)
  tagList(
    div(
      id = paste0(ns("line_plot"), "-legend"),
      style = paste(
        "max-width:100%;",
        "max-height:300px;",
        "overflow-y:auto;",
        "padding-right:6px;",
        "scrollbar-width:thin;",
        "-webkit-overflow-scrolling:touch;"
      ),
      tags$div(style = "margin:0 0 8px 0; color:#4D4D4D; font-weight:600; font-size:18px;",
               "States"),
      uiOutput(ns("legend"), container = div, inline = FALSE)
    )
  )
}

# =============== SERVER ==================
linePlotModuleServer <- function(id, data, dict, input_variable, input_states, Ymin, active_tab, input_color_by) {
  moduleServer(id, function(input, output, session) {
    
    clean_var <- reactive({sub("_[12]$", "", input_variable())}) 
    
    format_value <- function(values, type) {
      values <- as.double(values)
      if (isTRUE(type == "discrete"))   return(format(values, big.mark = ",", scientific = FALSE))
      if (isTRUE(type == "continuous")) return(format(round(values, 2), nsmall = 2, big.mark = ","))
      if (isTRUE(type == "percentage")) return(paste0(format(round(values, 2), nsmall = 2, big.mark = ","), "%"))
      format(values, big.mark = ",", scientific = FALSE)
    }
    
    # Helper to generate consistent colors based on selection
    get_color_map <- reactive({
      req(data(), input_states(), input_color_by())
      selected_states <- input_states()
      n_states <- length(selected_states)

      if (input_color_by() == "country") {
        country_colors_base <- c("ARGENTINA" = "#74ACDF", "BRAZIL" = "#3CB371", "MEXICO" = "#E03C31")
        countries_in_df <- unique(data()$country_name)
        missing_countries <- setdiff(countries_in_df, names(country_colors_base))

        if (length(missing_countries) > 0) {
          fallback_cols <- grDevices::hcl.colors(length(missing_countries), palette = "Set 2")
          names(fallback_cols) <- missing_countries
          return(c(country_colors_base, fallback_cols))
        }
        return(country_colors_base)
      } else {
        # Divergent/Qualitative Palette for States
        # We use 'Polychrome' or 'Set1' logic for maximum distinction
        if (n_states <= 8) {
          state_cols <- RColorBrewer::brewer.pal(max(3, n_states), "Set1")[1:n_states]
        } else if (n_states <= 12) {
          state_cols <- RColorBrewer::brewer.pal(n_states, "Paired")
        } else {
          # For many states, use a smooth but highly varied circular palette
          state_cols <- grDevices::hcl.colors(n_states, palette = "Zissou 1") 
        }
        return(setNames(state_cols, selected_states))
      }
    })

    output$line_plot <- highcharter::renderHighchart({
      req(data(), input_variable(), input_states(), active_tab() == "graph_tab", input_color_by())
      
      color_map <- get_color_map()
      
      df <- data() %>%
        dplyr::filter(state_name %in% input_states()) %>%
        dplyr::select(country_name, state_name, year, value = dplyr::all_of(input_variable())) %>%
        dplyr::mutate(
          date_year = suppressWarnings(as.Date(paste0(year, "-01-01"))),
          x_dt = as.POSIXct(date_year, tz = "UTC"),
          x_ms = as.numeric(x_dt) * 1000
        ) %>%
        tidyr::drop_na(date_year)
      
      req(nrow(df) > 0)
      
      pretty_name <- dict %>% dplyr::filter(variable == clean_var()) %>% dplyr::pull(pretty_name) %>%
        { if (length(.) == 0 || is.na(.)) clean_var() else . }
      var_type <- dict %>% dplyr::filter(variable == clean_var()) %>% dplyr::pull(type) %>%
        { if (length(.) == 0 || is.na(.)) "continuous" else . }

      y_min <- Ymin()
      y_max_pad <- as.numeric(max(df$value, na.rm = TRUE)) * 1.05
      x_min <- min(df$x_ms, na.rm = TRUE)
      x_max <- max(df$x_ms, na.rm = TRUE)
      
      df <- df %>% dplyr::mutate(.formatted_value = format_value(value, var_type))
      
      series_list <- purrr::map(input_states(), function(st) {
        df_state <- df %>% dplyr::filter(state_name == st)
        if (nrow(df_state) == 0) return(NULL)
        
        # Color logic: check if key is country name or state name
        color_key <- if(input_color_by() == "country") unique(df_state$country_name)[1] else st
        line_color <- color_map[[color_key]] %||% "#666666"
        
        list(
          name = st,
          type = "line",
          data = purrr::transpose(list(x = df_state$x_ms, y = df_state$value, f = df_state$.formatted_value)),
          color = line_color,
          lineWidth = 2,
          marker = list(enabled = TRUE, radius = 4),
          states = list(hover = list(lineWidthPlus = 1)),
          tooltip = list(valueDecimals = ifelse(var_type %in% c("continuous", "percentage"), 2, 0))
        )
      }) %>% purrr::compact()
      
      y_label <- switch(
        var_type,
        percentage = list(format = "{value:.2f}%"),
        continuous = list(formatter = highcharter::JS("function(){ return Highcharts.numberFormat(this.value, 2); }")),
        list(formatter = highcharter::JS("function(){ return Highcharts.numberFormat(this.value, 0); }"))
      )

      highcharter::highchart() %>%
        highcharter::hc_chart(
          type = "line",
          animation = FALSE,
          events = list(load = highcharter::JS(sprintf("
            function() {
              var legendId = '%s';
              var chartId  = '%s';
              function getChart(){ return Highcharts.charts.find(function(c){ return c && c.renderTo && c.renderTo.id === chartId; }); }
              var locked = null;
              var isBinding = false;
              function bindLegendEvents(){
                if (isBinding) return;
                isBinding = true;
                var root = document.getElementById(legendId);
                if(!root){ isBinding = false; return; }
                function setInactive(name){
                  var chart = getChart();
                  if(!chart) return;
                  chart.series.forEach(function(s){
                    if(!s.visible) return;
                    if(s.name === name){ s.setState('hover'); if(s.group) s.group.toFront(); } 
                    else { s.setState('inactive'); }
                  });
                  root.querySelectorAll('[data-state]').forEach(function(el){
                    var st = el.getAttribute('data-state');
                    el.style.opacity = (st === name ? '1' : '0.4');
                    el.style.boxShadow = (st === name ? 'inset 0 0 0 1px #FFA92A' : 'inset 0 0 0 1px #E6E6E6');
                  });
                }
                function clearStates(){
                  var chart = getChart();
                  if(!chart) return;
                  chart.series.forEach(function(s){ if(s.visible) s.setState('normal'); });
                  root.querySelectorAll('[data-state]').forEach(function(el){ el.style.opacity = '1'; el.style.boxShadow = 'inset 0 0 0 1px #E6E6E6'; });
                }
                root.querySelectorAll('[data-state]').forEach(function(el){
                  if (el.dataset.bound === '1') return;
                  el.dataset.bound = '1';
                  el.style.cursor = 'pointer';
                  el.addEventListener('mouseenter', function(){ if(!locked) setInactive(this.getAttribute('data-state')); });
                  el.addEventListener('mouseleave', function(){ if(!locked) clearStates(); });
                  el.addEventListener('click', function(){
                    var st = this.getAttribute('data-state');
                    if(locked === st){ locked = null; clearStates(); } 
                    else { locked = st; setInactive(st); }
                  });
                });
                isBinding = false;
              }
              bindLegendEvents();
              var legendRoot = document.getElementById(legendId);
              if(legendRoot) { (new MutationObserver(bindLegendEvents)).observe(legendRoot, { childList: true, subtree: true }); }
            }
          ", paste0(session$ns("line_plot"), "-legend"), session$ns("line_plot"))))
        ) %>%
        highcharter::hc_xAxis(type = "datetime", min = x_min, max = x_max, gridLineWidth = 1) %>%
        highcharter::hc_yAxis(title = list(text = pretty_name), min = y_min, max = if(!is.null(y_min)) y_max_pad else NULL, labels = y_label) %>%
        highcharter::hc_tooltip(shared = TRUE, useHTML = TRUE, headerFormat = "<span style='font-size:11px'>{point.x:%Y}</span><br/>", pointFormat = "<span style='font-size:9px'><b>{series.name}</b>: {point.f}</span><br/>") %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_add_series_list(series_list) %>%
        highcharter::hc_exporting(enabled = TRUE)
    })
    
    output$legend <- renderUI({
      req(data(), input_states(), input_variable(), input_color_by())
      
      color_map <- get_color_map()
      selected_states <- input_states()
      
      # Determine which states have data
      df_presence <- data() %>%
        dplyr::filter(state_name %in% selected_states) %>%
        dplyr::group_by(state_name, country_name) %>%
        dplyr::summarise(has_data = any(!is.na(!!sym(input_variable()))), .groups = "drop")
      
      states_with_data <- df_presence %>% dplyr::filter(has_data) %>% dplyr::pull(state_name)
      states_without_data <- setdiff(selected_states, states_with_data)
      
      # --- Helper for Chip UI ---
      make_chip <- function(st, col, interactive = TRUE) {
        tags$span(
          `data-state` = if(interactive) st else NULL,
          style = paste(
            "display:inline-flex;align-items:center;gap:8px;",
            "padding:5px 12px;border-radius:6px;", # Slightly more 'button' like
            "font-size:12px;margin:3px;font-weight:500;transition:all 0.2s;",
            if(interactive) {
              sprintf("background:#FFFFFF; border: 1.5px solid %s; color:#333; cursor:pointer;", col)
            } else {
              "background:#F5F5F5; border: 1.5px solid #DDD; color:#AAA; opacity:0.6; cursor:not-allowed;"
            }
          ),
          # The colored dot
          tags$span(style = sprintf("width:10px;height:10px;border-radius:2px;background:%s;", col)),
          st
        )
      }

      # --- 1. Main Legend Section ---
      if (input_color_by() == "country") {
        # Grouped by Country
        country_order <- unique(df_presence$country_name[df_presence$has_data])
        main_ui <- lapply(country_order, function(ctry) {
          col <- color_map[[ctry]] %||% "#666666"
          states_in_ctry <- df_presence$state_name[df_presence$country_name == ctry & df_presence$has_data]
          
          htmltools::div(
            style = "margin-bottom:12px;",
            htmltools::span(style = "display:block;font-weight:700;font-size:12px;color:#4D4D4D;margin-bottom:4px;", ctry),
            htmltools::div(style = "display:flex;flex-wrap:wrap;gap:4px;", 
                           lapply(states_in_ctry, function(st) make_chip(st, col)))
          )
        })
      } else {
        # Flat list by State
        main_ui <- htmltools::div(
          style = "display:flex;flex-wrap:wrap;gap:4px;",
          lapply(states_with_data, function(st) make_chip(st, color_map[[st]]))
        )
      }

      # --- 2. No Data Section ---
      no_data_ui <- if (length(states_without_data) > 0) {
        htmltools::div(
          style = "margin-top:10px;padding-top:8px;border-top:1px solid #EEE;",
          htmltools::span(style = "display:block;font-weight:700;font-size:11px;color:#888;margin-bottom:4px;", "No Data Available"),
          htmltools::div(style = "display:flex;flex-wrap:wrap;gap:4px;", 
                         lapply(states_without_data, function(st) make_chip(st, "#CCCCCC", FALSE)))
        )
      }

      htmltools::tagList(main_ui, no_data_ui)
    })
  })
}