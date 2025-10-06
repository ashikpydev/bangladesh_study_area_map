# app.R
# Bangladesh Study Area Map Generator (refined with conditional legend)
# Author: Ashiqur Rahman Rony (Enhanced by GPT-5)
# Run using: shiny::runApp()

# --- Packages ----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, leaflet, geodata, terra, sf, tidyverse, classInt,
  cowplot, viridis, ggnewscale, ggspatial, ggrepel, RColorBrewer
)

# --- Data path & download ----------------------------------------------------
data_path <- tempdir()

# Load administrative boundaries (GADM)
country_sf <- geodata::gadm(country = "BD", level = 0, path = data_path) |> st_as_sf()
divisions_sf <- geodata::gadm(country = "BD", level = 1, path = data_path) |> st_as_sf()
districts_sf <- geodata::gadm(country = "BD", level = 2, path = data_path) |> st_as_sf()
upazilas_sf <- geodata::gadm(country = "BD", level = 3, path = data_path) |> st_as_sf()
unions_sf <- geodata::gadm(country = "BD", level = 4, path = data_path) |> st_as_sf()

# Projection
crs_lambert <- "+proj=laea +lat_0=24 +lon_0=90 +datum=WGS84 +units=m +no_defs"

# UI choices
division_choices <- sort(unique(unions_sf$NAME_1))
district_choices_all <- sort(unique(unions_sf$NAME_2))
upazila_choices_all <- sort(unique(unions_sf$NAME_3))
union_choices_all <- sort(unique(unions_sf$NAME_4))

# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Bangladesh Study Area Map Generator"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select administrative areas to generate interactive and static publication maps."),
      selectizeInput("divisions", "Select Divisions", choices = division_choices, multiple = TRUE),
      selectizeInput("districts", "Select Districts", choices = NULL, multiple = TRUE),
      selectizeInput("upazilas", "Select Upazilas", choices = NULL, multiple = TRUE),
      selectizeInput("unions", "Select Unions", choices = NULL, multiple = TRUE),
      selectInput("display_level", "Display View", choices = c("Division", "District", "Upazila", "Union"), selected = "Union"),
      hr(),
      downloadButton("downloadMap", "Download High-Quality PNG")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Interactive Map", leafletOutput("interactiveMap", height = "650px")),
        tabPanel("Static Research Map", plotOutput("staticMap", height = "820px"))
      )
    )
  )
)

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Dynamic dropdown updates
  observe({
    if (length(input$divisions) > 0) {
      districts_choices <- sort(unique(unions_sf$NAME_2[unions_sf$NAME_1 %in% input$divisions]))
    } else districts_choices <- district_choices_all
    updateSelectizeInput(session, "districts", choices = districts_choices)
  })
  
  observe({
    if (length(input$districts) > 0) {
      upazilas_choices <- sort(unique(unions_sf$NAME_3[unions_sf$NAME_2 %in% input$districts]))
    } else upazilas_choices <- upazila_choices_all
    updateSelectizeInput(session, "upazilas", choices = upazilas_choices)
  })
  
  observe({
    if (length(input$upazilas) > 0) {
      unions_choices <- sort(unique(unions_sf$NAME_4[unions_sf$NAME_3 %in% input$upazilas]))
    } else unions_choices <- union_choices_all
    updateSelectizeInput(session, "unions", choices = unions_choices)
  })
  
  # Selected spatial subset (Lambert CRS)
  selected_areas <- reactive({
    sel <- unions_sf
    if (length(input$divisions) > 0) sel <- subset(sel, NAME_1 %in% input$divisions)
    if (length(input$districts) > 0) sel <- subset(sel, NAME_2 %in% input$districts)
    if (length(input$upazilas) > 0) sel <- subset(sel, NAME_3 %in% input$upazilas)
    if (length(input$unions) > 0) sel <- subset(sel, NAME_4 %in% input$unions)
    if (nrow(sel) == 0) return(NULL)
    st_transform(sel, crs_lambert)
  })
  
  # --- Interactive map -------------------------------------------------------
  output$interactiveMap <- renderLeaflet({
    sel <- selected_areas()
    country_wgs84 <- st_transform(country_sf, "EPSG:4326")
    if (is.null(sel)) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = country_wgs84, fillColor = "gray90", color = "gray60", weight = 0.5) %>%
        addControl("Select areas on left panel", position = "topright")
    } else {
      sel_wgs84 <- st_transform(sel, "EPSG:4326")
      pal <- colorFactor(viridis::viridis(length(unique(sel_wgs84$NAME_4))), domain = sel_wgs84$NAME_4)
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          data = sel_wgs84, fillColor = ~pal(NAME_4), color = "black", weight = 0.6, fillOpacity = 0.6,
          popup = ~paste0("<b>", NAME_4, "</b><br/>", NAME_3, ", ", NAME_2)
        ) %>%
        addPolygons(data = country_wgs84, fill = FALSE, color = "gray70", weight = 0.5) %>%
        addLegend("bottomright", pal = pal, values = sel_wgs84$NAME_4, title = "Union", opacity = 0.8)
    }
  })
  
  # --- Static Map ------------------------------------------------------------
  output$staticMap <- renderPlot({
    sel <- selected_areas()
    if (is.null(sel)) {
      plot.new()
      text(0.5, 0.5, "Select areas on the left to generate the map", cex = 1.3)
      return()
    }
    
    # Locator inset - highlight selected divisions
    sel_divisions <- subset(divisions_sf, NAME_1 %in% unique(sel$NAME_1))
    locator_map <- ggplot() +
      geom_sf(data = divisions_sf, fill = "gray95", color = "gray80", size = 0.15) +
      geom_sf(data = sel_divisions, fill = "#0072B2", color = "#005A9C", size = 0.2) +
      geom_sf(data = country_sf, fill = NA, color = "black", size = 0.2) +
      theme_void()
    
    # Determine coloring level based on lowest selection
    color_by <- if (length(input$unions) > 0) "NAME_4" else if (length(input$upazilas) > 0) "NAME_3" else if (length(input$districts) > 0) "NAME_2" else "NAME_1"
    
    # Determine split_by and base_sf for zoom/view level
    split_by <- switch(input$display_level,
                       "Division" = "NAME_1",
                       "District" = "NAME_2",
                       "Upazila" = "NAME_3",
                       "Union" = "NAME_4"
    )
    
    base_sf <- switch(input$display_level,
                      "Division" = subset(divisions_sf, NAME_1 %in% unique(sel$NAME_1)) |> st_transform(crs_lambert),
                      "District" = subset(districts_sf, NAME_2 %in% unique(sel$NAME_2)) |> st_transform(crs_lambert),
                      "Upazila" = subset(upazilas_sf, NAME_3 %in% unique(sel$NAME_3)) |> st_transform(crs_lambert),
                      "Union" = subset(unions_sf, NAME_4 %in% unique(sel$NAME_4)) |> st_transform(crs_lambert)
    )
    
    # Group sel by color_by for coloring
    sel_grouped <- sel |> 
      group_by(!!sym(color_by)) |> 
      summarise(geometry = st_union(geometry), .groups = "drop") |> 
      st_as_sf()
    
    # Unique levels for coloring
    unique_levels <- sort(unique(sel_grouped[[color_by]]))
    n_levels <- length(unique_levels)
    
    # Palette for selected areas - sequential for attractiveness
    if (n_levels > 1) {
      pal_cols <- hcl.colors(n_levels, "YlOrRd", rev = TRUE)
      names(pal_cols) <- unique_levels
    } else {
      pal_cols <- "#D55E00"
    }
    
    # Unique splits for submaps
    unique_splits <- sort(unique(sel[[split_by]]))
    
    # Dynamic locator position based on number of unique_splits
    if (length(unique_splits) > 1) {
      locator_x <- 0.78  # Top-right for multiple to avoid overlap
      locator_y <- 0.78
    } else {
      locator_x <- 0.02  # Top-left for single
      locator_y <- 0.68
    }
    
    if (length(unique_splits) > 1) {
      # Multiple splits: create submaps
      map_list <- list()
      for (split_value in unique_splits) {
        # Filter base_sf for this split
        base_split <- base_sf[base_sf[[split_by]] == split_value, ]
        
        # Filter sel_grouped intersecting this base_split
        sel_split <- sel_grouped[st_intersects(sel_grouped, st_union(base_split), sparse = FALSE), ]
        
        # Selected districts for context (if applicable)
        sel_districts_split <- if (input$display_level != "Division") subset(districts_sf, NAME_2 %in% unique(sel_split$NAME_2)) |> st_transform(crs_lambert) else NULL
        
        # Centroids and labels
        centroids_sf <- st_centroid(sel_split)
        coords <- st_coordinates(centroids_sf)
        labels_df <- data.frame(coords, label = sel_split[[color_by]])
        
        # Get bbox for zoom
        bbox <- st_bbox(base_split)
        xlim <- c(bbox["xmin"], bbox["xmax"])
        ylim <- c(bbox["ymin"], bbox["ymax"])
        
        # Submap
        map_split <- ggplot() +
          geom_sf(data = base_split, fill = "lightgray", alpha = 0.3, color = "gray", size = 0.3) +  # Base level shade
          geom_sf(data = sel_split, aes(fill = !!sym(color_by)) , color = "black", size = 0.25, alpha = 0.7) +
          scale_fill_manual(name = if (color_by == "NAME_4") "Union" else gsub("NAME_", "", color_by), values = pal_cols) +
          geom_text_repel(
            data = labels_df, aes(X, Y, label = label),
            size = 3.2, fontface = "bold", segment.color = "gray50", box.padding = 0.4, max.overlaps = 50
          ) +
          geom_sf(data = sel_districts_split, fill = NA, color = "gray60", size = 0.2) +
          annotation_scale(location = "bl", width_hint = 0.22) +
          annotation_north_arrow(location = "tl", style = north_arrow_minimal(text_size = 8)) +
          coord_sf(xlim = xlim, ylim = ylim, crs = crs_lambert, expand = TRUE) +
          labs(title = split_value, caption = "Data: GADM (boundaries)") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.caption = element_text(size = 8, color = "gray50", hjust = 1),
            axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
            panel.border = element_rect(color = "black", fill = NA),
            legend.position = if (n_levels > 1) "right" else "none"
          )
        
        map_list[[split_value]] <- map_split
      }
      
      # Arrange submaps
      sub_maps <- plot_grid(plotlist = map_list, ncol = min(length(map_list), 2), labels = "AUTO")
      
      # Overall title
      title_plot <- ggdraw() + draw_label("Study Area Map", fontface = "bold", size = 16, hjust = 0.5)
      main_map <- plot_grid(title_plot, sub_maps, ncol = 1, rel_heights = c(0.1, 1))
      
    } else {
      # Single split or no split
      sel_districts <- subset(districts_sf, NAME_2 %in% unique(sel$NAME_2)) |> st_transform(crs_lambert)
      
      # Base geometry for zoom
      base_geom <- base_sf
      
      # Centroids and labels
      centroids_sf <- st_centroid(sel_grouped)
      coords <- st_coordinates(centroids_sf)
      labels_df <- data.frame(coords, label = sel_grouped[[color_by]])
      
      # Get bbox for zoom
      bbox <- st_bbox(base_geom)
      xlim <- c(bbox["xmin"], bbox["xmax"])
      ylim <- c(bbox["ymin"], bbox["ymax"])
      
      main_map <- ggplot() +
        geom_sf(data = base_geom, fill = "lightgray", alpha = 0.3, color = "gray", size = 0.3) +  # Base level shade
        geom_sf(data = sel_grouped, aes(fill = !!sym(color_by)), color = "black", size = 0.25, alpha = 0.7) +
        scale_fill_manual(name = if (color_by == "NAME_4") "Union" else gsub("NAME_", "", color_by), values = pal_cols) +
        geom_text_repel(
          data = labels_df, aes(X, Y, label = label),
          size = 3.2, fontface = "bold", segment.color = "gray50", box.padding = 0.4, max.overlaps = 50
        ) +
        geom_sf(data = sel_districts, fill = NA, color = "gray60", size = 0.2) +
        annotation_scale(location = "bl", width_hint = 0.22) +
        annotation_north_arrow(location = "tl", style = north_arrow_minimal(text_size = 8)) +
        coord_sf(xlim = xlim, ylim = ylim, crs = crs_lambert, expand = TRUE) +
        labs(
          title = "Study Area Map",
          subtitle = paste0("Selected: ", paste0(unique_levels, collapse = ", ")),
          caption = "Data: GADM (boundaries)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5),
          plot.caption = element_text(size = 8, color = "gray50", hjust = 1),
          axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = if (n_levels > 1) "right" else "none"
        )
    }
    
    cowplot::ggdraw() +
      cowplot::draw_plot(main_map) +
      cowplot::draw_plot(locator_map, x = locator_x, y = locator_y, width = 0.22, height = 0.22)
  })
  
  # --- Download (PNG) --------------------------------------------------------
  output$downloadMap <- downloadHandler(
    filename = function() paste0("study_area_map_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 12, height = 8, dpi = 300)
    }
  )
}

# --- Run ---------------------------------------------------------------------
shinyApp(ui, server)
