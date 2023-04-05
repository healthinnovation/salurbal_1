library(biscale)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(sf)
library(ggpubr)
library(table1)
library(ggstatsplot)
library(innovar)
library(ggmosaic)
# 1. gg_barplot -----------------------------------------------------------
gg_barplot <- function(x,nrow = 1){
  g0 <- x %>% 
    ggplot(
      aes(
        x = bi_class,
        y = total,
        fill = bi_class
      )
    ) + 
    geom_bar(stat = 'identity') + 
    theme_minimal() + 
    theme(legend.position = "none") +
    labs(x = "", y = "")
  
  if(nrow == 1){
    g1 <- g0 + 
      scale_fill_manual(values = c("#AE384C","#BA7A8F","#CADECF")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  }
  else if(nrow == 2){ 
    g1 <- g0 + 
      scale_fill_manual(values = c("#75304A","#7F688A","#87A1C7")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  } else {
    g1 <- g0 + 
      scale_fill_manual(values = c("#3D2847","#425785","#4785BF")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  }
  return(g1)
} 

# 2. Biscale color palette - customized -----------------------------------

bi_pal2 <- function (pal, dim = 3, preview = TRUE) 
{
  if (missing(pal) == TRUE) {
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', and 'GrPink'.")
  }
  if (pal %in% c("Brown", "DkBlue", "DkCyan", "DkViolet", "GrPink") == 
      FALSE) {
    stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', and 'GrPink'.")
  }
  if (is.numeric(dim) == FALSE) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (dim != 2 & dim != 3) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (is.logical(preview) == FALSE) {
    stop("A logical scalar must be supplied for 'preview'. Please provide either 'TRUE' or 'FALSE'.")
  }
  if (preview == TRUE) {
    out <- bi_legend(pal = pal, dim = dim, size = 16)
  }
  else if (preview == FALSE) {
    if (pal == "DkViolet") {
      out <- pal_dkviolet2(n = dim)
    }
    else if (pal == "GrPink") {
      out <- pal_grpink(n = dim)
    }
    else if (pal == "DkBlue") {
      out <- pal_dkblue(n = dim)
    }
    else if (pal == "DkCyan") {
      out <- pal_dkcyan(n = dim)
    }
    else if (pal == "Brown") {
      out <- pal_brown(n = dim)
    }
  }
  return(out)
}

pal_dkviolet <- function(n){
  
  # construct palette
  if (n == 2){
    
    out <- c(
      "2-2" = "#3F2949", # high x, high y
      "1-2" = "#4885C1", # low x, high y
      "2-1" = "#AE3A4E", # high x, low y
      "1-1" = "#CABED0" # low x, low y
    )
    
  } else if (n == 3){
    
    out <- c(
      "3-3" = "#CADECF", # high x, high y
      "2-3" = "#BA7A8F",
      "1-3" = "#AE384C", # low x, high y
      "3-2" = "#87A1C7",
      "2-2" = "#7F688A", # medium x, medium y
      "1-2" = "#75304A",
      "3-1" = "#4785BF", # high x, low y
      "2-1" = "#425785",
      "1-1" = "#3D2847" # low x, low y
    )
    
  }
  return(out)
}

# gray pink palette
pal_grpink <- function(n){
  
  # construct palette
  if (n == 2){
    
    out <- c(
      "2-2" = "#574249", # high x, high y
      "1-2" = "#64ACBE", # low x, high y
      "2-1" = "#C85A5A", # high x, low y
      "1-1" = "#E8E8E8" # low x, low y
    )
    
  } else if (n == 3){
    
    out <- c(
      "3-3" = "#574249", # high x, high y
      "2-3" = "#627F8C",
      "1-3" = "#64ACBE", # low x, high y
      "3-2" = "#985356",
      "2-2" = "#AD9EA5", # medium x, medium y
      "1-2" = "#B0D5DF",
      "3-1" = "#C85A5A", # high x, low y
      "2-1" = "#E4ACAC",
      "1-1" = "#E8E8E8" # low x, low y
    )
    
  }
  
  # return output
  return(out)
  
}


bi_legend <- function(pal, dim = 3, xlab, ylab, size = 10, flip_axes = FALSE, rotate_pal = FALSE, pad_width = NA, pad_color = '#ffffff'){
  
  # global binding
  bi_class = bi_fill = x = y = NULL
  
  # check parameters
  if (missing(pal) == TRUE){
    stop("A palette must be specified for the 'pal' argument.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    if (dim == 2 & length(pal) != 4){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    } else if (dim == 3 & length(pal) != 9){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    if (pal %in% c("BlGold", "BlOrange", "BlYellow", "Brown", "Diverging", "DkBlue", "DkCyan", "DkViolet", "Fire", "GnPink", "GnPurple", "GrPink", "OrgPurple", "Reds", "Viridis") == FALSE){
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis'.")
    }
    
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (missing(xlab) == TRUE){
    xlab <- "x var "
  }
  
  if (is.character(xlab) == FALSE){
    stop("The 'xlab' argument must be a character string.")
  }
  
  if (missing(ylab) == TRUE){
    ylab <- "y var "
  }
  
  if (is.character(ylab) == FALSE){
    stop("The 'ylab' argument must be a character string.")
  }
  
  if (is.numeric(size) == FALSE){
    stop("The 'size' argument must be a numeric value.")
  }
  
  # nse
  xQN <- rlang::quo_name(rlang::enquo(xlab))
  yQN <- rlang::quo_name(rlang::enquo(ylab))
  
  # obtain palette
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    x <- pal
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    x <- switch(pal,
                "DkViolet" = pal_dkviolet(n = dim),
                "GrPink" = pal_grpink(n = dim),
                "DkBlue" = pal_dkblue(n = dim),
                "DkCyan" = pal_dkcyan(n = dim),
                "Brown" = pal_brown(n = dim),
                "BlGold" = pal_blgold(n = dim),
                "BlOrange" = pal_blorange(n = dim),
                "BlYellow" = pal_blyellow(n = dim),
                "Viridis" = pal_viridis(n = dim),
                "Diverging" = pal_diverging(n = dim),
                "GnPink" = pal_gnpink(n = dim),
                "GnPurple" = pal_gnpurp(n = dim),
                "OrgPurple" = pal_orgpurp(n = dim),
                "Fire" = pal_fire(n = dim),
                "Reds" = pal_reds(n = dim)
    )
    
    if(flip_axes){
      x <- bi_pal_flip(x)
    }
    
    if(rotate_pal){
      x <- bi_pal_rotate(x)
    }
    
  }
  
  # create tibble for plotting
  x <- dplyr::tibble(
    bi_class = names(x),
    bi_fill = x
  )
  
  # reformat
  leg <- tidyr::separate(x, bi_class, into = c("x", "y"), sep = "-")
  leg <- dplyr::mutate(leg, x = as.integer(x), y = as.integer(y))
  
  # create ggplot2 legend object
  legend <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = leg, mapping = ggplot2::aes(x = x, y = y, fill = bi_fill), lwd = pad_width, col = pad_color) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = substitute(paste(xQN, ""%->%"")), y = substitute(paste(yQN, ""%->%""))) +
    bi_theme() +
    ggplot2::theme(axis.title = ggplot2::element_text(size = size)) +
    ggplot2::coord_fixed()
  
  # return output
  return(legend)
  
}
bi_theme <- function(base_family = "sans", base_size = 24, bg_color = "#ffffff", font_color = "#000000", ...) {
  
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      
      # text defaults
      text = ggplot2::element_text(color = font_color),
      
      # remove all axes
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      
      # add a grid that blends into plot background
      panel.grid.major = ggplot2::element_line(color = bg_color, size = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      
      # background colors
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      legend.background = ggplot2::element_rect(fill = bg_color, color = NA),
      
      # borders and margins
      plot.margin = ggplot2::unit(c(.5, .5, .2, .5), "cm"),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(c(-.1, 0.2, .2, 0.2), "cm"),
      
      # titles
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, color = font_color, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = font_color,
                                            margin = ggplot2::margin(b = -0.1, t = -0.1, l = 2, unit = "cm"),
                                            face = "bold", debug = FALSE),
      legend.title = ggplot2::element_text(color = font_color),
      legend.text = ggplot2::element_text(hjust = 0, color = font_color),
      
      # captions
      plot.caption = ggplot2::element_text(size = ggplot2::rel(.6), hjust = .5,
                                           margin = ggplot2::margin(t = 0.2, b = 0, unit = "cm"),
                                           color = font_color),
      ...
    )
  
}

bi_scale_fill2 <- function(pal, dim = 3, flip_axes = FALSE, rotate_pal = FALSE, ...){
  
  # check parameters
  if (missing(pal) == TRUE){
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis' or supply a custom palette created with 'bi_pal_custom()'.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    if (dim == 2 & length(pal) != 4){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    } else if (dim == 3 & length(pal) != 9){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    if (pal %in% c("BlGold", "BlOrange", "BlYellow", "Brown", "Diverging", "DkBlue", "DkCyan", "DkViolet", "Fire", "GnPink", "GnPurple", "GrPink", "OrgPurple", "Reds", "Viridis") == FALSE){
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis'.")
    }
    
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  # obtain palette
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    x <- pal
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    x <- switch(pal,
                "DkViolet" = pal_dkviolet(n = dim),
                "GrPink" = pal_grpink(n = dim),
                "DkBlue" = pal_dkblue(n = dim),
                "DkCyan" = pal_dkcyan(n = dim),
                "Brown" = pal_brown(n = dim),
                "BlGold" = pal_blgold(n = dim),
                "BlOrange" = pal_blorange(n = dim),
                "BlYellow" = pal_blyellow(n = dim),
                "Viridis" = pal_viridis(n = dim),
                "Diverging" = pal_diverging(n = dim),
                "GnPink" = pal_gnpink(n = dim),
                "GnPurple" = pal_gnpurp(n = dim),
                "OrgPurple" = pal_orgpurp(n = dim),
                "Fire" = pal_fire(n = dim),
                "Reds" = pal_reds(n = dim)
    )
    
    if(flip_axes){
      x <- bi_pal_flip(x)
    }
    
    if(rotate_pal){
      x <- bi_pal_rotate(x)
    }
    
  }
  
  # apply to ggplot object
  ggplot2::scale_fill_manual(values = x, ...)
  
}

# 3. Maps biscale ---------------------------------------------------------
gg_bimap <- function(data, xlim, ylim){
  g0 <- data %>% 
    ggplot() + 
    geom_sf(
      data = dep,
      lwd = 0.5,
      fill = "#d9d9d9",
      show.legend = FALSE,
      color = "white"
    ) +
    geom_sf(
      data = data,
      lwd = 0.0,
      aes(fill = bi_class),
      show.legend = FALSE
    ) +
    theme(
      axis.text =  element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    ) +
    bi_scale_fill2(pal = "DkViolet", dim = 3) +
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    theme_bw()  +
    theme(
      axis.text =  element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    ) + 
    facet_grid(.~title) + 
    theme(
      strip.background = element_rect(
        colour = "black",
        fill = "white")
    )
}

#==========================================================================
# 2. Reading and processing data ------------------------------------------
bogota <- st_read("Procesado/Bogota.gpkg")
buenosaires <- st_read("Procesado/Buenos aires.gpkg")
guatemala <- st_read("Procesado/Guatemala.gpkg")
mexico <- st_read("Procesado/Mexico.gpkg")
panama <- st_read("Procesado/Panama.gpkg")
sanjose <- st_read("Procesado/San jose.gpkg")
santiago <- st_read("Procesado/Santiago.gpkg")
saopaolo <- st_read("Procesado/Sao paolo.gpkg")

# 3. Bogota -----------------------------------------------

bogota$CO_L3_ANALYTIC_09022022_CNSMINPR_L3 <- as.numeric(
  bogota$CO_L3_ANALYTIC_09022022_CNSMINPR_L3
)

data1 <- bi_class(
  bogota,
  x = CO_L3_ANALYTIC_09022022_CNSMINPR_L3,
  y = PM2.5_2017,
  style = "quantile",
  dim = 3
)

newdata <- data1 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "Education level →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Education level → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = bogota,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data1,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

basemap

end1 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.2, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.12, 0.45, 0.7, 0.7,
    scale = 0.4
  ) +
  theme_bw()

end1
##> Export final plot in a png format
ggsave(
  filename = "bogota.png",
  plot = end1,
  width = 9,
  height = 7,
  bg = "white",
  dpi = 300  
)

# 4. Buenos aires -----------------------------------------------

buenosaires$AR_L2_5_ANALYTIC_05172022_CNSMINPR_L3 <- as.numeric(
  buenosaires$AR_L2_5_ANALYTIC_05172022_CNSMINPR_L3
)

data2 <- bi_class(
  buenosaires,
  x = AR_L2_5_ANALYTIC_05172022_CNSMINPR_L3,
  y = PM2.5_2017,
  style = "quantile",
  dim = 3
)

newdata <- data2 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "Education level →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Education level → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = buenosaires,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data2,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

basemap

end2 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.13, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.18, 0.45, 0.7, 0.7,
    scale = 0.4
  ) +
  theme_bw()

end2
##> Export final plot in a png format
ggsave(
  filename = "buenos aires.png",
  plot = end2,
  width = 9,
  height = 7,
  bg = "white",
  dpi = 300  
)

# 5. Guatemala -----------------------------------------------

guatemala$GT_L3_ANALYTIC_09022022_CNSMINPR_L3 <- as.numeric(
  guatemala$GT_L3_ANALYTIC_09022022_CNSMINPR_L3
)

data3 <- bi_class(
  guatemala,
  x = GT_L3_ANALYTIC_09022022_CNSMINPR_L3,
  y = PM2.5_mean_2017,
  style = "quantile",
  dim = 3
)

newdata <- data3 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "Education level →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Education level → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = guatemala,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data3,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

basemap

end3 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.14, 0.45, 0.7, 0.7,
    scale = 0.4
  ) +
  theme_bw()

end3
##> Export final plot in a png format
ggsave(
  filename = "guatemala.png",
  plot = end3,
  width = 9,
  height = 7,
  bg = "white",
  dpi = 300  
)

# 6. Panama -----------------------------------------------

panama$PA_L3_ANALYTIC_09022022_CNSMINPR_L3 <- as.numeric(
  panama$PA_L3_ANALYTIC_09022022_CNSMINPR_L3
)

data4 <- bi_class(
  panama,
  x = PA_L3_ANALYTIC_09022022_CNSMINPR_L3,
  y = PM2.5_2017,
  style = "quantile",
  dim = 3
)

newdata <- data4 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "Education level →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Education level → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = panama,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data4,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

basemap

end4 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.25, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.14, 0.45, 0.7, 0.7,
    scale = 0.4
  ) +
  theme_bw()

end4
##> Export final plot in a png format
ggsave(
  filename = "panama.png",
  plot = end4,
  width = 9,
  height = 7,
  bg = "white",
  dpi = 300  
)

# 7. San jose -----------------------------------------------

sanjose$CR_L2_5_ANALYTIC_09022022_CNSMINPR_L3 <- as.numeric(
  sanjose$CR_L2_5_ANALYTIC_09022022_CNSMINPR_L3
)

sanjose$CR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3 <- as.numeric(
  sanjose$CR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3
)

data5 <- bi_class(
  sanjose,
  x = CR_L2_5_ANALYTIC_09022022_CNSMINPR_L3,
  y = CR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3,
  style = "quantile",
  dim = 3
)

newdata <- data5 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "Education level →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Education level → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = sanjose,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data5,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

basemap

end5 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.14, 0.45, 0.7, 0.7,
    scale = 0.4
  ) +
  theme_bw()

end5
##> Export final plot in a png format
ggsave(
  filename = "sanjose.png",
  plot = end5,
  width = 9,
  height = 7,
  bg = "white",
  dpi = 300  
)

# 8. Santiago -----------------------------------------------

santiago$CL_L3_ANALYTIC_09022022_CNSMINPR_L3 <- as.numeric(
  santiago$CL_L3_ANALYTIC_09022022_CNSMINPR_L3
)

santiago$CL_L3_ANALYTIC_09022022_APSPM25MEAN2017L3 <- as.numeric(
  santiago$CL_L3_ANALYTIC_09022022_APSPM25MEAN2017L3
)

data6 <- bi_class(
  santiago,
  x = CL_L3_ANALYTIC_09022022_CNSMINPR_L3,
  y = CL_L3_ANALYTIC_09022022_APSPM25MEAN2017L3,
  style = "quantile",
  dim = 3
)

newdata <- data6 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "Education level →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Education level → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = santiago,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data6,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

basemap

end6 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.14, 0.45, 0.7, 0.7,
    scale = 0.4
  ) +
  theme_bw()

end6
##> Export final plot in a png format
ggsave(
  filename = "santiago.png",
  plot = end6,
  width = 9,
  height = 7,
  bg = "white",
  dpi = 300  
)

# 9. Sao paolo -----------------------------------------------

saopaolo$BR_L2_5_ANALYTIC_09022022_CNSMINPR_L3 <- as.numeric(
  saopaolo$BR_L2_5_ANALYTIC_09022022_CNSMINPR_L3
)

saopaolo$BR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3 <- as.numeric(
  saopaolo$BR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3
)

data7 <- bi_class(
  saopaolo,
  x = BR_L2_5_ANALYTIC_09022022_CNSMINPR_L3,
  y = BR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3,
  style = "quantile",
  dim = 3
)

newdata <- data7 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "Education level →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Education level → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = saopaolo,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data7,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

basemap

end7 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.14, 0.45, 0.7, 0.7,
    scale = 0.4
  ) + 
  theme_bw()

end7
##> Export final plot in a png format
ggsave(
  filename = "saopaolo.png",
  plot = end7,
  width = 9,
  height = 7,
  bg = "white",
  dpi = 300  
)

# 10. Mexico -----------------------------------------------

mexico$MX_L3_ANALYTIC_05172022_CNSMINPR_L3 <- as.numeric(
  mexico$MX_L3_ANALYTIC_05172022_CNSMINPR_L3
)

mexico$MX_L3_ANALYTIC_09022022_APSPM25MEAN2017L3 <- as.numeric(
  mexico$MX_L3_ANALYTIC_09022022_APSPM25MEAN2017L3
)

data8 <- bi_class(
  mexico,
  x = MX_L3_ANALYTIC_05172022_CNSMINPR_L3,
  y = MX_L3_ANALYTIC_09022022_APSPM25MEAN2017L3,
  style = "quantile",
  dim = 3
)

newdata <- data8 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "Education level →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Education level → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = mexico,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data8,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

basemap

end8 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.14, 0.45, 0.7, 0.7,
    scale = 0.4
  ) + 
  theme_bw()

end8
##> Export final plot in a png format
ggsave(
  filename = "mexico.png",
  plot = end8,
  width = 9,
  height = 7,
  bg = "white",
  dpi = 300  
)

gg1 <- ggarrange(
  end1, end2, end3, end4,
  end5, end6, end7, end8,
  labels = c(
    "A. Bogota (Colombia)", "B. Buenos Aires (Argentina)", 
    "C. Guatemala city (Guatemala)", "D. Panama city (Panama)", 
    "E. San Jose de Costa Rica (Costa Rica)", "F. Santiago de Chile (Chile)",
    "G. Sao Paolo (Brasil)", "H. Mexico city (Mexico"),
  nrow = 3,
  ncol = 3
)

ggsave(
  filename = "city_map.png",
  plot = last_plot(),
  width = 20.5,
  height = 16,
  bg = "white",
  dpi = 300  
)

ggsave(
  filename = "city_map.pdf",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white"  
)

## Table 1 ===========================================

# 1. Bogota ------------

bogota$Population <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_SPTPOPL3)
bogota$Water <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSWATNETL3)
bogota$Sewage <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSSEWANYL3)
bogota$Floors <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSFLOORL3)
bogota$Overcrowding <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSCROWD3RML3)
bogota$Unemployment <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSUNEMPL3)
bogota$Labor_force <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSLABPARTL3)
bogota$School_attending <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSST1517L3)
bogota$Primary_education <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSMINPR_L3)
bogota$Secondary_education <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSMINHS_L3)
bogota$University_education <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSMINUN_L3)
bogota$Mean_Pm2.5 <- as.numeric(bogota$CO_L3_ANALYTIC_09022022_APSPM25MEAN2017L3)

table1(~ Population +
       Water +
       Sewage +
       Floors +
       Overcrowding +
       Unemployment +
       Labor_force +
       School_attending +
       Primary_education +
       Secondary_education +
       University_education +
       Mean_Pm2.5,
       data = bogota,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

# 2. Buenos aires ---------

buenosaires$Population <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_SPTPOPL3)
buenosaires$Water <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSWATNETL3)
buenosaires$Sewage <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSSEWANYL3)
buenosaires$Floors <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSFLOORL3)
buenosaires$Overcrowding <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSCROWD3RML3)
buenosaires$Unemployment <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSUNEMPL3)
buenosaires$Labor_force <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSLABPARTL3)
buenosaires$School_attending <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSST1517L3)
buenosaires$Primary_education <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSMINPR_L3)
buenosaires$Secondary_education <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSMINHS_L3)
buenosaires$University_education <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_CNSMINUN_L3)
buenosaires$Mean_Pm2.5 <- as.numeric(buenosaires$AR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3)

table1(~ Population +
         Water +
         Sewage +
         Floors +
         Overcrowding +
         Unemployment +
         Labor_force +
         School_attending +
         Primary_education +
         Secondary_education +
         University_education +
         Mean_Pm2.5,
       data = buenosaires,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

# 3. Guatemala ----------
guatemala$Population <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_SPTPOPL3)
guatemala$Water <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSWATNETL3)
guatemala$Sewage <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSSEWANYL3)
guatemala$Floors <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSFLOORL3)
guatemala$Overcrowding <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSCROWD3RML3)
guatemala$Unemployment <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSUNEMPL3)
guatemala$Labor_force <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSLABPARTL3)
guatemala$School_attending <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSST1517L3)
guatemala$Primary_education <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSMINPR_L3)
guatemala$Secondary_education <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSMINHS_L3)
guatemala$University_education <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_CNSMINUN_L3)
guatemala$Mean_Pm2.5 <- as.numeric(guatemala$GT_L3_ANALYTIC_09022022_APSPM25MEAN2017L3)

table1(~ Population +
         Water +
         Sewage +
         Floors +
         Overcrowding +
         Unemployment +
         Labor_force +
         School_attending +
         Primary_education +
         Secondary_education +
         University_education +
         Mean_Pm2.5,
       data = guatemala,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

# 4. Mexico ---------------
mexico$Population <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_SPTPOPL3)
mexico$Sewage <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSSEWANYL3)
mexico$Floors <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSFLOORL3)
mexico$Overcrowding <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSCROWD3RML3)
mexico$Unemployment <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSUNEMPL3)
mexico$Labor_force <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSLABPARTL3)
mexico$School_attending <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSST1517L3)
mexico$Primary_education <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSMINPR_L3)
mexico$Secondary_education <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSMINHS_L3)
mexico$University_education <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_CNSMINUN_L3)
mexico$Mean_Pm2.5 <- as.numeric(mexico$MX_L3_ANALYTIC_09022022_APSPM25MEAN2017L3)

table1(~ Population +
         Sewage +
         Floors +
         Unemployment +
         Labor_force +
         School_attending +
         Primary_education +
         Mean_Pm2.5,
       data = mexico,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

# 5. Panama ---------------
panama$Population <- as.numeric(panama$PA_L3_ANALYTIC_09022022_SPTPOPL3)
panama$Water <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSWATNETL3)
panama$Sewage <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSSEWANYL3)
panama$Floors <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSFLOORL3)
panama$Overcrowding <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSCROWD3RML3)
panama$Unemployment <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSUNEMPL3)
panama$Labor_force <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSLABPARTL3)
panama$School_attending <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSST1517L3)
panama$Primary_education <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSMINPR_L3)
panama$Secondary_education <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSMINHS_L3)
panama$University_education <- as.numeric(panama$PA_L3_ANALYTIC_09022022_CNSMINUN_L3)
panama$Mean_Pm2.5 <- as.numeric(panama$PA_L3_ANALYTIC_09022022_APSPM25MEAN2017L3)

table1(~ Population +
         Water +
         Sewage +
         Floors +
         Overcrowding +
         Unemployment +
         Labor_force +
         School_attending +
         Primary_education +
         Secondary_education +
         University_education +
         Mean_Pm2.5,
       data = panama,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

# 6. San Jose ---------------------------------
sanjose$Population <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_SPTPOPL3)
sanjose$Water <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSWATNETL3)
sanjose$Sewage <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSSEWANYL3)
sanjose$Floors <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSFLOORL3)
sanjose$Overcrowding <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSCROWD3RML3)
sanjose$Unemployment <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSUNEMPL3)
sanjose$Labor_force <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSLABPARTL3)
sanjose$School_attending <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSST1517L3)
sanjose$Primary_education <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSMINPR_L3)
sanjose$Secondary_education <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSMINHS_L3)
sanjose$University_education <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_CNSMINUN_L3)
sanjose$Mean_Pm2.5 <- as.numeric(sanjose$CR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3)

table1(~ Population +
         Water +
         Sewage +
         Floors +
         Overcrowding +
         Unemployment +
         Labor_force +
         School_attending +
         Primary_education +
         Secondary_education +
         University_education +
         Mean_Pm2.5,
       data = sanjose,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

# 7. Santiago -------------------
santiago$Population <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_SPTPOPL3)
santiago$Water <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSWATNETL3)
santiago$Sewage <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSSEWANYL3)
santiago$Floors <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSFLOORL3)
santiago$Overcrowding <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSCROWD3RML3)
santiago$Unemployment <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSUNEMPL3)
santiago$Labor_force <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSLABPARTL3)
santiago$School_attending <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSST1517L3)
santiago$Primary_education <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSMINPR_L3)
santiago$Secondary_education <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSMINHS_L3)
santiago$University_education <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_CNSMINUN_L3)
santiago$Mean_Pm2.5 <- as.numeric(santiago$CL_L3_ANALYTIC_09022022_APSPM25MEAN2017L3)

table1(~ Population +
         Water +
         Floors +
         Unemployment +
         Labor_force +
         School_attending +
         Primary_education +
         Secondary_education +
         University_education +
         Mean_Pm2.5,
       data = santiago,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

# 8. Sao Paolo -------------------------
saopaolo$Population <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_SPTPOPL3)
saopaolo$Water <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSWATNETL3)
saopaolo$Sewage <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSSEWANYL3)
saopaolo$Floors <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSFLOORL3)
saopaolo$Overcrowding <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSCROWD3RML3)
saopaolo$Unemployment <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSUNEMPL3)
saopaolo$Labor_force <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSLABPARTL3)
saopaolo$School_attending <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSST1517L3)
saopaolo$Primary_education <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSMINPR_L3)
saopaolo$Secondary_education <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSMINHS_L3)
saopaolo$University_education <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_CNSMINUN_L3)
saopaolo$Mean_Pm2.5 <- as.numeric(saopaolo$BR_L2_5_ANALYTIC_09022022_APSPM25MEAN2017L3)

table1(~ Population +
         Water +
         Sewage +
         Overcrowding +
         Unemployment +
         Labor_force +
         School_attending +
         Primary_education +
         Secondary_education +
         University_education +
         Mean_Pm2.5,
       data = saopaolo,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

## Box Violin plots ---------------

bv_1 <- ggstatsplot::ggbetweenstats(
  data = bogota %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(bogota$Primary_education)))))*as.numeric(factor(rank(bogota$Primary_education)))),5)),
  x = rankeo,
  y = Mean_Pm2.5,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "Mean PM 2.5",
  title = "Bogota: PM2.5 per rank of % Primary education population", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.5, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("ecomst") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_2 <- ggstatsplot::ggbetweenstats(
  data = buenosaires %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(buenosaires$Primary_education)))))*as.numeric(factor(rank(buenosaires$Primary_education)))),5)),
  x = rankeo,
  y = Mean_Pm2.5,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "Mean PM 2.5",
  title = "Buenos Aires: PM2.5 per rank of % Primary education population", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.5, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("ecomst") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_3 <- ggstatsplot::ggbetweenstats(
  data = guatemala %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(guatemala$Primary_education)))))*as.numeric(factor(rank(guatemala$Primary_education)))),5)),
  x = rankeo,
  y = Mean_Pm2.5,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "Mean PM 2.5",
  title = "Guatemala city: PM2.5 per rank of % Primary education population", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.5, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("ecomst") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_4 <- ggstatsplot::ggbetweenstats(
  data = mexico %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(mexico$Primary_education)))))*as.numeric(factor(rank(mexico$Primary_education)))),5)),
  x = rankeo,
  y = Mean_Pm2.5,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "Mean PM 2.5",
  title = "Mexico city: PM2.5 per rank of % Primary education population", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.5, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("ecomst") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_5 <- ggstatsplot::ggbetweenstats(
  data = panama %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(panama$Primary_education)))))*as.numeric(factor(rank(panama$Primary_education)))),5)),
  x = rankeo,
  y = Mean_Pm2.5,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "Mean PM 2.5",
  title = "Panama city: PM2.5 per rank of % Primary education population", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.5, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("ecomst") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_6 <- ggstatsplot::ggbetweenstats(
  data = sanjose %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(sanjose$Primary_education)))))*as.numeric(factor(rank(sanjose$Primary_education)))),5)),
  x = rankeo,
  y = Mean_Pm2.5,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "Mean PM 2.5",
  title = "San Jose de Costa Rica: PM2.5 per rank of % Primary education population", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.5, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("ecomst") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_7 <- ggstatsplot::ggbetweenstats(
  data = santiago %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(santiago$Primary_education)))))*as.numeric(factor(rank(santiago$Primary_education)))),5)),
  x = rankeo,
  y = Mean_Pm2.5,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "Mean PM 2.5",
  title = "Santiago de Chile: PM2.5 per rank of % Primary education population", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.5, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("ecomst") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_8 <- ggstatsplot::ggbetweenstats(
  data = saopaolo %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(saopaolo$Primary_education)))))*as.numeric(factor(rank(saopaolo$Primary_education)))),5)),
  x = rankeo,
  y = Mean_Pm2.5,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "Mean PM 2.5",
  title = "Sao Paolo: PM2.5 per rank of % Primary education population", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.5, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("ecomst") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

ggarrange(bv_1,
          bv_2,
          bv_3,
          bv_4,
          bv_5,
          bv_6,
          bv_7,
          bv_8,
          labels = "AUTO",
          nrow = 3,
          ncol = 3)

ggsave("boxviolin_education_8c.png", 
       plot = last_plot(),
       width = 27,
       height = 23,
       bg = "white",
       dpi = 300
       )

## Mosaic Plots ---------
bogota$PM2.5_cat <- "A"

bogota$PM2.5_cat[bogota$Mean_Pm2.5 <= 20] <- 0
bogota$PM2.5_cat[bogota$Mean_Pm2.5 > 20] <- 1

bogota$PM2.5_cat <- factor(bogota$PM2.5_cat, c("≤ 20","> 20"))

bogota$Educacion_cat <- "A"

bogota$Educacion_cat[bogota$Primary_education <= 80] <- "≤ 80"
bogota$Educacion_cat[bogota$Primary_education <= 90 & bogota$Primary_education > 80] <- "80 - 90"
bogota$Educacion_cat[bogota$Primary_education > 90] <- "> 90"

bogota$Educacion_cat <- factor(bogota$Educacion_cat, c("≤ 80", "80 - 90", "> 90"))


count <- table(bogota$Educacion_cat, 
               bogota$PM2.5_cat)
count
m1 <- mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
           col = c ("#dd1c1a", "#06aed5"), main = "Bogota: Primary educaiton frequency above 20 Pm2.5"
)

bogota %>%
  ggplot() + 
  geom_mosaic(aes(x = product(PM2.5_cat, Educacion_cat), fill = PM2.5_cat)) +
  theme_bw()


buenosaires$PM2.5_cat <- "A"

buenosaires$PM2.5_cat[buenosaires$Mean_Pm2.5 <= 15] <- "≤ 15"
buenosaires$PM2.5_cat[buenosaires$Mean_Pm2.5 > 15] <- "> 15"

buenosaires$PM2.5_cat <- factor(buenosaires$PM2.5_cat, c("≤ 15","> 15"))

buenosaires$Educacion_cat <- "A"

buenosaires$Educacion_cat[buenosaires$Primary_education <= 80] <- "≤ 80"
buenosaires$Educacion_cat[buenosaires$Primary_education <= 90 & buenosaires$Primary_education > 80] <- "80 - 90"
buenosaires$Educacion_cat[buenosaires$Primary_education > 90] <- "> 90"

buenosaires$Educacion_cat <- factor(buenosaires$Educacion_cat, c("≤ 80", "80 - 90", "> 90"))


count <- table(buenosaires$Educacion_cat, 
               buenosaires$PM2.5_cat)
count
m2 <- mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                 col = c ("#dd1c1a", "#06aed5"), main = "Buenos Aires: Primary educaiton frequency above 15 Pm2.5"
)

guatemala$PM2.5_cat <- "A"

guatemala$PM2.5_cat[guatemala$Mean_Pm2.5 <= 30] <- "≤ 30"
guatemala$PM2.5_cat[guatemala$Mean_Pm2.5 > 30] <- "> 30"

guatemala$PM2.5_cat <- factor(guatemala$PM2.5_cat, c("≤ 30","> 30"))

guatemala$Educacion_cat <- "A"

guatemala$Educacion_cat[guatemala$Primary_education <= 80] <- "≤ 80"
guatemala$Educacion_cat[guatemala$Primary_education <= 90 & guatemala$Primary_education > 80] <- "80 - 90"
guatemala$Educacion_cat[guatemala$Primary_education > 90] <- "> 90"

guatemala$Educacion_cat <- factor(guatemala$Educacion_cat, c("≤ 80", "80 - 90", "> 90"))


count <- table(guatemala$Educacion_cat, 
               guatemala$PM2.5_cat)
count
m3 <- mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                 col = c ("#dd1c1a", "#06aed5"), main = "Guatemala City: Primary educaiton frequency above 30 Pm2.5"
)

mexico$PM2.5_cat <- "A"

mexico$PM2.5_cat[mexico$Mean_Pm2.5 <= 23] <- "≤ 23"
mexico$PM2.5_cat[mexico$Mean_Pm2.5 > 23] <- "> 23"

mexico$PM2.5_cat <- factor(mexico$PM2.5_cat, c("≤ 23","> 23"))

mexico$Educacion_cat <- "A"

mexico$Educacion_cat[mexico$Primary_education <= 80] <- "≤ 80"
mexico$Educacion_cat[mexico$Primary_education <= 90 & mexico$Primary_education > 80] <- "80 - 90"
mexico$Educacion_cat[mexico$Primary_education > 90] <- "> 90"

mexico$Educacion_cat <- factor(mexico$Educacion_cat, c("≤ 80", "80 - 90", "> 90"))


count <- table(mexico$Educacion_cat, 
               mexico$PM2.5_cat)
count
m4 <- mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                 col = c ("#dd1c1a", "#06aed5"), main = "Mexico City: Primary educaiton frequency above 23 Pm2.5"
)

panama$PM2.5_cat <- "A"

panama$PM2.5_cat[panama$Mean_Pm2.5 <= 17] <- "≤ 17"
panama$PM2.5_cat[panama$Mean_Pm2.5 > 17] <- "> 17"

panama$PM2.5_cat <- factor(panama$PM2.5_cat, c("≤ 17","> 17"))

panama$Educacion_cat <- "A"

panama$Educacion_cat[panama$Primary_education <= 80] <- "≤ 80"
panama$Educacion_cat[panama$Primary_education <= 90 & panama$Primary_education > 80] <- "80 - 90"
panama$Educacion_cat[panama$Primary_education > 90] <- "> 90"

panama$Educacion_cat <- factor(panama$Educacion_cat, c("≤ 80", "80 - 90", "> 90"))


count <- table(panama$Educacion_cat, 
               panama$PM2.5_cat)
count
m5 <- mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                 col = c ("#dd1c1a", "#06aed5"), main = "Panama city: Primary educaiton frequency above 17 Pm2.5"
)

sanjose$PM2.5_cat <- "A"

sanjose$PM2.5_cat[sanjose$Mean_Pm2.5 <= 20] <- 0
sanjose$PM2.5_cat[sanjose$Mean_Pm2.5 > 20] <- 1

sanjose$PM2.5_cat <- factor(sanjose$PM2.5_cat, c("≤ 20","> 20"))

sanjose$Educacion_cat <- "A"

sanjose$Educacion_cat[sanjose$Primary_education <= 80] <- "≤ 80"
sanjose$Educacion_cat[sanjose$Primary_education <= 90 & sanjose$Primary_education > 80] <- "80 - 90"
sanjose$Educacion_cat[sanjose$Primary_education > 90] <- "> 90"

sanjose$Educacion_cat <- factor(sanjose$Educacion_cat, c("≤ 80", "80 - 90", "> 90"))


count <- table(sanjose$Educacion_cat, 
               sanjose$PM2.5_cat)
count
m6 <- mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                 col = c ("#dd1c1a", "#06aed5"), main = "San Jose de Puerto Rico: Primary educaiton frequency above 20 Pm2.5"
)

santiago$PM2.5_cat <- "A"

santiago$PM2.5_cat[santiago$Mean_Pm2.5 <= 30] <- "≤ 30"
santiago$PM2.5_cat[santiago$Mean_Pm2.5 > 30] <- "> 30"

santiago$PM2.5_cat <- factor(santiago$PM2.5_cat, c("≤ 30","> 30"))

santiago$Educacion_cat <- "A"

santiago$Educacion_cat[santiago$Primary_education <= 80] <- "≤ 80"
santiago$Educacion_cat[santiago$Primary_education <= 90 & santiago$Primary_education > 80] <- "80 - 90"
santiago$Educacion_cat[santiago$Primary_education > 90] <- "> 90"

santiago$Educacion_cat <- factor(santiago$Educacion_cat, c("≤ 80", "80 - 90", "> 90"))


count <- table(santiago$Educacion_cat, 
               santiago$PM2.5_cat)
count
m7 <- mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                 col = c ("#dd1c1a", "#06aed5"), main = "Santiago de Chile: Primary educaiton frequency above 30 Pm2.5"
)

saopaolo$PM2.5_cat <- "A"

saopaolo$PM2.5_cat[saopaolo$Mean_Pm2.5 <= 17] <- "≤ 17"
saopaolo$PM2.5_cat[saopaolo$Mean_Pm2.5 > 17] <- "> 17"

saopaolo$PM2.5_cat <- factor(saopaolo$PM2.5_cat, c("≤ 17","> 17"))

saopaolo$Educacion_cat <- "A"

saopaolo$Educacion_cat[saopaolo$Primary_education <= 80] <- "≤ 80"
saopaolo$Educacion_cat[saopaolo$Primary_education <= 90 & saopaolo$Primary_education > 80] <- "80 - 90"
saopaolo$Educacion_cat[saopaolo$Primary_education > 90] <- "> 90"

saopaolo$Educacion_cat <- factor(saopaolo$Educacion_cat, c("≤ 80", "80 - 90", "> 90"))


count <- table(saopaolo$Educacion_cat, 
               saopaolo$PM2.5_cat)
count
m8 <- mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                 col = c ("#dd1c1a", "#06aed5"), main = "Sao Paolo: Primary educaiton frequency above 17 Pm2.5"
)

m10 <- list(mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                       col = c ("#dd1c1a", "#06aed5"), main = "San Jose de Puerto Rico: Primary educaiton frequency above 20 Pm2.5"
),
mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
           col = c ("#dd1c1a", "#06aed5"), main = "Santiago de Chile: Primary educaiton frequency above 30 Pm2.5"
),
mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
           col = c ("#dd1c1a", "#06aed5"), main = "Sao Paolo: Primary educaiton frequency above 17 Pm2.5"
))

ggarrange(mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                     col = c ("#dd1c1a", "#06aed5"), main = "San Jose de Puerto Rico: Primary educaiton frequency above 20 Pm2.5"
          ),
          mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                     col = c ("#dd1c1a", "#06aed5"), main = "Santiago de Chile: Primary educaiton frequency above 30 Pm2.5"
          ),
          mosaicplot((count), sub = "Primary education level", ylab = "PM2.5", 
                     col = c ("#dd1c1a", "#06aed5"), main = "Sao Paolo: Primary educaiton frequency above 17 Pm2.5"
          ),
          labels = "AUTO",
          nrow = 1,
          ncol = 3)

ggsave("boxviolin_education_8c.png", 
       plot = last_plot(),
       width = 27,
       height = 23,
       bg = "white",
       dpi = 300
)

# SII & RII -----------------------------------------

bogota$PM2.5_cat <- 0

bogota$PM2.5_cat[bogota$Mean_Pm2.5 <= 20] <- 0

bogota$PM2.5_cat[bogota$Mean_Pm2.5 > 20] <- 1

bogota$PM2.5_cat <- as.numeric(bogota$PM2.5_cat)

bogota1 <- bogota %>%
  mutate(prim_quantile = ntile(Primary_education, 5))

bogota_total <- sum(as.numeric(bogota$CO_L3_ANALYTIC_09022022_CNSPOPL3), na.rm = T)

sii1 <- bogota1 %>%
  
  st_set_geometry(NULL) %>% 
  
  drop_na(prim_quantile) %>% 
  
  group_by(prim_quantile) %>% 
  
  summarize(pop = sum(as.numeric(CO_L3_ANALYTIC_09022022_CNSPOPL3), na.rm = T), 
            prop_p = pop/bogota_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CO_L3_ANALYTIC_09022022_CNSPOPL3) * PM2.5_cat, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
)  ## concentracion = con_i = (pt*lead(Lt, default=0))-(lead(pt, default=0)*Lt),


lm1 <- lm(tasa_cases~rank, 
   data = sii1)
  
summary(lm1)

p.low <- predict(lm1, data.frame(rank=0))
p.high <- predict(lm1, data.frame(rank=1))

(RII <- p.high/p.low)


sii1_man <- bogota1 %>%
  
  st_set_geometry(NULL) %>% 
  
  drop_na(prim_quantile) %>% 
  
  group_by(prim_quantile) %>% 
  
  summarize(pop = n(), 
            prop_p = pop/nrow(), ### nota: mejorar nrow
            ncases = sum(PM2.5_cat),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )  ## concentracion = con_i = (pt*lead(Lt, default=0))-(lead(pt, default=0)*Lt),


lm1 <- lm(tasa_cases~rank, 
          data = sii1)

