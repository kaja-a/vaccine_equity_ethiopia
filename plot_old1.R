# Plots for vaccine equity in Ethiopia study 

# load libraries
library (data.table)
library (ggplot2)
library (ggpubr)

# clear workspace
rm (list = ls ())

# ------------------------------------------------------------------------------
# Plot - Vaccination coverage
# Vaccination coverage in Ethiopia among children aged 12-23 months by 
# socioeconomic, geographic, maternal and child characteristics.
# ------------------------------------------------------------------------------

# read file with plot data
plot_data <- fread ("data_coverage.csv")
plot_list <- vector (mode = "list", length = 2)

# loop through 2 plots for vaccination coverage
for (i in 1:2) {
  
  plot_dat <- switch (i, 
                      "1" = plot_data [characteristics != "Household wealth" & 
                                         characteristics != "Religion" & 
                                         characteristics != "Ethnicity" & 
                                         characteristics != "Residence" & 
                                         characteristics != "Region"],
                      "2" = plot_data [characteristics == "Household wealth" | 
                                         characteristics == "Religion" | 
                                         characteristics == "Ethnicity" | 
                                         characteristics == "Residence" |
                                         characteristics == "Region"]
  )
  
  # plot title
  plot_title <- switch (i, 
                        "1" = "Basic vaccination coverage by maternal and child characteristics", 
                        "2" = "Basic vaccination coverage by socioeconomic and geographic characteristics")
  
  # plot
  plot_list [[i]] <- ggplot (data = plot_dat, 
                             aes (x = reorder (specific_characteristics, -coverage), 
                                  y = coverage, 
                                  fill = -coverage)) + 
    geom_bar (stat = "identity", width = 0.75, alpha=0.9) + 
    labs (x = "",
          y = "Basic vaccination coverage (%)", 
          title = plot_title) +
    coord_flip () + 
    facet_grid (characteristics ~ ., scales = "free") +
    theme_bw () + 
    theme(legend.position="none")
}

# print plot
print (plot_list [[1]])
print (plot_list [[2]])

# save plot to file
ggsave (filename = "plot_coverage_maternal_child.png", 
        plot = plot_list [[1]], 
        units = "in", width = 8, height = 8, 
        dpi = 300)

ggsave (filename = "plot_coverage_socioeconomic_geographic.png", 
        plot = plot_list [[2]], 
        units = "in", width = 8, height = 8, 
        dpi = 300)


# ------------------------------------------------------------------------------
# Plot - Adjusted odds ratios
# Inequities in vaccination coverage in Ethiopia associated with 
# socioeconomic, geographic, maternal and child characteristics.
# ------------------------------------------------------------------------------

# read file with plot data
plot_data <- fread ("data_aor.csv")
plot_list <- vector (mode = "list", length = 2)

# loop through 2 subplots
for (i in 1:2) {
  
  plot_dat <- switch (i, 
                      "1" = plot_data [characteristics != "Region" & 
                                         characteristics != "Birth order" & 
                                         characteristics != "Child" ],
                      "2" = plot_data [characteristics == "Region" | 
                                         characteristics == "Birth order" | 
                                         characteristics == "Child"]
                      )
  
  # plot
  plot_list [[i]] <- ggplot (data = plot_dat, 
                             aes (x = reorder (specific_characteristics, -AOR), 
                                  y = AOR, 
                                  fill = -AOR)) + 
    geom_bar (stat = "identity", width = 0.75, alpha=0.9) + 
    geom_errorbar (aes (ymin = low_95ci, ymax = high_95ci, width = 0.25), 
                   col = "orange") + 
    labs (x = "",
          y = "Adjusted odds ratios") +
    coord_flip () + 
    facet_grid (characteristics ~ ., scales = "free") +
    theme_bw () + 
    theme(legend.position="none")
  
}

# arrange plot columns and rows
p <- ggarrange (plotlist = plot_list, ncol = 2, nrow = 1)

p <- annotate_figure (p,
                     top = text_grob ("                     Adjusted odds ratios of basic vaccination coverage in children aged 12-23 months",
                                      color = "black", 
                                      size = 12))
# print plot
print (p)

# save plot to file
ggsave (filename = "plot_aor.png", plot = p, 
        units = "in", width = 8, height = 6.5, 
        dpi = 300)



