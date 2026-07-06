#### Total construction spending ####

ggplot(ttl_cnst, aes(x = Date, y = `TTL Construction Spending`)) +
  geom_line(color = ttl_cnst_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Total Construction Spending", x = "", y = "")

#### ENR exploratory plotting ####

# BCI plot

ggplot(enr_indices, aes(x = Date, y = BCI)) +
  geom_line(color = bci_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "ENR Building Cost Index (BCI)", x = "", y = "")

# CCI Plot

ggplot(enr_indices, aes(x = Date, y = CCI)) +
  geom_line(color = cci_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "ENR Construction Cost Index (CCI)", x = "", y = "")

# SLI Plot

ggplot(enr_indices, aes(x = Date, y = SLI)) +
  geom_line(color = sli_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "ENR Skilled Labor Index (SLI)", x = "", y = "")

# CLI Plot

ggplot(enr_indices, aes(x = Date, y = CLI)) +
  geom_line(color = cli_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "ENR Common Labor Index (CLI)", x = "", y = "")

# MPI Plot

ggplot(enr_indices, aes(x = Date, y = MPI)) +
  geom_line(color = mpi_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "ENR Material Price Index (MPI)", x = "", y = "")

# All Indices

ggplot(enr_indices, aes(Date)) +
  geom_line(aes(y = BCI, color = "BCI"), size = prime_line) +
  geom_line(aes(y = CCI, color = "CCI"), size = prime_line) +
  geom_line(aes(y = SLI, color = "SLI"), size = data_line) +
  geom_line(aes(y = CLI, color = "CLI"), size = data_line) +
  geom_line(aes(y = MPI, color = "MPI"), size = data_line) +
  scale_color_manual(values = enr_colors, name = "Indices") +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "ENR Cost Indices", y = "", x = "")
  
#### ENR Scaling (min max) ####

# all indices

ggplot(enr_scaled, aes(Date)) +
  geom_line(aes(y = BCI, color = "BCI"), size = prime_line) +
  geom_line(aes(y = CCI, color = "CCI"), size = prime_line) +
  geom_line(aes(y = SLI, color = "SLI"), size = data_line) +
  geom_line(aes(y = CLI, color = "CLI"), size = data_line) +
  geom_line(aes(y = MPI, color = "MPI"), size = data_line) +
  scale_color_manual(values = enr_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Normalized ENR Cost Indices", y = "", x = "")

# components

ggplot(enr_scaled, aes(Date)) +
  geom_line(aes(y = SLI, color = "SLI"), size = data_line) +
  geom_line(aes(y = CLI, color = "CLI"), size = data_line) +
  geom_line(aes(y = MPI, color = "MPI"), size = data_line) +
  scale_color_manual(values = enr_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Normalized ENR Component Indices", y = "", x = "")

#### Fred Labor exploratory plotting ####

# Federal Minimum Wage

ggplot(fred_data, aes(x = Date, y = fed_min_wage)) +
  geom_line(color = fed_min_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Federal Minimum Wage", y = "", x = "")

# CA Minimum Wage

ggplot(fred_data, aes(x = Date, y = cal_min_wage)) +
  geom_line(color = cal_min_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "California Minimum Wage", y = "", x = "")

# Construction Wages

ggplot(fred_data, aes(x = Date, y = const_ns_wage)) +
  geom_line(color = const_ns_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Construction Wages", y = "", x = "")

# All fred labor

ggplot(fred_data, aes(Date)) +
  geom_line(aes(y = fed_min_wage, color = "fed_min_wage"), size = data_line) +
  geom_line(aes(y = cal_min_wage, color = "cal_min_wage"), size = data_line) +
  geom_line(aes(y = const_ns_wage, color = "const_ns_wage"), 
            size = prime_line) +
  scale_color_manual(values = wage_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Wage Data", y = "", x = "")

#### Fred Scaling (min max) ####

ggplot(fred_scaled, aes(Date)) +
  geom_line(aes(y = fed_min_wage, color = "fed_min_wage"), size = data_line) +
  geom_line(aes(y = cal_min_wage, color = "cal_min_wage"), size = data_line) +
  geom_line(aes(y = const_ns_wage, color = "const_ns_wage"), 
            size = prime_line) +
  scale_color_manual(values = wage_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Normalized Wage Data", y = "", x = "")

#### Labor Exploratory plotting ####

# Normalized

ggplot(lab_scaled, aes(Date)) +
  geom_line(aes(y = SLI, color = "SLI"), size = data_line) +
  geom_line(aes(y = CLI, color = "CLI"), size = data_line) +
  geom_line(aes(y = fed_min_wage, color = "fed_min_wage"), size = data_line) +
  geom_line(aes(y = cal_min_wage, color = "cal_min_wage"), size = data_line) +
  geom_line(aes(y = const_ns_wage, color = "const_ns_wage"), 
            size = prime_line) +
  scale_color_manual(values = wage_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Normalized Wage Data & Wage Indices", y = "", x = "")

#### Material exploratory plotting ####

ggplot(mat, aes(x = Date, y = MPI)) +
  geom_line(color = mpi_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Material Price Index", y = "", x = "")

ggplot(mat, aes(x = Date, y = lumber)) +
  geom_line(color = lumber_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "PPI: Lumber", y = "", x = "")

ggplot(mat, aes(x = Date, y = paint)) +
  geom_line(color = paint_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "PPI: Paint", y = "", x = "")

ggplot(mat, aes(x = Date, y = steel)) +
  geom_line(color = steel_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "PPI: Steel", y = "", x = "")

ggplot(mat, aes(x = Date, y = concrete)) +
  geom_line(color = concrete_color, size = prime_line) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "PPI: Concrete", y = "", x = "")

ggplot(mat, aes(Date)) +
  geom_line(aes(y = lumber, color = "lumber"), size = data_line) +
  geom_line(aes(y = paint, color = "paint"), size = data_line) +
  geom_line(aes(y = steel, color = "steel"), size = data_line) +
  geom_line(aes(y = concrete, color = "concrete"), size = data_line) +
  scale_color_manual(values = mat_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Material Indices", y = "", x = "")

#### Material & lumber scaling ####

# Normalized

ggplot(mat_scaled, aes(Date)) +
  geom_line(aes(y = MPI, color = "MPI"), size = prime_line) +
  geom_line(aes(y = lumber, color = "lumber"), size = data_line) +
  geom_line(aes(y = paint, color = "paint"), size = data_line) +
  geom_line(aes(y = steel, color = "steel"), size = data_line) +
  geom_line(aes(y = concrete, color = "concrete"), size = data_line) +
  scale_color_manual(values = mat_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Normalized MPI & Material Indices", y = "", x = "")



