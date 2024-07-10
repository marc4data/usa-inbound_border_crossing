library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(reader)
library(scales)ƒ
library(hrbrthemes)
library(forcats)

getwd()
setwd("~/Documents/Projects/border_crossing_rstudio")

# Loading the data from CSV file, 
data_00_base <- data.frame(reader("Border_Crossing_Entry_Data.csv"))
# data_00_base <- data.frame(data_00_base)
# cleanse commas (e.g. 12,000) from the field that holds the measures numeric values
data_00_base$Value <- as.numeric(gsub("[^0-9.-]", "", data_00_base$Value))


# Make this happen (the plan of attack)
# Line Chart of Border Crossings per Month, Mexico vs Canada
# Bar Charts stratified by Modes of Crossing the Border, MEX vs CAN
# Nuances
#   Any drastic changes in the 20+ years of data collection.  COVID hit in March 2020.  How long was the recovery.  
#       Did 9/11 impact border crossings
#   MEX vs CAN, Volume, cyclical patterns, almost no Pedestrians from Canada, and MEX about 4:1 on most Modes.  CAN wins for Trains.
# 
#   Personal Vehicles
#      Strong cyclical for CAN
#      MEX steady # of cars but decreasing # of Passengers. Show graph that shows ratio of Passengers/Vehicle vs time
#      MEX peaks in December (holidays), trough in Feb

# Order of Operations from 
# Data
# Mapping
# Statistics
# Scales
# Geometries
# Facets
# Coordinates
# Theme     

# Sublime Tips: https://youtu.be/YgEuzl9nnbc?si=WJG5eyhRKPggAAP8
# Talented guy with ggplot lays it out in: https://ggraph.data-imaginist.com/reference/index.html


#coord_cartesian(xlim=c("1990", 2030, 5), ylim = c(0, 160000000)) + 

# breaks = scales::breaks_extended(8),
# labels = scales::label_number()  

# Potential enhancement
#  + scale_x_continuous(breaks=seq(1990, 2030, 10))  
# More potential enhancements (similar but differerent from a different resource)    
#   scale_x_continuous(breaks = c(3, 5, 6)) + 
#    scale_y_continuous(trans = 'log10')    
# coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) +


# rm(data_10a)
# library(paletteer) ; paletteer_c("viridis::inferno", n=10)

# Add some features to the BASE data frame.  This is the BASE that all sub-queries (data frames) will pull from.
data_00_base_mod <- data_00_base %>%
  mutate(Border_Code = ifelse(grepl("Canada", Border, fixed = TRUE), "CAN", "MEX")) %>%
  separate(Date, c("Month", "Year"), remove = FALSE)  %>%
  # mutate(Measure = factor(Measure, order = TRUE, levels = c("Bus Passengers","Buses","Pedestrians","Personal Vehicle Passengers","Personal Vehicles","Rail Containers Empty","Rail Containers Loaded","Train Passengers","Trains","Truck Containers Empty","Truck Containers Loaded","Trucks"))) 
  # mutate(Measure = factor(Measure, order = TRUE, levels = c("Personal Vehicles","Personal Vehicle Passengers","Pedestrians","Trucks","Truck Containers Loaded","Truck Containers Empty","Buses","Bus Passengers","Trains","Train Passengers","Rail Containers Loaded","Rail Containers Empty"))) 
  mutate(measure_summary = factor(Measure, order = TRUE, levels = c("Personal Vehicles","Personal Vehicle Passengers","Pedestrians","Trucks","Truck Containers Loaded","Truck Containers Empty","Buses","Bus Passengers","Trains","Train Passengers","Rail Containers Loaded","Rail Containers Empty"))) %>%
  mutate(Measure = factor(Measure, order = TRUE, levels = c("Trains","Train Passengers","Rail Containers Loaded","Rail Containers Empty","Buses","Bus Passengers","Trucks","Truck Containers Loaded","Truck Containers Empty","Pedestrians","Personal Vehicles","Personal Vehicle Passengers"))) %>%
  # mutate(Year_num) = as.integer(Year)
  mutate(state_delete_me = State)
  
data_00_base_mod$Year_num <- as.integer(data_00_base_mod$Year)

  # Augment with a dimension table to help control colors and sorting in the presentation tier
  # This is all stuff I decided on when building the Tableau dashboard.  I want to follow that lead with R to make it easier to compare/contrast output side-by-side
  dim_measure <- data.frame(
    Measure=c("Bus Passengers","Buses","Pedestrians","Personal Vehicle Passengers","Personal Vehicles","Rail Containers Empty","Rail Containers Loaded","Train Passengers","Trains","Truck Containers Empty","Truck Containers Loaded","Trucks")
    ,mode_main_sort=c(8,7,3,2,1,11,10,12,9,6,5,4)
    ,mode_color_code=c("#C799BC","#8074A8","#4E79A7","#F59C3C","#C14F22","#CDCECD","#5B6570","#89C8CC","#848E93","#F4D166","#B2C25B","#34844A")
  )
  
  # Left Join the Base table with supplemental information about the Measures to help with the presentation layer
  data_00_base_mod <- merge(x = data_00_base_mod, y = dim_measure, by = "Measure", all.x = TRUE)

# Round #1a
# High-level summary of CAN vs MEX vs YEAR vs MODE
data_10a_core <- data_00_base_mod %>%
  select(Year_num, Border_Code, Measure, Value, mode_main_sort, mode_color_code) %>%
  filter(Measure == "Personal Vehicles" | Measure == "Pedestrians" | Measure == "Trucks" | Measure == "Buses" | Measure =="Trains") %>%
  group_by(Year_num, Border_Code, Measure, mode_main_sort, mode_color_code) %>%
  summarise(annual_crossings = sum(Value), record_cnt = n(), .groups = 'keep') %>%
  arrange(Border_Code, Year_num, mode_main_sort, Measure) 

gg_10a_overview <- ggplot(data_10a_core) + 
    geom_col(aes(x=Year_num, y=annual_crossings, color = "gray80", fill = mode_color_code)) +
    scale_fill_identity() + 
    scale_color_identity() +
    facet_grid(, vars(Border_Code)) +
    # scale_colour_ipsum(fill) + 
    # theme_ipsum() +
    # theme_classic() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=0, hjust = .5),
          panel.background = element_rect(fill = "white",
                                          colour = "gray80",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray90"),
          panel.grid.minor = element_line(size = 0, linetype = 'solid',
                                          colour = "white")
          ) +
    theme(legend.position = "left") +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)
        # breaks = scales::breaks_extended(8),
        # labels = scales::label_number()  
    ) + 
    labs(
      x = 'Country / Year', 
      y = 'Annual Inbound Border Crossings',
      title = 'US Inbound Border Crossings by Country by Year',
      # subtitle = 'Color indicates the Mode of Transportation',
      caption = 'Bureau of Transportation Statistics'
    ) # + guides(fill = guide_legend(title = "Mode of Transportqtion"))
plot(gg_10a_overview)

################################################################################
# Round #1b
# High-level summary of CAN vs MEX vs Mode for 2023

  # Query the BASE dataset and shape (aggregate) for #1b (Border Crossing Volume by Mode by Country for a given year)
  data_10b_core <- data_00_base_mod %>%
    filter(Measure == "Personal Vehicles" | Measure == "Pedestrians" | Measure == "Trucks" | Measure == "Buses" | Measure =="Trains") %>%
    filter(Year == "2023") %>%
    select(Border_Code, Measure, mode_color_code, Value) %>%
    group_by(Border_Code, Measure, mode_color_code) %>%
    summarise(annual_crossings = sum(Value), record_cnt = n(), .groups = 'keep') %>%
    arrange(Border_Code, - annual_crossings)

  # Build #1b graph
  # 2024-06-27 
  gg_10b_year_summary <- ggplot(data_10b_core) + 
    geom_col(aes(x=Measure, y=annual_crossings, fill = mode_color_code)) +
    geom_text(aes(x=Measure, y=annual_crossings, label = paste(round(annual_crossings / 1e6, 2), "M")), vjust = 0.5, hjust = -0.2) +
    coord_flip() +
    scale_fill_identity() +
    facet_grid(vars(Border_Code, )) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle=0),
          panel.background = element_blank(),
          strip.background = element_rect(colour="gray50", fill="gray90"),
          panel.border = element_rect(colour = "gray50", fill=NA, size=1)) +
  #  theme(strip.text.y = element_text(size = 8, colour = "red", angle = 90)) +
  #  theme(strip.background.y = element_text(size = 8, colour = "red", angle = 90)) +
    scale_y_continuous(name = "Inbound Border Crossings",
                       # breaks = scales::breaks_extended(8),
                       # labels = scales::label_number()  
                       labels = unit_format(unit = "M", scale = 1e-6)
    ) + 
    labs(title = "Border Crossings by Mode and Country in 2023")
  plot(gg_10b_year_summary)


################################################################################
  
  # A second pass at this using a secondary dim field, measure_summary
  data_10b_core <- data_00_base_mod %>%
    filter(measure_summary == "Personal Vehicles" | measure_summary == "Pedestrians" | measure_summary == "Trucks" | measure_summary == "Buses" | measure_summary =="Trains") %>% 
    filter(Year == "2023") %>%
    select(Border_Code, measure_summary, mode_color_code, Value) %>%
    group_by(Border_Code, measure_summary, mode_color_code) %>%
    summarise(annual_crossings = sum(Value), record_cnt = n(), .groups = 'keep') %>%
    arrange(Border_Code, - annual_crossings)

  data_10b_core
  write.csv(data_10b_core, file = "chart_data.csv", row.names = FALSE)
  write.table(data_10b_core, file = "chart_data.txt", row.names = FALSE, sep = "|")
  
  data_10b_core_subtotal <- data_10b_core %>%
    group_by(Border_Code) %>%
    summarise(annual_crossings = sum(annual_crossings), record_cnt = n(), .groups = 'keep') %>%
    arrange(Border_Code, - annual_crossings) %>%
    # mutate(Border_Code = ifelse(grepl("Canada", Border, fixed = TRUE), "CAN", "MEX")) %>%
    mutate
  
  
  
  # "Trains","Train Passengers","Rail Containers Loaded","Rail Containers Empty","Buses","Bus Passengers","Trucks","Truck Containers Loaded","Truck Containers Empty","Pedestrians","Personal Vehicles","Personal Vehicle Passengers"
  # forcats::fct_reorder(streaming, score)
  
  # 2024-06-28 
  
  # ggplot(df, aes(x = forcats::fct_reorder(streaming, score), y = score)) + geom_col()  
  gg_10b_year_summary <- ggplot(data_10b_core) + 
    geom_col(aes(x=measure_summary, y=annual_crossings, fill = mode_color_code)) +
    geom_text(aes(x=measure_summary, y=annual_crossings, label = paste(round(annual_crossings / 1e6, 2), "M")), vjust = 0.5, hjust = -0.2) +
    coord_flip() +
    scale_fill_identity() +
    facet_grid(vars(Border_Code, )) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle=0),
          panel.background = element_blank(),
          strip.background = element_rect(colour="gray50", fill="gray90"),
          panel.border = element_rect(colour = "gray50", fill=NA, size=1)) +
    #  theme(strip.text.y = element_text(size = 8, colour = "red", angle = 90)) +
    #  theme(strip.background.y = element_text(size = 8, colour = "red", angle = 90)) +
    scale_y_continuous(name = "Inbound Border Crossings",
                       # breaks = scales::breaks_extended(8),
                       # labels = scales::label_number()  
                       labels = unit_format(unit = "M", scale = 1e-6)
    ) + 
    labs(title = "Border Crossings by Mode and Country in 2023")
  plot(gg_10b_year_summary)
  
# Third Try is a Charm (with help from StackOverflow)
# https://stackoverflow.com/questions/78705277/control-sorting-of-field-in-a-geom-col

    gg_10b_year_summary <- ggplot(data_10b_core, aes(
      x = annual_crossings,
      y = reorder(measure_summary, annual_crossings, FUN = sum)
  )) +
    geom_col(
        aes(fill = mode_color_code)
    ) + 
    geom_label(
      aes(
        label = paste(round(annual_crossings / 1e6, 2), "M")
      ),
      hjust = 0,
      fill = NA,
      label.size = 0
    ) +
    scale_fill_identity() +
    facet_grid(vars(Border_Code))  +
    theme_minimal() +
      theme(
        axis.text.y = element_text(angle = 0),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "gray50", fill = "gray90"),
        panel.border = element_rect(colour = "gray50", fill = NA, size = .2), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      scale_x_continuous(
        name = "Inbound Border Crossings",
        labels = unit_format(unit = "M", scale = 1e-6)
      ) +
      labs(title = "Border Crossings by Mode and Country in 2023") 

  plot(gg_10b_year_summary)
  
# Fourth try, this time going to simplify for a summary by country, with stacked bars color by Mode of Crossing
  gg_10c_year_summary <- 
  ggplot(data_10b_core, aes(
    x = annual_crossings,
    y = Border_Code # reorder(measure_summary, annual_crossings, FUN = sum)
  )) +
    geom_col(
      # aes(fill = mode_color_code,color = Border_Code)
      aes(color = "gray80", fill = mode_color_code)
    ) + 
    # geom_label(
    #   aes(
    #     label = paste(round(annual_crossings / 1e6, 2), "M")
    #   ),
    #   hjust = 0,
    #   fill = NA,
    #   label.size = 0
    # ) +
    scale_fill_identity() +
    # facet_grid(vars(Border_Code))  +
    theme_minimal() +
    theme(
      axis.text.y = element_text(angle = 0),
      panel.background = element_blank(),
      strip.background = element_rect(colour = "gray50", fill = "gray90"),
      panel.border = element_rect(colour = "gray50", fill = NA, size = .2), 
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank()
      # legend.title = element_text = "Mode of Border Crossing",
      # legend.text = element_text()
    ) +
    scale_x_continuous(
      name = "Inbound Border Crossings",
      labels = unit_format(unit = "M", scale = 1e-6)
    ) +
    labs(title = "Border Crossings by Mode and Country in 2023", color = "Border") 
  
  plot(gg_10b_year_summary)  
  