

run_comparison_with_old_data = function() {

library(tidyverse)
library(readxl)
library(httr)
library(gridExtra)

### Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# Directories
datadir = file.path(getwd(), "data")
outputdir = file.path(getwd(), "output")

# New data
gdp_exp = read_csv(file.path(outputdir, "gdp_expenditure_update.csv"), show_col_types = F)

# Economy labels
labels = read.csv(file.path(datadir, "lab_all.csv")) %>% 
  mutate(Label = replace(Label, Code == 498, "Republic of Moldova")) %>% 
  mutate(Label = replace(Label, Code == 410, "Republic of Korea")) %>% 
  mutate(Label = replace(Label, Code == 890, "Yugoslavia, Soc. Fed. Rep. of"))

# Old data 
old_data = read_csv(file.path(datadir, "US_GDPComponent.csv"))

component_lables = old_data %>% 
  filter(Year == 1970,
         Economy == "0000") %>% 
  select(Component, `Component Label`)

component_lables %>% 
  pull(`Component Label`)

unctaddf = old_data %>% 
  select(Economy, 
         `Economy Label`, 
         Year, 
         `Component Label`, 
         `US$ at current prices in millions`, 
         `US$ at constant prices (2015) in millions`) %>% 
  rename(	
    Economy_Label = `Economy Label`,
    IndicatorName = `Component Label`,
    current = `US$ at current prices in millions`,
    constant = `US$ at constant prices (2015) in millions`) %>% 
  pivot_longer(c(current, constant), names_to = "Prices", values_to = "Value") %>% 
  full_join(
    gdp_exp,
    by = join_by(Economy == Economy_Code,
                 Year == Year,
                 IndicatorName == IndicatorName,
                 Prices == Prices),
    suffix = c(".old", ".new")
  ) 

# Export to csv
unctaddf %>% 
  mutate(Economy_Label = Economy_Label.new) %>% 
  select(!c(Economy_Label.old, Economy_Label.new)) %>% 
  select(Economy, Economy_Label, everything()) %>% 
  write_csv(file.path(outputdir, "gdp_comparison.csv"))

# Create plots
plot_by_economy = function(economy_label, indicator) {
  margin_constant = 0.03
    p = unctaddf %>% 
      filter(Economy_Label.old == economy_label,
             IndicatorName == indicator) %>% 
      pivot_longer(c(Value.old, Value.new), names_to = "Release", values_to = "Value") %>% 
      mutate(Release = Release %>% 
               fct_recode(old = "Value.old", new = "Value.new") %>% 
               fct_relevel(c("old", "new"))) %>% 
      ggplot(aes(x=Year, y=Value, linetype=Prices, color=Release)) +
      geom_line(linewidth=1) +
      guides(
        color = guide_legend(
          position = "bottom",
          direction = "vertical",
          nrow=2,
          order = 1),
        linetype = guide_legend(
          position = "bottom",
          direction = "vertical",
          nrow=2)
      ) +
      theme_bw() + 
      theme(
        plot.margin = margin(margin_constant, margin_constant, margin_constant, margin_constant, "npc"),
        legend.spacing.x = unit(0.1, "npc")) + 
      labs(title = economy_label,
           subtitle = indicator,
           x = "",
           y = "USD") +
      scale_color_manual(values = c("#FBAF17", "#009EDB")) 
    return(p)
}  
plot_by_economy("Poland", "Gross domestic product (GDP)")

plot_by_indicator = function(indicator) {
  plot_given_indicator = function(economy_label) {
    return(plot_by_economy(economy_label, indicator))
  }
  Plots <- lapply(sort(unique(unctaddf$Economy_Label.old)), plot_given_indicator)
  return(Plots)  
}

# Export plots to pdf
Plots <- lapply(component_lables$`Component Label`, plot_by_indicator) 
Plots2 <- list_flatten(Plots)
myPlots <- do.call(marrangeGrob, list(grobs=Plots2, nrow = 3, ncol = 1))
ggsave(file.path(outputdir, "GDP_comparison_groups.pdf"), myPlots, height = 12, width = 8)

}

run_comparison_with_old_data()
