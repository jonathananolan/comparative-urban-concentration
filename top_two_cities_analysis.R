library(tidyverse)
library(scales)
library(broom)
library(gridExtra)
library(grid)
library(patchwork)
library(WDI)
source("ggplot_theme.R")

# Set radius distance (km)
city_data <- read_csv("https://city-density.s3.amazonaws.com/city-density.csv")
# Fetch population for all countries, latest available year
pop_data <- WDI(indicator = c("SP.POP.TOTL", 
                              "NY.GDP.PCAP.CD"),  
 start = 2023, end = 2023)


area_data <- WDI(
  indicator = "AG.SRF.TOTL.K2",
  start = 2000, end = 2023
)

# keep most recent value per country
area_latest <- area_data %>%
  group_by(iso2c) %>%
  filter(!is.na(AG.SRF.TOTL.K2)) %>%
  slice_max(year) %>%
  ungroup()






radius_km <- 100



top_two_by_country <- city_data %>% 
  filter(dist_km_round == radius_km) %>% 
  group_by(country_code_iso3c) %>% 
  arrange(desc(population_cum)) %>% 
  filter(row_number()<3) %>% 
  group_by(iso3c = country_code_iso3c) %>% 
  summarise(city_population = sum(population_cum))






share_in_cities <- pop_data %>% 
  inner_join(top_two_by_country) %>%
  rename(total_pop = SP.POP.TOTL) %>% 
  mutate(share = city_population/total_pop) %>% 
  ungroup() %>%
  left_join(area_latest %>% select(iso3c, 
                                   area_sqkm = AG.SRF.TOTL.K2), 
            by = "iso3c")

share_in_cities %>% 
  filter(total_pop > 15000000, total_pop < 40000000) %>% 
  mutate(country = fct_reorder(country,share)) %>% 
  arrange(share) %>% 
  mutate(percentile = row_number() / n()) %>% 
  ggplot(aes(x = country, y = share))+
  geom_bar(stat = "identity")+
  coord_flip()


lvl <- c("AUS","GBR","USA","CAN","NZL")

share_in_cities %>% 
  mutate(country_short = case_when(iso3c %in% lvl ~ country, 
                             T ~ "Other"),
         country_short = fct_relevel(country_short,"Other", after = Inf),
         num_listed = case_when(iso3c %in% lvl ~ 99, 
                                   T ~ 1),
         ) %>%  
  arrange(num_listed) %>% 
  filter(share <1) %>% 
  ggplot(aes(y = share,
             x = total_pop, 
             colour = country_short,
             group = country_short)) +
  scale_x_log10(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  )+
  geom_point(size = 3)+
  theme_jn_caption()+
  scale_colour_manual(
    values = unname(jn_colours$complementary[c(1, 2, 3, 5, 6, 4)]),
  ) +
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Australia is not abnormally concentrated in Sydney and Melbourne",
       subtitle = paste0("Share of country living within ", radius_km, "km of two biggest cities"),
       x = "Population of country (log scale)",
       y = element_blank(),
       caption = paste0("A few countries are excluded because they have a share >1 since the ", radius_km, "km radius includes neighbouring countries."),
       colour = "Country")

ggsave(paste0("atlas/concentration_by_country_",radius_km,".png"))

share_in_cities %>% 
  mutate(country_short = case_when(iso3c %in% lvl ~ country, 
                                   T ~ "Other"),
         country_short = fct_relevel(country_short,"Other", after = Inf),
         num_listed = case_when(iso3c %in% lvl ~ 99, 
                                T ~ 1),
  ) %>%  
  arrange(num_listed) %>% 
  filter(share <1) %>% 
  ggplot(aes(y = share,
             x = NY.GDP.PCAP.CD, 
             colour = country_short,
             group = country_short)) +
  scale_x_log10(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  )+
  geom_point(size = 3)+
  theme_jn_caption()+
  scale_colour_manual(
    values = unname(jn_colours$complementary[c(1, 2, 3, 5, 6, 4)]),
  ) +
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Australia is not abnormally concentrated in Sydney and Melbourne",
       subtitle = paste0("Share of country living within ", radius_km, "km of two biggest cities"),
       x = "GDP per capita (log scale)",
       y = element_blank(),
       caption = paste0("A few countries are excluded because they have a share >1 since the ", radius_km, "km radius includes neighbouring countries."),
       colour = "Country")



# --- Prep ---------------------------------------------------------------------
share_std <- share_in_cities %>%
  mutate(
    log_area = log10(area_sqkm),
    log_gdp  = log10(NY.GDP.PCAP.CD),
    log_population = log(total_pop),
    highlight = if_else(iso3c == "AUS", "Australia", "Other")
  ) %>%
  arrange(iso3c == "AUS") %>%   # AUS rows last -> draw on top
filter(share <1)
# Fit model
m <- lm(share ~ log_population + log_area + log_gdp, data = share_std)

# Tidy tables
tbl_coefs <- broom::tidy(m) %>%
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       log_population= "Log(Country population)",
                       log_area      = "Log(Area, sqkm)",
                       log_gdp       = "Log(GDP per cap.)"
  )) %>%
  transmute(
    Term = term,
    Estimate = sprintf("%.3f", estimate),
    `Std. Error` = sprintf("%.3f", std.error),
    `t value` = sprintf("%.2f", statistic),
    `p` = scales::pvalue(p.value, accuracy = 0.001)
  )

tbl_fit <- broom::glance(m) %>%
  transmute(
    `Adj. R²` = sprintf("%.3f", adj.r.squared),
    `RSE`     = sprintf("%.3f", sigma),
    `F (df1, df2)` = sprintf("%.1f (%d, %d)",
                             statistic,
                             df.residual - (m$rank - 1),
                             df.residual),
    `Model p` = scales::pvalue(p.value, accuracy = 0.001)
  )

g_coefs <- tableGrob(tbl_coefs, rows = NULL, theme = ttheme_minimal(base_size = 10))
g_fit   <- tableGrob(tbl_fit,   rows = NULL, theme = ttheme_minimal(base_size = 10))
g_reg   <- arrangeGrob(g_coefs, g_fit, ncol = 1,
                       top = textGrob("Regression: share ~ log_pop + log_area + log_gdp",
                                      gp = gpar(fontface = "bold", cex = 1.0)))
p_reg <- wrap_elements(g_reg)
# --- Plots --------------------------------------------------------------------
pal <- c("Australia" = "#1f77b4", "Other" = "grey75")

p_area <- ggplot(share_std, aes(x = log_area, y = share, color = highlight)) +
  geom_point(alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  scale_color_manual(values = pal, guide = "none") +
  labs(title = "Share vs Log(Area)", x = "Log10(Area, sq km)", y = "Share in top 2 cities") +
  theme_minimal(base_size = 12)

p_gdp <- ggplot(share_std, aes(x = log_gdp, y = share, color = highlight)) +
  geom_point(alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  scale_color_manual(values = pal, guide = "none") +
  labs(title = "Share vs Log(GDP per capita)", x = "Log10(GDP per capita, USD)", y = NULL) +
  theme_minimal(base_size = 12)

p_pop <- ggplot(share_std, aes(x =log_population , y = share, color = highlight)) +
  geom_point(alpha = 0.75, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = pal, guide = "none") +
  labs(title = "Share vs Log(Country population)", x = "Log10(Country population)", y = NULL) +
  theme_minimal(base_size = 12)

# 2x2 layout: (Area | GDP) / (Population | Regression table)
final_plot <- (p_area | p_gdp) / (p_pop | p_reg) +
  plot_annotation(
    title = "What predicts concentration in the top two cities?",
    subtitle = paste0("Linear model with log-transforms for area and GDP per capita; Australia highlighted. Countries where ", radius_km, "km radius means share in top cities is >1 excluded. "),
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )

# View and save
final_plot
ggsave(paste0("atlas/share_predictors_", radius_km, "km.png"), final_plot, width = 16, height = 9, units = "in", dpi = 200)
