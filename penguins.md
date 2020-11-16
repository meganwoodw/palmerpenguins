Penguins
================
slimy-green
11/15/2020

``` r
penguins_raw<- read_csv(here::here("inst/extdata/penguins_raw.csv"))
```

## Exploratory Analysis

### Visualizations

``` r
body_mass_graph <- penguins_raw %>% 
  mutate(Species = str_replace_all(Species, " .*", "")) %>% 
  ggplot(aes(x=Species, y = `Body Mass (g)`, fill = Species)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle =90, hjust=1)) +
  labs(title = "Body Mass of Penguins", subtitle = "By Species")

ggsave(filename = "body_mass_plot.png", plot = body_mass_graph)
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

``` r
body_mass_sex_graph <- penguins_raw %>% 
  mutate(Species = str_replace_all(Species, " .*", "")) %>% 
  na.omit(Sex) %>% 
  group_by(Sex) %>% 
  ggplot(aes(x=Species, y = `Body Mass (g)`, fill = Sex)) +
  geom_violin(area= "fill") +
  theme(axis.text.x = element_text(angle =90, hjust=1)) +
  labs(title = "Body Mass of Penguins", subtitle = "By Species and Sex")
```

    ## Warning: Ignoring unknown parameters: area

``` r
ggsave(filename = "body_mass_sex_graph.png", plot = body_mass_sex_graph)
```

    ## Saving 7 x 5 in image

``` r
body_mass_species <- penguins_raw %>% 
  mutate(Species = str_replace_all(Species, " .*", "")) %>% 
  ggplot(aes(x=Species, y = `Flipper Length (mm)`, fill = Species)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle =90, hjust=1)) +
  labs(title = "Body Mass of Penguins", subtitle = "By Species")

ggsave(filename = "body_mass_species.png", plot = body_mass_species)
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

``` r
beak_length_depth <- penguins_raw %>% 
  mutate(Species = str_replace_all(Species, " .*", "")) %>% 
  ggplot() +
  geom_point(aes(x= `Culmen Length (mm)` , y = `Culmen Depth (mm)`, color = Species)) +
  labs(title = "Culmen Length vs Culmen Depth", subtitle = "by Species")

ggsave(filename = "beak_length_depth.png", plot = beak_length_depth)
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 2 rows containing missing values (geom_point).

### Summary Statistics

``` r
penguins_table <- penguins_raw %>% 
  mutate(Species = str_replace_all(Species, " .*", "")) %>% 
  na.omit(Sex) %>% 
  group_by(Species, Sex) %>% 
  summarise(total = n(),
            med_culmen_length = median(`Culmen Length (mm)`), 
            med_culmen_depth = median(`Culmen Depth (mm)`),
            med_mass = median(`Body Mass (g)`), 
            med_flipper_length = median(`Flipper Length (mm)`))
```

    ## `summarise()` regrouping output by 'Species' (override with `.groups` argument)

``` r
knitr::kable(penguins_table)
```

| Species   | Sex    | total | med\_culmen\_length | med\_culmen\_depth | med\_mass | med\_flipper\_length |
| :-------- | :----- | ----: | ------------------: | -----------------: | --------: | -------------------: |
| Adelie    | FEMALE |     7 |                37.6 |               17.8 |      3300 |                  185 |
| Adelie    | MALE   |     6 |                40.1 |               19.3 |      4100 |                  196 |
| Chinstrap | FEMALE |     7 |                46.7 |               17.9 |      3400 |                  192 |
| Chinstrap | MALE   |     7 |                50.3 |               19.4 |      3800 |                  197 |
| Gentoo    | FEMALE |     4 |                44.1 |               13.7 |      4425 |                  212 |
| Gentoo    | MALE   |     3 |                49.6 |               16.0 |      5550 |                  225 |

``` r
penguins_island <- penguins_raw %>%
  mutate(Species = str_replace_all(Species, " .*", "")) %>%
  group_by(Island, Species) %>% 
  summarise(total = n())
```

    ## `summarise()` regrouping output by 'Island' (override with `.groups` argument)

``` r
knitr::kable(penguins_island)
```

| Island    | Species   | total |
| :-------- | :-------- | ----: |
| Biscoe    | Adelie    |    44 |
| Biscoe    | Gentoo    |   124 |
| Dream     | Adelie    |    56 |
| Dream     | Chinstrap |    68 |
| Torgersen | Adelie    |    52 |
