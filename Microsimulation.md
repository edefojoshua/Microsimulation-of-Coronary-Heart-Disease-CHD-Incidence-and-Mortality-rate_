Microsimulation of Coronary Heart Disease (CHD), Incidence and Mortality
rate
================
Joshua Edefo
2025-01-28

This microsimulation models the long-term health outcomes of a synthetic
population over 30 years, specifically focusing on Coronary Heart
Disease (CHD) incidence and mortality. Each individual in the population
is assigned sociodemographic characteristics such as age, sex, and risk
factors like BMI, systolic blood pressure (SBP), smoking, and alcohol
consumption. The simulation dynamically updates these characteristics
annually, recalculating the risk of developing CHD based on the
individual’s evolving profile. Mortality risk is also assessed,
influenced by both disease incidence and other factors. The model tracks
the population’s health states over time, simulating disease progression
and mortality events, while storing annual results for further analysis.
This approach enables the study of the long-term effects of risk factor
changes and disease incidence on population health, offering insights
into potential interventions or policy changes aimed at reducing disease
burden.

Libraries

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.3.3

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.3

``` r
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.3.2

## R Markdown

The microsimulation

``` r
## Simulating the population: 10000 individuals with random age, sex, and risk factors
set.seed(123)

## Define simulation parameters
n <- 10000  # Number of synthetic individuals
years <- 30  # Simulation time horizon (2013-2043)

## Generate a basic synthetic population
population <- data.frame(
  ID = 1:n,
  Age = sample(30:90, n, replace = TRUE),  # Random age between 30 and 90
  Sex = sample(c('Male', 'Female'), n, replace = TRUE),  # Random sex
  BMI = rnorm(n, mean = 28, sd = 5),  # BMI with mean 28 and SD 5
  SBP = rnorm(n, mean = 130, sd = 20),  # Systolic BP with mean 130 and SD 20
  Smoking = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),  # 40% smokers
  Alcohol = rnorm(n, mean = 10, sd = 5)  # Alcohol consumption (units per week)
)

## Define disease risk functions based on risk factors
# Example: Risk of CHD (Coronary Heart Disease) based on age, sex, BMI, and smoking
risk_CHD <- function(age, sex, BMI, smoking) {
  base_risk <- 0.05  # Base risk of CHD per year
  
  # Modify the base risk based on the risk factors
  risk <- base_risk + (age - 40) * 0.0015 + (BMI - 25) * 0.002 + (smoking * 0.05)
  if (sex == 'Female') risk <- risk * 0.8  # Adjust risk for females (lower risk by 20%)
  
  return(risk)
}

## Simulate the disease incidence over time (CHD in this case)
population <- population %>%
  mutate(
    Risk_CHD = mapply(risk_CHD, Age, Sex, BMI, Smoking),  # Apply risk function
    CHD_Incidence = rbinom(n, 1, Risk_CHD)  # Simulate CHD incidence using binomial distribution
  )

## Simulate mortality for each individual
## Mortality risk depends on age, sex, and presence of CHD (and possibly other conditions)
risk_mortality <- function(age, sex, CHD) {
  base_mortality <- 0.01  # Base annual mortality risk
  
  # Modify the base risk based on age, sex, and CHD
  risk <- base_mortality + (age - 40) * 0.001 + (CHD * 0.03)
  if (sex == 'Female') risk <- risk * 0.9  # Adjust mortality for females (10% lower risk)
  
  return(risk)
}

## Simulate mortality over time (30 years), considering risk factors and disease
population <- population %>%
  mutate(
    Mortality_Risk = mapply(risk_mortality, Age, Sex, CHD_Incidence),  # Apply mortality risk function
    Mortality = rbinom(n, 1, Mortality_Risk)  # Simulate mortality (1 if dead, 0 if alive)
  )

## Tracking the population health over the 30 years (e.g., disease prevalence and mortality)
years_of_simulation <- 30
simulation_results <- data.frame(Year = rep(2013:2042, each = n), ID = rep(1:n, times = years_of_simulation))

## Simulate the health state of each individual across years (simplified version)
for (year in 2013:2043) {
  # Update age
  population$Age <- population$Age + 1
  
  # Recalculate risk and disease outcomes each year
  population <- population %>%
    mutate(
      Risk_CHD = mapply(risk_CHD, Age, Sex, BMI, Smoking),
      CHD_Incidence = rbinom(n, 1, Risk_CHD),
      Mortality_Risk = mapply(risk_mortality, Age, Sex, CHD_Incidence),
      Mortality = rbinom(n, 1, Mortality_Risk)
    )
  
  ## Store results
  simulation_results$CHD[simulation_results$Year == year] <- population$CHD_Incidence
  simulation_results$Mortality[simulation_results$Year == year] <- population$Mortality
}

## Visualizing the results
## Plotting the prevalence of CHD over time
ggplot(simulation_results, aes(x = Year, y = CHD)) +
  geom_line(stat = "summary", fun = "mean", color = "blue") +
  labs(title = "Prevalence of CHD over Time", x = "Year", y = "Prevalence") +
  theme_minimal()
```

![](Microsimulation_files/figure-gfm/b-1.png)<!-- -->

``` r
## Plotting the mortality over time
ggplot(simulation_results, aes(x = Year, y = Mortality)) +
  geom_line(stat = "summary", fun = "mean", color = "red") +
  labs(title = "Mortality over Time", x = "Year", y = "Mortality Rate") +
  theme_minimal()
```

![](Microsimulation_files/figure-gfm/b-2.png)<!-- -->

session information

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] usethis_2.2.2 ggplot2_3.5.1 dplyr_1.1.4  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] vctrs_0.6.5       cli_3.6.1         knitr_1.44        rlang_1.1.1      
    ##  [5] xfun_0.40         purrr_1.0.2       generics_0.1.3    labeling_0.4.3   
    ##  [9] glue_1.6.2        colorspace_2.1-0  htmltools_0.5.8.1 scales_1.3.0     
    ## [13] fansi_1.0.4       rmarkdown_2.25    grid_4.3.1        munsell_0.5.0    
    ## [17] evaluate_0.21     tibble_3.2.1      fastmap_1.2.0     yaml_2.3.7       
    ## [21] lifecycle_1.0.3   compiler_4.3.1    fs_1.6.3          pkgconfig_2.0.3  
    ## [25] rstudioapi_0.15.0 farver_2.1.1      digest_0.6.33     R6_2.5.1         
    ## [29] tidyselect_1.2.0  utf8_1.2.3        pillar_1.9.0      magrittr_2.0.3   
    ## [33] withr_2.5.0       tools_4.3.1       gtable_0.3.4
