Modeling code
================
2020-03-30

## Initial Parameters

``` r
## BIG TODO: add citations for these numbers

initial_population_size = 20 # TODO - choose bigger number later
hospital_bed_capacity = 100 #TODO - fill in with real number later

probability_symptomatic = 2/3
probability_asymptomatic = 1 - probability_symptomatic

# TODO: break this out into it's own code chunk with explanation? Weighted average, solved equations to get 
basic_reproductive_number = 2.4
symptomatic_reproductive_number = basic_reproductive_number*(6/5)
asymptomatic_reproductive_number = basic_reproductive_number*(3/5)

time_until_infectious = 4.5 # This is figured from the incubation period, and rounded down from 4.6 so I can do steps in half-days.

# For the asymptomatic types
asymptomatic_time_from_infectious_to_recovery = 4 # This is figured from the generation time (confusing). Should this be longer??

# For the symptomatic types
time_infectious_before_symptom_onset = 0.5
time_from_symptom_onset_to_recovery = 3.5

time_until_seeking_care = 5 # This is the number of days after symptom onset that people seek hospital care if they need it, on average
time_in_hospital_bed = 10.5
time_until_death_if_no_care = 10
```

## States

``` r
SUCCEPTIBLE = "succeptible"

INFECTED_ASYMPTOMATIC = "infected_asymptomatic"
INFECTIOUS_ASYMPTOMATIC = "infectious_asymptomatic"

INFECTED_SYMPTOMATIC_PRE_SYMPTOMS = "infected_symptomatic_pre_symptoms"
INFECTIOUS_SYMPTOMATIC_PRE_SYMPTOMS = "infectious_symptomatic_pre_symptoms"

SYMPTOMATIC_NEED_HOSPITAL = "symptomatic_need_hospital"
SYMPTOMATIC_DONT_NEED_HOSPITAL = "symptomatic_dont_need_hospital"

NEED_HOSPITAL_SEEK_CARE = "need_hospital_seek_care"
NEED_HOSPITAL_DONT_SEEK_CARE = "need_hospital_dont_seek_care"

GET_HOSPITAL_CARE = "get_hospital_care"
DONT_GET_NEEDED_CARE = "dont_get_needed_care"

RECOVERED = "recovered"
DEAD = "dead"
```

## Change states on each timestep

``` r
# placeholder function that just returns the same state
change_state = function(prev_state){
  as.character(prev_state)
}
```

## Setting up and populating my dataframe

``` r
# TODO: create the population according to demographic markers, and randomly assign the infected person.
create_initial_population_with_one_infected = function(size){
  one_infected = as.factor(c(INFECTED_SYMPTOMATIC_PRE_SYMPTOMS))
  others_succeptible = rep(as.factor(c(SUCCEPTIBLE)), size - 1)
  
  fct_c(one_infected, others_succeptible)
}

# remove this later, printing just for debugging
create_initial_population_with_one_infected(initial_population_size)
```

    ##  [1] infected_symptomatic_pre_symptoms succeptible                      
    ##  [3] succeptible                       succeptible                      
    ##  [5] succeptible                       succeptible                      
    ##  [7] succeptible                       succeptible                      
    ##  [9] succeptible                       succeptible                      
    ## [11] succeptible                       succeptible                      
    ## [13] succeptible                       succeptible                      
    ## [15] succeptible                       succeptible                      
    ## [17] succeptible                       succeptible                      
    ## [19] succeptible                       succeptible                      
    ## Levels: infected_symptomatic_pre_symptoms succeptible

This is a table with each row representing one person in the population.
The first few columns include demographic and other information about a
person, and all of the columns labeled `day_n` represent that person’s
disease state at time
n.

``` r
# TODO: make a function to generate new day columns based on some initial parameter.
population = tibble(
  person_ids = 1:initial_population_size,
  day_1 = create_initial_population_with_one_infected(initial_population_size)
) %>% 
  mutate(., day_2 = as.factor(map_chr(day_1, change_state)))

# remove this later, printing just for debugging
population
```

    ## # A tibble: 20 x 3
    ##    person_ids day_1                             day_2                           
    ##         <int> <fct>                             <fct>                           
    ##  1          1 infected_symptomatic_pre_symptoms infected_symptomatic_pre_sympto…
    ##  2          2 succeptible                       succeptible                     
    ##  3          3 succeptible                       succeptible                     
    ##  4          4 succeptible                       succeptible                     
    ##  5          5 succeptible                       succeptible                     
    ##  6          6 succeptible                       succeptible                     
    ##  7          7 succeptible                       succeptible                     
    ##  8          8 succeptible                       succeptible                     
    ##  9          9 succeptible                       succeptible                     
    ## 10         10 succeptible                       succeptible                     
    ## 11         11 succeptible                       succeptible                     
    ## 12         12 succeptible                       succeptible                     
    ## 13         13 succeptible                       succeptible                     
    ## 14         14 succeptible                       succeptible                     
    ## 15         15 succeptible                       succeptible                     
    ## 16         16 succeptible                       succeptible                     
    ## 17         17 succeptible                       succeptible                     
    ## 18         18 succeptible                       succeptible                     
    ## 19         19 succeptible                       succeptible                     
    ## 20         20 succeptible                       succeptible

## Visualization

First, transform the table into a better shape for graphing, by getting
the total state counts at each time step.

``` r
population_to_visualize =
  pivot_longer(
    population,
    cols = starts_with("day_"),
    names_to = "day",
    values_to = "state",
    names_prefix = "day_"
  ) %>% 
  mutate(day = as.numeric(day)) %>% 
  group_by(day, state) %>% 
  summarize(count = n())

## NOTE:
## For transitory states like "infected", the count represents how many people are infected on that day.
## However, for a state like "dead", the count represents how many people total have died up until that point (because once you reach the "dead" state, you stay at that state forever).


# remove this later, printing just for debugging
population_to_visualize
```

    ## # A tibble: 4 x 3
    ## # Groups:   day [2]
    ##     day state                             count
    ##   <dbl> <fct>                             <int>
    ## 1     1 infected_symptomatic_pre_symptoms     1
    ## 2     1 succeptible                          19
    ## 3     2 infected_symptomatic_pre_symptoms     1
    ## 4     2 succeptible                          19

Graph the person-count of each state in a different color, with days on
the x-axis.

``` r
ggplot(population_to_visualize, 
       aes(x=day, y=count, color=state)) + 
  geom_line()
```

![](Modeling-code_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
