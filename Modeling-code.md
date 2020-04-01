Modeling code
================
2020-03-30

## Initial Parameters

I am arbitrarily choosing XX,XXX people for my simulation. I am also
choosing a timeline similar to what we saw in NYC, with *ADD IN DATES
AND INFORMATION*.

``` r
initial_population_size = 20 # TODO - choose bigger number later

# TODO - fill these in with real numbers
day_of_first_distancing_guideline = 10
day_of_stronger_distancing_directive = 15
length_of_distancing_directive = 90
```

These initial parameters come from my best efforts at parsing the
literature, specificially ARTICLE A and ARTICLE B.

``` r
## BIG TODO: add citations for these numbers

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

These numbers come from NYC open data.

``` r
hospital_bed_capacity = 100 #TODO - fill in with real number later

# TODO - fill in distributions of: age, health status/underlying conditions, "essential" jobs, poverty levels, incarcerated, homeless, detained immigrants, insurance coverage 
```

These numbers come from ARTICLE A, describing the distribution by age of
who needs hospital care, and what their mortality rate is.

``` r
# TODO: fill in from article.
```

And finally, based on some other research and best guesses:

``` r
# TODO: flesh out this section

probability_seek_care_insured = 1
probability_seek_care_uninsured = 0.5

# TODO: decide on cutoff poverty level for ignoring a stay-home directive
```

## States

The way I organized my thoughts for this project was to create a state
diagram (essentially a flow chart) to lay out all of the different
trajectories someone could take through this epidemic. Everyone starts
as “succeptible”, and from there you have different probabilities of
transitioning to other states, dependent on factors like random chance,
demographic characteristics, as well as actions taken by others (like
infecting you).

*TODO: put in an image of my state diagram*

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

I used another state diagram to map out who is staying at home, and who
is still out and about.

*TODO: put in an image of my 2nd state diagram*

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

![](Modeling-code_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
