Modeling code
================
2020-03-30

## Initial Parameters

``` r
initial_population_size = 20
```

## States

``` r
INFECTED = "infected"
SUCCEPTIBLE = "succeptible"
RECOVERED = "recovered"
DEAD = "dead"
```

## Change states on each timestep

``` r
# placeholder function that just returns the same state
change_state = function(prev_state){
  prev_state
}
```

## Setting up and populating my dataframe

``` r
# TODO: create the population according to demographic markers, and randomly assign the infected person.
create_initial_population_with_one_infected = function(size){
  one_infected = as.factor(c(INFECTED))
  others_succeptible = rep(as.factor(c(SUCCEPTIBLE)), size - 1)
  
  fct_c(one_infected, others_succeptible)
}

# remove this later, printing just for debugging
create_initial_population_with_one_infected(initial_population_size)
```

    ##  [1] infected    succeptible succeptible succeptible succeptible succeptible
    ##  [7] succeptible succeptible succeptible succeptible succeptible succeptible
    ## [13] succeptible succeptible succeptible succeptible succeptible succeptible
    ## [19] succeptible succeptible
    ## Levels: infected succeptible

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
    ##    person_ids day_1       day_2
    ##         <int> <fct>       <fct>
    ##  1          1 infected    1    
    ##  2          2 succeptible 2    
    ##  3          3 succeptible 2    
    ##  4          4 succeptible 2    
    ##  5          5 succeptible 2    
    ##  6          6 succeptible 2    
    ##  7          7 succeptible 2    
    ##  8          8 succeptible 2    
    ##  9          9 succeptible 2    
    ## 10         10 succeptible 2    
    ## 11         11 succeptible 2    
    ## 12         12 succeptible 2    
    ## 13         13 succeptible 2    
    ## 14         14 succeptible 2    
    ## 15         15 succeptible 2    
    ## 16         16 succeptible 2    
    ## 17         17 succeptible 2    
    ## 18         18 succeptible 2    
    ## 19         19 succeptible 2    
    ## 20         20 succeptible 2
