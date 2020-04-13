Modeling code
================
2020-03-30

## Initial Parameters

I am arbitrarily choosing XX,XXX people for my simulation. I am also
choosing a timeline similar to what we saw in NYC, with *ADD IN DATES
AND INFORMATION*.

``` r
initial_population_size = 20 # TODO - choose bigger number later
total_days = 100

# TODO - fill these in with real numbers
day_of_first_distancing_guideline = 10
day_of_stronger_distancing_directive = 15
length_of_distancing_directive = 90
```

These initial parameters come from my best efforts at parsing the
literature, specificially ARTICLE A and ARTICLE B.

``` r
## BIG TODO: add citations for these numbers

#probability_symptomatic = 2/3 # Take this out?
probability_asymptomatic = 1/3

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

Helper functions:

``` r
# Set up an empty table with columns for each day in the simulation.
# When someone is randomly selected to be infected, I put a 1 in the spot corresponding to that person (row) on that day (column).
# This is referenced by the change_state function, to transition people from SUCCEPTIBLE to INFECTED.
newly_infected = tibble(
  person_ids = 1:initial_population_size,
)

for (i in 1:total_days) {
  newly_infected =
    add_column(
      newly_infected,
      !!str_c("day_", i) := 0
      )
}

# This function gets called a variable number of times when someone is contageous.
# They infect a random person, according to the specific parameters of the situation (taking into account who and how many people are staying at home.)
# When someone is infected, a 1 gets placed in the newly_infected table for that specific person on the day they're infected.
infect_someone = function(day){
  day_col = str_c("day_", day)
  
  succeptibles = which(newly_infected[[day_col]] == 0)
  person_to_infect = sample(succeptibles,1)
  
  newly_infected[[day_col]][person_to_infect] <<- 1
  }

# remove this later, printing just for debugging
newly_infected
```

    ## # A tibble: 20 x 101
    ##    person_ids day_1 day_2 day_3 day_4 day_5 day_6 day_7 day_8 day_9 day_10
    ##         <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ##  1          1     0     0     0     0     0     0     0     0     0      0
    ##  2          2     0     0     0     0     0     0     0     0     0      0
    ##  3          3     0     0     0     0     0     0     0     0     0      0
    ##  4          4     0     0     0     0     0     0     0     0     0      0
    ##  5          5     0     0     0     0     0     0     0     0     0      0
    ##  6          6     0     0     0     0     0     0     0     0     0      0
    ##  7          7     0     0     0     0     0     0     0     0     0      0
    ##  8          8     0     0     0     0     0     0     0     0     0      0
    ##  9          9     0     0     0     0     0     0     0     0     0      0
    ## 10         10     0     0     0     0     0     0     0     0     0      0
    ## 11         11     0     0     0     0     0     0     0     0     0      0
    ## 12         12     0     0     0     0     0     0     0     0     0      0
    ## 13         13     0     0     0     0     0     0     0     0     0      0
    ## 14         14     0     0     0     0     0     0     0     0     0      0
    ## 15         15     0     0     0     0     0     0     0     0     0      0
    ## 16         16     0     0     0     0     0     0     0     0     0      0
    ## 17         17     0     0     0     0     0     0     0     0     0      0
    ## 18         18     0     0     0     0     0     0     0     0     0      0
    ## 19         19     0     0     0     0     0     0     0     0     0      0
    ## 20         20     0     0     0     0     0     0     0     0     0      0
    ## # … with 90 more variables: day_11 <dbl>, day_12 <dbl>, day_13 <dbl>,
    ## #   day_14 <dbl>, day_15 <dbl>, day_16 <dbl>, day_17 <dbl>, day_18 <dbl>,
    ## #   day_19 <dbl>, day_20 <dbl>, day_21 <dbl>, day_22 <dbl>, day_23 <dbl>,
    ## #   day_24 <dbl>, day_25 <dbl>, day_26 <dbl>, day_27 <dbl>, day_28 <dbl>,
    ## #   day_29 <dbl>, day_30 <dbl>, day_31 <dbl>, day_32 <dbl>, day_33 <dbl>,
    ## #   day_34 <dbl>, day_35 <dbl>, day_36 <dbl>, day_37 <dbl>, day_38 <dbl>,
    ## #   day_39 <dbl>, day_40 <dbl>, day_41 <dbl>, day_42 <dbl>, day_43 <dbl>,
    ## #   day_44 <dbl>, day_45 <dbl>, day_46 <dbl>, day_47 <dbl>, day_48 <dbl>,
    ## #   day_49 <dbl>, day_50 <dbl>, day_51 <dbl>, day_52 <dbl>, day_53 <dbl>,
    ## #   day_54 <dbl>, day_55 <dbl>, day_56 <dbl>, day_57 <dbl>, day_58 <dbl>,
    ## #   day_59 <dbl>, day_60 <dbl>, day_61 <dbl>, day_62 <dbl>, day_63 <dbl>,
    ## #   day_64 <dbl>, day_65 <dbl>, day_66 <dbl>, day_67 <dbl>, day_68 <dbl>,
    ## #   day_69 <dbl>, day_70 <dbl>, day_71 <dbl>, day_72 <dbl>, day_73 <dbl>,
    ## #   day_74 <dbl>, day_75 <dbl>, day_76 <dbl>, day_77 <dbl>, day_78 <dbl>,
    ## #   day_79 <dbl>, day_80 <dbl>, day_81 <dbl>, day_82 <dbl>, day_83 <dbl>,
    ## #   day_84 <dbl>, day_85 <dbl>, day_86 <dbl>, day_87 <dbl>, day_88 <dbl>,
    ## #   day_89 <dbl>, day_90 <dbl>, day_91 <dbl>, day_92 <dbl>, day_93 <dbl>,
    ## #   day_94 <dbl>, day_95 <dbl>, day_96 <dbl>, day_97 <dbl>, day_98 <dbl>,
    ## #   day_99 <dbl>, day_100 <dbl>

``` r
change_state = function(id, prev_state, day){ #TODO: Fix inputs elsewhere
  new_state = prev_state
  rand = runif(1) #Question: is it ok to use the same random number for every decision in this function?
  
  # Take out later, for debugging
  cat("\n\nPerson ID: ", id)
  cat("\nDay: ", day)
  cat("\nPrevious state: ", prev_state)
  cat("\nRandom number: ", rand)
  
  if (prev_state == SUCCEPTIBLE) {
    if (newly_infected[[str_c("day_", day)]][id] == 1) {
      if (rand <= probability_asymptomatic) {new_state = INFECTED_ASYMPTOMATIC}
      else {new_state = INFECTED_SYMPTOMATIC_PRE_SYMPTOMS}
    }
  }
  
  
  if (prev_state == INFECTED_SYMPTOMATIC_PRE_SYMPTOMS){
    if (rand < .3){ new_state = DEAD}
    else if (rand > .7) {new_state = RECOVERED}
  }
  
  # Take out later, for debugging
  cat("\nNew state: ", new_state)
  
  new_state
}

#change_state(2, INFECTED_SYMPTOMATIC_PRE_SYMPTOMS, 3)
```

## Setting up and populating my dataframe

``` r
# TODO: create the population according to demographic markers, and randomly assign the infected person.
create_initial_population_with_one_infected = function(size){
  one_infected = c(INFECTED_SYMPTOMATIC_PRE_SYMPTOMS)
  others_succeptible = rep(c(SUCCEPTIBLE), size - 1)
  
  c(one_infected, others_succeptible)
}

# OLD CODE: using factors. Don't delete until I've figured it out.
# create_initial_population_with_one_infected = function(size){
#   one_infected = as.factor(c(INFECTED_SYMPTOMATIC_PRE_SYMPTOMS))
#   others_succeptible = rep(as.factor(c(SUCCEPTIBLE)), size - 1)
#   
#   fct_c(one_infected, others_succeptible)
# }

# remove this later, printing just for debugging
create_initial_population_with_one_infected(initial_population_size)
```

    ##  [1] "infected_symptomatic_pre_symptoms" "succeptible"                      
    ##  [3] "succeptible"                       "succeptible"                      
    ##  [5] "succeptible"                       "succeptible"                      
    ##  [7] "succeptible"                       "succeptible"                      
    ##  [9] "succeptible"                       "succeptible"                      
    ## [11] "succeptible"                       "succeptible"                      
    ## [13] "succeptible"                       "succeptible"                      
    ## [15] "succeptible"                       "succeptible"                      
    ## [17] "succeptible"                       "succeptible"                      
    ## [19] "succeptible"                       "succeptible"

This is a table with each row representing one person in the population.
The first few columns include demographic and other information about a
person, and all of the columns labeled `day_n` represent that person’s
disease state at time
n.

``` r
# TODO: make a function to generate new day columns based on some initial parameter.
# TODO: decide what to do about states as factors. Currently I just made everything strings because I couldn't keep things straight.
population = tibble(
  person_ids = 1:initial_population_size,
  day_1 = create_initial_population_with_one_infected(initial_population_size)
)

for (day in 2:total_days) {
  prev_day = population[[(str_c("day_", day - 1))]]
  population =
    add_column(
      population,
      !!str_c("day_", day) := flatten_chr(map2(population$person_ids, prev_day, change_state, day))
      )
  }
```

    ## 
    ## 
    ## Person ID:  1
    ## Day:  2
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6902254
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.05963121
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7328921
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8432829
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.3205984
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7732579
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.1450962
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.716561
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.01000406
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8332217
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.1840499
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.03561239
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.2292922
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.3065861
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.1599445
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.06480791
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.3215991
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.03899219
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7263609
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7413005
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  3
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3630274
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2898523
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.03124722
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.8063401
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.8294029
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.04195461
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1127703
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1439877
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7630775
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2988467
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7417998
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.07451054
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.6196241
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2750145
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.9018687
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.501858
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.9654219
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.4658627
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.426929
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2067282
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  4
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5073546
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.03795801
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.6223378
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7502696
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.6428329
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.6513737
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.0968285
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.03643648
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.187348
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.01202594
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.4194083
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.4473304
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.9343089
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.6971473
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.6841419
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.1556454
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7724946
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.2832847
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5738512
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.9524828
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  5
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.04178434
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.3571707
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.0575384
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.6711642
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5807825
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.1233554
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.3423045
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.9856381
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5001716
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.8882889
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.9486342
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.3601656
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5364273
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5392739
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.2423616
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.7519654
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.2027107
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.2485481
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.7747494
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.9435245
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  6
    ## Previous state:  dead
    ## Random number:  0.8779113
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8719811
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.2319532
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.3380502
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8160459
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.6947462
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.2610378
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8390388
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.4544721
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8330017
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.6053192
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.134509
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.4523719
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.2257719
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.1218901
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.9212898
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8890403
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5679257
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.6086769
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.4146571
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  7
    ## Previous state:  dead
    ## Random number:  0.1357023
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.4624678
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9959958
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.3922085
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9527834
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.8329354
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.8307403
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.6254453
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5860562
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.7839514
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.3445599
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.1037759
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.4602408
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9843978
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5096021
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9952142
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.7458838
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.4574855
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.3504574
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5080797
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  8
    ## Previous state:  dead
    ## Random number:  0.8402132
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.5226497
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.5489975
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.5218525
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8348879
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.01323728
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.9373311
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6668651
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8478858
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.9269989
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6823638
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.06815014
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6046033
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7177937
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7179856
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.0320872
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.808326
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8254855
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.3579664
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7285579
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  9
    ## Previous state:  dead
    ## Random number:  0.4105627
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.1718983
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.0314136
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.983242
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.2906036
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.2651922
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.6443564
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.2074511
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7901163
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.4327431
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.4826373
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.9293841
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7702572
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7505114
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.947928
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.3711775
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.3422828
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7626329
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.2635458
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5481005
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  10
    ## Previous state:  dead
    ## Random number:  0.1814357
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3870692
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.05020644
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.8833248
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3379146
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2581912
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.05756955
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1743893
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.07264711
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1346789
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2275794
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3483313
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.04278141
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2138672
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.9257762
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.116219
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1384521
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.4738018
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.6521351
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.09889395
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  11
    ## Previous state:  dead
    ## Random number:  0.9870003
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3444886
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.1445167
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5382622
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2952823
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.6029932
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9150577
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3128735
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2728403
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.05625187
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.6005334
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.4222928
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.8840443
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.4551713
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3974353
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9535641
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3901053
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.6673484
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9917697
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.7058797
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  12
    ## Previous state:  dead
    ## Random number:  0.6445819
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.4948553
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.4157518
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7401202
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.6547702
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7735375
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2493582
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7974671
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.391337
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.8650086
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2329908
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2386215
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7166277
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.4281724
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.3658319
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.02116345
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.5203083
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.4199443
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.3342121
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.5928688
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  13
    ## Previous state:  dead
    ## Random number:  0.4377445
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.1252518
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.2554466
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.826816
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.8391575
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.1493875
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.1434055
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.3093584
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.138098
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.8325186
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.3856185
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.50386
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.6663082
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.0892488
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.5871938
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.3204995
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9796935
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4487604
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9297612
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9799793
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  14
    ## Previous state:  dead
    ## Random number:  0.1171371
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.8965573
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3956251
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.07088039
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.1937063
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6873171
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.5576563
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.2105307
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.4780952
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6901881
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.1456175
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.7113324
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.948366
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.9106929
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.1690314
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.124308
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.1266571
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3558187
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.221067
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.2516136
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  15
    ## Previous state:  dead
    ## Random number:  0.6219649
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.9274375
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.05559924
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.7913127
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.5958783
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.9313847
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6443799
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.5216455
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.9240345
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6690487
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8842148
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.1585502
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.801811
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.620853
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.1128418
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.195773
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.1325644
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.7901425
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.2523179
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.5877323
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  16
    ## Previous state:  dead
    ## Random number:  0.985786
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.6196643
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.1088511
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.6825181
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.7341954
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2419949
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.5684835
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2488776
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.9814735
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8000642
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.001400626
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.6983378
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.1011805
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.4612465
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8233941
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.4323009
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2294327
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.9166013
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.4859918
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.3251063
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  17
    ## Previous state:  dead
    ## Random number:  0.7038096
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.6258691
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.1903284
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.56862
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.5136759
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.9049088
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.1678535
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.4919814
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.786687
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.7880783
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.04795919
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.6673503
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.5919479
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.1506505
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.1714349
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.0199834
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.4402024
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.6925399
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.214683
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.1212138
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  18
    ## Previous state:  dead
    ## Random number:  0.2300061
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.0998489
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2126142
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.0446857
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.8090429
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.03122001
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7853897
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.9458461
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.3543609
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.6965318
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.3595321
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.01117878
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.1950621
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.378162
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7570468
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.0004435407
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.8400101
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2099813
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.08342594
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7344572
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  19
    ## Previous state:  dead
    ## Random number:  0.576138
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.1583103
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.1353107
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.8732464
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.979859
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.5250992
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.3530318
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.6038557
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.4805639
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.8822922
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.1171386
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.1098833
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.0276771
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.50226
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.4376864
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.3862094
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.794564
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.5672781
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.8350035
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.824156
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  20
    ## Previous state:  dead
    ## Random number:  0.3642884
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.0240658
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.2163467
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5987943
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3678992
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3410632
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.2402003
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.4689475
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.9158026
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.4823891
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.6408409
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.8372633
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.790488
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.9841419
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.9762699
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3939292
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.4395647
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.799517
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3080728
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.07446833
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  21
    ## Previous state:  dead
    ## Random number:  0.1742198
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.3914138
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.6047604
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.02119605
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.9561517
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.860998
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.5655172
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7821676
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.3052852
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.1883566
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.2158155
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.09314228
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.004506734
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.8637367
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7511405
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.6246104
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7393425
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.3119636
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.686765
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.2702765
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  22
    ## Previous state:  dead
    ## Random number:  0.1111135
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.2823854
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3216655
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.2558677
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3745813
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.7062632
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.07835951
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.07830066
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3110351
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.106354
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3986795
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.7665275
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4667841
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.1530058
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.9657482
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.7127105
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4735575
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.2907754
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.45207
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.9467404
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  23
    ## Previous state:  dead
    ## Random number:  0.429954
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.504289
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.8355795
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.2713943
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4017319
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.2913067
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4881072
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4719051
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.6573513
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.6592891
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.8665866
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4347505
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.9015639
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.5890878
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.5442776
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.6940294
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.1458087
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.1182616
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4468382
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.3729523
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  24
    ## Previous state:  dead
    ## Random number:  0.6479413
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.6843583
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.1436581
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.748418
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.5819705
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.2633927
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.8686962
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.6343946
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.4520309
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.2166081
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7686658
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7095242
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.8139722
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.07822441
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.07719393
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.5105722
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.0483499
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.2485967
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.6508118
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.04677311
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  25
    ## Previous state:  dead
    ## Random number:  0.04696699
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.5113832
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.1030282
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.8909425
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.6040958
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.853572
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.9938971
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.4832201
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.1364899
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.05849703
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.07102187
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.308451
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.9033057
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.8144368
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.5402957
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.2918672
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.4494782
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.3187128
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.8702153
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.9365
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  26
    ## Previous state:  dead
    ## Random number:  0.1170727
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.08885662
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.4380806
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2480211
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.01785594
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.0009593011
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.522034
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.9951382
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5244302
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.7561645
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.205453
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.4690244
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5101742
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.07911866
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.0008871153
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.3123904
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.8622942
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.4655368
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.7256599
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.4824903
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  27
    ## Previous state:  dead
    ## Random number:  0.2237419
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.07082364
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.7580408
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.9185442
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.3084777
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.09853375
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.956355
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4846336
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.07521996
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.06127625
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.2644905
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.8204602
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.08780799
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4820967
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.9543444
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4480858
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4911366
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.107623
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.8681522
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.5873406
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  28
    ## Previous state:  dead
    ## Random number:  0.5929034
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.3386365
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.8741546
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6302375
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.9131743
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.7618552
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.711747
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.4670498
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.390348
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.1609136
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.9882533
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.7287834
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.4731958
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.9091998
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.5819504
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.9938547
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.1208826
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.3709388
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.2739839
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.3464638
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  29
    ## Previous state:  dead
    ## Random number:  0.426192
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9764838
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.2857033
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7948563
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.8072025
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.574885
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.5412971
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.0910621
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7651203
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.3381213
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.8009566
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.1501492
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.373149
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7254113
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.5262273
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7795968
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.3536211
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.3583589
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.8954422
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9146992
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  30
    ## Previous state:  dead
    ## Random number:  0.7336708
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.3096049
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.6217839
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.9978528
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.7156279
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.3942943
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.4566237
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.3535359
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.1880631
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.6283831
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.01582186
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.9287113
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.9525949
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.2476587
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.9430672
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.01057063
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.6287678
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.9557497
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.8485868
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.8779208
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  31
    ## Previous state:  dead
    ## Random number:  0.7417305
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.2425562
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.6302742
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.0429964
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.3371122
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.2241879
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.7832259
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.3623603
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.842975
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.202224
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.7832378
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.7386717
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.3064479
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.6982684
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.3599555
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.186744
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9898771
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.7495922
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.7677431
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.4087105
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  32
    ## Previous state:  dead
    ## Random number:  0.6898215
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.7218202
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.2521119
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.8671676
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.242177
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.5558434
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.4193682
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.3618781
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.5234627
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.8494496
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.9889559
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.1565799
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.9904942
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.2445872
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.880887
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.2229289
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.4577694
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.9476206
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.7027403
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.7275165
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  33
    ## Previous state:  dead
    ## Random number:  0.7512122
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5743431
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.9020725
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5931473
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.3115575
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.1260996
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.570489
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.20237
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.1820444
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.34112
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.9943721
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5484385
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.4641553
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.4077372
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.6449533
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.2584949
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.7348483
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.7477645
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.6841196
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.02028098
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  34
    ## Previous state:  dead
    ## Random number:  0.6644873
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4366083
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.2116912
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3626042
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.175685
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3604727
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.6835842
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4016265
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.6541248
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3860512
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4357423
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4548976
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.2832331
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.5163425
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4979833
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.1356344
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4375597
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.1343281
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.9138262
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.2743695
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  35
    ## Previous state:  dead
    ## Random number:  0.4094259
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.2027429
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6931201
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6820894
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.8994584
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.1446469
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6855576
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.3115307
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.8797316
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.7671629
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.82891
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.01937917
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.9243743
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.9446485
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.2926517
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.3461028
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.4731228
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.73975
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.09675177
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.9262277
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  36
    ## Previous state:  dead
    ## Random number:  0.5587078
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.3037824
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.9514274
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.1050649
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.8305354
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.2288876
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.1132575
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.3591176
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5240647
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.6100386
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.4771336
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5248068
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.3384193
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.459366
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.8616664
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.2095742
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.08136719
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.4504428
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.9382701
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7559371
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  37
    ## Previous state:  dead
    ## Random number:  0.7327094
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6519093
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.3757651
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.96872
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.1007459
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.5345504
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6792258
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.5259417
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.8389442
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.0917067
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.4769771
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.7610835
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.8416496
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.03341138
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6046296
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.2618588
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6451656
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6582201
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9431292
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.8063456
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  38
    ## Previous state:  dead
    ## Random number:  0.9587035
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.3279349
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.1833211
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9201444
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.2236212
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.3183066
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.8468338
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.1894921
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9620663
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5954078
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.06204059
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5715136
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5643058
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9468972
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.1348556
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.1464792
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.2851437
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.3213128
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5913281
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.6243679
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  39
    ## Previous state:  dead
    ## Random number:  0.6905977
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.6908995
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.469647
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.5482033
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.2816787
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.884329
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.9927095
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.7055066
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.04180966
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.1707352
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.676426
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.7450584
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.06351108
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.199372
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.9934115
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.1149606
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.229539
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.2597661
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.6817998
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.7258323
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  40
    ## Previous state:  dead
    ## Random number:  0.2951928
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.9736309
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.5205229
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.5566587
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.7292376
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.9355236
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.2247419
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.06003673
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.682486
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.06517189
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.5488884
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.3945046
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.7512942
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.2932543
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.4687858
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.6391634
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.2450995
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.7384256
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.2704316
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.3943075
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  41
    ## Previous state:  dead
    ## Random number:  0.8928076
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7472637
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.4113061
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.9237182
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.2577132
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7035525
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.620619
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.5847948
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.6800861
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.5645886
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.4604738
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.6254272
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.5095544
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.728723
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7096391
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.9060993
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.0379459
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.9416137
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.1210883
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.35983
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  42
    ## Previous state:  dead
    ## Random number:  0.4195764
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.8872121
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.05018445
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.5382217
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.4951978
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.2314076
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.3768791
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.1747973
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.05259272
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.6987749
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.7456255
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.05071748
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.6431151
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.01383236
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.5490563
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.9603933
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.8677789
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.360802
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.2181106
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.548947
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  43
    ## Previous state:  dead
    ## Random number:  0.9351532
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1616632
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.04359317
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8375593
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.661993
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.9257224
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.7498859
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1670797
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.01984263
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.7884069
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8233801
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1417431
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.0932931
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.05163537
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.6966705
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.6511797
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1120241
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.3892569
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.787705
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.635472
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  44
    ## Previous state:  dead
    ## Random number:  0.0782622
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.630607
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8079698
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.7750769
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.7759731
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.9866696
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.1860404
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.5898221
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.6511441
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.06146189
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.7093449
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.3660901
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.1694294
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.2651091
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.6871311
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8401884
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.1887097
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8444588
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.7468913
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.178195
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  45
    ## Previous state:  dead
    ## Random number:  0.3404935
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.9032554
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.7175036
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.456869
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.5034813
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.3128161
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.6421487
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.6960688
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1696872
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4487184
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.3835951
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4217815
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.259078
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.2095611
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.2605954
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1729273
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.9670083
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.03243031
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.6773754
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.7959401
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  46
    ## Previous state:  dead
    ## Random number:  0.04057546
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.4483067
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.1357945
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2604538
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5810563
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.7587462
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.4559722
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5911077
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.8764672
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2208921
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.3823722
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.9784448
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.8684482
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.7713371
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5440986
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6447435
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5330025
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5820134
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2401323
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.784796
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  47
    ## Previous state:  dead
    ## Random number:  0.7396613
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.9932687
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.2302338
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.4388706
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.9009473
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.4497705
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.7402152
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.1904326
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.4987691
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.9885514
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.7489158
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3049981
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.694484
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.5547134
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.5159794
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3915135
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.6452096
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.01673435
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.1940632
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.6404084
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  48
    ## Previous state:  dead
    ## Random number:  0.843769
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.4637745
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.3937493
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.02670351
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.04790043
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.164488
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1680438
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.5826976
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.7352727
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.3518883
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.8737494
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.07513694
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.03864497
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.165727
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.8674256
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1097692
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.0592145
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.6449531
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.3489636
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.937741
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  49
    ## Previous state:  dead
    ## Random number:  0.4567412
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.9321423
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.8955318
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.3883264
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.6896188
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.2782818
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.2066297
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.3419227
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.8965439
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.02154154
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.1374617
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.2996302
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.7851473
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.5050704
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.93579
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.7899393
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.09221591
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.8244368
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.4883021
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.5953288
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  50
    ## Previous state:  dead
    ## Random number:  0.6663418
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.9174856
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.3675299
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.7817662
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.2293759
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.6160365
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.9313983
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.7572504
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.4137219
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.6776967
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.3124113
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.8206704
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.5608906
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.1130248
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.6940082
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.7464089
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.7013758
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.4384016
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.5983017
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.9754205
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  51
    ## Previous state:  dead
    ## Random number:  0.902201
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.9962621
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.9359119
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.5613255
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.604241
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6904204
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.487814
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.7506092
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.9458511
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.2177945
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.02302622
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.9845919
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.7950541
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.09060337
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.8821703
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.04502751
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.5796819
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6421296
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6550039
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6507162
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  52
    ## Previous state:  dead
    ## Random number:  0.65741
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.2634835
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.05083183
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.1053828
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.03379394
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5679286
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.2671142
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.08369711
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5923852
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.2003708
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.8635068
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.3363284
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5287398
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.4134871
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.9823956
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.988381
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.226563
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.2789743
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.4648721
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.7526366
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  53
    ## Previous state:  dead
    ## Random number:  0.5487208
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.5726669
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.9661741
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.08819874
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.5857488
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2762717
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.5661444
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.5663014
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.9622144
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.243412
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.7622871
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.8072003
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.4218828
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2686883
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.1774025
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.4470835
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.9095677
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.8851709
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.3027933
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.7144282
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  54
    ## Previous state:  dead
    ## Random number:  0.08428628
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.6934139
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.222075
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5982681
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.2565933
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.591498
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5740602
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.8080268
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.277089
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.707482
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5224367
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.8791117
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.4152014
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.4267529
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.7490859
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.9815699
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.8372504
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.0948925
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.4621067
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.8900697
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  55
    ## Previous state:  dead
    ## Random number:  0.7806514
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7379985
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.9354861
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.3462379
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.3466037
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.1536108
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.8291629
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7120915
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.2951155
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5719458
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5298699
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7110446
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7129625
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.4310401
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.4712587
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.4726442
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.583826
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.2046258
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7726656
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5044028
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  56
    ## Previous state:  dead
    ## Random number:  0.1360766
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.7882833
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.6050883
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9834809
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.1540004
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.241407
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.6984274
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4911782
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.02430655
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.03634622
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.5797193
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.7224722
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3853395
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9975038
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.1243096
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.2580427
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.2914117
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.7489853
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3842971
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9435042
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  57
    ## Previous state:  dead
    ## Random number:  0.2909369
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.678297
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.125376
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.4772922
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.05203904
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.4042615
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.3396083
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.4756845
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.8436081
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.8934318
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.4903044
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.6706362
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.6829882
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.07530397
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.3522689
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.5155048
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.6524875
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.7304175
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.07396221
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.6053083
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  58
    ## Previous state:  dead
    ## Random number:  0.8793838
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.3008851
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.1624552
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4658074
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.007976556
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.208219
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.484461
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.7280611
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.02436476
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.3467845
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.8832996
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.08262411
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.9750283
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.8955474
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.9597578
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.8911607
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.5962119
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.409622
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.8727247
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4800939
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  59
    ## Previous state:  dead
    ## Random number:  0.9661694
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.1813984
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3113971
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3358834
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.7682965
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.9638173
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.717444
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3160791
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3428059
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.2670546
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.1998973
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.9407628
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.1414747
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.5468803
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.2501108
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.1849065
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.8106113
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.9723752
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3259398
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.6845064
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  60
    ## Previous state:  dead
    ## Random number:  0.3589379
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.1760682
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.7984539
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.6410208
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.2836659
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.8937525
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9391851
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.3629082
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9068102
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.02776165
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.01260333
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.1367167
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.125038
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.09534569
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.5807064
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.04261206
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.1039942
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.2581205
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.2372478
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9545767
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  61
    ## Previous state:  dead
    ## Random number:  0.9460812
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.9235829
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.4619981
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.3803091
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.608424
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.2477322
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.250466
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.5962327
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.7038209
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.005905701
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.9890199
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.3623107
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.8095076
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.9400909
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.6619899
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.5203282
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.1709345
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.5142019
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.9109537
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.3951518
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  62
    ## Previous state:  dead
    ## Random number:  0.04316565
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.2567792
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.7250105
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.9634013
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.8270383
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.4727532
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.2981957
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.00646867
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.02755714
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.7235505
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.4576237
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6760894
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.4004602
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.1479288
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.1599191
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.7286022
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.3572915
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.5845938
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.5668084
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.8650322
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  63
    ## Previous state:  dead
    ## Random number:  0.5438826
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.01522954
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6828812
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.3064741
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6363324
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.4214416
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.09990047
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.07087378
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.09461623
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.428271
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9519393
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9751436
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6251157
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.2042705
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.5586527
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.154253
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.5499389
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.009335403
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.8237955
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.8150053
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  64
    ## Previous state:  dead
    ## Random number:  0.3075026
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.53038
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.3262103
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.400494
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.07485174
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.7344542
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.06151188
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.1425598
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2719343
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.1887276
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5584771
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.1755748
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2236769
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.9147386
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.8628011
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.009840027
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5668739
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5567523
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.8228393
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5955031
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  65
    ## Previous state:  dead
    ## Random number:  0.620374
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6184868
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.126682
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.684505
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.652469
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.7673923
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.4304249
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.5835343
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.1941626
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6852765
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.2496921
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.5476428
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.5716253
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6310629
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.8945005
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.8762998
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.5864397
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.9290406
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.2183616
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.3006495
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  66
    ## Previous state:  dead
    ## Random number:  0.8225937
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.3425572
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.04772358
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.07653617
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2926202
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.6588548
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.5162169
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2043392
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.6982049
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.4308766
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.665788
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.1448441
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.1897206
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.07282675
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2167378
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.7110822
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.431705
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.8977775
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.5503848
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.3919996
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  67
    ## Previous state:  dead
    ## Random number:  0.6688846
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.007346967
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.3304602
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.864686
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.1564626
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.7847716
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.6024827
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.3550888
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.5372754
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.4770121
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.9616262
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.1623155
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.4789578
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.3687152
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.9837375
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.3808859
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2142695
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.6477854
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.372442
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.6502565
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  68
    ## Previous state:  dead
    ## Random number:  0.02912823
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.02186665
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.4520224
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.572337
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.3096046
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9365556
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.5859673
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.8146739
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.3646266
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.4652704
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.6525704
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.8486106
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.3768401
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.7586881
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.1244642
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.4066329
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9305803
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.3329903
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.8167786
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.6230312
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  69
    ## Previous state:  dead
    ## Random number:  0.8560135
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.1679538
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.4069648
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.09864842
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.5205804
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.843653
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3657898
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.2121108
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.4915707
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.4422903
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3215572
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.05665476
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.6943991
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.9705032
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.788835
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.8349122
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.4745486
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.816702
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.8546706
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3618996
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  70
    ## Previous state:  dead
    ## Random number:  0.5108025
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.03828304
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.6840423
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.363342
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7386264
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.2731158
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.08007902
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.979068
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.5750146
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.1277186
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.901995
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.8891981
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.02763991
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.2448933
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.03529457
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7535514
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.02958791
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.04104374
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.1667005
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.8978288
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  71
    ## Previous state:  dead
    ## Random number:  0.9705106
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.7264719
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.3612506
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.9100262
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.2039722
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.8843444
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.06359911
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.4664415
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.8688503
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.5805062
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6041931
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.9087082
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.8121907
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.7674014
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.02453479
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.3882371
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.0784817
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.9718311
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.1874279
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.01296649
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  72
    ## Previous state:  dead
    ## Random number:  0.6617703
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.05545796
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.3381268
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.5316885
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.9210545
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.5531293
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.3547058
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.5869059
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.6522436
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.5189516
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7796345
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.6486502
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.8378184
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.3118616
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.06092639
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.9425509
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.4534308
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.6001704
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.3229078
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.5251694
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  73
    ## Previous state:  dead
    ## Random number:  0.9384376
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.7649472
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.3190318
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.2655867
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.8821717
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.3358744
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.4087822
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.03915765
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.5833207
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.8956535
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.252477
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.9384355
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.7948266
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.2840976
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.880773
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.9834938
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.907147
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.5897335
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.3843958
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.08411099
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  74
    ## Previous state:  dead
    ## Random number:  0.2165332
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.723843
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.09484941
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.1094685
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.3497426
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.07246759
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.9231286
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.8140914
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.497463
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.318449
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.06030118
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.08654488
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.5357998
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.01937641
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.5624535
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.5582327
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.1034716
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.1409387
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.8877251
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.9185699
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  75
    ## Previous state:  dead
    ## Random number:  0.2231337
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8947672
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.371839
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.9854798
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.800017
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8467897
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.1135997
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.7547784
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.5040234
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2393781
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8590455
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2092299
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.5383178
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8288164
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.670588
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8029734
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.4305457
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.4608897
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.1106492
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.7542971
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  76
    ## Previous state:  dead
    ## Random number:  0.2072253
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.5233642
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.9099986
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.4298525
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.6350637
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.4831689
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.9255188
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.568257
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.4984884
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.2596444
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7586829
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.213401
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.2097701
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.5765512
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.2470375
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.9332432
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7936292
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.5885531
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3130812
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3667849
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  77
    ## Previous state:  dead
    ## Random number:  0.1677979
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.3991218
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.5926036
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.5485683
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.08758823
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.7060782
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.8265306
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.01445535
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.6583781
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.6036931
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.8181934
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.5752985
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.243655
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.2762848
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.6630623
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.4819288
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.7316392
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.3596708
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.5748543
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.7030703
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  78
    ## Previous state:  dead
    ## Random number:  0.8460863
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9225349
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.6179873
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.5541755
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.1814965
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.5685185
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.4839949
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.363048
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.2181057
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.5788712
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9706308
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.3354317
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.4315809
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9127549
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.4405216
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.7350082
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.08823141
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.919788
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.867036
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9045165
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  79
    ## Previous state:  dead
    ## Random number:  0.09418361
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.1206868
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.9197432
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.6402207
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.1292648
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.5763544
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.4461918
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.7349785
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.7113595
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.5006967
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.02555083
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.3894079
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.3443766
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.9722809
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.3964924
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.583462
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.8600774
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.6679451
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.125047
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.3631994
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  80
    ## Previous state:  dead
    ## Random number:  0.08851796
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.531126
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5670988
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.2090929
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.8766591
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.01952859
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5499144
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.8470422
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.3979556
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.920954
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.3680537
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5650316
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.07341742
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.1537535
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.6302453
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5955512
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.9380854
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.2414219
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.1226599
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.379853
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  81
    ## Previous state:  dead
    ## Random number:  0.7742138
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.9067635
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.476794
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.9260962
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3426903
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.7615278
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.26322
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.1531699
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.806557
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.8103234
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.19149
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.09994989
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.4007233
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.5119901
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.742027
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3318171
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.7977447
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.8585057
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.2790109
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.4083949
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  82
    ## Previous state:  dead
    ## Random number:  0.6283599
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7611167
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.6944994
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7915737
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.6432566
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.1546988
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7339565
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7578088
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.3692826
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.4876097
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.3620025
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.8797901
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.4140607
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.08435848
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.1234211
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.6328068
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7598333
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.9210219
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.06803227
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.1281953
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  83
    ## Previous state:  dead
    ## Random number:  0.4602262
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9939347
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.517128
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.0159668
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.05246914
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9855422
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.2077646
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.8360486
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.8835417
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.2290434
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.7330387
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.3459196
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9305688
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.4229432
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.990939
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.2110035
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.635224
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.1401086
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.7272178
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.5903213
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  84
    ## Previous state:  dead
    ## Random number:  0.3412434
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.06168775
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.6518204
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.8150415
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.6143166
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.06223688
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3674735
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3056333
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.302226
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3972266
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.07907797
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.9370882
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.5142188
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.6886024
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.8712641
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.5513108
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.187165
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.08372141
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.7884026
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.2411473
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  85
    ## Previous state:  dead
    ## Random number:  0.4389464
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.6493187
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.4200348
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.9739551
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.8634278
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.3740372
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.7910917
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.7946032
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.7755388
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.06441919
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2732705
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.9546076
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.5776586
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.4839727
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.9118573
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.06427499
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.1008663
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.6129125
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2024535
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2481553
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  86
    ## Previous state:  dead
    ## Random number:  0.7332053
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.9618836
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4657929
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.5290915
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.82981
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.01845992
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.608487
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.409783
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.787356
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.02996889
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.9296894
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.7804616
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.1029141
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.3812942
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.03719683
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.07910087
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.6899791
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.419599
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.5879981
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.6272749
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  87
    ## Previous state:  dead
    ## Random number:  0.9926552
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.5278182
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4917306
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4349974
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3536233
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3122738
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.2276556
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6316658
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6521385
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4442368
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.5478934
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.2532746
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.5386782
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.8115952
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4579683
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6432236
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6103184
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6185507
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.05720944
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.9801476
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  88
    ## Previous state:  dead
    ## Random number:  0.8505028
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.5284028
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.310825
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.6366428
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.9249101
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.9431934
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.8309718
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.7764057
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.6369309
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.9944563
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.4724329
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.001679974
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.8733504
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.918639
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.5627853
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.5808723
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.9743163
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.5645952
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.4589098
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.5111994
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  89
    ## Previous state:  dead
    ## Random number:  0.08740996
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.9110844
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.08521911
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.5564434
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.5141285
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.3947543
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4183058
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.3322338
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4504447
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.09828091
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.6740037
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.8128897
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.5975482
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.3064261
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.5290286
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.5507565
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.5312086
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.673102
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4102525
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4499447
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  90
    ## Previous state:  dead
    ## Random number:  0.6763302
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.294203
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.2225267
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.8386133
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.8744194
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.6340535
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.735782
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.5494774
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9808445
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.6943183
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4706222
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.1651477
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4379658
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.36373
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.2861064
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.03500745
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.455085
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.6312771
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9086459
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.8738598
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  91
    ## Previous state:  dead
    ## Random number:  0.7356763
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.7482482
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.886816
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.9805412
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.2711923
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.3985787
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6757371
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.8596546
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6304955
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.4886619
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.9349164
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.473704
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.9804947
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6628736
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.09820874
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.03602613
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.433614
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6037891
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6600335
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.8985535
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  92
    ## Previous state:  dead
    ## Random number:  0.881664
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.216863
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.4372965
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.09358954
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.6115967
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.7524681
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.3715964
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.04223764
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8030834
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.6806121
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.1870385
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.2570393
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8209761
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.3688605
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.4550534
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.9809481
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8509415
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.0007128229
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8401391
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.3357167
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  93
    ## Previous state:  dead
    ## Random number:  0.3201482
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3003377
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.461635
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.8786439
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3776204
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.7640375
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.210639
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.03233863
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3408278
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.4018577
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.06964848
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.5055687
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.856518
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.01988748
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.5797115
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.249193
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.8404283
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.4273982
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.2587183
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3115912
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  94
    ## Previous state:  dead
    ## Random number:  0.6151868
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.3835675
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.2675928
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.881863
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.2276257
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4057759
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.9320473
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4158191
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.8461578
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.767599
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.564195
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.781919
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4631825
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.3787667
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.7467797
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.6244267
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.2921744
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.1684747
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.008980647
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.06030072
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  95
    ## Previous state:  dead
    ## Random number:  0.7632328
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.1838979
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.1028342
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.08900649
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3330946
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.6119537
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.8398334
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3935765
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.2582677
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.4353418
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.5898357
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3198729
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.2299458
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.9300686
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.05076283
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.04073717
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3973207
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3052581
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.6630628
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.7041536
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  96
    ## Previous state:  dead
    ## Random number:  0.1106606
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.3042003
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.919252
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.6217287
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.8382399
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.9055192
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.07159114
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.421035
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.9483234
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.09363432
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.2048537
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.932139
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.05323512
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.175529
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.7448031
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.4445558
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.4228313
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.9541207
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.6312934
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.6068964
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  97
    ## Previous state:  dead
    ## Random number:  0.3943135
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.6855883
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3210912
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2562797
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7588865
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.5364901
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.273171
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7763801
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7138283
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.006075508
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3777181
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7967381
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2752372
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.8373093
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.5021993
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.6727755
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.703624
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7117102
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.9925858
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.1229223
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  98
    ## Previous state:  dead
    ## Random number:  0.7589451
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.9229722
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.02320427
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.7070668
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.604065
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.8083646
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.4703262
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.1354794
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.8093502
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.1488296
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.02407473
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.4271469
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.415437
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.7924887
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.3278799
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.6632176
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.6827087
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.6502163
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.326097
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.2896563
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  99
    ## Previous state:  dead
    ## Random number:  0.1254499
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.1518861
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.219604
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.516177
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.1496153
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.4940822
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.5393292
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.3104135
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.6556381
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.4531283
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.8584537
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.1880736
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.778119
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2285251
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.5733594
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.4968843
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.8709261
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2670191
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.09567145
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.682102
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  100
    ## Previous state:  dead
    ## Random number:  0.8231815
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.8566343
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.4817462
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.05315447
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2890373
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.1577084
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.08012048
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.4801405
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.885215
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.02475856
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.4468847
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.9281042
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.288961
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.7516458
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.09379615
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.9042782
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.9458165
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.5181365
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.3219314
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.6001906
    ## New state:  succeptible

``` r
# remove this later, printing just for debugging
population
```

    ## # A tibble: 20 x 101
    ##    person_ids day_1 day_2 day_3 day_4 day_5 day_6 day_7 day_8 day_9 day_10
    ##         <int> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> 
    ##  1          1 infe… infe… infe… infe… dead  dead  dead  dead  dead  dead  
    ##  2          2 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ##  3          3 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ##  4          4 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ##  5          5 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ##  6          6 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ##  7          7 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ##  8          8 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ##  9          9 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 10         10 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 11         11 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 12         12 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 13         13 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 14         14 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 15         15 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 16         16 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 17         17 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 18         18 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 19         19 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## 20         20 succ… succ… succ… succ… succ… succ… succ… succ… succ… succe…
    ## # … with 90 more variables: day_11 <chr>, day_12 <chr>, day_13 <chr>,
    ## #   day_14 <chr>, day_15 <chr>, day_16 <chr>, day_17 <chr>, day_18 <chr>,
    ## #   day_19 <chr>, day_20 <chr>, day_21 <chr>, day_22 <chr>, day_23 <chr>,
    ## #   day_24 <chr>, day_25 <chr>, day_26 <chr>, day_27 <chr>, day_28 <chr>,
    ## #   day_29 <chr>, day_30 <chr>, day_31 <chr>, day_32 <chr>, day_33 <chr>,
    ## #   day_34 <chr>, day_35 <chr>, day_36 <chr>, day_37 <chr>, day_38 <chr>,
    ## #   day_39 <chr>, day_40 <chr>, day_41 <chr>, day_42 <chr>, day_43 <chr>,
    ## #   day_44 <chr>, day_45 <chr>, day_46 <chr>, day_47 <chr>, day_48 <chr>,
    ## #   day_49 <chr>, day_50 <chr>, day_51 <chr>, day_52 <chr>, day_53 <chr>,
    ## #   day_54 <chr>, day_55 <chr>, day_56 <chr>, day_57 <chr>, day_58 <chr>,
    ## #   day_59 <chr>, day_60 <chr>, day_61 <chr>, day_62 <chr>, day_63 <chr>,
    ## #   day_64 <chr>, day_65 <chr>, day_66 <chr>, day_67 <chr>, day_68 <chr>,
    ## #   day_69 <chr>, day_70 <chr>, day_71 <chr>, day_72 <chr>, day_73 <chr>,
    ## #   day_74 <chr>, day_75 <chr>, day_76 <chr>, day_77 <chr>, day_78 <chr>,
    ## #   day_79 <chr>, day_80 <chr>, day_81 <chr>, day_82 <chr>, day_83 <chr>,
    ## #   day_84 <chr>, day_85 <chr>, day_86 <chr>, day_87 <chr>, day_88 <chr>,
    ## #   day_89 <chr>, day_90 <chr>, day_91 <chr>, day_92 <chr>, day_93 <chr>,
    ## #   day_94 <chr>, day_95 <chr>, day_96 <chr>, day_97 <chr>, day_98 <chr>,
    ## #   day_99 <chr>, day_100 <chr>

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

    ## # A tibble: 200 x 3
    ## # Groups:   day [100]
    ##      day state                             count
    ##    <dbl> <chr>                             <int>
    ##  1     1 infected_symptomatic_pre_symptoms     1
    ##  2     1 succeptible                          19
    ##  3     2 infected_symptomatic_pre_symptoms     1
    ##  4     2 succeptible                          19
    ##  5     3 infected_symptomatic_pre_symptoms     1
    ##  6     3 succeptible                          19
    ##  7     4 infected_symptomatic_pre_symptoms     1
    ##  8     4 succeptible                          19
    ##  9     5 dead                                  1
    ## 10     5 succeptible                          19
    ## # … with 190 more rows

Graph the person-count of each state in a different color, with days on
the x-axis.

``` r
ggplot(population_to_visualize, 
       aes(x=day, y=count, color=state)) + 
  geom_line()
```

![](Modeling-code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
