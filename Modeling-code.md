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
  new_state = prev_state # The default is to stay at the same state. This always happens for RECOVERED or DEAD
  rand = runif(1) #Question: is it ok to use the same random number for every decision in this function?
  
  if (prev_state == SUCCEPTIBLE) {
    if (newly_infected[[str_c("day_", day)]][id] == 1) {
      if (rand <= probability_asymptomatic) {new_state = INFECTED_ASYMPTOMATIC}
      else {new_state = INFECTED_SYMPTOMATIC_PRE_SYMPTOMS}
    }
  }

  if (prev_state == INFECTED_ASYMPTOMATIC) {
    # WAIT 4.5 days before turning infectious
  }

  if (prev_state == INFECTIOUS_ASYMPTOMATIC) {
    # WAIT 4 days before recovering
    # INFECT OTHERS according to parameters
  }

  if (prev_state == INFECTED_SYMPTOMATIC_PRE_SYMPTOMS) {
    # WAIT 4.5 days before turning infectious
  }

  if (prev_state == INFECTIOUS_SYMPTOMATIC_PRE_SYMPTOMS) {
    # WAIT .5 days before turning symptomatic 
    # INFECT OTHERS according to parameters
  }

  if (prev_state == SYMPTOMATIC_NEED_HOSPITAL) {
    # SEEK CARE with 5 day delay, or don't
  }

  if (prev_state == SYMPTOMATIC_DONT_NEED_HOSPITAL) {
    # WAIT 3.5 days then recover
  }

  if (prev_state == NEED_HOSPITAL_SEEK_CARE) {
    # immediately either get care or don't
  }

  if (prev_state == NEED_HOSPITAL_DONT_SEEK_CARE) {
    # immediately transition to don't get care (maybe delete this state if it's redundant??)
  }

  if (prev_state == GET_HOSPITAL_CARE) {
    # probabalistically die or recover, after 10.5 days in bed
  }

  if (prev_state == DONT_GET_NEEDED_CARE) {
    # die after 10 days
  }

  # Take out later, for debugging
  cat("\n\nPerson ID: ", id)
  cat("\nDay: ", day)
  cat("\nPrevious state: ", prev_state)
  cat("\nRandom number: ", rand)
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
    ## Random number:  0.4158772
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8477274
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8375563
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.5798427
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.840313
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.129773
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.09002102
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.494171
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.775062
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.2693718
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.5777151
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8145195
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.09956032
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.5024239
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8365842
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.3091025
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.4353493
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.4531857
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7040619
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.3537343
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  3
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7518386
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1065668
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1541073
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.6813716
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.06203461
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1825095
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2253933
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.3442618
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.04349475
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.9251339
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.4747882
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.9870338
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.41288
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.631555
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7978468
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.170631
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.6386911
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.3052004
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.4768104
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2395831
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  4
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.4036654
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.2932269
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.8233287
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.09223325
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.04929942
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.9099518
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5862436
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7744091
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.1051357
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.8280339
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.01964211
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.4271676
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.1436962
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.002673033
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5142571
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.1695895
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.3375601
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7174246
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.4437856
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7136463
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  5
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6032484
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.02794617
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.8211845
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.8364389
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.7474903
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.3275909
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5625545
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.4253233
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.006332606
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.6500303
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.4369817
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5281714
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.8905587
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5903618
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.08641995
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.08164586
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5393957
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5664545
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5298532
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.1159557
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  6
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6325702
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.08067264
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8182318
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5413006
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.9658539
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5317311
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.7060264
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.7728878
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5567202
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.456426
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.6769042
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.04157519
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.1339082
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.3908959
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.08587169
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5493234
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8514324
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.828793
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.6987797
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.4017372
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  7
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6573311
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.002432293
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.07543243
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.4033592
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.6094516
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.1268838
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.7276557
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.8844235
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.6019805
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.6208125
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.926078
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.7890063
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.08867656
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.7139411
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5827832
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.04263354
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.3814885
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.3643195
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.2393511
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.8221888
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  8
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.9100694
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.9753704
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6371431
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.4827593
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6785523
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.5995918
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.157888
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.5443989
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.5258395
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6968808
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7125116
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.4014055
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.1831565
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.1989367
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8493473
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.2363509
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6202484
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7378519
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.2741037
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6539029
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  9
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.111856
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.01896354
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5793728
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.006573849
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.4034893
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.6413831
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5295847
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7107005
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5427309
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.1256776
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.003439453
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.8444967
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.6945239
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.4448723
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.9673732
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.9450027
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5369708
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.539583
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.8538218
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7347507
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  10
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5327925
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2327365
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.9066198
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.4891038
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.7275324
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.522154
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.6256791
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.8612849
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1469939
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.713665
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1688395
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.6725749
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2029688
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1118397
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3272631
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.6297312
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.09606592
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.5193865
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.6481393
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3681049
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  11
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3133291
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9115541
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.7660307
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5339824
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5860119
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2120403
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3281712
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.06227539
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2902509
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9733413
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.6595136
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9173925
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2020173
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.4356453
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.8501244
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.1710054
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.04623617
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9484064
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.961651
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2049171
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  12
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2376663
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.145194
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2510602
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7755875
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.5862642
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2186597
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.1525706
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.5428724
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.9806853
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.8241102
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.3630728
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.8289984
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.8951015
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.6224952
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2497304
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.6436652
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.8503357
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.9479534
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.915222
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.6687883
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  13
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2573724
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.07475455
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.07321509
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.6786872
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4247857
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.3342328
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.06400733
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.3059402
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4949374
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.05995665
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.8645324
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.5059084
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4337116
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.2048593
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.0821857
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4596472
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4608752
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.5401088
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.1544722
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4148454
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  14
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2405826
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.894435
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3703523
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3310023
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.9662068
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.2378921
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.1259198
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.9860159
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6126461
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6895688
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.750909
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.5251021
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3971819
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.905565
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.9978571
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.08748965
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.539339
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.2060258
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.03431575
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.943419
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  15
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3820902
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6301356
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.2836144
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.0114918
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6897568
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.7150053
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8287511
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.5564059
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.4158558
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.9294304
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6865352
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.217055
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.2686854
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.1749255
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8508052
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6684068
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.1702446
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.38122
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.7060613
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8106744
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  16
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3576724
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.9944082
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.09399193
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.5725492
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.4724035
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.5891711
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.7239155
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.3575856
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.1912463
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2432329
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.340286
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2352183
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.1519813
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8660541
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.416842
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2858858
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.5499053
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.007595763
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8731398
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.7194854
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  17
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7796467
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.2633977
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.66605
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.6465769
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.05976925
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.4249796
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.6450473
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.8584466
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.7038436
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.172327
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.4952772
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.2200422
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.3491602
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.9622493
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.4471794
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.8816689
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.2819821
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.172831
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.2817132
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.9597467
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  18
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6476997
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.3330812
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.9809465
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.1736206
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2195893
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.0975835
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2805816
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.4111671
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.1581443
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.671494
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.8038492
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.5076247
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.8612663
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2504864
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7494779
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.9888133
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.1250796
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.1001542
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2290599
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.3928826
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  19
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1727754
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9116557
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9500897
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.5161951
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.766786
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.4572003
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.8673867
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9840545
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9865744
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9742803
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9754041
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9735245
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.2511007
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.4869488
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.4776179
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.6428612
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.06889773
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.06302125
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.3603546
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.3570389
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  20
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7610301
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3782928
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3943877
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.7603428
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.6706837
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.1343283
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.8975455
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3456698
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.7612772
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.7188134
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.774279
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5144992
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.04600204
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.9564802
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.4851411
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.7803687
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5322592
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.224612
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3675006
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5439169
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  21
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7103153
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.546767
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.1937052
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.5591132
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.07246779
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.4100723
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.6985444
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.4077065
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.1694714
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.5513301
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7103195
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7486826
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.02628904
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.9597612
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.8395055
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.5300755
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.4462841
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.3925622
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7609132
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.9572765
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  22
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2169323
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4946443
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.2888922
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.9604749
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3069685
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.8141731
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.2759943
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.407215
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.8885428
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.2025189
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4782334
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.8968577
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3671476
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.6300234
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.07456299
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.07726833
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3823558
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.9332352
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.6540291
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3017842
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  23
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3400093
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.08049759
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.2335101
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.1169071
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4258621
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.05413345
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.6448161
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.2890015
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.7415131
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4892225
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.5584716
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.3088361
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.3841817
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.7264892
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.1129757
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.7611069
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4659101
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.6646502
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.07849133
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.3360768
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  24
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.0822234
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.6006778
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.2980368
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.4550625
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.8802571
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.9384907
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7179606
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.140788
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.8498827
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.6822323
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.2906697
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.8547575
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.3669308
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.1654473
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.8496565
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.2872579
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.1125493
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7489851
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.02354625
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.9940907
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  25
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2854858
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.5336509
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.9755283
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.8466913
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.5562274
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.2873152
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.7564458
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.8680045
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.7835203
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.81094
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.1420278
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.2415871
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.02352129
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.7035925
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.9122246
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.4072159
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.2301752
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.06224026
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.4645711
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.6155369
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  26
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.873913
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.8506731
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.4353267
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.7282887
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.03286879
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.04182417
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2272767
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2867115
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5507655
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.676622
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.06304645
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.3043214
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.8871666
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2440216
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2356441
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5588681
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.7280084
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5006075
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.3879811
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.7470864
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  27
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2758215
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.7035998
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.7188096
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.3006764
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4559309
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.6481873
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.1100288
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4455056
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.6747194
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.01099798
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.7923777
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4255835
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.3534018
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.1372657
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.1224003
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.8112381
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.5827527
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.6168648
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4554889
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4090372
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  28
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1352586
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.764785
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.9602578
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.9735208
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.4059509
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.5153891
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.982364
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6750569
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.9353182
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.7529341
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.2935889
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.0913436
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6008486
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.125349
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.1127303
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.2605301
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.977282
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.885979
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.7803017
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.2769182
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  29
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.711276
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.6706437
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9642919
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.1520476
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.06834643
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.4572
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.1648357
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.5241351
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.6830396
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7037378
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9088742
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.8644557
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9554216
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.8730901
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.4723052
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.6969368
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7642863
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9260141
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9317093
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.2398028
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  30
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1607862
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.4914674
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.2236632
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.8433756
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.7693304
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.2092255
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.6554635
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.07827661
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.505351
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.8604074
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.7736549
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.7798298
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.8503683
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.123235
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.663969
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.718449
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.6376976
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.6546397
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.170658
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.03737507
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  31
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.9639169
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.1628743
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.3751563
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9461547
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.4784615
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.760697
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.7914937
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.8773715
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.8577805
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.8092311
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.6096167
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9351349
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.8512652
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.4541246
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9094371
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.4353936
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.2188655
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.2299555
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.1017726
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.2841581
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  32
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.4946468
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.3156514
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.2846535
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.942218
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.7049694
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.5962078
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.1902634
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.004324064
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.6017932
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.7843881
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.3485703
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.4068616
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.1590366
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.8827167
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.5596039
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.2092411
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.006935789
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.3991542
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.600519
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.0642194
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  33
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3448141
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.9631862
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.4044024
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.3521627
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.7305343
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.1882201
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.8495057
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.07117023
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.4367493
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.9878282
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.7655078
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.9279625
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.4685654
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.04738079
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.8881732
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.9851119
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.8851261
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.229521
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5912593
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.7972718
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  34
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7543713
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3960917
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4774916
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.6528766
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.5441536
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.9236102
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.8969538
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.7633165
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4609798
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.7181085
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.5386841
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.5569089
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.9925218
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.8329991
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.7856516
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.8484972
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.2347862
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4037353
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4664395
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.6306405
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  35
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.8793826
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.2799477
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.4181394
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.441184
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.3877264
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.9901877
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.2689414
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.712665
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.7724694
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.8011505
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.003692811
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.08307323
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.1506521
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.444273
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.1925454
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.09817232
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.8462764
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.4852307
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.5072673
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.7991023
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  36
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.06424796
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.8230548
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.2756091
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.1010678
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7233635
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5589565
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.9781441
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.8225993
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.4482983
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.9584039
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5217485
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.303924
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.8449373
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5023365
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.09858114
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5204431
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.06131992
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.8535682
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.02070097
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7615365
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  37
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.9161943
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.5328064
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.4931334
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.2174587
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.04507732
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.0998202
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.7705338
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6112695
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9334671
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9173667
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.8985521
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9517114
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.8087326
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.3770301
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.4112057
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.3236043
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.7289307
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.821536
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.4014505
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.05406836
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  38
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3768635
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.7573086
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.7540679
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.1298076
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5017512
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.646291
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.7113834
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5135072
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9979568
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.2256946
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9795345
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.06604975
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.3476968
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.05175884
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.1390836
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.8145808
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.7825488
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.8167409
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.4234013
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.3018636
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  39
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.8286341
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.1998289
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.2624726
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.8806305
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.3309136
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.3238258
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.4162903
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.7796223
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.480505
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.1735666
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.487614
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.5899999
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.8037009
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.2103218
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.4192306
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.04035145
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.6018403
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.7201965
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.2787921
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.32466
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  40
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5835691
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.7009467
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.1867749
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.9694059
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.8432465
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.6319781
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.5259288
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.7840424
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.4618684
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.4475209
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.9265061
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.3860318
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.6235795
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.915424
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.0437411
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.3429775
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.2835273
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.7582124
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.6423823
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.8494884
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  41
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2647464
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.4830727
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.1227315
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7901207
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.07427742
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7612544
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.507604
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.786353
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7001899
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.2002187
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.3269577
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.8698718
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7641182
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.1876064
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.129918
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.3454868
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.6795891
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.6814845
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.6901033
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.3775119
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  42
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.663604
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.5979722
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.483095
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.8295995
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.3135532
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.4695004
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.1217943
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.1420081
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.5718805
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.988035
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.9717962
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.3113572
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.6823721
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.02604063
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.156847
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.2040854
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.08848588
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.3350719
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.732065
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.4187683
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  43
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2434251
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.173553
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8138609
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1999049
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.9549631
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8524188
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.0142317
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.2900901
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.3216433
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.0465311
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.9387338
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.7950789
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.7557324
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.3291794
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.4783001
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.03923785
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.6886994
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.07902315
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8505069
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.7938376
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  44
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6889029
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8072912
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.2357201
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.9423939
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.9161204
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8919666
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.0205793
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.392506
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8353711
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.2154165
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.5650825
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.9392405
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.7451713
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8306818
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.5984575
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.7144842
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8968585
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.2534678
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8462266
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.3776622
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  45
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1380326
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4459054
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.8427271
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.02337248
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.5844434
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.5372945
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.3476597
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.5817502
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4022876
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.2901255
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.8732771
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.03412921
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1479558
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.2983885
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.7043192
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.3703494
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.2497129
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.9048274
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4718055
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4824665
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  46
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3507881
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5745557
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6016649
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6276231
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.7844241
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6121951
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6503298
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6835005
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.1395496
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2077155
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6743473
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.759154
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2470713
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2188642
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.3731105
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6444241
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5970907
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.4527308
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2454295
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.01339346
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  47
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.04112538
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.8684054
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.865275
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3516065
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.5761395
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.2620343
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.01741835
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.02167233
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3994774
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.90145
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.4830012
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.5749942
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3377495
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.506042
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.1304354
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.2263111
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.9685716
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.4117342
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.9438801
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3515565
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  48
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1884553
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.08300531
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.7747945
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.2299638
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.3983655
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.3424025
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.7144241
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.211473
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.5545362
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.5456283
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.2264209
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.922619
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.34964
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.8858747
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1407342
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1544271
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1601432
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1752159
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.4895886
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.4521014
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  49
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3327053
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.7130378
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.4850078
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.6482373
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.2956109
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.7688587
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.9970278
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.646769
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.739886
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.8318783
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.7939921
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.8483239
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.9422137
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.1362456
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.1419175
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.03440845
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.5863767
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.03530003
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.3458952
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.2234398
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  50
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6463274
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.4269276
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.3718094
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.289041
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.04778498
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.4879606
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.0008450949
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.09849716
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.8797639
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.7622337
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.3169638
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.9980584
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.748832
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.1424187
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.5674335
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.7988178
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.5086017
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.07768208
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.3341417
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.3472582
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  51
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5785824
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.3829322
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.3716962
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6007096
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.171225
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.01893374
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.5834121
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.140278
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.632318
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.9073471
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.348511
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.2346735
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.111696
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.4925068
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.01933356
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.4073557
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.1921247
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.4240244
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.4360149
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.293441
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  52
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2645465
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.4509551
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.9397464
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.6153517
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5423027
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.2634437
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.09567266
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.6830855
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.3840936
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.09189765
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5502568
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.8914738
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5733091
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.3342587
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.4085057
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.02685009
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5519073
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5490608
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.03061334
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.2658072
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  53
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7716467
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.9242482
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.08030147
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.01206446
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.4192816
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.57205
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.01612011
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.8808784
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.81717
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2318655
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.02713542
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.1334686
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.5410678
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2543558
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2224422
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.8084883
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.4805521
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2689765
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.1896617
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.420235
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  54
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5144149
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.6608146
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5193644
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.9908518
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.07973883
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.6112124
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.8232255
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.6480073
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5432809
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.4661267
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.06156544
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.7269386
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.7213565
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.3371016
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.6229515
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.2003598
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.03068494
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.1947954
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.6329424
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.4948486
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  55
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7013093
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.07886519
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.9112376
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.1387622
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.8287638
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.431791
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.2026749
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5920349
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.3346489
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.2335217
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.1783572
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.938131
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7404731
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5872386
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.2973596
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5802332
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.3972622
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.9870249
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.1147992
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.9898397
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  56
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.4624215
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9413345
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.7779027
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.8618602
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.6751329
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4197455
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4419215
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.02620879
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.394151
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4834103
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9963496
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3299179
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.1645039
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9132171
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3272146
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.05954302
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3347901
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4020065
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.5413945
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.03282045
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  57
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7150154
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.2640293
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.6613938
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.7770824
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.6932435
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.9111115
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.8137765
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.8967243
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.9736651
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.323927
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.5099745
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.9294081
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.5694317
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.7575458
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.4530947
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.6012223
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.1415218
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.3793124
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.5292145
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.5526079
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  58
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.07359207
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.8344145
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.3042987
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.6456981
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.9234243
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.6179777
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4479952
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.9988624
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.9274516
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.9070091
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.1053075
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.1927796
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4745133
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4667523
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4987544
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.6546885
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.003705493
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.1616246
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4966099
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.3745362
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  59
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.350184
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.8278876
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.7493599
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.2114256
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.2247689
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.2342829
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.7217002
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3845871
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.02216474
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.9991139
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.2512704
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.0724258
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.5803357
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.5706823
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.8267661
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.6940362
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.5955374
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3445776
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.6731122
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.174515
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  60
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2037516
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.5462945
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.0448943
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.6116331
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.6869726
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.02267986
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.8093185
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.6666461
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.04101605
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.4046197
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.2749633
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.2097652
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.714091
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.728966
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.8536602
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.6965742
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9996433
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.7624983
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.75951
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9510828
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  61
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5454301
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.7047225
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.6453241
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.5043755
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.7363296
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.6524221
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.2056617
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.769627
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.1777001
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.8631576
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.6884087
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.4140027
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.9581964
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.8110684
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.1904891
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.7111853
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.5896543
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.393509
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.2018808
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.2992962
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  62
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.9739478
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.7458955
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.3293808
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.9507405
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.08112861
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.8908449
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6236959
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.2176253
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.05966929
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.5467174
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.7293408
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.1605425
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6535812
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.1114156
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.3276138
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.8240716
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.0195483
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.4428305
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.5409072
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.9917328
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  63
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.4838251
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.928308
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.01699221
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.4794524
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.3806605
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.4349275
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6960567
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.4533676
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.3297134
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.8149555
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.03595837
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.2441736
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9193877
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.4510953
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9539717
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9689204
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.01747279
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9015776
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.3886029
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.06658377
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  64
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.8574202
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.1521177
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.9664029
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.3746759
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2591541
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2848723
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.8418751
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.675773
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.3607079
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.8966144
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.3815506
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5017915
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5288565
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.202903
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2877846
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.6624652
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.6657588
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.6967843
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.8858655
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.7590012
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  65
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.03334319
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.5867898
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.8846589
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.08794883
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.1673511
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.4935007
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.262071
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.1919434
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.2520186
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.9212244
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.1461405
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.04297451
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.4049216
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.168877
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.9029806
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.9236713
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.1905282
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6568422
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.3635427
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6120519
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  66
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2735524
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.3214927
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.1565514
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.3043689
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.5084511
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.4659644
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.4202079
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2938794
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.04677819
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.9801283
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.5433599
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2722165
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2564608
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.7500585
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.009087481
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.5908158
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.1909899
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2465847
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.5782221
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.9597202
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  67
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.4836575
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.6677509
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2847528
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2256779
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.3721077
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.9247042
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2113334
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.8945825
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.6412219
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.808892
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.7711485
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.9396149
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.4288757
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.4756825
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.5872073
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.5571583
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2351358
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.4001943
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.4363341
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.338828
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  68
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1996043
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.4468435
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9846041
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.482151
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.4230153
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.1189713
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9171226
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9016671
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.7126144
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.5348203
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9907162
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.2135735
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.0807563
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9702301
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.03974202
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9311421
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.002658615
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.1580503
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9927553
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.3211183
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  69
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.868826
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3911782
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.5915928
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.1530476
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.02061341
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3984644
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.6799522
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.6027004
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.6884216
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.07828819
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.8873348
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.7481582
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.4273689
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.5627686
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.6906741
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.9733493
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.274632
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.8059024
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.5813537
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.6639999
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  70
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.8850807
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.538916
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.2370255
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.5216243
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.83899
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.05096079
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.6664317
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.1832335
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.4186305
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.198907
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7726651
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7054173
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.5380935
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7855812
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.6701196
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.4316213
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.9049859
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.8959524
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.412401
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.5242003
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  71
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.05543499
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6413747
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.9629362
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.1019875
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.8509319
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.1400323
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.8493087
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6731999
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.08417932
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.4343328
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.9094208
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.5839148
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.4645092
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.4454141
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.8218394
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.2186588
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.1031828
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.3459346
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.9528389
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6848444
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  72
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.258677
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.02754172
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.4476149
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.6560966
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7572412
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7259555
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.05773946
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7389747
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.187625
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.09136311
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.6885358
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.1114579
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.465048
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7353095
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7870549
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.4386071
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.6964242
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.8172433
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.05331223
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.1241587
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  73
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.305317
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.5306466
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.1749981
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.624875
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.9539559
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.6453859
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.8114425
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.08317865
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.9577249
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.9429524
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.3101675
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.1439272
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.9915297
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.746755
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.2089177
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.694623
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.6757225
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.03847494
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.3493496
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.187501
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  74
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3580848
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.1503805
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.9181261
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.1147117
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.7296763
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.05686375
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.2728383
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.9432084
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.1784645
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.6203601
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.2507147
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.9010641
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.297867
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.04623456
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.3942567
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.3436162
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.725181
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.3639692
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.2813873
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.9996874
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  75
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.9855228
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.156217
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2814607
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.1723642
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.4504689
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.5404058
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.3500149
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.6792236
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.3459513
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.09034288
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.6440877
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2565969
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.7151924
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.3932338
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.1679844
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.7731719
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.5345757
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.5735747
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.1699774
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.6615445
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  76
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.375514
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3763773
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.2972451
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.4853091
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.821849
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.1074356
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.4094535
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.6963641
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.1673069
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7534517
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7793592
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.4998011
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3903989
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.1288792
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.6438438
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7890446
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3832474
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3776911
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.1245231
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.043137
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  77
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1973753
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.424718
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1904757
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.8825411
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1616355
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.8387953
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1571319
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.6893862
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1359685
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.267644
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.8371338
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.943081
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.9211444
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.01351681
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1753897
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.02851116
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.4983141
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.8477697
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1020065
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.9125788
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  78
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6914365
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.2128575
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.6828031
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.0157999
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.7555766
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.05760174
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.2287529
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9004778
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.8248568
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.5924572
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.1605246
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.0402549
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.8665753
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9317215
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.1241597
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.1845336
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.769117
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.03704029
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.7812195
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.6850072
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  79
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.330456
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.3910708
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.2395588
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.1579
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.1741863
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.563209
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.9000472
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.3128501
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.5203816
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.50971
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.1301274
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.7209173
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.9355958
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.8721481
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.8497008
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.07186676
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.7644936
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.6798427
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.7217119
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.2186755
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  80
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7431051
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.9470512
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.7567327
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.6173016
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.3844302
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.4497982
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.1555843
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.08320658
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.2933513
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.548186
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.9924377
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5386111
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.3493065
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.1060463
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.006053391
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5393322
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.07349173
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.9683765
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.2162949
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.3174442
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  81
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.01018085
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3979367
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.9050045
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.1058529
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3463568
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.1230346
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.404631
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.8125019
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3336086
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.9155601
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.481542
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.2690564
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.9888114
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.9125485
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.6367689
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.1776406
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.1818295
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.01125759
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.6086123
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.908286
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  82
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5467393
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.8955692
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.8104002
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.812144
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.1355066
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.3775462
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.2659528
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.9392023
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.3192702
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.2347299
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7182169
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.349814
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.2827265
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.006168094
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.8093406
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.9578881
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.4433885
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.0624178
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.07191822
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7916849
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  83
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6809186
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.7282615
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.3712123
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.8955689
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.6706601
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.5229073
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.8402464
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.6968529
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.7846389
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.703505
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.003409894
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.5895782
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.6608716
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.4940197
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.2608752
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.2745129
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9477762
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9633863
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9554695
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.06441463
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  84
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5822515
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.603956
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.382632
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.8570651
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.2573158
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.0441058
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.9466927
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.09705531
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.7830989
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.1340064
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.4583037
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.313315
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.32528
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3791645
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.7828495
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.7232106
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.2943334
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.6325874
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.7342793
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.7713836
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  85
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.06584991
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.0129425
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.5070195
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.679379
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2011132
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.7760582
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2063958
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.02964238
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.3641994
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.908409
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.8467394
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.5233882
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.4093208
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.8324964
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.1720627
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.8021545
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.5228204
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.9257126
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.9729908
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2296711
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  86
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5983231
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.305109
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.7941792
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.3403114
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4973764
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.9351588
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.7884907
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.1322349
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.04056624
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4787818
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.985258
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.5712606
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.8578346
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.6759285
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.8761413
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.6300576
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.7057027
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.3241514
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.05534978
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.5947057
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  87
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6653881
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.8884215
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.03998201
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4491941
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.5856282
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.794889
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4494537
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3565858
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6067028
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3233
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3891511
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.8744108
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.9207155
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4265584
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.5967053
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.808284
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6054183
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.8052877
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.7081825
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.7266858
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  88
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3606948
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.8737155
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.01002796
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.2229778
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.199557
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.01348904
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.5822257
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.2474092
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.9976174
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.8678892
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.3413612
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.9421638
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.7345541
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.54395
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.7395402
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.9244673
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.5158218
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.8142425
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.7862517
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.7277725
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  89
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.08090954
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.545295
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.9121805
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.1724994
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.7399104
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.693348
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.07245148
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.3063693
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.6992268
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.01253932
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.1306344
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.1008701
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4469843
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.889757
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.3047714
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.2411833
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.7029492
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.1576442
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.6606468
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.1236757
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  90
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.4146767
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4262414
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.1914936
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.1599358
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.5343937
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.6748883
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.215466
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.1380635
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9139398
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4237402
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.8318121
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4248435
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.3034177
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.07392591
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.2977805
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.5846618
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.3525735
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.255164
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.736871
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.156908
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  91
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1754163
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.173996
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.4162537
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.3714289
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.382077
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.8144754
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.8381669
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.384308
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.02503432
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6817279
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.4716057
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.4328114
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.8893295
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.3300574
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.1086907
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.9893288
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.3839795
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.848556
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.7679706
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6491526
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  92
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.01991325
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.9700022
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.4413768
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.5571486
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.7020996
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8947436
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.9242203
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8719736
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8214096
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.3869832
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.772122
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.5293844
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.5738006
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.3131329
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.5087863
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.646223
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.4894013
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8591579
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.5240843
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.9975881
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  93
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1316689
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.6839337
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.6875519
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3584981
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3792575
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.5275938
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.9542756
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.5953487
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.7875702
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.1921607
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.2359813
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.2189644
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.4851381
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.9168128
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3253611
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.6816692
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.8257883
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.4868978
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.1285392
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.1150184
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  94
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1407556
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.06026528
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.8106164
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.3412583
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.09491086
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.8594185
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.9037013
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.2992606
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.6367879
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.8050188
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.8956636
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4937849
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4200168
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.9409239
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.3521597
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4368868
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4781777
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.1644573
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.710658
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.2187495
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  95
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7626172
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.04628666
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.4516799
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.5637988
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.0685067
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.06621774
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.8193813
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.7619701
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.9801527
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.5383486
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.1852252
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.8267365
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.8412803
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.1884462
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3125869
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.6750867
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.1852145
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.2688185
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.7430498
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.8107158
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  96
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.8763881
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.1188908
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.4063099
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.7359574
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.9923995
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.3058726
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.5040151
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.7420745
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.1643462
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.5171573
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.1382431
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.9442136
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.1387306
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.411041
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.587077
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.3340338
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.09623606
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.2278151
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.3035783
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.3997363
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  97
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3930123
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.8722276
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.6340261
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7922918
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.5754746
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.8754668
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.794428
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.4335887
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.8101027
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3917566
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7243867
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3031102
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.5135539
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.4198942
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.9160693
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3133893
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7199386
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3376586
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.01468591
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2428056
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  98
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6498723
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.7437202
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.9978104
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.9801383
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.3056153
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.1302299
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.1879235
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.03704588
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.6685097
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.4979673
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.04496449
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.8757992
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.126024
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.7154445
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.1691112
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.2326989
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.9223235
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.4838761
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.6494333
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.9543805
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  99
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5908645
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.7169093
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.6711761
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.1722761
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2632311
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.9360281
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.3139956
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2363591
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.7883993
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2189161
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.3726546
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2025375
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2522638
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.343545
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.4245121
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.287341
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.4677458
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.04464875
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.7301219
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.9551784
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  100
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1347652
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2670611
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.593212
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.9295112
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.1226096
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.3159205
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.437042
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.9884927
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2166165
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.7073314
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2426489
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.4955635
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.090604
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.4081903
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2712491
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.8858328
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.70873
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.6033809
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.9808518
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2289578
    ## New state:  succeptible

``` r
# remove this later, printing just for debugging
population
```

    ## # A tibble: 20 x 101
    ##    person_ids day_1 day_2 day_3 day_4 day_5 day_6 day_7 day_8 day_9 day_10
    ##         <int> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> 
    ##  1          1 infe… infe… infe… infe… infe… infe… infe… infe… infe… infec…
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
    ##  9     5 infected_symptomatic_pre_symptoms     1
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
