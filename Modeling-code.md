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

time_until_infectious = 4 # This is figured from the incubation period, and rounded down from 4.6 so I can do steps in full days.

# For the asymptomatic types
asymptomatic_time_from_infectious_to_recovery = 4 # This is figured from the generation time (confusing). Should this be longer??

# For the symptomatic types
time_infectious_before_symptom_onset = 1
time_from_symptom_onset_to_recovery = 4

time_until_seeking_care = 5 # This is the number of days after symptom onset that people seek hospital care if they need it, on average
time_in_hospital_bed = 10
time_until_death_if_no_care = 10
```

These numbers come from NYC open
data.

``` r
initial_hospital_bed_capacity = 100 #TODO - fill in with real number later
hospital_beds_available = initial_hospital_bed_capacity

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
change_state = function(id, prev_state, day, population){ #TODO: Fix inputs elsewhere
  new_state = prev_state # The default is to stay at the same state. This always happens for RECOVERED or DEAD
  rand = runif(1) #Question: is it ok to use the same random number for every decision in this function?
  
  if (prev_state == SUCCEPTIBLE) {
    if (newly_infected[[str_c("day_", day)]][id] == 1) {
      if (rand <= probability_asymptomatic) {new_state = INFECTED_ASYMPTOMATIC}
      else {new_state = INFECTED_SYMPTOMATIC_PRE_SYMPTOMS}
    }
  }

  if (prev_state == INFECTED_ASYMPTOMATIC) {
    # wait 4 days before turning infectious
    earlier_state_to_check = population[[str_c("day_", day - time_until_infectious)]][id]
    if (!is.null(earlier_state_to_check) && earlier_state_to_check == INFECTED_ASYMPTOMATIC) {
      new_state = INFECTIOUS_ASYMPTOMATIC
      
      # infect others according to parameters
      # TODO - finish this section (infect how many people? on what days?)
      infect_someone(day + 1)
    }
  }

  if (prev_state == INFECTIOUS_ASYMPTOMATIC) {
    # wait 4 days before recovering
    earlier_state_to_check = population[[str_c("day_", day - asymptomatic_time_from_infectious_to_recovery)]][id]
    if (!is.null(earlier_state_to_check) && earlier_state_to_check == INFECTED_ASYMPTOMATIC) {
      new_state = RECOVERED
    }
  }

  if (prev_state == INFECTED_SYMPTOMATIC_PRE_SYMPTOMS) {
    # wait 4 days before turning infectious
    earlier_state_to_check = population[[str_c("day_", day - time_until_infectious)]][id]
    if (!is.null(earlier_state_to_check) && earlier_state_to_check == INFECTED_SYMPTOMATIC_PRE_SYMPTOMS) {
      new_state = INFECTIOUS_SYMPTOMATIC_PRE_SYMPTOMS
      
      # infect others according to parameters
      # TODO - finish this section (infect how many people? on what days?)
      infect_someone(day + 1)
    }
  }

  if (prev_state == INFECTIOUS_SYMPTOMATIC_PRE_SYMPTOMS) {
    # wait 1 day before turning symptomatic. severity of illness is based on demographics
    earlier_state_to_check = population[[str_c("day_", day - time_infectious_before_symptom_onset)]][id]
    if (!is.null(earlier_state_to_check) && earlier_state_to_check == INFECTIOUS_SYMPTOMATIC_PRE_SYMPTOMS) {
      if (rand < 0.2) { # TODO - instead of being a number, rely on a table (to be filled in later)
        new_state = SYMPTOMATIC_NEED_HOSPITAL
      } else { 
        new_state = SYMPTOMATIC_DONT_NEED_HOSPITAL
      }
    }
  }

  if (prev_state == SYMPTOMATIC_NEED_HOSPITAL) {
    # seek care with 5 day delay or don't, depending on demographics
    earlier_state_to_check = population[[str_c("day_", day - time_until_seeking_care)]][id]
    if (!is.null(earlier_state_to_check) && earlier_state_to_check == SYMPTOMATIC_NEED_HOSPITAL) {
      if (rand < 0.3) { # TODO - instead of being a number, rely on a table or function (to be filled in later)
        new_state = NEED_HOSPITAL_DONT_SEEK_CARE
      } else {
        new_state = NEED_HOSPITAL_SEEK_CARE
      }
    }
  }

  if (prev_state == SYMPTOMATIC_DONT_NEED_HOSPITAL) {
    # wait 4 days then recover
    earlier_state_to_check = population[[str_c("day_", day - time_from_symptom_onset_to_recovery)]][id]
    if (!is.null(earlier_state_to_check) && earlier_state_to_check == SYMPTOMATIC_DONT_NEED_HOSPITAL) {
      new_state = RECOVERED
    }
  }

  if (prev_state == NEED_HOSPITAL_SEEK_CARE) { ## IS THIS REDUNDANT/1 step delayed? PUT INTO SYMPTOMATIC_NEED_HOSPITAL?
    # immediately either get care or don't, depending on bed availability
    if (hospital_beds_available > 0) {
      hospital_beds_available = hospital_beds_available - 1
      new_state = GET_HOSPITAL_CARE
    } else {
      new_state = DONT_GET_NEEDED_CARE
    }
  }

  if (prev_state == NEED_HOSPITAL_DONT_SEEK_CARE) { ## IS THIS REDUNDANT/1 step delayed? PUT INTO SYMPTOMATIC_NEED_HOSPITAL?
    # immediately transition to don't get care
    new_state = DONT_GET_NEEDED_CARE
  }

  if (prev_state == GET_HOSPITAL_CARE) {
    # after 10 days in bed, probabalistically die or recover
    earlier_state_to_check = population[[str_c("day_", day - time_in_hospital_bed)]][id]
    if (!is.null(earlier_state_to_check) && earlier_state_to_check == GET_HOSPITAL_CARE) {
      if (rand < 0.5) { # TODO - instead of being a number, rely on a table or function (to be filled in later)
        new_state = DEAD
      } else {
        new_state = RECOVERED
      }
      hospital_beds_available = hospital_beds_available + 1
    }
  }

  if (prev_state == DONT_GET_NEEDED_CARE) {
    # die after 10 days
    earlier_state_to_check = population[[str_c("day_", day - time_until_death_if_no_care)]][id]
    if (!is.null(earlier_state_to_check) && earlier_state_to_check == DONT_GET_NEEDED_CARE) {
      new_state = DEAD
    }
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
# TODO: decide what to do about states as factors. Currently I just made everything strings because I couldn't keep things straight.
run_simulation = function(initial_population_size, total_days) {
  population = tibble(
    person_ids = 1:initial_population_size,
    day_1 = create_initial_population_with_one_infected(initial_population_size)
  )
  
  for (day in 2:total_days) {
    prev_day = population[[(str_c("day_", day - 1))]]
    population =
      add_column(
        population,
        !!str_c("day_", day) := flatten_chr(map2(population$person_ids, prev_day, change_state, day, population))
      )
  }
  
  population
}

population = run_simulation(initial_population_size, total_days)
```

    ## 
    ## 
    ## Person ID:  1
    ## Day:  2
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.4089264
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.2426733
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.3906138
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.6488351
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.03611637
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7870682
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.002460883
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7241532
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8944167
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.5976398
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7131404
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.2525878
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.4014143
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.9275316
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.9343152
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.05361324
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.1391818
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.1199414
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.1480188
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8021691
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  3
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3920902
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.8926534
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7480844
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.4041351
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2279744
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.6146601
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7722822
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2142588
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.4645919
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2132908
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1462128
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7759546
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.5824353
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.8178942
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.03174639
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1021241
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7333079
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7381041
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.8189246
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.5454205
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  4
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.8261401
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5980803
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.4337382
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.4812131
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7101066
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5363974
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.9790267
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5377319
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5563789
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7265112
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5367291
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.6053319
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.02669382
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.9764204
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.352232
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.9055727
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7817386
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.347899
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7067895
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.2030991
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  5
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2048982
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.155224
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.9977464
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.327697
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.8841722
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.4904717
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.1530213
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.9000201
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5834944
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.9490458
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5652649
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.2449288
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5015709
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.6534372
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.4574514
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.7875669
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.3947893
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.3029422
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.5458148
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.2864702
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  6
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.6542932
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.4273344
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.2145516
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.160815
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8035246
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5756797
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.3664406
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.01300773
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.6753176
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.003139607
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8479168
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.8681816
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  13
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5853371
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.1709364
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5726751
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.6650879
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.2962395
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.5330625
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.4375975
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.1879828
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  7
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.7765722
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.2928032
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.33552
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.0202993
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.1378517
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5346419
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.3619457
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9192499
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.6797136
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9385943
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.1829584
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  7
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.4009163
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  13
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5005577
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.8836924
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.01962104
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.2871239
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.8273454
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.7188151
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5100326
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.7793456
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  8
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.6397825
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.1615266
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.3207085
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.01455032
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6354194
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.2700039
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7177486
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8719685
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.4977863
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.07808585
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.4263643
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  8
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2586663
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  13
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8797642
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.08559361
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8715555
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.4076341
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.9345554
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.855942
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.9258616
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.728877
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  9
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.965488
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7545065
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.8070454
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5871663
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.4612233
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.9215354
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.6180303
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5034876
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.3331198
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.4048213
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5107171
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  9
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.9871938
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  13
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5254019
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.07873316
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5457806
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.3175994
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.8159083
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7003068
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.800888
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.2814601
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  10
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.5531016
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.7682774
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.7596539
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.7145466
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2396118
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1473384
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.8601747
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3953132
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.9206804
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.6961928
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1604443
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  10
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3791825
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  13
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2611304
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3797142
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3893987
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2293079
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.8836351
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3246783
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.9929517
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3515778
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  11
    ## Previous state:  recovered
    ## Random number:  0.7476271
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.1550974
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9154509
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5576285
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3458825
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5192832
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5954008
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5126391
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.4501994
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5522328
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2964236
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  11
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.05249978
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  13
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9644635
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.741492
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.00642725
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2050764
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.7961818
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.7490261
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3508674
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.1177384
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  12
    ## Previous state:  recovered
    ## Random number:  0.1851369
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2363728
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.5651501
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7043427
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.1507107
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.8206015
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.6937967
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.5484015
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2109072
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  12
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.05901332
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.4984773
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  12
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.7020213
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  13
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7226628
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.03134359
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.6497139
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.3523652
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2945672
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.9308219
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.4140282
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.9265891
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  13
    ## Previous state:  recovered
    ## Random number:  0.4895078
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.03095949
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4927207
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.8925247
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.1825463
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.08327289
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.8770625
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.7175056
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.8167394
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  13
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1860802
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.2275808
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  13
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.9295146
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  13
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.2972654
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9700733
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.317137
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.6597159
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.03361471
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9361192
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.3033273
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.2068479
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  14
    ## Previous state:  recovered
    ## Random number:  0.162477
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6045232
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3018237
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.1815029
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.8914282
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6496179
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.5688659
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.5184828
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.981309
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  14
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.08725677
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.7551549
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  14
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.8985906
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  13
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.5456875
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.253848
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.2368719
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.2727172
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6723068
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3469249
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.8575587
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.06490395
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  15
    ## Previous state:  recovered
    ## Random number:  0.6127013
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.9407883
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.4704657
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.5739712
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.02103449
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6956969
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.4129637
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.3526564
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.2259305
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  15
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1759237
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8251872
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  15
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.529509
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  13
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.2152517
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.1987172
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.7909396
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8911732
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.119802
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8637411
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.06588197
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.1974425
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  16
    ## Previous state:  recovered
    ## Random number:  0.0005496568
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8774558
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8029626
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.689062
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8766078
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.3778016
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.9582825
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.4299927
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  9
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.7223532
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  16
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.7325709
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  11
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.5787127
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  16
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.3347762
    ## New state:  need_hospital_seek_care
    ## 
    ## Person ID:  13
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.1921706
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.916894
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8969674
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.1375381
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8520962
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2584016
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.4198683
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.4592149
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  17
    ## Previous state:  recovered
    ## Random number:  0.4836637
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.07305893
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.008525124
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.7682821
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.865639
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.1897143
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.4198858
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  17
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.08818584
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  9
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.8752596
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  17
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.8780564
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  11
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.4924914
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  17
    ## Previous state:  need_hospital_seek_care
    ## Random number:  0.6608463
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.608291
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.7381803
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.7038869
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.8447508
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.7239336
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.3136003
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.8680061
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.6849628
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  18
    ## Previous state:  recovered
    ## Random number:  0.7454505
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7802951
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.8572806
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2472162
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.9610003
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.1660021
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.9192543
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  18
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6299842
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  9
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.5490237
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  18
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.1296538
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  11
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.8263453
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  18
    ## Previous state:  get_hospital_care
    ## Random number:  0.4993248
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.6591519
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.04222616
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.06409361
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.762571
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.4764142
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.5891194
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.4143221
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.9012365
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  19
    ## Previous state:  recovered
    ## Random number:  0.2238092
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.5527695
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.08339726
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.1918091
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.6627903
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.4054196
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.5904563
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  19
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.9705296
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  9
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9666283
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  19
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.9594971
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  11
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.7629875
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  19
    ## Previous state:  get_hospital_care
    ## Random number:  0.1142115
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.6412647
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.1671408
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.8069584
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.697462
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.8186167
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.149618
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.7173351
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.3006416
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  20
    ## Previous state:  recovered
    ## Random number:  0.1632542
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.0798661
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5802054
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.2533526
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.8233758
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.318645
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.7011416
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  20
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.01297656
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  9
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.8652812
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  20
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.1707172
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.7570717
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  20
    ## Previous state:  get_hospital_care
    ## Random number:  0.7067534
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.06795747
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.2268764
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.4595654
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5228327
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5333166
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.2813131
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3779923
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5038202
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  21
    ## Previous state:  recovered
    ## Random number:  0.231137
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7470634
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.2452326
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7750027
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.9030772
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.8079648
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.1971959
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  21
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.5600861
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  9
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.2556641
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  21
    ## Previous state:  recovered
    ## Random number:  0.8117156
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.3576963
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  21
    ## Previous state:  get_hospital_care
    ## Random number:  0.1483822
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.6594986
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.4353196
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.4976013
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.2132049
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.9438455
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.5054071
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.4128181
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.05998341
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  1
    ## Day:  22
    ## Previous state:  recovered
    ## Random number:  0.2652196
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.8442734
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4207695
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.09073666
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.8501158
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.6462318
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3398471
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  22
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.9718428
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  9
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.6358797
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  22
    ## Previous state:  recovered
    ## Random number:  0.07366914
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3364103
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  22
    ## Previous state:  get_hospital_care
    ## Random number:  0.9002477
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4103947
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.9703195
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.9904855
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4583157
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.007504817
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.3166881
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.6971703
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  22
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.02161831
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  1
    ## Day:  23
    ## Previous state:  recovered
    ## Random number:  0.716762
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.08561164
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.6895778
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.3185773
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.3550472
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.5043884
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.8953123
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  23
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.64489
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  9
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4384093
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  23
    ## Previous state:  recovered
    ## Random number:  0.7685213
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.9948286
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  23
    ## Previous state:  get_hospital_care
    ## Random number:  0.3551838
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.6787629
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.8870368
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.9570383
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.8623818
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.5696015
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.2688995
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.08747512
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  23
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.2133866
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  1
    ## Day:  24
    ## Previous state:  recovered
    ## Random number:  0.03045276
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7077912
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.009707964
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7900387
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.4100319
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.71078
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.5784825
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  24
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.2433572
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  9
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.6986121
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  24
    ## Previous state:  recovered
    ## Random number:  0.3495141
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.5364633
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  24
    ## Previous state:  get_hospital_care
    ## Random number:  0.7346398
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.4233639
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.509651
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.1259068
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.791572
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.06976276
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.1121955
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.921047
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  24
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.4710585
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  1
    ## Day:  25
    ## Previous state:  recovered
    ## Random number:  0.7922187
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.780453
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.1194598
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.5983998
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.7911573
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.6908471
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.7205345
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  25
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.6739478
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.601961
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  25
    ## Previous state:  recovered
    ## Random number:  0.8662646
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.5334403
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  25
    ## Previous state:  get_hospital_care
    ## Random number:  0.8708008
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.01564856
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.9805077
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.3651219
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.656407
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.975102
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.4486998
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.9701105
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  25
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.9493621
    ## New state:  infectious_asymptomatic
    ## 
    ## Person ID:  1
    ## Day:  26
    ## Previous state:  recovered
    ## Random number:  0.5073689
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2893919
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.00637969
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  4
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.4743703
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.7047164
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.1819437
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2343801
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  26
    ## Previous state:  recovered
    ## Random number:  0.3880744
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5295809
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  26
    ## Previous state:  recovered
    ## Random number:  0.656026
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.8644851
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  26
    ## Previous state:  get_hospital_care
    ## Random number:  0.8654036
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  13
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.8752769
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2032412
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.6131017
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.07990735
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2719048
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.31206
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.8376085
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  26
    ## Previous state:  infectious_asymptomatic
    ## Random number:  0.664138
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  27
    ## Previous state:  recovered
    ## Random number:  0.9956549
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.1165491
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  27
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.7123808
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  4
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.213376
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.3349905
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.8903462
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.3581874
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  27
    ## Previous state:  recovered
    ## Random number:  0.3188194
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.2282718
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  27
    ## Previous state:  recovered
    ## Random number:  0.4613508
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.696996
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  27
    ## Previous state:  get_hospital_care
    ## Random number:  0.3893845
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.3367199
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.969311
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.2315561
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4112686
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.2440508
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.7003553
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.2862519
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  27
    ## Previous state:  recovered
    ## Random number:  0.6927212
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  28
    ## Previous state:  recovered
    ## Random number:  0.87294
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.07929283
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  28
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.132009
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  4
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.3579585
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.334597
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.801307
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6459477
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  28
    ## Previous state:  recovered
    ## Random number:  0.6339246
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.2825337
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  28
    ## Previous state:  recovered
    ## Random number:  0.5498416
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.5188603
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  28
    ## Previous state:  dead
    ## Random number:  0.169505
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.4347824
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.1447354
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.8608447
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.7600717
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.2462953
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6635299
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6443607
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  28
    ## Previous state:  recovered
    ## Random number:  0.09398313
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  29
    ## Previous state:  recovered
    ## Random number:  0.7937565
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.255763
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  29
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.3276911
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  4
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.2397506
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.8019888
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.485069
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.01550945
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  29
    ## Previous state:  recovered
    ## Random number:  0.3039506
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.674957
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  29
    ## Previous state:  recovered
    ## Random number:  0.2492295
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.2031102
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  29
    ## Previous state:  dead
    ## Random number:  0.8961107
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9740092
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.6569969
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.03428559
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.5832168
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.5217548
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.5326424
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.2460784
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  29
    ## Previous state:  recovered
    ## Random number:  0.8426195
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  30
    ## Previous state:  recovered
    ## Random number:  0.8785963
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.256913
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  30
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.1721154
    ## New state:  infectious_asymptomatic
    ## 
    ## Person ID:  4
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.2073967
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.5425421
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.2062402
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.2285139
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  30
    ## Previous state:  recovered
    ## Random number:  0.2131464
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.5028735
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  30
    ## Previous state:  recovered
    ## Random number:  0.8581292
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.5716251
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  30
    ## Previous state:  dead
    ## Random number:  0.2094643
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.5079602
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.3073259
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.7424834
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.8836415
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.8734721
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.9499357
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.1293897
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  30
    ## Previous state:  recovered
    ## Random number:  0.8807634
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  31
    ## Previous state:  recovered
    ## Random number:  0.9492274
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.1744392
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  31
    ## Previous state:  infectious_asymptomatic
    ## Random number:  0.457729
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9830715
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.6455862
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9091952
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.8110263
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  8
    ## Day:  31
    ## Previous state:  recovered
    ## Random number:  0.7884564
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.5337738
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  31
    ## Previous state:  recovered
    ## Random number:  0.9115308
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.7544539
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  31
    ## Previous state:  dead
    ## Random number:  0.08315839
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.598692
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.5268327
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.09886387
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.06777978
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.6860059
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9433976
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.01225606
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  31
    ## Previous state:  recovered
    ## Random number:  0.554697
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.5926304
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.08431278
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.2509446
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.9082632
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.9893989
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.07168205
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  32
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5854115
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  8
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.8294132
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.01161081
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.3654831
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.8227432
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  32
    ## Previous state:  dead
    ## Random number:  0.5640456
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.7009555
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.2734433
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.7935343
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.6325995
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.5242305
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.5296088
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.1643616
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.4721652
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.7508636
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5548016
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.5145014
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.6649272
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.1653244
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.6925761
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  33
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.262956
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  8
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.1823037
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.4619906
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.5933513
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.916701
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  33
    ## Previous state:  dead
    ## Random number:  0.985779
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.7132553
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5766578
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.152556
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.9176516
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.6036889
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.6817953
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5614234
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.3073255
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.4944076
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.9649645
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.1749747
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.9172224
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.2568726
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.7486876
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  34
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.170075
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  8
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.5386427
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3956414
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.7272736
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.7982985
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  34
    ## Previous state:  dead
    ## Random number:  0.6591346
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.02039515
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3445213
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4336023
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.04834279
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.725693
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.1039665
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.9516229
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.5102871
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.3650985
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6613418
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.4449759
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6512808
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.3322726
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.9635818
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  35
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5121402
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  8
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.3116858
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.7417159
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.2700666
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.1740052
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  35
    ## Previous state:  dead
    ## Random number:  0.7385825
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6807195
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.9777508
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6251532
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.5242172
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.8941572
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.1143597
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.5252411
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.3406955
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.1176885
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.008244637
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.3534314
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.03179158
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.0810151
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.09904587
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  36
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.3061737
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  8
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.7664199
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7298405
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.6981874
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.1989805
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  36
    ## Previous state:  dead
    ## Random number:  0.817522
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.596494
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.8267634
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7360207
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.9184082
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.3562276
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5592665
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7927584
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.2852531
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.9876117
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.3667817
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.722262
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.5622748
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6661301
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.4051879
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  37
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.9654881
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  8
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.2194353
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.1090925
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.9796109
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.5904755
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  37
    ## Previous state:  dead
    ## Random number:  0.571044
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9999501
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.5166993
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9030005
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.4200403
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.622623
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.1307716
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.03451932
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.006889443
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.9425488
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.3302183
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.3641878
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.6685174
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5254316
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.6218286
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  38
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.9544759
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  8
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.7135276
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5474909
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.3411529
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9492062
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  38
    ## Previous state:  dead
    ## Random number:  0.8054309
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.02186177
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.119103
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5746691
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9306828
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9364508
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9591054
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5757945
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.06887369
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.008843961
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.3933706
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.725019
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.8086239
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.6489152
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.1743033
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  39
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.4081144
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  8
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.9870651
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.03902086
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.4780802
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.04560912
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  39
    ## Previous state:  dead
    ## Random number:  0.1443558
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.6010814
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.2719709
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.1576231
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.5394489
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.2291969
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.5289128
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.9107509
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.6027566
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.009117207
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.3494283
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.996849
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.7982539
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.2767681
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.5552167
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  40
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.2248099
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.4524638
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.08502552
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.4350159
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.5838357
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  40
    ## Previous state:  dead
    ## Random number:  0.0876671
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.8977013
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.4491493
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.8198587
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.262609
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.258131
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.6211805
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.8500918
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.5196464
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.668407
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.006367837
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.8885351
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.3025354
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.1520685
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.2369197
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.2647067
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.5401588
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.01334171
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.7621354
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.5466709
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  41
    ## Previous state:  dead
    ## Random number:  0.3415447
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.9296133
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.3968422
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.02347803
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.4830763
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.6454256
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.1579053
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.4455946
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.9322994
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.06329842
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.615351
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.3536619
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.1863619
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.932459
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.3775007
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.1399179
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.4030427
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.03084591
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.7074473
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.1637979
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  42
    ## Previous state:  dead
    ## Random number:  0.2414716
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.533059
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.08411121
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.6326091
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.3153168
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.6813297
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.7719333
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.5292536
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.344574
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.8161141
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8569378
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.4020637
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.7950314
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.169791
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.4040821
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.1643673
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.260289
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1333527
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.8720033
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.2852928
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  43
    ## Previous state:  dead
    ## Random number:  0.512154
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.3821533
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1635749
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.5459762
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.06479286
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8098044
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8860837
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.7316456
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.4578338
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.4111213
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.9109442
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.5606893
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.0629825
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.2360765
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.7516498
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.7282504
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.6439717
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.2034465
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.1770027
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8819069
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  44
    ## Previous state:  dead
    ## Random number:  0.9341297
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.6793663
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.3126442
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.943505
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.492137
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.6457705
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.6859757
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.5485611
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.7206086
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.6561203
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1324194
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.5611698
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4350365
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.9966376
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.9327613
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.2322766
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.7565022
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1027992
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.9171934
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1122922
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  45
    ## Previous state:  dead
    ## Random number:  0.3376372
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.8753052
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4296596
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.8689083
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.581617
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.9423968
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1823557
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.9799252
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.2619917
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.6477424
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.9468413
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.1674287
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.03445676
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2242753
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.7573606
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.5229142
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.6289886
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.1471263
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.5239402
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2867978
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  46
    ## Previous state:  dead
    ## Random number:  0.2315818
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.9871704
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.3526734
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.8963822
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.460501
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.564283
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.8680591
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5524444
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.4315889
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.9701354
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.2459891
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.1462357
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.9494933
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.05093337
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.1310418
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.3873071
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.4561506
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.850741
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.6380559
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3718635
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  47
    ## Previous state:  dead
    ## Random number:  0.4404086
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.9234587
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.202343
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.05034152
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.8842161
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.1226097
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.2406563
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.2053614
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.7154591
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.4534439
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.9490596
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.4457079
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1619484
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.01941878
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.993117
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.4040662
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.5390171
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.9543556
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.5594619
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.6878679
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  48
    ## Previous state:  dead
    ## Random number:  0.7988343
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.3357368
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.8094914
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.4499239
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.9410746
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.310993
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1322316
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.4185843
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.2472901
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.09891174
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.9955273
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.1588823
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.697369
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.6939923
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.9452271
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.5925407
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.5098855
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.104049
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.5288235
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.007225059
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  49
    ## Previous state:  dead
    ## Random number:  0.9687566
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.07043535
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.6992966
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.9800685
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.8329648
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.9665263
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.3986173
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.7227185
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.1404413
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.322349
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.5253237
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.7845184
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.7937766
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.3914162
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.04004376
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.185087
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.646964
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.8702771
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.8836772
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.4147559
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  50
    ## Previous state:  dead
    ## Random number:  0.2590151
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.2959032
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.2656404
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.4667265
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.2094054
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.8910805
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.4156112
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.1052757
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.9460929
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.9476558
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.02215654
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.9970257
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.1676742
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.08378423
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.9104747
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.8538743
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.9589413
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.2603492
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.4377823
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.4146261
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  51
    ## Previous state:  dead
    ## Random number:  0.9972973
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.2230795
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.1748252
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6252249
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.2826937
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6692243
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.9311587
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.2656003
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.1230829
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.9316499
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.7085384
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.7805378
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.8382364
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5614704
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.8410351
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.21344
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.398826
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5156476
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.8896122
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.3033137
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  52
    ## Previous state:  dead
    ## Random number:  0.3280144
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.2515776
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.07261375
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.8838161
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.2591243
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.6096866
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.8389728
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.6771195
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.1449418
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.739441
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.7127269
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.8510563
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.5505233
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.0004962126
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.8903376
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.3526814
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.931438
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.6776902
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.3911414
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.4108352
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  53
    ## Previous state:  dead
    ## Random number:  0.9099447
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.614145
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.4262084
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.1904246
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2141115
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.3998587
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.1826228
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.6450997
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.3146735
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.2027657
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.4443287
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.9837144
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.9382158
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.835106
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.1603031
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.1533408
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.7115209
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5836018
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.08844002
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.08033522
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  54
    ## Previous state:  dead
    ## Random number:  0.4931771
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.9280527
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5927252
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.9253523
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.8706451
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.08134064
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.06347094
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.4941344
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.792366
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.8087389
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.3387634
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.2907684
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5193765
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7439053
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.03431445
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.8800621
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.808829
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7535769
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.4869732
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.1710338
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  55
    ## Previous state:  dead
    ## Random number:  0.8339295
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.8639244
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.9156725
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.731316
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.8833592
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.731222
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.6195052
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5632636
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.06034389
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.02413829
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.359955
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.1248354
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.8607593
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9027331
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9245157
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.9308715
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.6885489
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9359083
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.8904851
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3974161
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  56
    ## Previous state:  dead
    ## Random number:  0.7151684
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4948316
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.02389112
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.6941067
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.7053986
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3586393
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4819358
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.1852777
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.4604168
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.4436381
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.1986755
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.9191397
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.08227133
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.000503045
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.01969463
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.5518575
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.155537
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.4255848
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.5508607
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.682469
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  57
    ## Previous state:  dead
    ## Random number:  0.6837945
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.08806614
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.3779454
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.5752769
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.02715004
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.1886885
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.7929647
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.3080101
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.11177
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.8169052
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.1384762
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.07526441
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.6517607
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.3303197
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.5158843
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.433984
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.9041243
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4999912
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.6235266
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4894047
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  58
    ## Previous state:  dead
    ## Random number:  0.05378369
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.5777396
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.7866312
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.3519535
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.373531
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.2848194
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4145154
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.2751677
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.2475483
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.8851283
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.1910231
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.7265017
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3879503
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3586579
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.7520915
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.2844079
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.8550567
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.4825402
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.5264337
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.6538205
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  59
    ## Previous state:  dead
    ## Random number:  0.9243603
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.9872217
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.6173026
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.7070228
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.6929006
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.5123377
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.532949
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.5254411
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.5737229
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.2096104
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.4172524
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.8079716
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.3943766
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.2350606
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9490529
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.9675004
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.6428999
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.1002838
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.7817933
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.5606213
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  60
    ## Previous state:  dead
    ## Random number:  0.8235581
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.5738329
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.8585713
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.873035
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.0479234
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.6249865
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.7018531
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.5358597
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.07018028
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.5379923
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.07244329
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.3606486
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.7524836
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.4221776
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.5773195
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.219243
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.4441226
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.1980905
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.5673644
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.1589513
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  61
    ## Previous state:  dead
    ## Random number:  0.9966847
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.8080187
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.6738709
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.0166972
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.5854409
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.1679551
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.8832194
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.1754708
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.1479974
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.3131525
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.434179
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.2609155
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6015718
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.00760935
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6853432
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.2902903
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.6350594
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.8106976
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.3790421
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.9688195
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  62
    ## Previous state:  dead
    ## Random number:  0.6136562
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.9813308
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.292221
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.5882271
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.3494461
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.2226595
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.2601033
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6205475
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.110547
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.1869181
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.2449845
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.310645
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6984943
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6107895
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6983653
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.7527639
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.8150651
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6087711
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.1929166
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.321217
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  63
    ## Previous state:  dead
    ## Random number:  0.7273954
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.647516
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.2580459
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.2312188
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.2044906
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.1924955
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9091904
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9333019
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.6869684
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.4096369
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.00536598
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.6176597
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.7269752
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.6323048
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.1959196
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.1453097
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.06869583
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.3438475
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.330624
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.9015432
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  64
    ## Previous state:  dead
    ## Random number:  0.1642386
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.843419
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.9297704
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2277975
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2519018
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.9895886
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5056378
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5800315
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.8608554
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.432409
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.936976
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.5078464
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.634379
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.2802771
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.7188599
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.03813508
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.8395745
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6551475
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.8142364
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6537238
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  65
    ## Previous state:  dead
    ## Random number:  0.7792335
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.9754476
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.01404138
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.3941709
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.5401367
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6889863
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.01441943
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.05729218
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.2807818
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.08430162
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.3808951
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.7562154
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.7293958
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.02716253
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.08938376
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.2974284
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.860939
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.6679706
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.4523463
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.3658577
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  66
    ## Previous state:  dead
    ## Random number:  0.4930835
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.7068547
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.6235854
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2374939
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.6655564
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.7918642
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.6925337
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.9794861
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.1308316
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.4214833
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.1068636
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.695066
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2698564
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.3736875
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.4011994
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.5885012
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.4591317
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.1946003
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.7076139
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.3434814
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  67
    ## Previous state:  dead
    ## Random number:  0.2363412
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.9825931
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.6574665
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.1008162
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2444117
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2777578
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.09671329
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.08985358
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.9611016
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.4201042
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.7283409
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.7835851
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.6501838
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.01286635
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.1567963
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.920059
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.4385323
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.2178678
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.1775794
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.664272
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  68
    ## Previous state:  dead
    ## Random number:  0.9311908
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.5983847
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.3570047
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.4165377
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.7162512
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.4022288
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9644097
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.5358412
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.1091488
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.05472969
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.906826
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.3252121
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.7855183
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.1549707
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.7770866
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.5529646
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.9074728
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.1566108
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.2845933
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3755425
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  69
    ## Previous state:  dead
    ## Random number:  0.869963
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.4869565
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.05368399
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.1497001
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.09338276
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3961239
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3408921
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.3719416
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.7493618
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.06004556
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.4337538
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.03917673
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.3271574
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7470638
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.3232833
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.4538952
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.7203584
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.5824914
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.7424531
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7010187
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  70
    ## Previous state:  dead
    ## Random number:  0.5268708
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.4443536
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.9165738
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.8412178
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.1291043
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7845908
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.1070624
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.710927
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.9070639
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.1428696
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.7582955
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.9536329
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6810786
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.2295724
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.5557656
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.6289777
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.6398128
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.2185329
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.8161043
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.0070357
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  71
    ## Previous state:  dead
    ## Random number:  0.9123042
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6424863
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6977091
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6484324
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.9338877
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.2311812
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.8088845
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.7751056
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.4637586
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.5956839
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.1094168
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.03589373
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.408989
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.02550938
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.944566
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.1173843
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.4686945
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.8098583
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.5872775
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.02426274
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  72
    ## Previous state:  dead
    ## Random number:  0.3505112
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.6211436
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.0178202
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.5197927
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.8902892
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.3950116
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.1692699
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.1570068
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.5602683
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.4081321
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.4768352
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.08009658
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.8318064
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.38857
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.6436005
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.8438404
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.8193502
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.980886
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.3974051
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.2228698
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  73
    ## Previous state:  dead
    ## Random number:  0.6369803
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.08247787
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.9105148
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.6967442
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.897924
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.3258681
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.4567793
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.7172451
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.6043208
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.6656992
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.6925626
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.3257715
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.6988926
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.2715736
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.09837635
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.8968491
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.4956558
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.7188558
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.9819167
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.05590365
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  74
    ## Previous state:  dead
    ## Random number:  0.8867321
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.4572333
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.8511312
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.7145482
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.7391227
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.1076434
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.4998562
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.3585947
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.3600859
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.278084
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.3259792
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.1409287
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.6141635
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.0856699
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.508182
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.2187649
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.5309431
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.628436
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.5394772
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2466916
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  75
    ## Previous state:  dead
    ## Random number:  0.6871806
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2415002
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.4325869
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.4288106
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8303165
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.4232904
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.9551506
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.4808985
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.1606545
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.7643159
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3543515
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.09046027
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.9753229
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.1086697
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.8206657
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.5349369
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.2956799
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7558918
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.1330765
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.005790817
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  76
    ## Previous state:  dead
    ## Random number:  0.1963094
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7101483
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.8154913
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.6321334
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3590503
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.4041563
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.00796515
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.9149626
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.4686343
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.9202518
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.3250577
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.3324169
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.7469989
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1129871
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.436303
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.5318985
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.2675672
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.2815083
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.1348176
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.674599
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  77
    ## Previous state:  dead
    ## Random number:  0.3426229
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.2755839
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.6837253
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.2416604
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.9141131
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1062802
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.08056216
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.2288049
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.7602129
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.8629253
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.7827223
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.5356833
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.5907982
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.7932469
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.09089064
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.3359589
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.6280993
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9681309
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.4530775
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.007111088
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  78
    ## Previous state:  dead
    ## Random number:  0.1505566
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.8856492
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.6227841
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9529453
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.4058444
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.3143545
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9890225
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.2436574
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.6900254
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.103396
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.5266721
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.7105908
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.9018093
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.828875
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.6253278
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.3185275
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.1982971
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.09014544
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.9910399
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.2431383
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  79
    ## Previous state:  dead
    ## Random number:  0.576273
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.08260066
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.8701883
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.2363092
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.5849683
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.880811
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.4996845
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.8296357
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.1570528
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.144585
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.1608952
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.4570342
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5663006
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.3127568
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.691139
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.5296613
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.5327299
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.1040665
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.7441413
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.9686
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  80
    ## Previous state:  dead
    ## Random number:  0.7003859
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5445599
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.888933
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.4473236
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.1676359
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.8002019
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.2695685
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.7440989
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.831561
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.8638104
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.09033823
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.06414296
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.1640205
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.496529
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.02267454
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.7595411
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.7684733
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.1384938
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.02467398
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.8444921
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  81
    ## Previous state:  dead
    ## Random number:  0.0631442
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.9618598
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.2823474
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.585483
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.4241643
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.7509444
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.4680881
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.7901374
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.938721
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.4432035
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.4328884
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.6754486
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.03035572
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.3189295
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.01009295
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.9645179
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.2042986
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7272708
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.03776105
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.6601231
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  82
    ## Previous state:  dead
    ## Random number:  0.06258332
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.4988539
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.8354567
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7361496
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.1492878
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.4456838
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.6719231
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.5553831
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.207304
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.4943463
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.1735763
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.4988381
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.5645813
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.7215506
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9752313
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.6134299
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.7443344
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.2033783
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.585388
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.8454975
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  83
    ## Previous state:  dead
    ## Random number:  0.5836604
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.5072538
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.7062719
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.3453995
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.7954724
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.5426896
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.0337222
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.6492061
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.9638778
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.05855333
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.09162608
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.4318166
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3274864
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3172713
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.1517978
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.5364931
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.2100942
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.4235929
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.4566761
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.6498443
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  84
    ## Previous state:  dead
    ## Random number:  0.4442742
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.1833286
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.03145017
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.5106215
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.6776669
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3713235
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3122931
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.8178015
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.6721496
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.3144233
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.4898502
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.3199293
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.3173905
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.6421562
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.8104862
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.6736409
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.9560394
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.5192613
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.5357631
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.6200332
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  85
    ## Previous state:  dead
    ## Random number:  0.9264289
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.4554406
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2058335
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.8663664
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.3811781
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.7326891
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.05865018
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.978462
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.3166129
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.08202964
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.9509452
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.1334299
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.8532444
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.9869291
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.08561477
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.5543347
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.9248812
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.3045146
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.06654428
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.7122233
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  86
    ## Previous state:  dead
    ## Random number:  0.4841149
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.6207653
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4883126
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.5673485
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.5130556
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.6187232
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4591743
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.9025969
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.6596192
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.8989988
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4679421
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.9908849
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3193814
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.9691769
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6962292
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.9556062
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.2922677
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4323317
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.2690784
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.5570208
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  87
    ## Previous state:  dead
    ## Random number:  0.2591233
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3920046
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.2915919
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.2269353
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.03972504
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3205103
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.09125253
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.836694
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.1683261
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.1062997
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.3721269
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.524386
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.02190226
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.1487346
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.4268702
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.1353383
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.7435041
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.07549533
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.6196068
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.733123
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  88
    ## Previous state:  dead
    ## Random number:  0.1858607
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.7548136
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.4180686
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.3463741
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.6063177
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.2051487
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.7954835
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.6531669
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.1801558
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.4680817
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.0681466
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.4431164
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4112973
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.04911816
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.3853635
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.1078134
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.4324009
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.04757774
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.7825252
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.786896
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  89
    ## Previous state:  dead
    ## Random number:  0.1368492
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.1253414
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.8572126
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4048433
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.2261218
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.7159399
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.5481117
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.6643946
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.7850049
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.5518234
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9888196
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.777803
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.6472854
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.6678
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.7631156
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.002445654
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.1660246
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.593616
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.3155149
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.5525998
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  90
    ## Previous state:  dead
    ## Random number:  0.8724007
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4463717
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.0583631
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.8382302
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.1784593
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4107836
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9306088
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.5497559
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.8777068
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.1567132
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.5288026
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.7644688
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.3542952
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.2093463
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.4853682
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.4137822
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.7633138
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.2231559
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.2494626
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.2282018
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  91
    ## Previous state:  dead
    ## Random number:  0.7564532
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.7042363
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.9748295
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.0496255
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.1965209
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.1206163
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.8554692
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6038502
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.7318973
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.06727754
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.501102
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.6462896
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.625023
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.7839142
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.4272156
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.2720874
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.6006041
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.6652935
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.5839861
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.2508183
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  92
    ## Previous state:  dead
    ## Random number:  0.9122727
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.2522696
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.9512812
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.9034778
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8029688
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.2129538
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.8051306
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.5817741
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.2527877
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.1398724
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3904138
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.7580312
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.2566825
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.06243454
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.4462633
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.8691637
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.3322567
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.08160093
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.8516317
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.8003757
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  93
    ## Previous state:  dead
    ## Random number:  0.759183
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.5292865
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.6490181
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.3962251
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.1787452
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.2190985
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.7608345
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.8053285
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.6763903
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.4390005
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.3574476
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.6358689
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.6663466
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4006
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.4255071
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.5691111
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.5809235
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.8140878
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.4846152
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.969329
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  94
    ## Previous state:  dead
    ## Random number:  0.01319522
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.03797797
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.227776
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.02938995
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.9931847
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.6229577
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.02470002
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.8419324
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.005919785
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.5274485
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.2029565
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.994588
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.5940969
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3990502
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.2590326
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.4913283
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.2745601
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.6118016
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.186176
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.5190907
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  95
    ## Previous state:  dead
    ## Random number:  0.8823803
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.9588278
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.131886
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.8408817
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.1971424
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.4495386
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.5854595
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.6340682
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.8426499
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.3994956
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.5915203
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.07020121
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.4733357
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.121121
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.7288687
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.5491156
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.9675471
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.6873069
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.04205946
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.1660872
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  96
    ## Previous state:  dead
    ## Random number:  0.4573313
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.01008911
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.3037612
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.6699214
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.0705559
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.2890676
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.8017372
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.2546334
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.7469329
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.7892247
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2972158
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.03291903
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7508637
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.9850083
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2070732
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.9379458
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.5108457
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.4060115
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.4018812
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3141766
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  97
    ## Previous state:  dead
    ## Random number:  0.3393669
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.385087
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3425975
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.8007806
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.6836759
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2429776
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2819382
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2993826
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.267272
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.6711694
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.8150517
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.2753532
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.5157146
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.9417305
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.6081183
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.9998123
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.6025047
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.6134824
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.0476286
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.9682715
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  98
    ## Previous state:  dead
    ## Random number:  0.132992
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.150134
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.3264873
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.100638
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.7990264
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.4494772
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.1583748
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.4858158
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.465632
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.5925825
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.6454872
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.4532411
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.9215187
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.1911897
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.9983923
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.3954297
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.0694032
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.4555972
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.9996689
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2313481
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  99
    ## Previous state:  dead
    ## Random number:  0.2354175
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.8147109
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.8311649
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.5269788
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.9233124
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.05433818
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.7531349
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.5856396
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.9134509
    ## New state:  recovered
    ## 
    ## Person ID:  1
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.2701074
    ## New state:  recovered
    ## 
    ## Person ID:  2
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.3785917
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.4660643
    ## New state:  recovered
    ## 
    ## Person ID:  4
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.3426395
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.5575457
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.01559914
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.4736636
    ## New state:  recovered
    ## 
    ## Person ID:  8
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.3097446
    ## New state:  recovered
    ## 
    ## Person ID:  9
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2913344
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.0947243
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.8912009
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  100
    ## Previous state:  dead
    ## Random number:  0.4230155
    ## New state:  dead
    ## 
    ## Person ID:  13
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.7419186
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.3927429
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2127808
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.3195873
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.9374594
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.4746554
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2705868
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.8441128
    ## New state:  recovered

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

    ## # A tibble: 332 x 3
    ## # Groups:   day [100]
    ##      day state                               count
    ##    <dbl> <chr>                               <int>
    ##  1     1 infected_symptomatic_pre_symptoms       1
    ##  2     1 succeptible                            19
    ##  3     2 infected_symptomatic_pre_symptoms       1
    ##  4     2 succeptible                            19
    ##  5     3 infected_symptomatic_pre_symptoms       1
    ##  6     3 succeptible                            19
    ##  7     4 infected_symptomatic_pre_symptoms       1
    ##  8     4 succeptible                            19
    ##  9     5 infectious_symptomatic_pre_symptoms     1
    ## 10     5 succeptible                            19
    ## # … with 322 more rows

Graph the person-count of each state in a different color, with days on
the x-axis.

``` r
ggplot(population_to_visualize, 
       aes(x=day, y=count, color=state)) + 
  geom_line()
```

![](Modeling-code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
