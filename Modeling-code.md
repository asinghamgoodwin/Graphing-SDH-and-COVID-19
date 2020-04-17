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
    ## Random number:  0.5056189
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.05626092
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7540533
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.7581559
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8300333
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.9911717
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.3263472
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.12771
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.6880302
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.1611388
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8712257
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.4687487
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.9601811
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.6434255
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.4892698
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8526457
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.6025187
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.2893562
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.8237077
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  2
    ## Previous state:  succeptible
    ## Random number:  0.4693671
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  3
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7047031
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.6577303
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1619865
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.9383991
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7405465
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1025113
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.8186909
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.7328332
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.3389361
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.8445859
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.9820436
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.4681185
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.4252454
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.2074167
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.5850585
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.9619557
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.9709811
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.3927648
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.1319707
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  3
    ## Previous state:  succeptible
    ## Random number:  0.6680126
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  4
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6331358
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.3967471
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.4141346
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.8666397
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.2867993
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.7789378
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.4950689
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.8458875
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.8070653
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.3008574
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.9233406
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.3083262
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.3511293
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.8774768
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.6956942
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.8091159
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.8006344
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.3844784
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.2768978
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  4
    ## Previous state:  succeptible
    ## Random number:  0.5366029
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  5
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.167163
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  2
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.6401809
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.02793452
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.4536767
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.0231692
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.8401001
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.1382983
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.7324927
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.653504
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.147324
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.05474567
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.4926365
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.4361412
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.005211411
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.4243651
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.9471468
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.8530026
    ## New state:  succeptible
    ## 
    ## Person ID:  18
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.1700546
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.9822927
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  5
    ## Previous state:  succeptible
    ## Random number:  0.1948849
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  6
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.08630626
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.3687446
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.2120783
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.1704473
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.1378496
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.2288066
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.08685924
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.9818525
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.3224566
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.4576201
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.486738
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.1595493
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.7175753
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.4374899
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.571129
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.2987163
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.1797011
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  18
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.07886023
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.3758156
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  6
    ## Previous state:  succeptible
    ## Random number:  0.390451
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  7
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.7460431
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.4024393
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.2510231
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.07178463
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.09204066
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5687976
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.4825659
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9411717
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.2512306
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.2735041
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9718129
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.92219
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.8497491
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.02442374
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.8786015
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9239947
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  7
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.1244482
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  18
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.9655085
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5285345
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  7
    ## Previous state:  succeptible
    ## Random number:  0.5163264
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  8
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.5118653
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7834502
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.9622724
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7523495
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8735665
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.222429
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.3443988
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8102983
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.4145703
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.8731616
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.4316568
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.9564571
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.3579235
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.164622
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.05827018
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.9274781
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  8
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.03158083
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  18
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.6024949
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.2927809
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  8
    ## Previous state:  succeptible
    ## Random number:  0.7179352
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  9
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.7746591
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.3915061
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.1181759
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7603037
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.9653333
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.3657951
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.3591385
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.7979735
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.8638607
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.6787583
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.02696128
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5485571
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.3983031
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.8864099
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.1503021
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.6777708
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  9
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.04993577
    ## New state:  infected_asymptomatic
    ## 
    ## Person ID:  18
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.5600467
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.4059896
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  9
    ## Previous state:  succeptible
    ## Random number:  0.08653129
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  10
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.136248
    ## New state:  symptomatic_need_hospital
    ## 
    ## Person ID:  2
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.550047
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3532968
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.744614
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.5146171
    ## New state:  succeptible
    ## 
    ## Person ID:  6
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.009080473
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.9508925
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.4093151
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.07289397
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.8294498
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3990899
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.9479277
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3619136
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.4357046
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.3429119
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.5091546
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  10
    ## Previous state:  infected_asymptomatic
    ## Random number:  0.3006245
    ## New state:  infectious_asymptomatic
    ## 
    ## Person ID:  18
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.1606815
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.2659916
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  10
    ## Previous state:  succeptible
    ## Random number:  0.9543309
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  11
    ## Previous state:  symptomatic_need_hospital
    ## Random number:  0.524849
    ## New state:  need_hospital_seek_care
    ## 
    ## Person ID:  2
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3044584
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.8576128
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.1156131
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3813731
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  6
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.3853291
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.147808
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2067582
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.04009936
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.4894787
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.7037848
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.5374451
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.1481545
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.6878117
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9306626
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.6316335
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  11
    ## Previous state:  infectious_asymptomatic
    ## Random number:  0.1870246
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.2563206
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.9018554
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  11
    ## Previous state:  succeptible
    ## Random number:  0.004954233
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  12
    ## Previous state:  need_hospital_seek_care
    ## Random number:  0.2084683
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.4939373
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7756239
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.04278241
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  12
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7076559
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  6
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.264828
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.977929
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.01058148
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.9001834
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7856042
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7298714
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.4069239
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.5578687
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.2609113
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.7234939
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.8466485
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  12
    ## Previous state:  recovered
    ## Random number:  0.2871611
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.1304142
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.3908349
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  12
    ## Previous state:  succeptible
    ## Random number:  0.6849737
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  13
    ## Previous state:  get_hospital_care
    ## Random number:  0.7343709
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.1028839
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.8348192
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9073415
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  13
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.01660771
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  6
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.5722472
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.340947
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9275125
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.4693512
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.1656014
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.849473
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.2941413
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.7784073
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9264926
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.3137485
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.924844
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  13
    ## Previous state:  recovered
    ## Random number:  0.4118904
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.9020883
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.03824833
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  13
    ## Previous state:  succeptible
    ## Random number:  0.7181439
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  14
    ## Previous state:  get_hospital_care
    ## Random number:  0.5601376
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.008346251
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.5860197
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.5200387
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  14
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6399303
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  6
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.2741375
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.2049173
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.5433172
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.9972093
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.4793906
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3079467
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.7511761
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6284933
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.8172366
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.3495662
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.6759604
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  14
    ## Previous state:  recovered
    ## Random number:  0.8373957
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.04059673
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.7699389
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  14
    ## Previous state:  succeptible
    ## Random number:  0.1117419
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  15
    ## Previous state:  get_hospital_care
    ## Random number:  0.1123564
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8381123
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6358265
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8566732
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  15
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.649667
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  6
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.09385887
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.04962349
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.47952
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8060946
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.7758832
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8365203
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6558972
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8124295
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.6363819
    ## New state:  succeptible
    ## 
    ## Person ID:  15
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.5692132
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.9483711
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  15
    ## Previous state:  recovered
    ## Random number:  0.6136462
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.8077974
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.440109
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  15
    ## Previous state:  succeptible
    ## Random number:  0.9936062
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  16
    ## Previous state:  get_hospital_care
    ## Random number:  0.3855696
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.7317924
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.7880595
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.6688528
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  16
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.6614184
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  6
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.07350922
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.8075903
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2143542
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.4277764
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2336228
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.655629
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.1412463
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.839473
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.3401924
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  15
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.9423228
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.5594678
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  16
    ## Previous state:  recovered
    ## Random number:  0.07429561
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.2105841
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.3951438
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  16
    ## Previous state:  succeptible
    ## Random number:  0.821457
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  17
    ## Previous state:  get_hospital_care
    ## Random number:  0.029975
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.9254564
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.6704916
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.2447506
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  17
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.194108
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  6
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.6570197
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.165191
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.02547096
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.9114212
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.2528351
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.3360072
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.3505593
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.8331614
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  17
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3881405
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  15
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.9110186
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.4198785
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  17
    ## Previous state:  recovered
    ## Random number:  0.08965319
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.1100035
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.3493854
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  17
    ## Previous state:  succeptible
    ## Random number:  0.1507009
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  18
    ## Previous state:  get_hospital_care
    ## Random number:  0.5035167
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.4666995
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7229431
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2686446
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  18
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.5803707
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  6
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7605374
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.734047
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.1518689
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.251152
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.3622509
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.4558108
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.4969122
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7363082
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  18
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.2871682
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  15
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7066902
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.6448039
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  18
    ## Previous state:  recovered
    ## Random number:  0.288795
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.2066455
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.7686944
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  18
    ## Previous state:  succeptible
    ## Random number:  0.8286911
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  19
    ## Previous state:  get_hospital_care
    ## Random number:  0.3058061
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.1023544
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.6506116
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.8697104
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  19
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.7631539
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  6
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.7245535
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.3795766
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.5846758
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.8660374
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.3249543
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.2654941
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.217597
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.3563963
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  19
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.03653198
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  15
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.9067037
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.1485321
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  19
    ## Previous state:  recovered
    ## Random number:  0.8351932
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.01467288
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.410542
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  19
    ## Previous state:  succeptible
    ## Random number:  0.2158093
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  20
    ## Previous state:  get_hospital_care
    ## Random number:  0.508858
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.712087
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.2412817
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5518926
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  20
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.6448185
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.8974036
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.2667184
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.06334127
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.5928516
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.3528916
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.8350486
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.4690811
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.7673754
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  20
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.5606706
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  15
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.9760309
    ## New state:  succeptible
    ## 
    ## Person ID:  16
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.8061547
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  20
    ## Previous state:  recovered
    ## Random number:  0.6192549
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.456928
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.4817038
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  20
    ## Previous state:  succeptible
    ## Random number:  0.1676345
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  21
    ## Previous state:  get_hospital_care
    ## Random number:  0.5962484
    ## New state:  get_hospital_care
    ## 
    ## Person ID:  2
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.2798428
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.6035542
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.921302
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  21
    ## Previous state:  recovered
    ## Random number:  0.9673191
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.9590833
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.4050726
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.5514945
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.5141402
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.5995833
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.1448763
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.1938359
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.02348745
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  21
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.2572808
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  15
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.721972
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  16
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.006847531
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  21
    ## Previous state:  recovered
    ## Random number:  0.291864
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.7339787
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.9297182
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  21
    ## Previous state:  succeptible
    ## Random number:  0.9894426
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  22
    ## Previous state:  get_hospital_care
    ## Random number:  0.3142088
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.04261391
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.150835
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.1908533
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  22
    ## Previous state:  recovered
    ## Random number:  0.8816486
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.7572266
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.1510428
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.349635
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4927834
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.1550162
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.7571739
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.6630301
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.6350883
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  22
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.4378481
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  15
    ## Day:  22
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7501755
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  16
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.4539953
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  22
    ## Previous state:  recovered
    ## Random number:  0.6153956
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.8331582
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.1455752
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  22
    ## Previous state:  succeptible
    ## Random number:  0.9138198
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  23
    ## Previous state:  dead
    ## Random number:  0.795958
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.8031189
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.147295
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4974883
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  23
    ## Previous state:  recovered
    ## Random number:  0.2898888
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.1767127
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.01323453
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.8016289
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.3615382
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.9159366
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.5677042
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.3655561
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.2937741
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  23
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.02978905
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  15
    ## Day:  23
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1068228
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  16
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.553116
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  23
    ## Previous state:  recovered
    ## Random number:  0.8744152
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.492054
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.4430868
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  23
    ## Previous state:  succeptible
    ## Random number:  0.06575111
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  24
    ## Previous state:  dead
    ## Random number:  0.6086055
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.9740842
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.3525569
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7877754
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  24
    ## Previous state:  recovered
    ## Random number:  0.6496414
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.4045877
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.4327465
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.1873277
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.4559975
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.3485596
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7508943
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.06761061
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.6855615
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  24
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.713341
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  15
    ## Day:  24
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.3565895
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  16
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.6588417
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  24
    ## Previous state:  recovered
    ## Random number:  0.1442678
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.5515027
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.8565204
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  24
    ## Previous state:  succeptible
    ## Random number:  0.7530932
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  25
    ## Previous state:  dead
    ## Random number:  0.9663396
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.1498284
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.7951527
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.5642122
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  25
    ## Previous state:  recovered
    ## Random number:  0.7954317
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.6180591
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.0989826
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.9063533
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.7380105
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.3555172
    ## New state:  succeptible
    ## 
    ## Person ID:  11
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.1075055
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.3940901
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.2303048
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  25
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.6481889
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  25
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.1506881
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  16
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.7875394
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  25
    ## Previous state:  recovered
    ## Random number:  0.282552
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.3065366
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.1530977
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  25
    ## Previous state:  succeptible
    ## Random number:  0.3528866
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  26
    ## Previous state:  dead
    ## Random number:  0.03375667
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.2187273
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.9856358
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.7862172
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  26
    ## Previous state:  recovered
    ## Random number:  0.1919181
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5603292
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.3410582
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.3636698
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.7247468
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.4479792
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.9513402
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.0312834
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5196427
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  26
    ## Previous state:  recovered
    ## Random number:  0.4902114
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  26
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.7386335
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  16
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.5652444
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  26
    ## Previous state:  recovered
    ## Random number:  0.7468737
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.1785984
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.550886
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  26
    ## Previous state:  succeptible
    ## Random number:  0.8742991
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  27
    ## Previous state:  dead
    ## Random number:  0.9963578
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.7578499
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.3348293
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.2769761
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  27
    ## Previous state:  recovered
    ## Random number:  0.1845844
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4530667
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.8405707
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.9274799
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.6364551
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  27
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.8249822
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4160101
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.5025762
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.4612173
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  27
    ## Previous state:  recovered
    ## Random number:  0.3948325
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  27
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.457326
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  16
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.06981608
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  27
    ## Previous state:  recovered
    ## Random number:  0.4132146
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.41745
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.829328
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  27
    ## Previous state:  succeptible
    ## Random number:  0.04061689
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  28
    ## Previous state:  dead
    ## Random number:  0.2641498
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.412447
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6769914
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6366513
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  28
    ## Previous state:  recovered
    ## Random number:  0.8029135
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.7932413
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.3596492
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.7918741
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6120188
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  28
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.7173671
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.3768924
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.3353334
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.7983055
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  28
    ## Previous state:  recovered
    ## Random number:  0.9200671
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  28
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.7324729
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  16
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6083941
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  28
    ## Previous state:  recovered
    ## Random number:  0.5868736
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.6063964
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.3504435
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  28
    ## Previous state:  succeptible
    ## Random number:  0.0834792
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  29
    ## Previous state:  dead
    ## Random number:  0.3852323
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.616761
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.554316
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.9195405
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  29
    ## Previous state:  recovered
    ## Random number:  0.5756827
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.240809
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.5329114
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.1836969
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.3614824
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  29
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.9188923
    ## New state:  infected_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7318724
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.3459085
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7131403
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  29
    ## Previous state:  recovered
    ## Random number:  0.2568475
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  29
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.5230177
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  16
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.1386202
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  29
    ## Previous state:  recovered
    ## Random number:  0.7634995
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.4342795
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.8703999
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  29
    ## Previous state:  succeptible
    ## Random number:  0.7890085
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  30
    ## Previous state:  dead
    ## Random number:  0.8985203
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.8349501
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.6093605
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.549018
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  30
    ## Previous state:  recovered
    ## Random number:  0.8921031
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.3947882
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.2012943
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.3864148
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.55813
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  30
    ## Previous state:  infected_symptomatic_pre_symptoms
    ## Random number:  0.6452399
    ## New state:  infectious_symptomatic_pre_symptoms
    ## 
    ## Person ID:  11
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.9467845
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.890294
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.64933
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  30
    ## Previous state:  recovered
    ## Random number:  0.3290393
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  30
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.4913664
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.223359
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  30
    ## Previous state:  recovered
    ## Random number:  0.548758
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.6134337
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.3746327
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  30
    ## Previous state:  succeptible
    ## Random number:  0.1946793
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  31
    ## Previous state:  dead
    ## Random number:  0.6914439
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.4743182
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.5611498
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9389831
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  31
    ## Previous state:  recovered
    ## Random number:  0.549712
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.7470733
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.4970964
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.8482834
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9862415
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  31
    ## Previous state:  infectious_symptomatic_pre_symptoms
    ## Random number:  0.8170373
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  11
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.6016332
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.2001083
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.2804254
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  31
    ## Previous state:  recovered
    ## Random number:  0.002840585
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  31
    ## Previous state:  recovered
    ## Random number:  0.4027684
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.8311641
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  31
    ## Previous state:  recovered
    ## Random number:  0.6352709
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.08262631
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.9656042
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  31
    ## Previous state:  succeptible
    ## Random number:  0.8828181
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  32
    ## Previous state:  dead
    ## Random number:  0.5505112
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.1463418
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.6063478
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.9646552
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.9550505
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.9836938
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.5683982
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.2548702
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.07361252
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  32
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.5947375
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  11
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.6340456
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.4529098
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.6166579
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.981205
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.5375073
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.7999743
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  32
    ## Previous state:  recovered
    ## Random number:  0.2756032
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.07150321
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.1355136
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  32
    ## Previous state:  succeptible
    ## Random number:  0.1638255
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  33
    ## Previous state:  dead
    ## Random number:  0.7615484
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.3752046
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.1809892
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.05975775
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.0872431
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.8759205
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.04125781
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5064005
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.2570177
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  33
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.05933374
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  11
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.8167835
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.2960363
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.337452
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.7040772
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.2420484
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5728531
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  33
    ## Previous state:  recovered
    ## Random number:  0.6761577
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5941276
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.278165
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  33
    ## Previous state:  succeptible
    ## Random number:  0.5871886
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  34
    ## Previous state:  dead
    ## Random number:  0.1541076
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.4344524
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3969128
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.5865989
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.2437818
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.461659
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.8343499
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.454219
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3287532
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  34
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.7905252
    ## New state:  symptomatic_dont_need_hospital
    ## 
    ## Person ID:  11
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.8449521
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.2824158
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.2527309
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.5010164
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.5416686
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.3980581
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  34
    ## Previous state:  recovered
    ## Random number:  0.5063971
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.615314
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.8905476
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  34
    ## Previous state:  succeptible
    ## Random number:  0.9623747
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  35
    ## Previous state:  dead
    ## Random number:  0.9580126
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.8215615
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.7755244
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.2005306
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.2465783
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.5424553
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.7081861
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.980717
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.5529634
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  35
    ## Previous state:  symptomatic_dont_need_hospital
    ## Random number:  0.6926093
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6473445
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.1961194
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.004599786
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.3178846
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.3537054
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.7218212
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  35
    ## Previous state:  recovered
    ## Random number:  0.3003561
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.1443064
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.3933941
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  35
    ## Previous state:  succeptible
    ## Random number:  0.6538173
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  36
    ## Previous state:  dead
    ## Random number:  0.3145855
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7465097
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.4852967
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7901039
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.733718
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.03005021
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5014158
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.9213398
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.5013389
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.9381462
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.21331
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.03791542
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.331929
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.4059614
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.9768322
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.7725866
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  36
    ## Previous state:  recovered
    ## Random number:  0.2992369
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.276521
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.06098438
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  36
    ## Previous state:  succeptible
    ## Random number:  0.3315777
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  37
    ## Previous state:  dead
    ## Random number:  0.9816488
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.7149675
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9564864
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.5966545
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.6250229
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.683093
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6761371
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.761206
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.03949708
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.479044
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9677144
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.5420123
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.9711281
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.4093325
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.8642849
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.518319
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  37
    ## Previous state:  recovered
    ## Random number:  0.3432758
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.4232367
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.4762259
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  37
    ## Previous state:  succeptible
    ## Random number:  0.6116593
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  38
    ## Previous state:  dead
    ## Random number:  0.8199919
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.04314232
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.7407365
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.09259286
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.2531491
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.9801854
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.1943433
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.4160014
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.8625469
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.8690999
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5566415
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.4601462
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.1665306
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.9990265
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.6048562
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.6796499
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  38
    ## Previous state:  recovered
    ## Random number:  0.6721284
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.3612163
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.7955729
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  38
    ## Previous state:  succeptible
    ## Random number:  0.5341651
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  39
    ## Previous state:  dead
    ## Random number:  0.6399358
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.8984533
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.3273913
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.963859
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.07731131
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.9444427
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.3180116
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.416571
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.3680933
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.4871015
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.1795091
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.3746158
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.4750434
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.4039582
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.6279375
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.5324855
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  39
    ## Previous state:  recovered
    ## Random number:  0.6931943
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.7520761
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.5067596
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  39
    ## Previous state:  succeptible
    ## Random number:  0.462348
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  40
    ## Previous state:  dead
    ## Random number:  0.2700037
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.5419966
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.4487878
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.803139
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.04122847
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.2806528
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.4471844
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.8958119
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.458744
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.9590447
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.7551411
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.684704
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.258546
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.008234621
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.9193391
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.9036685
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  40
    ## Previous state:  recovered
    ## Random number:  0.9778523
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.1701725
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.3164344
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  40
    ## Previous state:  succeptible
    ## Random number:  0.1279394
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  41
    ## Previous state:  dead
    ## Random number:  0.3107273
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.3146252
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7004876
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.4470686
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.3213154
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.6910972
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7578951
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.2040641
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.09723603
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.481516
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.1800332
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7635606
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.7624256
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.535332
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.9774425
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.2138701
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  41
    ## Previous state:  recovered
    ## Random number:  0.3447775
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.05222263
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.09977275
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  41
    ## Previous state:  succeptible
    ## Random number:  0.2156405
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  42
    ## Previous state:  dead
    ## Random number:  0.9939849
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.09568742
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.3459314
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.03267378
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.493572
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.6452949
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.7155348
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.5978841
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.9350228
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.3790342
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.1138869
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.1925662
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.2798524
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.8540205
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.2701632
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.8728151
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  42
    ## Previous state:  recovered
    ## Random number:  0.1997777
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.2308418
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.734306
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  42
    ## Previous state:  succeptible
    ## Random number:  0.06576611
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  43
    ## Previous state:  dead
    ## Random number:  0.9067315
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.9081225
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.5554779
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.6610949
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.09353805
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.7041869
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.4668188
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.4730452
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.5839482
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.1775587
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.5871784
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1522105
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.1179575
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.601326
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.6643892
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.6376733
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  43
    ## Previous state:  recovered
    ## Random number:  0.5532959
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.6714423
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.5551668
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  43
    ## Previous state:  succeptible
    ## Random number:  0.8556144
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  44
    ## Previous state:  dead
    ## Random number:  0.5250749
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.4452594
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.9930185
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.4159869
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.3323535
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.04578857
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.7752549
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.6501317
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.9477787
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.4386012
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.4004533
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.3940625
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.940477
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.330405
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.1186962
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8116436
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  44
    ## Previous state:  recovered
    ## Random number:  0.2870451
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.2072981
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.234302
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  44
    ## Previous state:  succeptible
    ## Random number:  0.8943306
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  45
    ## Previous state:  dead
    ## Random number:  0.3090207
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.2181926
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.5267422
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4226379
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.1251311
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.3535171
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1297202
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1939872
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.9347715
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.7733834
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.07300955
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1872243
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.4679008
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.2566848
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.3846819
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.122778
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  45
    ## Previous state:  recovered
    ## Random number:  0.4015875
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.6503032
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.1196883
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  45
    ## Previous state:  succeptible
    ## Random number:  0.8705886
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  46
    ## Previous state:  dead
    ## Random number:  0.8283726
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2024209
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.3669295
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.6096411
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.1729011
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.03421403
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.7155594
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.207268
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.8165234
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.09735443
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.5543543
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.308039
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.1552314
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.9464505
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.504631
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.345846
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  46
    ## Previous state:  recovered
    ## Random number:  0.5489255
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.7811169
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.2345586
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  46
    ## Previous state:  succeptible
    ## Random number:  0.7915076
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  47
    ## Previous state:  dead
    ## Random number:  0.4942999
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.347388
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.1353325
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.174362
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.08119607
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.05608457
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3476853
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.3760623
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.5382166
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.7521007
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.144152
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.8601651
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.4156179
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.9877879
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.7784383
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.7145918
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  47
    ## Previous state:  recovered
    ## Random number:  0.8537139
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.8997489
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.1095408
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  47
    ## Previous state:  succeptible
    ## Random number:  0.01421663
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  48
    ## Previous state:  dead
    ## Random number:  0.2029214
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1250557
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.02683319
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.03498545
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.2691868
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.788283
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.03687503
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.889395
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.767817
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.0003930042
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1003251
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.4554074
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.8630901
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.4906961
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.5129971
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.6461405
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  48
    ## Previous state:  recovered
    ## Random number:  0.0738736
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1456707
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.9455736
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  48
    ## Previous state:  succeptible
    ## Random number:  0.1832385
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  49
    ## Previous state:  dead
    ## Random number:  0.8969782
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.67519
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.928918
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.2877588
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.6438238
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.719629
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.2889718
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.3202672
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.6907312
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.6679639
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.7441127
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.5058835
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.8935625
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.1312042
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.3856039
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.9777946
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  49
    ## Previous state:  recovered
    ## Random number:  0.8025311
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.8580984
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.399049
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  49
    ## Previous state:  succeptible
    ## Random number:  0.7520365
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  50
    ## Previous state:  dead
    ## Random number:  0.915354
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.6771185
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.6134733
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.8991922
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.787517
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.396298
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.02222
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.2225561
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.1234328
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.2886068
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.7201036
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.9846675
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.5585201
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.7829953
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.6337737
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.9370023
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  50
    ## Previous state:  recovered
    ## Random number:  0.2127671
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.2069412
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.1214226
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  50
    ## Previous state:  succeptible
    ## Random number:  0.1576066
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  51
    ## Previous state:  dead
    ## Random number:  0.3857165
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6457073
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.1170813
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.7484995
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.003181948
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.3910924
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.373276
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.8051925
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.2592515
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.8236462
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.09450883
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.6999756
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.1099639
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.6792062
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.36412
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.1833752
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  51
    ## Previous state:  recovered
    ## Random number:  0.7263031
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.06115699
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.8940723
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  51
    ## Previous state:  succeptible
    ## Random number:  0.0258021
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  52
    ## Previous state:  dead
    ## Random number:  0.680946
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.9256074
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.8828392
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.06795877
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.3278099
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.4183893
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.1037227
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.7062681
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.7795309
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.6463681
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5556319
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.9519723
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5954591
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.7141057
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.1585971
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.3554957
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  52
    ## Previous state:  recovered
    ## Random number:  0.09957734
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.7653302
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.1940116
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  52
    ## Previous state:  succeptible
    ## Random number:  0.5192103
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  53
    ## Previous state:  dead
    ## Random number:  0.8676429
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.6726569
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.9657433
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.5253139
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.4208473
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.5942359
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.09183928
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.6193587
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2005928
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.8856343
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.4595957
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.8917109
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.8701067
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.5036245
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.8572257
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.2753527
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  53
    ## Previous state:  recovered
    ## Random number:  0.9921705
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.05684975
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.1634418
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  53
    ## Previous state:  succeptible
    ## Random number:  0.9255004
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  54
    ## Previous state:  dead
    ## Random number:  0.2320652
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.1979618
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5352654
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.02665158
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.404403
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.944458
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.5268335
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.004964484
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.2068589
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.8310615
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.968191
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.9742197
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.6977651
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.7979625
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.5076999
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.2537697
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  54
    ## Previous state:  recovered
    ## Random number:  0.3221246
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.1060974
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.9214149
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  54
    ## Previous state:  succeptible
    ## Random number:  0.08027015
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  55
    ## Previous state:  dead
    ## Random number:  0.2195657
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.2166525
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.7902719
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.6142064
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.05667152
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.4985043
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.883228
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.9066981
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.916998
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.1722227
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.6271045
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.5702274
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.3339878
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.6058563
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.3120143
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.136825
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  55
    ## Previous state:  recovered
    ## Random number:  0.5130896
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.3226132
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.4877142
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  55
    ## Previous state:  succeptible
    ## Random number:  0.3896837
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  56
    ## Previous state:  dead
    ## Random number:  0.8089427
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.9512167
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4726585
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.08056699
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.05545922
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.6171958
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.5687428
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4402251
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.753631
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.408627
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.4424521
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3374706
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.7416486
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.7579679
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.7188423
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.2369338
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  56
    ## Previous state:  recovered
    ## Random number:  0.5513471
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.3640998
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.1073268
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  56
    ## Previous state:  succeptible
    ## Random number:  0.668592
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  57
    ## Previous state:  dead
    ## Random number:  0.820681
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.07268315
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.358834
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.1880111
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.6310713
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.03113558
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.01049309
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.01285892
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.03530031
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.7679938
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.7385182
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.282833
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.7930218
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.5421996
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.2098312
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.04043877
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  57
    ## Previous state:  recovered
    ## Random number:  0.5907417
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.9343453
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.7480654
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  57
    ## Previous state:  succeptible
    ## Random number:  0.1936103
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  58
    ## Previous state:  dead
    ## Random number:  0.9729454
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.4026086
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.7064205
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.03496658
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.1184654
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.6955543
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.693507
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.8622683
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.8898919
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.2882787
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.7467503
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.762542
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.8840868
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.3649307
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.7208187
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.1246718
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  58
    ## Previous state:  recovered
    ## Random number:  0.6548372
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.6308889
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.1883743
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  58
    ## Previous state:  succeptible
    ## Random number:  0.3693809
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  59
    ## Previous state:  dead
    ## Random number:  0.1327271
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.3583955
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.9499763
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.2876331
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.7609018
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.6165001
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.7833671
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.8967288
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.001503514
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.008157182
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.01900896
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.7756504
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.8715761
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.5378827
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.002366986
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.2027785
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  59
    ## Previous state:  recovered
    ## Random number:  0.50542
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.4744955
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.4249183
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  59
    ## Previous state:  succeptible
    ## Random number:  0.7289745
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  60
    ## Previous state:  dead
    ## Random number:  0.7694309
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.8921063
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.6449552
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.7847235
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.6067218
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.5540333
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.7263935
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.8779004
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.2841344
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.3197663
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9605331
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.3275301
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9962079
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.7126006
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.03106265
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9510342
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  60
    ## Previous state:  recovered
    ## Random number:  0.4159694
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.9350394
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.04924101
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  60
    ## Previous state:  succeptible
    ## Random number:  0.5282883
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  61
    ## Previous state:  dead
    ## Random number:  0.08602305
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.08354016
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.6715067
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.4234975
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.3788919
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.5419132
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.3820448
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.1201612
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.8204906
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.2103357
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.4044646
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.8334022
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.7097586
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.4153699
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.3478983
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.4071845
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  61
    ## Previous state:  recovered
    ## Random number:  0.731762
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.2567159
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.4010411
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  61
    ## Previous state:  succeptible
    ## Random number:  0.9232457
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  62
    ## Previous state:  dead
    ## Random number:  0.8744004
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6239889
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.9387288
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.2893942
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.7631909
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.4395854
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.7531276
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.3663598
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.1201548
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.7538622
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.5461537
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.01989742
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6547894
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.2228947
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.5894094
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.3002753
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  62
    ## Previous state:  recovered
    ## Random number:  0.4738948
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.412503
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.7938027
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  62
    ## Previous state:  succeptible
    ## Random number:  0.6621012
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  63
    ## Previous state:  dead
    ## Random number:  0.9982942
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.9820962
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.7310528
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.05229638
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.1549959
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.07723646
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.7564544
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.358848
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.4437477
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.01020986
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.6202971
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.1636661
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.3199009
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.8292594
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.9810525
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.2380546
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  63
    ## Previous state:  recovered
    ## Random number:  0.09691273
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.5604995
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.4130269
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  63
    ## Previous state:  succeptible
    ## Random number:  0.8377763
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  64
    ## Previous state:  dead
    ## Random number:  0.3512037
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.547313
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.0155916
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.3261911
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.9514472
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2056308
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5882187
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.2280632
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.4448545
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.4275763
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.5917865
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.6109905
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.6426016
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.2865293
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.4751946
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.8551596
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  64
    ## Previous state:  recovered
    ## Random number:  0.6398664
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.9034262
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.3625994
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  64
    ## Previous state:  succeptible
    ## Random number:  0.7337939
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  65
    ## Previous state:  dead
    ## Random number:  0.4804477
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6272702
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.5460501
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.230058
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.8074407
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.9713849
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.05612864
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6796606
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.848298
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.7590576
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.8778217
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.9721716
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.4017466
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.1638682
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.7906374
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.9472829
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  65
    ## Previous state:  recovered
    ## Random number:  0.9381483
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.812566
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.2874369
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  65
    ## Previous state:  succeptible
    ## Random number:  0.6388412
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  66
    ## Previous state:  dead
    ## Random number:  0.8921989
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.312729
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.1219991
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.2819849
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.8803548
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.4207892
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.6861818
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.4924581
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.5167294
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.971029
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.8192133
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.1134362
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.5628873
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.7869403
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.174051
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.03582318
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  66
    ## Previous state:  recovered
    ## Random number:  0.5373335
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.1894779
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.9791958
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  66
    ## Previous state:  succeptible
    ## Random number:  0.3483419
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  67
    ## Previous state:  dead
    ## Random number:  0.9715152
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.7310962
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.7596188
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2739691
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.3825696
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.7055135
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.6545289
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.06404346
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.2221057
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.8279355
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.1167105
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.9094565
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.8613571
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.410736
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.04731298
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.06739165
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  67
    ## Previous state:  recovered
    ## Random number:  0.002043368
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.4917549
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.9351338
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  67
    ## Previous state:  succeptible
    ## Random number:  0.7712845
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  68
    ## Previous state:  dead
    ## Random number:  0.7292945
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.04637163
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.3066366
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.1682611
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.4939474
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.8664705
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.07956113
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.9312415
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.4693259
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.6825787
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.00365404
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.94602
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.8622621
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.7324222
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.8395482
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.2812005
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  68
    ## Previous state:  recovered
    ## Random number:  0.2625177
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.6392067
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.6847784
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  68
    ## Previous state:  succeptible
    ## Random number:  0.1295705
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  69
    ## Previous state:  dead
    ## Random number:  0.0003041585
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.4804444
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.4164904
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.1062295
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.8534478
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.09053395
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.7742495
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.9545599
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.1666889
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.9569755
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.02693023
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.6861889
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.5129436
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.1287026
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.3846083
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.9236069
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  69
    ## Previous state:  recovered
    ## Random number:  0.002019744
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.8993737
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.009224874
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  69
    ## Previous state:  succeptible
    ## Random number:  0.8021789
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  70
    ## Previous state:  dead
    ## Random number:  0.146412
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.9870092
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.9336491
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.5637805
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.8677179
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.6974253
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.1819551
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.483295
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.8905735
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.7411095
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.7811765
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.8782243
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.8927622
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.9294947
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.2739544
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.3854908
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  70
    ## Previous state:  recovered
    ## Random number:  0.866321
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.526835
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.02554816
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  70
    ## Previous state:  succeptible
    ## Random number:  0.0253703
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  71
    ## Previous state:  dead
    ## Random number:  0.3247416
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.2032279
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.562789
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.5057389
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.04839995
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.9533225
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.01891192
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.1613359
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.1604116
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.05659698
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.6294063
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.018972
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.1435511
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.6445895
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.1329998
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.09153608
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  71
    ## Previous state:  recovered
    ## Random number:  0.7091683
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.2329482
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.4264351
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  71
    ## Previous state:  succeptible
    ## Random number:  0.5978119
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  72
    ## Previous state:  dead
    ## Random number:  0.706983
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.8052088
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.3998044
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.4753414
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.7891331
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.3100309
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.04848697
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7194594
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.06057797
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.1949947
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.1620727
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.4040483
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.3031214
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.9746922
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.9020543
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.8286252
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  72
    ## Previous state:  recovered
    ## Random number:  0.378377
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7718203
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.5014012
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  72
    ## Previous state:  succeptible
    ## Random number:  0.7331671
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  73
    ## Previous state:  dead
    ## Random number:  0.7342104
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.5503557
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.5398538
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.5865792
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.8254471
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.2434787
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.8092687
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.1739862
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.262805
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.5837517
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.5574103
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.6870837
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.7275964
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.7958256
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.3946
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.2182282
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  73
    ## Previous state:  recovered
    ## Random number:  0.1930407
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.154383
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.5554708
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  73
    ## Previous state:  succeptible
    ## Random number:  0.3438317
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  74
    ## Previous state:  dead
    ## Random number:  0.6315387
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.4647583
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.8602889
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.7048521
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.4446315
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.5197564
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.5238709
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.85076
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.7438113
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.05320516
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.01159155
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.3339963
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.5218088
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.5250987
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.382128
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.4222467
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  74
    ## Previous state:  recovered
    ## Random number:  0.1068185
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.9446433
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.7336637
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  74
    ## Previous state:  succeptible
    ## Random number:  0.7407151
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  75
    ## Previous state:  dead
    ## Random number:  0.4638574
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8521292
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2988498
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.777356
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.2177968
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8091448
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.3509494
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2536959
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.9000865
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.8276918
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.8662945
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.3391392
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.9984802
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.8154171
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.7666141
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.3515472
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  75
    ## Previous state:  recovered
    ## Random number:  0.9477522
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.1143277
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.815916
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  75
    ## Previous state:  succeptible
    ## Random number:  0.2686519
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  76
    ## Previous state:  dead
    ## Random number:  0.9249901
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.769481
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7870442
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.02493319
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.2101134
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.2119256
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.5872606
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.08840533
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.7027998
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.5748791
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.2789902
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.8662129
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.6701861
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.4013201
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.4127721
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.3278109
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  76
    ## Previous state:  recovered
    ## Random number:  0.7706126
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.181256
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.5696777
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  76
    ## Previous state:  succeptible
    ## Random number:  0.6124641
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  77
    ## Previous state:  dead
    ## Random number:  0.5540568
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.57868
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.6749354
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.8220408
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.06069387
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.6362139
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.7485156
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.3637287
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.6606561
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.09339511
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.7418527
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.1530065
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.4981354
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.2416795
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.6583277
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.9812017
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  77
    ## Previous state:  recovered
    ## Random number:  0.5490986
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.5548223
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.7296633
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  77
    ## Previous state:  succeptible
    ## Random number:  0.9931895
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  78
    ## Previous state:  dead
    ## Random number:  0.0143074
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.06460326
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.7306017
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.2117113
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.6691569
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.8947532
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.4819856
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.09068941
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.9820769
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.2287686
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.4738943
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.02391114
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.5070711
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.3822202
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.1017935
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.7622479
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  78
    ## Previous state:  recovered
    ## Random number:  0.2404161
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.5968799
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.532439
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  78
    ## Previous state:  succeptible
    ## Random number:  0.01821629
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  79
    ## Previous state:  dead
    ## Random number:  0.6809909
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.0594526
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.3431995
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.09515439
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.3816982
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.01093763
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.5370408
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.7483047
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.7203107
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.427555
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.5655708
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.6293235
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.2695685
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.4651524
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.3411267
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.06466726
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  79
    ## Previous state:  recovered
    ## Random number:  0.6707173
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.7232729
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.281954
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  79
    ## Previous state:  succeptible
    ## Random number:  0.352267
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  80
    ## Previous state:  dead
    ## Random number:  0.7352869
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.3901995
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.2603538
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.2878593
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.7060339
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.8713355
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.8302532
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.6077077
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5045847
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.09344579
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.4487345
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.784675
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.9123341
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.4925667
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.6579801
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.5597216
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  80
    ## Previous state:  recovered
    ## Random number:  0.7160748
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.1800797
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.4671335
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  80
    ## Previous state:  succeptible
    ## Random number:  0.7515773
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  81
    ## Previous state:  dead
    ## Random number:  0.2480126
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.6365553
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3110602
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.4688677
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.1516425
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.7802113
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.2359645
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.4299668
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.834358
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.5214274
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3706201
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.1099824
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.4259276
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.7563425
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.8451776
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.04747441
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  81
    ## Previous state:  recovered
    ## Random number:  0.5601398
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.2483296
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3438945
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  81
    ## Previous state:  succeptible
    ## Random number:  0.3857798
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  82
    ## Previous state:  dead
    ## Random number:  0.3705086
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.9124957
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.6131212
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.3807834
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.3295764
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.8313344
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.9784503
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.2252031
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.1330598
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.6698678
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.3026556
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7914834
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.2333319
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.1025105
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.8323718
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.4311442
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  82
    ## Previous state:  recovered
    ## Random number:  0.220656
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.2975401
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7991732
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  82
    ## Previous state:  succeptible
    ## Random number:  0.7457793
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  83
    ## Previous state:  dead
    ## Random number:  0.751332
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.3040646
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9305006
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.6592368
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.710995
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.458165
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.1044885
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9526098
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.5783491
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.2768196
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.9381521
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.3108797
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.8402652
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.3254134
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.9329714
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.8237627
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  83
    ## Previous state:  recovered
    ## Random number:  0.1533812
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.201567
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.6292361
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  83
    ## Previous state:  succeptible
    ## Random number:  0.4087124
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  84
    ## Previous state:  dead
    ## Random number:  0.9642922
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.8538368
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.2912706
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.1089885
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.9884622
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.5187287
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3734192
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.990534
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.1130581
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.52116
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.424491
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.3916647
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.4271949
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.3372794
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.6430537
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.9703687
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  84
    ## Previous state:  recovered
    ## Random number:  0.3667529
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.4526198
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.8188983
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  84
    ## Previous state:  succeptible
    ## Random number:  0.7550467
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  85
    ## Previous state:  dead
    ## Random number:  0.8968758
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.3743114
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.7669175
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.5019312
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.6454769
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.226833
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.1043725
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.06594728
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2484276
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.6838585
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2784168
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.3637845
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.005921949
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.2934051
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.9641
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.01305069
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  85
    ## Previous state:  recovered
    ## Random number:  0.5754416
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.2490554
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.1713243
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  85
    ## Previous state:  succeptible
    ## Random number:  0.1037111
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  86
    ## Previous state:  dead
    ## Random number:  0.2514622
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.5714697
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4505939
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.2584366
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.4555587
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.6444715
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.1225845
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.7181517
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.2327713
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.3657538
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.8729743
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.3484504
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4319315
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.5484106
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.03839162
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.5238281
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  86
    ## Previous state:  recovered
    ## Random number:  0.2782446
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4451589
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.4803126
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  86
    ## Previous state:  succeptible
    ## Random number:  0.6122188
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  87
    ## Previous state:  dead
    ## Random number:  0.03224506
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.9422611
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.01102823
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.5120402
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.3212534
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.1232625
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.07277586
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3108312
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.5371517
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.7429938
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3576084
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.3601162
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.9410166
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.6217533
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.2335452
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.8124723
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  87
    ## Previous state:  recovered
    ## Random number:  0.3661501
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.4609849
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.2788837
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  87
    ## Previous state:  succeptible
    ## Random number:  0.6627884
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  88
    ## Previous state:  dead
    ## Random number:  0.504075
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.4251725
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.2279882
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.03863575
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.2300007
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.4865981
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.002501797
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.8709234
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.4406198
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.3423417
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.5330506
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.2459702
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.9189613
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.1139153
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.8697807
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.0890635
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  88
    ## Previous state:  recovered
    ## Random number:  0.174714
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.2354006
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.929495
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  88
    ## Previous state:  succeptible
    ## Random number:  0.6179239
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  89
    ## Previous state:  dead
    ## Random number:  0.3989484
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.3938002
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.784475
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4113993
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.06182961
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.9707388
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.6647338
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.2003973
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.918431
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.375182
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.3387599
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.399297
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.9295156
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.8107164
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.6258402
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.9712967
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  89
    ## Previous state:  recovered
    ## Random number:  0.7031396
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.4820936
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.987304
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  89
    ## Previous state:  succeptible
    ## Random number:  0.1246941
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  90
    ## Previous state:  dead
    ## Random number:  0.3722016
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4289153
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9856244
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.494337
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.5882046
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.4962388
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.2556009
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.1243942
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9081021
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.6945056
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.3322414
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9298331
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.8024849
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.2459036
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.5120059
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.9740583
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  90
    ## Previous state:  recovered
    ## Random number:  0.2206925
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.7572654
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.7228619
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  90
    ## Previous state:  succeptible
    ## Random number:  0.5236649
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  91
    ## Previous state:  dead
    ## Random number:  0.7180205
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.3638047
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.2014289
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.4282066
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.9584909
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6165976
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.07582319
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.9823121
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.7821624
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.8114512
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.3894457
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.1630916
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6552418
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.5693224
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.382042
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.6186306
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  91
    ## Previous state:  recovered
    ## Random number:  0.8149179
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.2206598
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.22018
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  91
    ## Previous state:  succeptible
    ## Random number:  0.2683225
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  92
    ## Previous state:  dead
    ## Random number:  0.6027153
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.08662516
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.1181719
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.9190117
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.680752
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.9923604
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.2192988
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.7606092
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.03706926
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.1080869
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.4073681
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.1875721
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.04838138
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.1556813
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.2897205
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.6037008
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  92
    ## Previous state:  recovered
    ## Random number:  0.4543397
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.2505318
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.162398
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  92
    ## Previous state:  succeptible
    ## Random number:  0.4363696
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  93
    ## Previous state:  dead
    ## Random number:  0.1108822
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.2118743
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.8827384
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.5462706
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.3221317
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.4528438
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.07983613
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.6884099
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.6585746
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.4574785
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.7117469
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.4725696
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.4797198
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.001453
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.3681924
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.2609624
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  93
    ## Previous state:  recovered
    ## Random number:  0.1802467
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.5163501
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.2287189
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  93
    ## Previous state:  succeptible
    ## Random number:  0.7003083
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  94
    ## Previous state:  dead
    ## Random number:  0.3774994
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.7246508
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.007571885
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.7564542
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.07556973
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.8872294
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.09835848
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.5116225
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.2981718
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.3038866
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.1828604
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.08016174
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.7675097
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.1164484
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.2818135
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.3496314
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  94
    ## Previous state:  recovered
    ## Random number:  0.7307828
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.5356244
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.05424919
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  94
    ## Previous state:  succeptible
    ## Random number:  0.6439418
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  95
    ## Previous state:  dead
    ## Random number:  0.5287417
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.2523883
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.7233399
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.1369497
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.08632289
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.7113626
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3538739
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.6610284
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.4327921
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.5612809
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3831629
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.9794662
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.3753443
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.4400429
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.1638853
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.6350231
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  95
    ## Previous state:  recovered
    ## Random number:  0.6683179
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.4508474
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.07893839
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  95
    ## Previous state:  succeptible
    ## Random number:  0.468156
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  96
    ## Previous state:  dead
    ## Random number:  0.7023471
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.9279094
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.4771313
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.1114812
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.7992985
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.1611936
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.6775419
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.7325122
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.7403829
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.959472
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.4549082
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.3767503
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.995387
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.1219433
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.6959588
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.391515
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  96
    ## Previous state:  recovered
    ## Random number:  0.2927421
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.3722235
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.02576652
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  96
    ## Previous state:  succeptible
    ## Random number:  0.2720774
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  97
    ## Previous state:  dead
    ## Random number:  0.8447692
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.6731055
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.6346995
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.5739422
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.3843046
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.8779213
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7651902
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.3713498
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.9495113
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.4102849
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.1118595
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.7004532
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.4181585
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.4360656
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.3143773
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.9386726
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  97
    ## Previous state:  recovered
    ## Random number:  0.003939247
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.2570848
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.4385012
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  97
    ## Previous state:  succeptible
    ## Random number:  0.4062087
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  98
    ## Previous state:  dead
    ## Random number:  0.2041169
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.7922525
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.08936575
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.02081146
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.400495
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.8189291
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.4798226
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.1229983
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.8044214
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.3907608
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.05755923
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.2668895
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.1158386
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.1918248
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.4543717
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.6780122
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  98
    ## Previous state:  recovered
    ## Random number:  0.7065343
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.7175006
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.01814137
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  98
    ## Previous state:  succeptible
    ## Random number:  0.9200237
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  99
    ## Previous state:  dead
    ## Random number:  0.123507
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.7376726
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.2744252
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.1476973
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.6093669
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.6503868
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.5366684
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.9580392
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.3169815
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.06245776
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.6818508
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.1868865
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.1047715
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.1188855
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.1799747
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.5154391
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  99
    ## Previous state:  recovered
    ## Random number:  0.7499282
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.4062499
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.5151859
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  99
    ## Previous state:  succeptible
    ## Random number:  0.5550948
    ## New state:  succeptible
    ## 
    ## Person ID:  1
    ## Day:  100
    ## Previous state:  dead
    ## Random number:  0.6791193
    ## New state:  dead
    ## 
    ## Person ID:  2
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2051997
    ## New state:  succeptible
    ## 
    ## Person ID:  3
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.5323589
    ## New state:  succeptible
    ## 
    ## Person ID:  4
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.5662553
    ## New state:  succeptible
    ## 
    ## Person ID:  5
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.9562898
    ## New state:  recovered
    ## 
    ## Person ID:  6
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.1848842
    ## New state:  succeptible
    ## 
    ## Person ID:  7
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.5333036
    ## New state:  succeptible
    ## 
    ## Person ID:  8
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.05710871
    ## New state:  succeptible
    ## 
    ## Person ID:  9
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.9074955
    ## New state:  succeptible
    ## 
    ## Person ID:  10
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.06904192
    ## New state:  recovered
    ## 
    ## Person ID:  11
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.3379268
    ## New state:  succeptible
    ## 
    ## Person ID:  12
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.05073884
    ## New state:  succeptible
    ## 
    ## Person ID:  13
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.11894
    ## New state:  succeptible
    ## 
    ## Person ID:  14
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.1096933
    ## New state:  recovered
    ## 
    ## Person ID:  15
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.5835339
    ## New state:  recovered
    ## 
    ## Person ID:  16
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.7478171
    ## New state:  succeptible
    ## 
    ## Person ID:  17
    ## Day:  100
    ## Previous state:  recovered
    ## Random number:  0.2674906
    ## New state:  recovered
    ## 
    ## Person ID:  18
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.3780095
    ## New state:  succeptible
    ## 
    ## Person ID:  19
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.2042054
    ## New state:  succeptible
    ## 
    ## Person ID:  20
    ## Day:  100
    ## Previous state:  succeptible
    ## Random number:  0.4387532
    ## New state:  succeptible

``` r
# remove this later, printing just for debugging
population
```

    ## # A tibble: 20 x 101
    ##    person_ids day_1 day_2 day_3 day_4 day_5 day_6 day_7 day_8 day_9 day_10
    ##         <int> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> 
    ##  1          1 infe… infe… infe… infe… infe… symp… symp… symp… symp… sympt…
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
    ## 17         17 succ… succ… succ… succ… succ… infe… infe… infe… infe… infec…
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

    ## # A tibble: 331 x 3
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
    ## # … with 321 more rows

Graph the person-count of each state in a different color, with days on
the x-axis.

``` r
ggplot(population_to_visualize, 
       aes(x=day, y=count, color=state)) + 
  geom_line()
```

![](Modeling-code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
