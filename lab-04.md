Lab 04 - La Quinta is Spanish for next to Denny’s
================
Cailey Fay
10.13.25

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

Each row represents a Denny’s location. The variables are the address,
city, state, zip code, longitude, and lattitude. There are 1643 Denny’s
and 6 variables.

### Exercise 2

The La Quinta dataset is 909 by 6. The rows represent the motel, and the
variables are the address, city, state, zip, longitude, and latitude.

### Exercise 3

There are dennys outside of the US - Canada, Puerto Rico, UK. Same thing
with La Quinta - they are in Canada, Mexico, Turkey, etc.

### Exercise 4

I would look at longitude and latitude and do a filter so that we
isolate to just the rectangle encompassing all of the US minus Hawaii
and Alaska and then I’d probably manually add in the Alaska / Hawaii
because how many could there really be?

### Exercise 5

There are no Denny’s outside of the US in this data file

``` r
dennys %>%
  filter(!(state %in% states$abbreviation))
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: address <chr>, city <chr>, state <chr>, zip <chr>,
    ## #   longitude <dbl>, latitude <dbl>

### Exercise 6

``` r
dn <- dennys %>%
    mutate(country="United States")
```

### Exercise 7

There are 14 laquintas outside of the US. They belong to the following
countries: Mexico x 10, colombia, Canada x2, Honduras. Abbreviations are
AG, QR, CH, NL, ANT, ON, VE, PU, SL, FM, and BC.

``` r
internationallaquinta <- laquinta %>%
  filter(!(state %in% states$abbreviation))
```

### Exercise 8

``` r
newlaquinta <- laquinta %>%
  mutate(country = case_when(
  state %in% state.abb ~ "United States",
  state %in% c("ON", "BC") ~ "Canada",
  state == "ANT" ~ "Colombia",
  state %in% c("AG", "QR", "CH", "NL", "PU","SL", "FM") ~ "Mexico",
  state == "FM"~"Honduras"
  ))
  
lq <- newlaquinta %>%
  filter(country == "United States")
```

### Exercise 9

California and Texas have the most, at 403 and 200, respectively.

``` r
dn %>%
  count(state) %>%
  inner_join(states, by = c("state" = "abbreviation")) %>%
  arrange(desc(n))
```

    ## # A tibble: 51 × 4
    ##    state     n name            area
    ##    <chr> <int> <chr>          <dbl>
    ##  1 CA      403 California   163695.
    ##  2 TX      200 Texas        268596.
    ##  3 FL      140 Florida       65758.
    ##  4 AZ       83 Arizona      113990.
    ##  5 IL       56 Illinois      57914.
    ##  6 NY       56 New York      54555.
    ##  7 WA       49 Washington    71298.
    ##  8 OH       44 Ohio          44826.
    ##  9 MO       42 Missouri      69707.
    ## 10 PA       40 Pennsylvania  46054.
    ## # ℹ 41 more rows

### Exercise 10

DC has the most Denny’s, with 29 per 1000 square miles. This is a little
misleading, though, because of how tiny DC is. DC only has 2 Dennys, and
RI comes in second place with 5 Dennys. The third place for number of
denny’s per 1000 square miles is California, which has 403 Dennys.

``` r
dn_state <- dn %>%
  count(state) %>%
  inner_join(states, by = c("state" = "abbreviation")) %>%
  arrange(desc(n)) 

dn_state <- dn_state %>%
  mutate(dn_count = n / area)

dn_state <- dn_state %>%
  mutate(dn_per1kmiles = dn_count * 1000)

dn_state %>%
  arrange(desc(dn_per1kmiles))
```

    ## # A tibble: 51 × 6
    ##    state     n name                     area dn_count dn_per1kmiles
    ##    <chr> <int> <chr>                   <dbl>    <dbl>         <dbl>
    ##  1 DC        2 District of Columbia     68.3 0.0293          29.3  
    ##  2 RI        5 Rhode Island           1545.  0.00324          3.24 
    ##  3 CA      403 California           163695.  0.00246          2.46 
    ##  4 CT       12 Connecticut            5543.  0.00216          2.16 
    ##  5 FL      140 Florida               65758.  0.00213          2.13 
    ##  6 MD       26 Maryland              12406.  0.00210          2.10 
    ##  7 NJ       10 New Jersey             8723.  0.00115          1.15 
    ##  8 NY       56 New York              54555.  0.00103          1.03 
    ##  9 IN       37 Indiana               36420.  0.00102          1.02 
    ## 10 OH       44 Ohio                  44826.  0.000982         0.982
    ## # ℹ 41 more rows

Rhode island has the most La Quinta’s per 1000k miles, followed by
Florida and Connecticut.

``` r
lq_state <- lq %>%
  count(state) %>%
  inner_join(states, by = c("state" = "abbreviation")) %>%
  arrange(desc(n)) 

lq_state <- lq_state %>%
  mutate(lq_count = n / area)

lq_state <- lq_state %>%
  mutate(lq_per1kmiles = lq_count * 1000) 

lq_state %>%
  arrange(desc(lq_per1kmiles))
```

    ## # A tibble: 48 × 6
    ##    state     n name             area lq_count lq_per1kmiles
    ##    <chr> <int> <chr>           <dbl>    <dbl>         <dbl>
    ##  1 RI        2 Rhode Island    1545. 0.00129          1.29 
    ##  2 FL       74 Florida        65758. 0.00113          1.13 
    ##  3 CT        6 Connecticut     5543. 0.00108          1.08 
    ##  4 MD       13 Maryland       12406. 0.00105          1.05 
    ##  5 TX      237 Texas         268596. 0.000882         0.882
    ##  6 TN       30 Tennessee      42144. 0.000712         0.712
    ##  7 GA       41 Georgia        59425. 0.000690         0.690
    ##  8 NJ        5 New Jersey      8723. 0.000573         0.573
    ##  9 MA        6 Massachusetts  10554. 0.000568         0.568
    ## 10 LA       28 Louisiana      52378. 0.000535         0.535
    ## # ℹ 38 more rows

Mapping Denny’s and La Quintas in the US:

``` r
dn <- dn %>%
  mutate(establishment = "Denny's")
lq <- lq %>%
  mutate(establishment = "La Quinta")
dn_lq <- bind_rows(dn, lq)
# made the new combined variable successfully 
ggplot(dn_lq, mapping = aes(
  x = longitude,
  y = latitude,
  color = establishment
)) +
  geom_point(alpha = .85) +
  labs(title = "Denny's and La Quinta Locations Across the United States",
       x = "Longitude",
       y = "Latitude",
       color = "Establishment")
```

![](lab-04_files/figure-gfm/exercise10-1.png)<!-- -->

### Exercise 11

``` r
NConly <- dn_lq %>%
  filter(state == "NC")
ggplot(NConly, mapping = aes(
  x = longitude,
  y = latitude,
  color = establishment
)) +
  geom_point() +
    labs(title = "Denny's and La Quinta Locations In North Carolina",
       x = "Longitude",
       y = "Latitude",
       color = "Establishment")
```

![](lab-04_files/figure-gfm/exercise11-1.png)<!-- -->

### Exercise 12

The joke most certainly holds for Texas, but only sort of for NC.

``` r
TXonly <- dn_lq %>%
  filter(state == "TX")
ggplot(TXonly, mapping = aes(
  x = longitude,
  y = latitude,
  color = establishment
)) +
  geom_point(alpha = .75) +
    labs(title = "Denny's and La Quinta Locations In Texas",
       x = "Longitude",
       y = "Latitude",
       color = "Establishment")
```

![](lab-04_files/figure-gfm/exercise12-1.png)<!-- -->
