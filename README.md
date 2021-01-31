
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aoristic

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jerryratcliffe/aoristic.svg?branch=master)](https://travis-ci.com/jerryratcliffe/aoristic)
<!-- badges: end -->

The goal of aoristic is to make sense of temporally vague data. It can
sometimes be difficult to ascertain when some events (such as property
crime) occur because the victim is not present when the crime happens As
a result, police databases often record a *start* (or *from*) date and
time, and an *end* (or *to*) date and time. The *start* datetime usually
references when the victim last saw their stolen property, and the *to*
date-time records when they first discovered their property missing. The
period between the *start* datetime and *end* date-time is referred to
as the event’s *time span*.

The time span between these date-times can be minutes, hours, or
sometimes days: hence the term ‘Aoristic’, a word meaning “denoting
simple occurrence of an action without reference to its completeness,
duration, or repetition”. It has its origins in the Greek word
*aoristos* which means *undefined*. For events with a location describes
with either a latitude/longitude or X,Y coordinate pair, and a start and
end date-time, this package generates an aoristic data frame with
aoristic weighted probability values for each hour of the week, for each
row. Various descriptive and graphic outputs are available.

## What’s new in Version 1.1.0?

  - This version removes a convoluted process of outputting a formatted
    table to a jpeg with a simpler mechanism. This avoids the user
    downloading a third-party software package. The change occurs in the
    ‘aoristic.summary’ function.
  - Adds a simple plot output option with new function ‘aoristic.plot’

## Previous versions

### Version 1.0.0

Version 0.6 was originally released on CRAN in 2015 by Dr. George
Kikuchi then of Fresno State University and now at the Philadelphia
Police Department. Given his extensive responsibilities he has been
unable to maintain and update the program since the initial release.
With his permission, the package has been taken over in 2020 and updated
by Dr. Jerry Ratcliffe of Temple University.

Much of the original functionality has been discontinued and replaced by
this updated package. In particular, version 1.0.0 onwards dispenses
with rounding to the nearest hour for time spans, and uses a
minute-by-minute method. In earlier versions (as was common in aoristic
approaches until recently) time spans were rounded to the hour. So an
event that happened between 10.55am and 11.55am would have an aoristic
weight of 0.5 assigned to each hour, 1000-1059 and 1100-1159. This is
despite the majority of the event occurring in the 11am hour. That
rounding is removed in version 1.0.0 and aoristic weightings are
assigned by the minute.

The kml mapping function from v0.6 is replaced here with a simpler plot
function that maps the individual points for an user-selected hour. See
?aoristic.map

There is a new graph function that plots the overall aoristic
distribution for an entire week, as well as each individual day of the
week. see ?aoristic.graph

There is some rudimentary data checking in aoristic; however, most users
will find that their effort is getting the date-time variables into the
correct format. See the formatting example below for guidance.

## Installation

You can install the released version of aoristic from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("aoristic")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jerryratcliffe/aoristic")
```

## Data formatting example

The package has some limited error checking; however, the main challenge
that users will face is getting the data into the correct datetime
format. Most of the heavy lifting is done by the aoristic.df() function.
The user passes the name of a data frame and four parameters
representing columns that contain

  - **Xcoord** a vector of the event X coordinate or latitude (passed
    through for user)

  - **Ycoord** a vector of the event Y coordinate or longitude (passed
    through for user)

  - **DateTimeFrom** a vector for the ‘From’ datetime (POSIXct date-time
    object)

  - **DateTimeTo** a vector for the ‘To’ datetime (POSIXct date-time
    object)

The package ‘lubridate’ is recommended as a way to more easily get the
date time data into the correct format. As a demonstration, consider one
of the datasets available in the aoristic package.

``` r
library(aoristic)
data(NYburg)
head(NYburg)
#>     CMPLNT_FR_DT CMPLNT_FR_TM CMPLNT_TO_DT CMPLNT_TO_TM X_COORD_CD Y_COORD_CD
#> 30    2019-01-04    0.6180556   2019-01-04    0.6444444     982546     206109
#> 78    2019-01-01    0.7847222   2019-01-01    0.7909722     985962     202878
#> 127   2019-01-01    0.3125000   2019-01-01    0.3361111     999874     238251
#> 203   2019-01-02    0.5416667   2019-01-02    0.7708333    1001526     243602
#> 216   2019-01-04    0.7500000   2019-01-04    0.8333333     983355     211219
#> 233   2019-01-01    0.2083333   2019-01-01    0.3361111     999874     238251
```

The data consist of the crime *from* date (CMPLNT\_FR\_DT) and time
(CMPLNT\_FR\_TM), the crime *to* date and time (CMPLNT\_TO\_DT and
CMPLNT\_TO\_TM), and X and Y coordinates of the crime event. Data
preparation in this case will involve three steps (for START and END
date-times): 1. Convert the times from (Excel originated) fractions of
the day 2. Combine the dates and times into a new variable 3. Convert
the new variable into a date-time format

#### 1\. Convert times

The two time variables are in fractions of the day. We can replace the
existing variables by recasting them in a more readable format, and view
the result.

``` r
NYburg$CMPLNT_FR_TM <- format(as.POSIXct((NYburg$CMPLNT_FR_TM) * 86400, origin = "1970-01-01"), "%H:%M")
NYburg$CMPLNT_TO_TM <- format(as.POSIXct((NYburg$CMPLNT_TO_TM) * 86400, origin = "1970-01-01"), "%H:%M")
head(NYburg)
#>     CMPLNT_FR_DT CMPLNT_FR_TM CMPLNT_TO_DT CMPLNT_TO_TM X_COORD_CD Y_COORD_CD
#> 30    2019-01-04        09:50   2019-01-04        10:28     982546     206109
#> 78    2019-01-01        13:50   2019-01-01        13:59     985962     202878
#> 127   2019-01-01        02:30   2019-01-01        03:03     999874     238251
#> 203   2019-01-02        08:00   2019-01-02        13:30    1001526     243602
#> 216   2019-01-04        13:00   2019-01-04        15:00     983355     211219
#> 233   2019-01-01        00:00   2019-01-01        03:03     999874     238251
```

#### 2\. Combine dates and times

The aoristic functions expect the date and time variables to be in a
single column, with a space separating them. We can do that with this
code, which creates two new variables:

``` r
NYburg$STARTDateTime <- paste(NYburg$CMPLNT_FR_DT,NYburg$CMPLNT_FR_TM, sep=' ')
NYburg$ENDDateTime <- paste(NYburg$CMPLNT_TO_DT,NYburg$CMPLNT_TO_TM, sep=' ')
head(NYburg)
#>     CMPLNT_FR_DT CMPLNT_FR_TM CMPLNT_TO_DT CMPLNT_TO_TM X_COORD_CD Y_COORD_CD
#> 30    2019-01-04        09:50   2019-01-04        10:28     982546     206109
#> 78    2019-01-01        13:50   2019-01-01        13:59     985962     202878
#> 127   2019-01-01        02:30   2019-01-01        03:03     999874     238251
#> 203   2019-01-02        08:00   2019-01-02        13:30    1001526     243602
#> 216   2019-01-04        13:00   2019-01-04        15:00     983355     211219
#> 233   2019-01-01        00:00   2019-01-01        03:03     999874     238251
#>        STARTDateTime      ENDDateTime
#> 30  2019-01-04 09:50 2019-01-04 10:28
#> 78  2019-01-01 13:50 2019-01-01 13:59
#> 127 2019-01-01 02:30 2019-01-01 03:03
#> 203 2019-01-02 08:00 2019-01-02 13:30
#> 216 2019-01-04 13:00 2019-01-04 15:00
#> 233 2019-01-01 00:00 2019-01-01 03:03
```

#### 3\. Convert new variables into date-time objects

The past stage is to use the convenience of the lubridate package to
convert the string of dates and times into a date-time object:

``` r
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
NYburg$STARTDateTime <- ymd_hm(NYburg$STARTDateTime, tz = "")
NYburg$ENDDateTime <- ymd_hm(NYburg$ENDDateTime, tz = "")
#> Warning: 49 failed to parse.
```

You get a warning that 49 observations failed to parse, because they are
missing data (= *NA*). This sometimes happens when the police know
exactly when the crime took place, and they only record the start
date-time. We can see the final result of all this formatting:

``` r
head(NYburg)
#>     CMPLNT_FR_DT CMPLNT_FR_TM CMPLNT_TO_DT CMPLNT_TO_TM X_COORD_CD Y_COORD_CD
#> 30    2019-01-04        09:50   2019-01-04        10:28     982546     206109
#> 78    2019-01-01        13:50   2019-01-01        13:59     985962     202878
#> 127   2019-01-01        02:30   2019-01-01        03:03     999874     238251
#> 203   2019-01-02        08:00   2019-01-02        13:30    1001526     243602
#> 216   2019-01-04        13:00   2019-01-04        15:00     983355     211219
#> 233   2019-01-01        00:00   2019-01-01        03:03     999874     238251
#>           STARTDateTime         ENDDateTime
#> 30  2019-01-04 09:50:00 2019-01-04 10:28:00
#> 78  2019-01-01 13:50:00 2019-01-01 13:59:00
#> 127 2019-01-01 02:30:00 2019-01-01 03:03:00
#> 203 2019-01-02 08:00:00 2019-01-02 13:30:00
#> 216 2019-01-04 13:00:00 2019-01-04 15:00:00
#> 233 2019-01-01 00:00:00 2019-01-01 03:03:00
```

With the data formatted properly, we can start to use the aoristic
functions. For example, you should always check the data to familiarize
yourself with any missing data, or to see if any observations have
logical errors where the from date-time occurs before the to date-time.
The aoristic.df function can handle this, but it is always good to know
your data.

``` r
aor.chk.df <- aoristic.datacheck(NYburg, 'X_COORD_CD', 'Y_COORD_CD', 'STARTDateTime', 'ENDDateTime')
#> 
#> ---- Aoristic data check -------------------------------------------
#>      49 rows were missing END/TO datetime values.
#>      40 rows had END/TO datetimes before START/FROM datetimes.
#>      In the aoristic.datacheck data frame these rows are indicated
#>      with missing end datetimes = 1 and start/end logical errors = 2
#>      See the aoristic.datacheck column. Also see ?aoristic.datacheck
#>      Coordinates check:
#>      No missing or zero coordinates.
```
