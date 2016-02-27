# Severe Weather Impact on The US Economy and Public Health
mail@darrenbishop.com  


```r
library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

opts_chunk$set(message=FALSE, fig.path="figures/", fig.width=10, fig.lp = "Figure: ", fig.align = "center")
```

## Synopsis

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities.  
Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

In this study we explore the public health and econimic impact of severe storm events in the United States.

To achieve this, we process and analyse the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.  
This database tracks characteristics of major storms and weather events in the United States in the years between 1950 and late 2011; in particular we select **FATALITIES**, **INJURIES**, **PROPDMG** (with **PROPDMGEXP**), **CROPDMG** (with **CROPDMGEXP**) as public health and economic impact indicators.

Results are presented to reveal the comparative impact of the different types of storm events.

## Data Processing

### Load the Storm Data

```r
load_storm_data <- function() {
    
    if (class(try(nrow(storm_data), silent = T)) == "try-error") {
            
        if (!file.exists("repdata-data-StormData.csv.bz2")) {
            
            download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "repdata-data-StormData.csv.bz2", mode = "wb")
        }
        
        print("Loading data-set from disk...")
        
        tbl_df(read.csv(bzfile("repdata-data-StormData.csv.bz2")))
    }
    else {
        storm_data
    }
}

storm_data = load_storm_data()
```

```
## [1] "Loading data-set from disk..."
```

### Clean the Storm Data

#### Event Type Normalization
To compare the public health and economic effects of the storm events, the data must first be cleaned.

Section 2.1.1, Table 1 of the [Storm Data documentation][SDDET] informs that there are 48 official storm event-types

The *Event Types Available* figure of NOAA's [Storm Events Database][SDETA] website page shows that prior to 1996, no more than 3 weather event-types, namely Tornados, Thunderstorm Winds and Hail, are available for analysis.  
Thus all data prior to 1996 is disregarded as not fit for comparison, with this analysis therefore focusing on 1996 to 2011, inclusive.  

To facilitate this, **BGN_DATE** is first converted from a character vector to a Date object, then used for the filtering.


```r
filtered_storm_data = storm_data %>%
    mutate(BGN_DATE = mdy_hms(BGN_DATE)) %>%
    filter(BGN_DATE > dmy("01/01/1996"))
```

Despite limiting the data to a more comparable subset, the storm events remaining are still recorded with varying accuracy, with respect to event-type specification.  


```r
no_raw_event_types = length(levels(filtered_storm_data$EVTYPE))
```
Indeed, the raw-data actually contains 985 recorded event-types. For the most part this discrepancy is due to input errors, mixed casing, pluralization e.g. *Whirlwind* vs. *WHIRLWIND*, *Wildfire* vs. *Wildfires* etc or unofficial adjectives and adverbs e.g. *URBAN*, *ABNORMAL*, *ABNORMALLY*, *UNSEASONABLEY*, prefixed on an otherwise ok event-type.

Thus a best effort attempt is made to reconcile these storm event-types.  

An iterative, heuristic approach was used to develop a process to normalise the raw-data event-types.
This normalisation process is itself iterative, where the process exits when no more changes are detected.  

Within this process, the event-types are:

1. Converted to UPPER case
1. All numbers, punctuation and spaces are removed, just leaving alphabetic characters
1. Processed by an ordered list of regular-expressions.  
They are applied heuristically, designed to correct input errors as much as possible:
    1. Match and remove trailing *s*'s to (naively) fix pluralization
    1. Match longest-prefixes that identify one of the 48 official event-types
    1. Match and remove supurfluous leading adjectives, that is, trimming a prefix

The development of this process was iterative, so far as after each execution of the process, the results were examined and modifications were made to the implementation i.e. the regular expressions, and run again, and so on...


```r
normalize_event_types <- function (event_types, n = 1) {
    
    if (n == 1) {
        print("Normalising event-types")
    }
    
    print(paste("  Iteration #", n))
    
    patterns = list(
        plural_match = "^(.*)[sS]$",
        prefix_match = "^(THU|TSTM|TORN|FLOOD|SLEET|BLIZZARD|TROPICALSTORM|WINTER(?:STORM|WEATHER)|HURRICANE|LIGHTNING|HIGHWIND|LIGHTSNOW).*$",
        trimmed_prefix_match = "^(?:URBAN|ABNORMAL(?:LY)?|UNSEASONABL[EY]|SUMMARY)(.+)",
        keep_match = "(.+)"
    )
    
    patterns_string = paste("(?:", paste(patterns, collapse = "|"), ")", sep = "")
    
    replacement_string = paste(lapply(seq_along(patterns), function(i) { sprintf("\\%d", i) }), collapse = "")
    
    normalized_event_types =
        gsub(patterns_string, replacement_string,
            gsub("[^[:alpha:]]", "",
                 toupper(event_types)))
    
    if (length(unique(sort(normalized_event_types))) < length(unique(sort(event_types)))) {
        normalize_event_types(normalized_event_types, n + 1)
    }
    else {
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "TORN"), "TORNADO")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "THU"), "THUNDERSTORM")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "TSTM"), "THUNDERSTORM")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "WILDFORESTFIRE"), "WILDFIRE")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "FLOODFLASHFLOOD"), "FLOOD")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "SMLSTREAMFLD"), "FLOOD")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "RIVERFLOOD"), "FLOOD")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "RIVERFLOODING"), "FLOOD")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "FLASHFLOODING"), "FLASHFLOOD")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "MARINETSTMWIND"), "MARINETHUNDERSTORMWIND")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "SMALLHAIL"), "HAIL")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "LANDSLIDE"), "DEBRISFLOW")
        normalized_event_types = replace(normalized_event_types, which(normalized_event_types == "LAKEEFFECTSNOW"), "LAKEEFFECTNOW")
        normalized_event_types
    }
}
```

#### Damage Costs Normalization
As outline in Section 2.7, Paragraph 3 of the [Storm Data documentation][SDDET], an alphabetic character is used to indicate the magnitude of damage costs; this is captured in **PROPDMGEXP** and **CROPDMGEXP** for **PROPDMG** and **CROPDMG**, respectively.  

Looking closer at the magnitude data there is a mix of values, alphabetic, numeric and some other erroneous values e.g. *+*, *-*, *?*.

A normalisation procedure was developed to apply the magnitude data as multipliers where:

* Numeric values are interpreted as powers of 10 i.e. **2** is interpreted as **1E2** or 100.
* **H**, **K**, **M**, **B** etc take the standarad currency meaning as **1E2** (hundred), **1E3** (thousand), **1E6** (million), **1E9** (billion) etc, respectively.
* Missing values and other erroneous values are interpreted as no multiplier is needed, that is, multiply by **1**.


```r
normalise_damage <- function(dt, multipliers = c(H=100, h=100, K=1000, k=1000, M=1E6, m=1E6, B=1E9, b=1E9, U=1)) {
    
    dt %>%
        mutate(
            PROPDMGEXP = ifelse(is.na(PROPDMGEXP) | !(trimws(PROPDMGEXP) %in% names(multipliers)), "U", trimws(PROPDMGEXP)),
            CROPDMGEXP = ifelse(is.na(CROPDMGEXP) | !(trimws(PROPDMGEXP) %in% names(multipliers)), "U", trimws(CROPDMGEXP))
        ) %>%
        mutate(
            PROPDMGnorm = PROPDMG * ifelse(is.numeric(PROPDMGEXP), 10^as.numeric(PROPDMGEXP), multipliers[PROPDMGEXP]),
            CROPDMGnorm = CROPDMG * ifelse(is.numeric(CROPDMGEXP), 10^as.numeric(CROPDMGEXP), multipliers[CROPDMGEXP])
        )
}
```

#### Applying the Normalization Procedures
The raw storm-data is re-shaped by:

1. Applying the normalisation to the event-types (removing redundant levels)
1. Applying the normalisation to the damages i.e. converting 1.5 M to 1.5E6, 2.3K to 2.3E3 etc
1. Dropping all columns except EVTYPE, BGN_DATE, FATALITIES, INJURIES, PROPDMGnorm, CROPDMGnorm
1. Then finally grouping by event-type


```r
normalized_event_types = droplevels(as.factor(normalize_event_types(filtered_storm_data$EVTYPE)))
```

```
## [1] "Normalising event-types"
## [1] "  Iteration # 1"
## [1] "  Iteration # 2"
## [1] "  Iteration # 3"
```

```r
normalized_storm_data = filtered_storm_data %>%
    mutate(EVTYPE = normalized_event_types) %>%
    normalise_damage()
```


```r
cleaned_storm_data = normalized_storm_data %>%
    select(EVTYPE, BGN_DATE, FATALITIES, INJURIES, PROPDMGnorm, CROPDMGnorm) %>%
    group_by(EVTYPE)

no_clean_event_types = length(levels(cleaned_storm_data$EVTYPE))
```

With this data cleaning strategy, the number of event-types is reduced to 308.  

The decision to stop modifying the iterative normalisation process (and the regular expressions therein) was based on the exploratory analysis of the clean-data.


```r
summarised_storm_data = cleaned_storm_data %>%
    summarise(
        PropertyDamage = sum(PROPDMGnorm),
        CropDamage = sum(CROPDMGnorm),
        Fatalities = sum(FATALITIES),
        Injuries = sum(INJURIES),
        Costly = sum(PROPDMGnorm) + sum(CROPDMGnorm),
        Harmful = sum(FATALITIES) + sum(INJURIES)
    )

rank_size = 20

ranked_storm_data = summarised_storm_data %>%
    gather(summary, value, -EVTYPE) %>%
    group_by(summary) %>%
    mutate(rank = min_rank(desc(value))) %>%
    filter(rank <= rank_size) %>%
    arrange(rank)

harmful_table = ranked_storm_data %>% filter(summary == "Harmful") %>% ungroup() %>% select(rank, EVTYPE, value)

costly_table = ranked_storm_data %>% filter(summary == "Costly") %>% ungroup() %>% select(rank, EVTYPE, value)

kable(harmful_table %>% bind_cols(costly_table),
      col.names = c("Rank", "Event Type", "Cases", "Rank", "Event Type", "Cost"),
      align = "l",
      caption = "Table 1. a) 20 most harmful storm events vs. b) 20 most costly storm events, normalized and stabilized",
      format.args = list(big.mark = ',', decimal.mark = '.'))
```



Table: Table 1. a) 20 most harmful storm events vs. b) 20 most costly storm events, normalized and stabilized

Rank   Event Type      Cases    Rank   Event Type      Cost          
-----  --------------  -------  -----  --------------  --------------
1      TORNADO         22,178   1      THUNDERSTORM    2,381,998,520 
2      EXCESSIVEHEAT   8,188    2      FLASHFLOOD      1,408,679,250 
3      FLOOD           7,282    3      TORNADO         1,277,966,730 
4      THUNDERSTORM    5,508    4      HAIL            1,075,458,480 
5      LIGHTNING       4,790    5      FLOOD           1,018,763,960 
6      FLASHFLOOD      2,561    6      LIGHTNING       490,465,290   
7      WILDFIRE        1,543    7      HIGHWIND        332,884,270   
8      WINTERSTORM     1,483    8      WILDFIRE        130,864,510   
9      HEAT            1,459    9      WINTERSTORM     128,864,480   
10     HURRICANE       1,448    10     HEAVYSNOW       90,984,810    
11     HIGHWIND        1,318    11     STRONGWIND      65,164,700    
12     RIPCURRENT      1,045    12     ICESTORM        58,202,120    
13     HEAVYSNOW       805      13     HEAVYRAIN       57,980,550    
14     FOG             772      14     TROPICALSTORM   53,071,940    
15     HAIL            730      15     DROUGHT         37,387,670    
16     WINTERWEATHER   544      16     HURRICANE       30,435,840    
17     BLIZZARD        455      17     BLIZZARD        21,700,480    
18     STRONGWIND      409      18     STORMSURGE      19,268,490    
19     ICESTORM        400      19     DEBRISFLOW      18,985,940    
20     TROPICALSTORM   395      20     WINTERWEATHER   16,923,400    

As you can see from Table 1.a. and Table 1.b:  

* Each indicator for public health and economic impact was ranked by total (sum), taking the top 20 scores for each.
* In all cases, the 1st and 20th scores differed by 2-orders of magnitude
* There also appeared to be no duplicates amongst each set of 20 scores/event-types

Thus it is safe to assume that no duplicated event-type exists, such that aggregating those duplicates would affect the top 20 ranking and even less so, the top 10 ranking.

## Results

```r
fmt <- function(n, long = FALSE, suffixes = c(u = "", k = "thousand", m = "million", b = "billion", t = "trillion")) {
    
    exp = floor(log(n) / log(1000))
    suf = if (long) sprintf(" %s", suffixes[[exp+1]]) else sub("u", "", names(suffixes[exp+1]))
    sprintf("%.4g%s", n / 1000^exp, suf)
}

format_harmful <- function(format, rank, evtype, i, f, t) {
    
    sprintf(format, rank, evtype, fmt(i), fmt(f), fmt(t))
}

top_10_harmful = ranked_storm_data %>%
    filter(summary == "Harmful") %>%
    head(10) %>%
    left_join(summarised_storm_data, by = "EVTYPE") %>%
    mutate(label = format_harmful("%i. %s\n(%s / %s / %s)", rank, EVTYPE, Injuries, Fatalities, value))

no1_harmful = top_10_harmful[1,]

no2_harmful = top_10_harmful[2,]

top_10_costly = ranked_storm_data %>%
    filter(summary == "Costly") %>%
    head(10) %>%
    mutate(label = sprintf("%i. %s ($%s)", rank, EVTYPE, fmt(value)))

no1_costly = top_10_costly[1,]

no2_costly = top_10_costly[2,]
```

### 1. Across the United States, which types of events are most harmful with respect to population health?


```r
# Types of events are determined by the EVTYPE variable
health_storm_data = cleaned_storm_data %>%
    filter(EVTYPE %in% top_10_harmful$EVTYPE) %>%
    mutate(DATE = ymd(sub("^(\\d{4})-.+$", "\\1-01-01", BGN_DATE))) %>%
    group_by(EVTYPE, DATE)

summarized_health_storm_data = health_storm_data %>%
    summarise(Fatalities = sum(FATALITIES, na.rm = T), Injuries = sum(INJURIES, na.rm = T)) %>%
    gather(severity, cases, Injuries, Fatalities)
```


```r
ggplot(data = summarized_health_storm_data, aes(x = DATE, y = log(cases), color = EVTYPE)) +
	geom_line(size = 1.5) +
	scale_x_datetime(name = "Year", labels = date_format("%Y"), breaks = date_breaks("2 years")) +
	scale_y_continuous(name = "Cases (log)") +
    scale_color_discrete(name = "Event Types Ranked by Total\n(Injuries / Fatalities / Total)", labels = top_10_harmful$label, breaks = top_10_harmful$EVTYPE) +
	facet_grid(severity ~ .) +
    labs(title = "Figure 1. Shows the top 10 most harmful storm events between 1996 and 2011, compared year on year") +
    theme(plot.title = element_text(size = 12, hjust = 0.5, vjust = 2)) +
    guides(color = guide_legend(keyheight = 0.35, default.unit = "inch"))
```

<div class="figure" style="text-align: center">
<img src="figures/unnamed-chunk-8-1.png" alt="Top 10 Harmful (Fatality/Injury) Storm Events"  />
<p class="caption">Top 10 Harmful (Fatality/Injury) Storm Events</p>
</div>

### 2. Across the United States, which types of events have the greatest economic consequences?


```r
# Types of events are determined by the EVTYPE variable
economic_storm_data = cleaned_storm_data %>%
    filter(EVTYPE %in% top_10_costly$EVTYPE) %>%
    mutate(DATE = ymd(sub("^(\\d{4})-.+$", "\\1-01-01", BGN_DATE))) %>%
    group_by(EVTYPE, DATE)

summarized_economic_storm_data = economic_storm_data %>%
    summarise(count = n(), sumJOINTDMG = sum(PROPDMGnorm, na.rm = T) + sum(CROPDMGnorm, na.rm = T), meanJOINTDMG = mean(PROPDMGnorm + CROPDMGnorm, na.rm = T)) %>%
    mutate(log1000_sumJOINTDMG = log(sumJOINTDMG, 1000)) %>%
    mutate(mill_sumJOINTDMG = sumJOINTDMG/1E6)
```


```r
ggplot(data = summarized_economic_storm_data, aes(x = DATE, y = mill_sumJOINTDMG, color = EVTYPE)) +
    geom_line(size = 1.5) + 
    geom_point() +
    scale_x_datetime(name = "Year", labels = date_format("%Y"), breaks = date_breaks("2 years")) +
    scale_y_continuous(name = "Total Damage $ (millions)") +
    scale_color_discrete(name = "Ranked Event Types (Total Cost)", labels = top_10_costly$label, breaks = top_10_costly$EVTYPE) +
    labs(title = "Figure 2. Shows the top 10 most costly storm events between 1996 and 2011, compared year on year") +
    theme(plot.title = element_text(size = 12, hjust = 0.5, vjust = 2)) +
    guides(color = guide_legend(keyheight = 0.35, default.unit = "inch"))
```

<div class="figure" style="text-align: center">
<img src="figures/unnamed-chunk-10-1.png" alt="Top 10 Costly (Economic) Storm Events"  />
<p class="caption">Top 10 Costly (Economic) Storm Events</p>
</div>

## Conclusions
Due to the strategy and decisions made to limit and clean the data, conclusions can only be drawn for the period from 1996 through to 2011. 

### Public Health

Figure 1. shows that **Tornados** consistently have the greatest impact on US public health over the period, with approximately **22,000 cases** of injury or loss of life, as compared to the next greatest, **Excessive-Heat** (which is also less prevelant over the period), with approximately 8,000 cases.  

Drilling down into the data further, the vast majority of Tornado cases (93.2%) were injuries; Excessive-Heat caused approximately 300 more fatalities.

### Economy

Figure. 2 shows that **Thunderstorms** consistently have the greatest impact on the US economy over the period, causing approximately **$2.38 billion** worth of damage, 59.1% more than the next most costly, **Flash-Flood**, causing approximately $1.41 billion worth of damage.  

It is worth noting that, despite NOAA's classification and distinction between **Flood** and **Flash-Flood**, if these two types of events are combined, the corresponding combined damage figure becomes $2.43 billion, surpassing that of Thunderstorms.  

[SDDET]: https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
[SDETA]: http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
