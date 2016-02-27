setwd("/Users/Darren/Workspaces/Courses/data-science/reproducible-research/assignment2")

library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

load_storm_data <- function() {

    
    if (class(try(nrow(storm_data), silent = T)) == "try-error") {
    
        if (!file.exists("repdata-data-StormData.csv.bz2")) {
        
            download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "repdata-data-StormData.csv.bz2", mode = "wb")
        }
        
        print("Loading data-set from disk...")
        
        tbl_df(read.csv(bzfile("repdata-data-StormData.csv.bz2"), na.strings = ""))
    }
    else {
        storm_data
    }
}

storm_data = load_storm_data()

filtered_storm_data = storm_data %>%
    mutate(BGN_DATE = mdy_hms(BGN_DATE)) %>%
    filter(BGN_DATE > dmy("01/01/1996"))

no_raw_event_types = length(levels(filtered_storm_data$EVTYPE))

normalize_event_types <- function (event_types, n = 1) {

    if (n == 1) {
        print("Normalising event-types")
    }
    
    print(paste("  Iteration #", n))
    
    patterns = list(
        plural_match = "^(.*)[sS]$",
        prefix_match = "^(THU|TSTM|TORN|FLOOD|SLEET|BLIZZARD|TROPICALSTORM|WINTER(?:STORM|WEATHER)|HURRICANE|LIGHTNING|HIGHWIND).*$",
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

if (class(try(length(normalized_event_types), silent = T)) == "try-error") {
    
    normalized_event_types = droplevels(as.factor(normalize_event_types(filtered_storm_data$EVTYPE)))
}

normalized_storm_data = filtered_storm_data %>%
    mutate(EVTYPE = normalized_event_types) %>%
    normalise_damage()

cleaned_storm_data = normalized_storm_data %>%
    select(EVTYPE, BGN_DATE, FATALITIES, INJURIES, PROPDMGnorm, CROPDMGnorm) %>%
    group_by(EVTYPE)

no_clean_event_types = length(levels(cleaned_storm_data$EVTYPE))

summarised_storm_data = cleaned_storm_data %>%
    summarise(
        count = n(), sumPROPDMG = sum(PROPDMGnorm), sumsCROPDMG = sum(CROPDMGnorm), sumFATALITIES = sum(FATALITIES), sumINJURIES = sum(INJURIES),
        costly = sum(PROPDMGnorm) + sum(CROPDMGnorm),
        harmful = sum(FATALITIES) + sum(INJURIES)
    )

rank_size = 20

ranked_storm_data = summarised_storm_data %>%
    gather(summary, value, count:harmful) %>%
    group_by(summary) %>%
    mutate(rank = min_rank(desc(value))) %>%
    filter(rank <= rank_size) %>%
    arrange(rank)

harmful_table = ranked_storm_data %>% filter(summary == "harmful") %>% select(rank, EVTYPE, value)

harmful_table$summary = NULL

kable(harmful_table,
      col.names = c("Rank", "Event Type", "Cases"),
      align = "l",
      caption = "Table 1. 20 most harmful storm events, normalized and stabilized")

format_1000 <- function(evtype, n, rank, fmt, suffixes = c("", "k", "m", "b", "t")) {
    
    exp = floor(log(n) / log(1000))
    sprintf(fmt, rank, evtype, n / 1000^exp, suffixes[exp+1])
}

top_10_harmful = ranked_storm_data %>%
    filter(summary == "harmful") %>%
    mutate(label = format_1000(EVTYPE, value, rank, fmt = "%i. %s (%.4g%s)")) %>%
    head(10)

print(top_10_harmful)

top_10_costly = ranked_storm_data %>%
    filter(summary == "costly") %>%
    mutate(label = format_1000(EVTYPE, value, rank, fmt = "%i. %s ($%.4g%s)")) %>%
    head(10)

# Types of events are determined by the EVTYPE variable
health_storm_data = cleaned_storm_data %>%
    filter(EVTYPE %in% top_10_harmful$EVTYPE) %>%
    mutate(DATE = ymd(sub("^(\\d{4})-.+$", "\\1-01-01", BGN_DATE))) %>%
    group_by(EVTYPE, DATE)

summarized_health_storm_data = health_storm_data %>%
    summarise(count = n(), sumJOINTCASES = sum(FATALITIES, na.rm = T) + sum(INJURIES, na.rm = T), meanJOINTCASES = mean(FATALITIES + INJURIES, na.rm = T)) %>%
    mutate(log_sumJOINTCASES = log(sumJOINTCASES)) %>%
    mutate(hun_sumJOINTCASES = sumJOINTCASES/1E2)

print(
    ggplot(data = summarized_health_storm_data, aes(x = DATE, y = log_sumJOINTCASES, color = EVTYPE)) +
    geom_line(size = 1.5) +
    scale_x_datetime(name = "Year", labels = date_format("%Y"), breaks = date_breaks("2 years")) +
    scale_y_continuous(name = "Total Cases (log)") +
    scale_color_discrete(name = "Ranked Event Types (Total Fatality/Injury)", labels = top_10_harmful$label, breaks = top_10_harmful$EVTYPE) +
    labs(title = "Top 10 Harmful (Fatality/Injury) Storm Events")
)

# Types of events are determined by the EVTYPE variable
# economic_storm_data = cleaned_storm_data %>%
#     filter(EVTYPE %in% top_10_costly$EVTYPE) %>%
#     mutate(DATE = ymd(sub("^(\\d{4})-.+$", "\\1-01-01", BGN_DATE))) %>%
#     group_by(EVTYPE, DATE)
# 
# summarized_economic_storm_data = economic_storm_data %>%
#     summarise(count = n(), sumJOINTDMG = sum(PROPDMGnorm, na.rm = T) + sum(CROPDMGnorm, na.rm = T), meanJOINTDMG = mean(PROPDMGnorm + CROPDMGnorm, na.rm = T)) %>%
#     mutate(log1000_sumJOINTDMG = log(sumJOINTDMG, 1000)) %>%
#     mutate(mill_sumJOINTDMG = sumJOINTDMG/1E6)
# 
# print(
#     ggplot(data = summarized_economic_storm_data, aes(x = DATE, y = mill_sumJOINTDMG, color = EVTYPE)) +
#     geom_line(size = 1.5) + 
#     geom_point() +
#     scale_x_datetime(name = "Year", labels = date_format("%Y"), breaks = date_breaks("2 years")) +
#     scale_y_continuous(name = "Total Damage $ (millions)") +
#     scale_color_discrete(name = "Ranked Event Types (Total Cost)", labels = top_10_costly$label, breaks = top_10_costly$EVTYPE) +
#     labs(title = "Top 10 Costly (Economic) Storm Events")
# )
