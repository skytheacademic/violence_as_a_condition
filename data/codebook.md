---
title: "Violence as a Condition: Structure, Composition, and the Use of Lethal Force"
subtitle: "Codebook"
format: 
  typst:
    margin:
      top: 1cm
---

**Feb. 22, 2026**

**Authors:** **Sky Kunkel**^[Postdoctoral Research Associate, Gender and Security Sector Lab, Cornell University. Email: [sk3386@cornell.edu](mailto:sk3386@cornell.edu), web: [www.skytheacademic.com](http://www.skytheacademic.com)] \ \ \ \ **Matthew Kyle Ellis**^[Visiting Assistant Professor, Purdue University. Email: [ellis151@purdue.edu](mailto:[ellis151@purdue.edu]), web: [matt-ellis.weebly.com](https://matt-ellis.weebly.com/)]

---

**Replication Materials:** [https://github.com/skytheacademic/violence_as_a_condition](https://github.com/skytheacademic/violence_as_a_condition)

This codebook describes the variables for "Violence as a Condition" in the *Journal of Conflict Resolution*. The data used (`Kunkel-Ellis-final.RDS`) is an event-level dataset covering the Central African Republic from April 2018 through December 2022. The unit of observation is an individual political violence event involving state forces and/or the Wagner Group. Data are drawn from ACLED and PRIO.

## Dataset Sources

This dataset integrates variables from multiple sources. For detailed information about variables from external datasets, please consult the original codebooks:

### ACLED Variables
Event-level variables are derived from the Armed Conflict Location & Event Data (ACLED) Project. These variables capture information on political violence events, actor types, fatalities, and geographic location. For complete definitions, coding rules, and methodology, please refer to the ACLED Codebook (included in this repository as `ACLED_Codebook_v1_January-2021.pdf`).

**Citation**: Raleigh, Clionadh, Andrew Linke, Håvard Hegre and Joakim Karlsen. 2010. "Introducing ACLED-Armed Conflict Location and Event Data." *Journal of Peace Research* 47(5): 651-660.

### PRIO-GRID Variables
Resource variables (`diam`, `gold`) are derived from the PRIO datasets. PRIO provides a standardized spatial grid structure with global coverage at 0.5 x 0.5 decimal degree resolution, integrating data on geography, resources, and socioeconomic conditions. For complete definitions and methodology, please refer to the PRIO-GRID Codebook (included in this repository as `PRIO-GRID-Codebook.pdf`).

**Citation**: Tollefsen, Andreas Forø, Håvard Strand & Halvard Buhaug (2012) "PRIO-GRID: A unified spatial data structure." *Journal of Peace Research* 49(2): 363-374.

Resource deposit data are drawn from the following sources:

- **Diamonds**: 
  - Gilmore, Elisabeth, Nils Petter Gleditsch, Päivi Lujala & Jan Ketil Rød. 2005. "Conflict Diamonds: A New Dataset." *Conflict Management and Peace Science* 22(3): 257–292.
  - Lujala, Päivi, Nils Petter Gleditsch & Elisabeth Gilmore, 2005. "A Diamond Curse? Civil War and a Lootable Resource." *Journal of Conflict Resolution*, 49(4): 538–562.
- **Gold**: 
  - Balestri, Sara, 2015. "GOLDATA: The Gold deposits dataset codebook", Version 1.2. *UCSC-Cognitive Science and Communication Research Centre* WP 02/15, Milan. doi:10.13140/RG.2.1.1730.8648
  - Balestri, Sara, 2012. "Gold and civil conflict intensity: evidence from a spatially disaggregated analysis", Peace Economics. *Peace Science and Public Policy*, 18(3): 1-17.
doi:10.1515/peps-2012-0012.

## ACLED Variables (Retained from Source)

The following variables are retained directly from the ACLED source data. Please see the ACLED Codebook for full definitions.

| Variable | Description | Type |
|----------|-------------|------|
| `event_date` | Date on which the event occurred (YYYY-MM-DD) | Date |
| `year` | Calendar year of the event | Integer |
| `time_precision` | Level of certainty of the event date (1 = exact, 2 = week, 3 = month) | Integer |
| `event_type` | ACLED event type (e.g., Violence against civilians, Explosions/Remote violence) | Character |
| `sub_event_type` | ACLED sub-event type (e.g., Attack, Abduction/forced disappearance) | Character |
| `actor1` | Named actor involved in the event | Character |
| `assoc_actor_1` | Named actor associated with or identifying Actor 1 | Character |
| `inter1` | Numeric code indicating the type of Actor 1 | Integer |
| `actor2` | Named actor involved in the event | Character |
| `assoc_actor_2` | Named actor associated with or identifying Actor 2 | Character |
| `inter2` | Numeric code indicating the type of Actor 2 | Integer |
| `interaction` | Numeric code indicating the interaction between Actor 1 and Actor 2 types | Integer |
| `country` | Country in which the event took place (Central African Republic) | Character |
| `admin1` | Largest sub-national administrative region of the event | Character |
| `admin2` | Second largest sub-national administrative region of the event | Character |
| `admin3` | Third largest sub-national administrative region of the event | Character |
| `location` | Specific location name of the event | Character |
| `latitude` | Latitude of the event location (decimal degrees) | Numeric |
| `longitude` | Longitude of the event location (decimal degrees) | Numeric |
| `geo_precision` | Level of certainty of the event location (1 = exact, 2 = near, 3 = region) | Integer |
| `notes` | Short description of the event | Character |
| `fatalities` | Number of reported fatalities during the event | Integer |

## Project-Specific Variables

The following variables were created specifically for this analysis:

### Treatment & Identification Variables

| Variable | Description | Type |
|----------|-------------|------|
| `t_ind` | Wagner Group treatment indicator. 1 if Wagner Contractors present during a violent event, 0 otherwise | Binary |
| `iv` | Instrumental variable for Wagner re-positioning in preparation for and after the invasion of Ukraine. Binary: 1 if the event occurred after November 1, 2021, 0 if before | Binary |
| `score` | Running variable for regression discontinuity design. Computed as the log-transformed number of days after the November 1, 2021 cutoff: log(days + 1). Events before the cutoff receive a score of log(1) = 0 | Numeric |

### Outcome Variables

| Variable | Description | Type |
|----------|-------------|------|
| `death` | Binary indicator for lethal violence. 1 if reported fatalities > 0, 0 otherwise | Binary |

### Control Variables

| Variable | Description | Type |
|----------|-------------|------|
| `event.lag` | Lagged count of militia/rebel events. Sum of all ACLED events involving rebel groups or militias in the Central African Republic during the preceding calendar month | Integer |
| `fatalities.lag` | Lagged fatality count from militia/rebel events. Sum of reported fatalities from all ACLED events involving rebel groups or militias during the preceding calendar month | Integer |
| `diam` | Diamond deposit indicator. Binary: 1 if secondary (alluvial) or primary (kimberlite) diamond deposits have been found within the PRIO-GRID cell containing the event, 0 otherwise. Combines static (`diamsec_s`, `diamprim_s`) and time-varying (`diamsec_y`, `diamprim_y`) PRIO-GRID indicators | Binary |
| `gold` | Gold deposit indicator. Binary: 1 if placer, surface, or vein gold deposits have been found within the PRIO-GRID cell containing the event, 0 otherwise. Combines static (`goldplacer_s`, `goldsurface_s`, `goldvein_s`) and time-varying (`goldplacer_y`, `goldsurface_y`, `goldvein_y`) PRIO-GRID indicators | Binary |

## Notes

- **Output formats:** The final dataset is provided in three formats: `.RDS` (R), `.csv`, and `.dta` (Stata). Variable names containing periods (e.g., `event.lag`) are converted to underscores (e.g., `event_lag`) in the Stata export.
