"./scripts"
- clean_data.R: Merges and preprocesses PRIO grid and ACLED conflict event datasets into a merged, event-level dataset.

- analyze_data.R: Executes all stastical models and figures in the paper and appendix.

- plot_data.R: Generates tables and figures not related to the statistical analysis.

"./results"
- provides the plots, figures, tables, and all results in the paper.

"./data"

ACLED data:
	- Contains data on armed conflict and violence.
	- Observed at event-level, with day and latitude and longitude.
	- Downloaded from https://acleddata.com/data-export-tool/ on Sept. 1, 2022.
	- "1900-01-01-2022-06-16-Central_African_Republic.csv" is the original CSV file on all CAR violence downloaded by SK.

GADM data:
	- Contains adminstrative data
	- Downloaded from https://gadm.org/download_country.html ON Oct. 4, 2022
	- Each folder named after the three letter country code is the original shapefiles downloaded on the administrative boundaries of CAR and surrounding countries.