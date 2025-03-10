# Crime Analysis Project

An analysis of relationships between political leaning, incarceration rates, and crime rates across US states.

## Project Overview

This project explores potential correlations between state-level political affiliations, incarceration policies, and crime outcomes. Using data from federal sources, the analysis creates a comprehensive visualization that allows for examination of these relationships and challenges common political narratives about crime.

![Main Visualization](/output/crime_politics_final.png)

## Key Features

- **Multi-dimensional visualization** showing relationships between political leaning, incarceration rates, and crime types
- **Data integration** from multiple federal sources including election data, FBI crime statistics, and Bureau of Justice Statistics
- **Standardized metrics** for cross-state comparison
- **Categorized crime analysis** separating person, property, and society crimes

## Data Sources

The project uses the following data sources:

- **Political Data**: County-level presidential election results (2020-2024)
- **Crime Data**: FBI crime statistics categorized by:
  - Crimes against persons
  - Crimes against property
  - Crimes against society
- **Incarceration Data**: Bureau of Justice Statistics imprisonment rates

## Sources

- Carson, E. Ann, and Rich Kluckow. "Table 7. Imprisonment rates of U.S. residents, based on sentenced prisoners under the jurisdiction of state or federal correctional authorities, by sex, age, and jurisdiction, 2021 and 2022." Prisoners in 2022 – Statistical Tables, Bureau of Justice Statistics, 30 Nov. 2023, NCJ 307149.
- "State Offense Tables (Totals): Crimes Against Persons; Society; Property Offenses, Offense Category by State, 2023." Crime Data Explorer, Federal Bureau of Investigation, 23 Sept. 2024, cde.ucr.cjis.gov/.
- Tony McGovern, et al. United States General Election Presidential Results by District, Ward, or County from 2008 to 2024. Zenodo, 16 Jan. 2025, doi:10.5281/zenodo.14223604.

## Project Structure

```
Crime_Analysis/
├── data/                   # Raw and processed data files
│   ├── crime/              # Crime statistics from FBI
│   ├── incarceration/      # BJS imprisonment data
│   ├── political/          # Election data
│   └── processed/          # Output from data processing
├── output/                 # Visualization outputs
└── scripts/                # R scripts for data processing and visualization
```

## Processing Pipeline

This project follows a structured data pipeline:

1. **Data Processing** (scripts 01-03):
   - Process political data to determine state political leanings
   - Extract state-level incarceration rates
   - Calculate crime rates by category for each state

2. **Data Integration** (script 04):
   - Merge all processed datasets
   - Create derived metrics and standardize values

3. **Visualization** (script 05):
   - Create comprehensive visualization showing:
     - Political leaning via color (blue-to-red gradient)
     - Incarceration rates via pattern density
     - Crime rates via stacked bar charts by category

## Key Findings

The visualization reveals several insights:
- Complex relationships between political leaning and crime rates
- Variable incarceration policies across politically similar states
- Different patterns of crime types across regions

## Requirements

- R 4.0 or later
- Required R packages:
  - tidyverse
  - sf
  - tigris
  - ggpattern
  - shadowtext
  - cowplot
  - stringr

## Usage

To reproduce the analysis:

1. Clone this repository
2. Open the project in RStudio using the .Rproj file
3. Run scripts in numerical order (01-05)
4. Examine generated files in the `data/processed/` and `output/` directories

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.