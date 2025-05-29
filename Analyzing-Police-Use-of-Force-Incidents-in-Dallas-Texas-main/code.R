---
title: 'Dataset Analysis of Dallas, Texas Police '
author: "Sulaiman Dauda"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Load required libraries
library(dplyr) # data manipulation
library(tibble) # data manipulation
library(tidyr) # data manipulation
library(ggplot2) # data visualization
library(knitr) # report generation
library(gridExtra) # grid arrangement of plots
library(leaflet) # interactive mapping
library(lubridate) # date and time manipulation
library(plotly) # interactive visualizations

```

### Data Cleaning and Exploration

We start by loading our dataset file named "37-00049_UOF-P_2016_prepped.csv" and store its data in a dataframe called "police_incidents".

```{r load dataset}
# Load the dataset
police_incidents <- read.csv("37-00049_UOF-P_2016_prepped.csv")
```

<br>

##### **Data Overview**

Before we dive into visualising our data, let's begin by having a quick overview of the dataframe to get a clear understanding of its variables, data types, and any missing values. This will enable us to make informed decisions on how best to visualise and analyse the data.

```{r data overview}

# Print dimensions of the dataset
dim(police_incidents)

# Print data structure
str(police_incidents)

# Print top rows of the data
head(police_incidents)
```

The dataset contains 2384 observations (rows) and 47 variables (columns) for incidents recorded. The variables include officer and subject demographics, incident location, and details on the use of force. However, the data has missing information, and most variables are in character format, requiring addressing before conducting any analysis.

<br>

##### **Select variables needed for analysis**

To improve the quality of our data analysis and visualisation, we need to carefully select a subset of variables from the dataset that are relevant to our report.

```{r select variable for analysis}

# Select subset of columns from the dataset to a new dataframe

police_incidents_selected <- police_incidents %>% 
  select(INCIDENT_DATE, INCIDENT_TIME, OFFICER_GENDER, OFFICER_RACE, OFFICER_YEARS_ON_FORCE, OFFICER_INJURY, SUBJECT_RACE, SUBJECT_GENDER, SUBJECT_INJURY, LOCATION_LATITUDE, LOCATION_LONGITUDE, LOCATION_FULL_STREET_ADDRESS_OR_INTERSECTION, TYPE_OF_FORCE_USED1)

```

We selected the relevant variables, including incident date and time, officer and subject demographics, injuries sustained, and location information. These variables allow us to focus on the critical aspects of the data and improve the accuracy and reliability of our results.

<br>

##### **Remove first row and treat missing value**

Let's remove the first row of the dataset since it only contains column names repeated, and replace any blank or missing values with NA.

```{r Remove the first row and replace missing value}

# Remove the first row of the data frame
police_incidents_selected <- police_incidents_selected[-1,]

# Replace missing values with NA
police_incidents_selected <- police_incidents_selected %>% 
  mutate_all(~na_if(.,""))

```

This helps to clean up the dataset and ensure that any missing values are properly accounted for before proceeding with further analysis.

<br>

##### **Convert date and time to format suitable for analysis**

Let's convert the incident date and time from its original format to 'hour:minute:second' and 'day/month/year' formats, respectively.

```{r date and time converted}

# Convert incident time to 24-hour format 
police_incidents_selected$INCIDENT_TIME <- format(strptime(police_incidents_selected$INCIDENT_TIME, format = "%I:%M:%S %p"), format = "%H:%M:%S")

# Convert incident time to 24-hour format incident date to dd/mm/yyyy format
police_incidents_selected$INCIDENT_DATE <- format(as.Date(police_incidents_selected$INCIDENT_DATE, format = "%m/%d/%y"), format = "%d/%m/%Y")
```

This step is crucial to ensure that the date and time variables are correctly formatted for further analysis and visualization of the project.

<br>

##### **Convert columns to numeric type suitable for analysis**

Columns like officer years on force, location latitude, and longitude are stored as characters and not suitable for analysis. To fix this, we can convert these columns to a numeric type using the as.numeric function.

```{r Convert columns to numeric type suitable for analysis}

# Convert officer years on force column to numeric type suitable for analysis
police_incidents_selected$OFFICER_YEARS_ON_FORCE <- as.numeric(police_incidents_selected$OFFICER_YEARS_ON_FORCE)

# Convert location latitude column to numeric type suitable for analysis
police_incidents_selected$LOCATION_LATITUDE <- as.numeric(police_incidents_selected$LOCATION_LATITUDE)

# Convert location longitude column to numeric type suitable for analysis
police_incidents_selected$LOCATION_LONGITUDE <- as.numeric(police_incidents_selected$LOCATION_LONGITUDE)
```

Converting these columns to a numeric type will allow us to perform numerical operations and analyses, improving our understanding of the data.

## Data Visualization

##### **Gender distribution**

The table below revealed that male officers were involved in a significantly higher number of incidents overall, with 1,772 incidents involving male officers and male subjects. However, there were also incidents involving female officers, with a total of 77 incidents involving female officers and female subjects and 160 incidents involving female officers and male subjects.

*Table 1: Incidents by Subject Race*

```{r  gender table}

# Generate a matrix table of counts for combinations of officer and subject gender
knitr::kable(as.matrix(table(police_incidents_selected$OFFICER_GENDER, police_incidents_selected$SUBJECT_GENDER))) 
```

*Figure 1: Distribution of Incidents by gender*

```{r  gender plot}

# Create a data frame of counts for combinations of officer and subject gender
incidents_gender <- as.data.frame(table(police_incidents_selected$OFFICER_GENDER, police_incidents_selected$SUBJECT_GENDER))

# Create a stacked bar plot of the proportion of subject gender by officer gender
ggplot(incidents_gender, aes(x = Var1, y = Freq, fill = Var2)) + # Use Var1, Freq for the x and y aesthetics and # Use Var2 to fill the bars respectively
  geom_col(position = "fill") + # Set the position of the bars to "fill"
  scale_fill_manual(values = c("#E1EDF2", "#89CFF0", "#DADADA", "#6FD2F6")) + # Customize the fill colors
  xlab("Officer Gender") + # Add labels for the x axes
  ylab("Proportion") + # Add labels for the y axes
  ggtitle("Proportion of Subject Gender by Officer Gender") + # Add a title to the plot
  theme_minimal() # Use the theme_minimal() for the plot
```

The resulting plot below suggests clearly that there may be gender-specific patterns in police incidents.

<br>

##### **Officer and subject Involved in Incidents by race**

Table 2 shows that the majority of police incidents (55.94%) involved black subjects, followed by Hispanic subjects at 21.99% and white subjects at 19.72%. Other races and unknown races accounted for a small percentage of incidents. Table 3 shows that white officers were the most frequent participants in incidents, accounting for nearly 62% of all cases. Hispanic officers accounted for 20.23% of incidents, followed by black officers at 14.31%. Other races and unknown races accounted for a small percentage of incidents.

*Table 2: Officer involved in incident by Race*

```{r  officer race count}
# Compute a count of incidents for each officer race
officer_race_count <- police_incidents_selected %>% # Create a new variable for incidents count
  group_by(OFFICER_RACE) %>% # Group the data by officer race
  summarize(count = n()) %>% # calculate the count of each officer race
  mutate(percentage = round(count/sum(count)*100,2)) %>% # Round percentage to two decimal places
  arrange(desc(percentage)) # Sort in descending order based on percentage
knitr::kable(officer_race_count) # Create a formatted table of the officer race count

```

*Table 3: Subject involved in incident by Race*

```{r  subject race count}

# Group data frame by subject race and summarize the count of each race
subject_race_count <- police_incidents_selected %>%
  group_by(SUBJECT_RACE) %>% # Group the data by subject race
  summarize(count = n()) %>% # Calculate the count of each subject race
  mutate(percentage = round(count/sum(count)*100,2)) %>% # Round percentage to two decimal places
  arrange(desc(percentage)) # Sort in descending order based on percentage
knitr::kable(subject_race_count) # Create a formatted table of the officer race count
```

*Figure 2 & 3: Incidents involvement by Race*

```{r,  officer race plot}

# Create a bar plot of officer race on x-axis and Percentage on y-axis
officer_race_plot <- ggplot(officer_race_count, aes(x = reorder(OFFICER_RACE, -percentage), y = percentage)) + # Reorder the x-axis values by descending order of percentage
  geom_col(fill = "#89CFF0") + # Plot the bars with color fill
  ggtitle("Officer by Race") + # Add plot title
  xlab("Officer Race") + # x-axis label
  ylab("Percentage") # y-axis label
```

```{r,  subject race plot}

# Create a bar plot of subject race on x-axis and Percentage on y-axis
subject_race_plot <- ggplot(subject_race_count, aes(x = reorder(SUBJECT_RACE, -percentage), y = percentage)) + # Reorder the x-axis values by descending order of percentage
  geom_col(fill = "#49636f") + # Plot the bars with color fill
  ggtitle("Subject by Race") + # Add plot title
  xlab("Subject Race") + # x-axis label
  ylab("Percentage") # y-axis label
```

```{r,  officer and subject race plot, fig.width=9, fig.height=4}

# Plot Officer and subject race side by side using gridExtra package
grid.arrange(officer_race_plot, subject_race_plot, ncol = 2)
```

The plots suggest that there may be racial disparities in police incidents, as black and Hispanic subjects are disproportionately involved, while white officers are over-represented as participants.

<br>

##### **Frequency of incidents by officer and subject race**

As shown in table 4, the frequency of incidents involving officers and subjects of different races. The majority of incidents involved white officers and black subjects, for a total of 846 incidents. This was followed by incidents involving Hispanic officers and black subjects, for a total of 230 incidents. Incidents involving American Indian officers had the lowest count among all officer races, with only 8 incidents reported in total.

*Table 4: Frequency of Incidents by Officer and Subject Race*

```{r  officer race who stopped subject race}

# Create a matrix table of officer race and subject race
race_incidents <- as.data.frame.matrix(table(police_incidents_selected$OFFICER_RACE, police_incidents_selected$SUBJECT_RACE))

# Display resulting table
knitr::kable(race_incidents)

```

The figure 4 below shows the frequency of incidents by officer and subject race. It highlights the large number of incidents involving white officers and black subjects.

*Figure 4: Frequency of Incidents by Officer and Subject Race*

```{r  race incidents}

# pivot the table from wide to long format.
race_incidents <- race_incidents %>%
  rownames_to_column(var = "officer_race") %>% # convert the rownames to a column named officer race.
  pivot_longer(cols = -officer_race, names_to = "subject_race", values_to = "count") # reshape the table to long format.

# create a ggplot object with x-axis as officer race, y-axis as subject race, and fill with count.
ggplot(race_incidents, aes(x = officer_race, y = subject_race, fill = count)) +
  geom_tile(color = "white") + # Add a tile layer to the plot with white border.
  scale_fill_gradient(low = "lightblue", high = "#49636f") + # Set the fill color gradient for the tiles.
  ggtitle("Incidents by Officer and Subject Race") + # Set the plot title.
  xlab("Officer Race") +  # Set the x-axis label.
  ylab("Subject Race") # Set the x-axis label.

```

The findings of the race analysis suggest that incidents involving black subjects were the most frequent, and white officers were the most frequent participants in incidents. This provides valuable insights into the relationship between the race of police officers and the subjects involved in incidents. These findings raise concerns about potential racial disparities in police incidents and highlight the need for further investigation and efforts to address any biases or discrimination in law enforcement.

<br>

##### **Incidents by days of the week**

The plotted data clearly shows difference in the number of incidents reported on various days of the week. Incidents on Sundays and Saturdays were reported more frequently than any other day, with Friday having the third-highest number of reported incidents. Thursday, Tuesday, and Wednesday had a relatively lower number of incidents reported compared to the other days. Interestingly, Monday had the lowest number of incidents reported among all the days of the week.

*Table 4: Incidents by days of the week*

```{r  incidents by days of the week}
# Add a new column for incident weekdays
police_incidents_selected$INCIDENT_WEEKDAYS <- weekdays(as.Date(police_incidents_selected$INCIDENT_DATE, format = "%d/%m/%Y"))

# Count the number of incidents by day of the week and order chronologically.
incidents_by_day_of_week <- police_incidents_selected %>% # Add a new column to the selected dataset.
  count(INCIDENT_WEEKDAYS, sort = FALSE) %>% # Count the number of incidents by day of the week.
  mutate(INCIDENT_WEEKDAYS = factor(INCIDENT_WEEKDAYS, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) # Sort the days in ascending order.

knitr::kable(incidents_by_day_of_week) # Display the resulting table.
```

*Figure 5: Incidents by days of the week*

```{r plot day of the week}
# Create a histogram of the incident counts by day of the week
ggplot(incidents_by_day_of_week, aes(x = INCIDENT_WEEKDAYS, y = n)) + 
  geom_col(fill = "#89CFF0") + # Set the plot fill color.
  ggtitle("Incidents by Day of Week") + # Set the plot title.
  xlab("Day of Week") + # Set the plot title.
  ylab("Incident Count") # Set the x-axis label.

```

Based on our analysis, we found that there were more incidents reported on weekends, particularly on Sundays and Saturdays.

<br>

##### **Incidents by months**

The plot shows the trend of the incident count over time, with each point representing a month.

*Figure 6: Incidents by months of the year*

```{r  incidents by months, fig.width=9}
# Convert date and time columns to datetime format
police_incidents_selected$DATE_TIME <- dmy_hms(paste(police_incidents_selected$INCIDENT_DATE, police_incidents_selected$INCIDENT_TIME))

# Group the data by month and count the number of incidents
incidents_by_month <- police_incidents_selected %>% 
  group_by(month = floor_date(DATE_TIME, unit = "month")) %>% # Group the data by month.
  summarize(INCIDENT_COUNT = n()) # count the number of incidents.

# Create a plotly object from the ggplot object
incidents_by_month_interactive <- ggplotly(
  ggplot(incidents_by_month, aes(x = month, y = INCIDENT_COUNT)) +  # Create a ggplot object with month on the x-axis and incident count on the y-axis
    geom_line(color = "#49636f") +  # Add line to the plot with a custom color.
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +  # Change x-axis scale to show dates, abbreviate month and year and set breaks at the first day of each month.
    labs(title = "Incidents by Month",  # Add title to the plot.
         x = "Date",  # Set the x-axis label.
         y = "Number of Incidents")  # Set the y-axis label.
)

# Show the interactive plot
incidents_by_month_interactive  # Displays the interactive plot in the RStudio Viewer or the web browser.

```

The data shows that there was a noticeable fluctuation in the incident count over the course of the year. In particular, there was a sharp increase in March 2016, followed by a steep decline in December 2016. This suggests that there may be seasonal patterns in the incident rate.

<br>

##### **Type of force used**

It can be seen in the table that the most common types of force used by police officers were verbal commands, weapon displays at the person, and holding a suspect down, in that order. These three types of force were used significantly more often than other methods, including taser use and joint locks.

*Table 5: Type of force used*

```{r  type of forced used table}

# Count the number of incidents by type of force used and select the top 10
incidents_by_force_top10 <- police_incidents_selected %>% 
  count(TYPE_OF_FORCE_USED1, sort = TRUE) %>% # Count number of incidents and sorts in descending order.
  top_n(10) # Select top 10 based on incidents count

# Sort the types of force used in descending order
incidents_by_force_top10$TYPE_OF_FORCE_USED1 <- factor(incidents_by_force_top10$TYPE_OF_FORCE_USED1, 
                                                       levels = rev(incidents_by_force_top10$TYPE_OF_FORCE_USED1))

knitr::kable(incidents_by_force_top10) # Display resulting table
```

*Figure 7: Type of force used*

```{r  type of forced used plot}
# Create the bar plot
ggplot(incidents_by_force_top10, aes(y = n, x = TYPE_OF_FORCE_USED1, fill = n)) + # Initialize ggplot object, specify x and y-axis and map fill color.
  geom_bar(stat = "identity") + #Add a bar layer to the plot.
  scale_fill_gradient(low = "#89CFF0", high = "#415b68") + # Set fill color gradient for the bars.
  ggtitle("Top 10 Types of Force Used") + # Add plot title.
  xlab("Type of Force Used") + # Add x-axis title.
  ylab("Number of Incidents") + # Add y-axis title
  theme_minimal() + # Apply a minimal theme 
  theme(legend.position = "none") + # Remove plot legend
  coord_flip() # Flip axes to create horizontal bar
```

Based on the plot presented in figure 7, it can be concluded that police officers prioritize using communication skills and their authority to de-escalate situations and gain control over suspects. The data also shows that the next most common methods of force used were displaying a weapon and holding a suspect down, indicating that officers tend to rely on non-lethal approaches to subdue suspects.

<br>

##### **Location point of incidents**

Based on the map, it can be inferred that incidents occurred throughout the city and were not limited to a specific neighborhood or region. This indicates that the incidents were not concentrated in any one area but rather spread out across the entire city.

*Figure 8: Incidents location on map*

```{r  Location, fig.width=9}
# Select the relevant variables and filter the data
police_incidents_filtered <- police_incidents_selected %>%
  select(LOCATION_LATITUDE, LOCATION_LONGITUDE, LOCATION_FULL_STREET_ADDRESS_OR_INTERSECTION) %>%
  filter(!is.na(LOCATION_LATITUDE) & !is.na(LOCATION_LONGITUDE)) # Remove location latitude and longitude with missing.

# Create the map with circle markers
location_map <- police_incidents_filtered %>% 
  leaflet() %>% # Create a new.
  addTiles() %>% # Adds tiles to the map.
  setView(-96.7970, 32.7767, zoom = 12) %>% # Set the zoom level to 12.
  addCircleMarkers(radius = 5, # Adds circle markers to the map and adjust radius.
                   color = "#0078FF", # Add circle color.
                   weight = 1, # Adjust circle weight.
                   fillColor = "#0078FF", # Add cirle fill.
                   fillOpacity = 0.6, # Adjust circle opacity.
                   popup = ~LOCATION_FULL_STREET_ADDRESS_OR_INTERSECTION, # Set display in a pop-up when click a marker.
                   lat = ~LOCATION_LATITUDE, # specify location latitude column.
                   lng = ~LOCATION_LONGITUDE) # specify location longitude column.
# Display map in the output.
location_map 
```

<br>

##### **Injured officer in relation to years on force**

From the plot analysis, we can conclude that there is a negative correlation between the number of incidents and years on force. This means that officers with more years of experience tend to be involved in fewer incidents. Additionally, the majority of officers have not been injured, and those who have been injured have lower incident counts. Notably, officers with 1-3 years of experience have a higher likelihood of being injured compared to those with more years of experience.

*Figure 9: Year in force and injury distribution*

```{r year in force injury, fig.width=9}
# Aggregate data by year in force and injury status
officer_injury_status <- police_incidents_selected %>% # Create dataframe by grouping.
  count(OFFICER_YEARS_ON_FORCE, OFFICER_INJURY) %>% # Count incidents in each group
  mutate(OFFICER_INJURY = factor(OFFICER_INJURY, levels = c("No", "Yes"))) # convert to a factor and set levels to "No" and "Yes"

# Create scatter plot
ggplot(officer_injury_status, aes(x = OFFICER_YEARS_ON_FORCE, y = n, color = OFFICER_INJURY)) + # Initializes a new plot, set x and y-axis and color
  geom_point(size = 4) + # Adds points to the plot with a size of 4.
  scale_color_manual(values = c("#415b68", "#0078FF")) + # specify colors for each factor 
  labs(title = "Officer Injury by Years on Force", # Set title color legend title
       x = "Years on Force", # Add x-axis label
       y = "Incident Count", # Add y-axis label
       color = "Officer Injury") + # Add color legend title.
  theme_update() # sets a style for the plot.
```

These findings suggest that more experienced officers are better equipped to handle incidents and avoid injury, while newer officers may require additional support and training to reduce their risk of injury.
