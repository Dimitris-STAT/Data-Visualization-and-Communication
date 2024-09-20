
#### Load and Install essentials
#install.packages(c("scales","lubridate","dplyr", "ggplot2", "ggrepel"))
library(ggplot2)
library(ggrepel)
library(dplyr)
library(lubridate)
library(scales)



# Read the CSV file (if not already done)
load("final_merged.Rdata")

#### Create year, month, day_of_week_numeric, day_of_week and hour_of_day columns ####
# Format datetime values
df$started_at <- as.POSIXct(df$started_at, format = "%Y-%m-%d %H:%M:%S")

df$year <- format(df$started_at, "%Y")
df$month <- format(df$started_at, "%m")

# Extract day of the week (numeric)
df$day_of_week_numeric <- wday(df$started_at)
# Convert day of the week to character
df$day_of_week <- weekdays(df$started_at)

df$hour_of_day <- format(df$started_at, "%H")



#### Insert Price Column ####
df <- df %>%
    mutate(
        rideable_type = ifelse(rideable_type == "", "classic_bike", rideable_type),
        member_casual = tolower(member_casual),
        
        price = ifelse(
            member_casual == "casual",
            1 + ifelse(rideable_type == "classic_bike", 0.05, 0.15) * (duration / 60),
            ifelse(
                rideable_type == "classic_bike",
                pmax(0, (duration - 45 * 60) / 60 * 0.05),
                duration / 60 * 0.10
            )
        )
    )





#### Plot για διαφάνεια 5 ####



# Ensure hour_of_day is numeric
df <- df %>%
    mutate(hour_of_day = as.numeric(format(started_at, "%H")))

# Calculate average rides per hour and identify peak hours
avg_rides <- df %>%
    group_by(hour_of_day) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(peak = ifelse(count > mean(count), "Peak", "Non-Peak"))

# Determine the range of peak hours
peak_hours <- avg_rides %>% filter(peak == "Peak") %>% pull(hour_of_day)
peak_start <- min(peak_hours)
peak_end <- max(peak_hours)

# Calculate the counts at peak start and end hours
peak_start_count <- avg_rides %>% filter(hour_of_day == peak_start) %>% pull(count)
peak_end_count <- avg_rides %>% filter(hour_of_day == peak_end) %>% pull(count)

# Plot the line plot with highlighted peak hours
ggplot(avg_rides, aes(x = hour_of_day, y = count)) +
    geom_line(color = "skyblue", linewidth = 1) +
    geom_ribbon(data = avg_rides %>% filter(hour_of_day >= peak_start & hour_of_day <= peak_end), 
                aes(ymin = 0, ymax = count, fill = "Peak Hours"), alpha = 0.3) +
    annotate("segment", x = peak_start, xend = peak_start, y = 0, yend = peak_start_count, 
             linetype = "dashed", color = "black", linewidth = 1) +
    annotate("segment", x = peak_end, xend = peak_end, y = 0, yend = peak_end_count, 
             linetype = "dashed", color = "black", linewidth = 1) +
    labs(title = "Ride Start Time Distribution", x = "Hour of the Day", y = "Number of Ride Starts") +
    scale_fill_manual(values = c("Peak Hours" = "#FF5733"), guide = guide_legend(title = "")) +
    theme_minimal() +
    scale_x_continuous(breaks = 0:23) +  # Ensure all hours are displayed
    scale_y_continuous(labels = comma_format(scale = 1e-3, suffix = "k")) +
    theme(axis.title.x = element_text(vjust = -1.5, hjust = 0),  # Adjust x-axis title position
          axis.title.y = element_text(vjust = 3, hjust = 0),  # Adjust y-axis title position
          panel.grid.major.y = element_line(color = "gray", linetype = "solid"),  # Vertical grid lines
          panel.grid.major.x  = element_blank(),  # Remove horizontal grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          legend.position = "top",  # Position the legend
          legend.key.width = unit(1, "cm"))  # Set the legend key width







#### Plot για διαφάνεια 15 ####



# Summarize the data
summary_df <- df %>%
    group_by(rideable_type) %>%
    summarise(total_revenue = sum(price)) %>%
    ungroup() %>%  # Ungroup to avoid group-wise operations in the next steps
    mutate(percentage = total_revenue / sum(total_revenue) * 100) %>%
    arrange(desc(total_revenue))  # Order by total_revenue descending

# Create a named vector for new y-axis labels
new_labels <- c("classic_bike" = "Classic Type", "docked_bike" = "Docked Type", "electric_bike" = "Electric Type")

# Plot the data without fill colors and with updated y-axis labels
ggplot(summary_df, aes(x = total_revenue, y = reorder(rideable_type, total_revenue))) +
    geom_bar(stat = "identity", width = 0.5, fill = "#20B2AA") +  # Set fill color to light blue
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              hjust = -0.1, size = 3.5) +  # Add percentage labels at the end of each bar
    theme_minimal() +
    labs(
        title = "",
        x = "Total Revenue",
        y = "",
        fill = "Bike Types"
    ) +
    scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),  # Apply the short scale transformation for x-axis
        sec.axis = sec_axis(
            ~ . / sum(summary_df$total_revenue) * 100,
            name = "Percentage of Total Revenue",
            labels = function(x) paste0(x, "%")  # Format as percentage
        ),
        breaks = c(seq(0, max(summary_df$total_revenue), by = 0.1 * sum(summary_df$total_revenue)), sum(summary_df$total_revenue) * 0.5),  # Align grid lines at 10% intervals and add a specific break for 50%
        limits = c(0, max(summary_df$total_revenue) * 1.2)  # Add some space on the right
    ) +
    scale_y_discrete(labels = new_labels) +  # Use the named vector for y-axis labels
    theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.title.x.top = element_text(size = 11),  # For the secondary x-axis title
        axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        axis.text.x = element_text(size = 10),
        axis.text.x.top = element_text(size = 10),  # For the secondary x-axis labels
        legend.position = "none",  # Remove legend
        panel.grid.major.x = element_line(color = "grey80"),  # Add major x-axis grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor x-axis grid lines
        panel.grid.major.y = element_blank()  # Remove major y-axis grid lines
    )






#### PLot για διαφάνεια 4 #### 
df$day_of_week <- weekdays(df$started_at)

# Create a vector of full day names
full_day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Convert full day names to acronyms
day_acronyms <- substr(full_day_names, 1, 3)

# Replace full day names in df$day_of_week with acronyms
df$day_of_week <- day_acronyms[match(df$day_of_week, full_day_names)]

# Specify the order of days of the week
day_order <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

# Group data by year and day of the week, and count the number of rides
df_summary <- df %>%
    group_by(year, day_of_week, day_of_week_numeric) %>%
    summarise(total_rides = n(), .groups = 'drop')

# Convert day_of_week to factor with specified order
df_summary$day_of_week <- factor(df_summary$day_of_week, levels = day_order)

# Drop rows with NA in day_of_week_numeric column
df_summary <- na.omit(df_summary)

# Create the line plot without month grouping and faceting by year
plt <- ggplot(df_summary, aes(x = day_of_week_numeric, y = total_rides, group = year, color = year)) +
    # Geometric annotations that play the role of grid lines
    geom_vline(xintercept = seq(1, 7, by = 1), color = "grey91", linewidth = .6) +
    geom_segment(data = tibble(y = seq(50000, max(df_summary$total_rides), by = 50000), x1 = 1, x2 = 7), 
                 aes(x = x1, xend = x2, y = y, yend = y), 
                 inherit.aes = FALSE, color = "grey91", linewidth = .6) +
    geom_segment(data = tibble(y = 50000, x1 = 1, x2 = 7), 
                 aes(x = x1, xend = x2, y = y, yend = y), 
                 inherit.aes = FALSE, color = "grey60", linewidth = .8) +
    geom_line(linewidth = .9) +
    labs(title = "Total Rides by Day of Week",
         x = "",
         y = "Total Rides",
         color = "Year") +
    scale_color_discrete(name = "Year") +
    scale_x_continuous(breaks = 1:7, labels = day_order) +  # Set x-axis to display day names
    scale_y_continuous(limits = c(50000, max(df_summary$total_rides)), labels = label_number(scale_cut = cut_short_scale())) +  # Format y-axis labels
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
          panel.grid.minor.y = element_blank(),
          legend.position = "none")  # Remove legend

# Add year labels at the end of the lines
plt <- plt + 
    geom_text_repel(data = df_summary %>% group_by(year) %>% top_n(1, day_of_week_numeric),
                    aes(label = year),
                    nudge_x = 0.2,
                    direction = "y",
                    hjust = 0,
                    size = 4,
                    segment.size = .7,
                    segment.alpha = .5,
                    segment.linetype = "dotted",
                    box.padding = .4,
                    segment.curvature = -0.1,
                    segment.ncp = 3,
                    segment.angle = 20)

plt


