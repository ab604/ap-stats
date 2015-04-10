#  Make some boxplots
# A.Bailey 10th April 2015
library(ProjectTemplate)
library(ggplot2)
load("cache/plot.ap.RData")
load("cache/plot.mhc.RData")
# Plot AP data -----------------------------------------------------------------

# Open device
png(file = "graphs/ap_boxplot.png", width = 800, height = 600, units = "px")

# Create plot
p1 <- ggplot(plot.ap,aes(x=ap.term_vec,y=ap.pubyear_vec)) +
        # Apply aesthetics
        geom_boxplot() +
        xlab("") +
        # Amend y-axis label
        ylab("Publication Year") +
        # Create yearly ticks 
        scale_y_continuous(breaks=seq(1980,2015,5)) +
        # Remove the legend title
        #theme(legend.title=element_blank()) +
        # Add a plot title
        ggtitle("Publication Year of First 30 Google Scholar Hits")

# Print plot
print(p1)

# Close connection
dev.off()  

# Plot MHC data ----------------------------------------------------------------
plot.title <- bquote(atop('Publication Year of First 30 Google Scholar Hits' , 
                          'for MHC I related search terms'))

# Open device
png(file = "graphs/mhc_boxplot.png", width = 800, height = 600, units = "px")

# Create plot
p2 <- ggplot(plot.mhc,aes(x=mhc.term_vec,y=mhc.pubyear_vec)) +
        # Apply aesthetics
        geom_boxplot() +
        xlab("") +
        # Amend y-axis label
        ylab("Publication Year") +
        # Create yearly ticks 
        scale_y_continuous(breaks=seq(1980,2015,5)) +
        # Add a plot title
        ggtitle(plot.title)

# Print plot
print(p2)

# Close connection
dev.off()  
