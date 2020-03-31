library("ggplot2")

# Import data
data <- read.table("corona_data.txt", sep = "\t", header = TRUE, as.is = TRUE)

# Re-work data
data$new.cases <- NA
data$new.cases[1] <- data$CH[1]

## Compute new daily cases
for (row in 2:nrow(data)) {
  data$new.cases[row] <- data$CH[row] - data$CH[row-1]
}

## Average weekly new cases 
data$new.cases.av7 <- NA
for (row in 7:nrow(data)) {
  data$new.cases.av7[row] <- mean(data$new.cases[(row-6):row])
}

## Discard uncomplete data
data <- data[data$ds.complete,]


## Modify date format
data$date2 <- NA

for (row in 1:nrow(data)) {
  v <- unlist(strsplit(data$Date[row], "-"))
  data$date2[row] <- paste0(v[3],".",v[2])
}

## Specify data for linear regression

data$exp.phase <- FALSE
data$exp.phase[1:23] <- TRUE

## Compute linear regression

r <- lm(data$new.cases.av7[data$exp.phase] ~ data$CH[data$exp.phase])
i <- r$coefficients[1]
s <- r$coefficients[2]

## Compute linear regression

r.log <- lm(log10(data$new.cases.av7[data$exp.phase]) ~ log10(data$CH[data$exp.phase]))
i.log <- r.log$coefficients[1]
s.log <- r.log$coefficients[2]


# Plot the data

## Linear scale
plot <- ggplot(data, aes(x=CH, y=new.cases.av7, label = date2)) +
  geom_abline(intercept=i,slope=s, colour="red",size=1.3,linetype="dashed") +
  geom_line() +
  geom_point() +
  ylab("New confirmed cases (in the past 7 days)") +
  xlab("Total cases") +
  ylim(0, max(data$CH)*s) +
  geom_text(position = position_nudge(x = 700, y = -(max(data$CH)*s/40)), color = "azure4", size = 3.5, angle = -20) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(plot)

png(filename = "new-cases_vs_total-cases_lin-plot.png", units = "in", res = 300, width = 6, height = 5)
print(plot)
dev.off()


  ## Log scale
plot.log <- ggplot(data, aes(x=CH, y=new.cases.av7, label = date2)) +
  geom_abline(intercept=i.log,slope=s.log, colour="red",size=1.3,linetype="dashed") +
  geom_line() +
  geom_point() +
  scale_y_log10(name = "New confirmed cases in the past 7 days (log10)") +
  scale_x_log10(name = "Total cases (log10)") +
  geom_text(position = position_nudge(x = 0.2, y = -0.075), color = "azure4", size = 3.5, angle = -20) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(plot.log)

png(filename = "new-cases_vs_total-cases_log-plot.png", units = "in", res = 300, width = 6, height = 5)
print(plot.log)
dev.off()
