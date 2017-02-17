# This function calculates and displays the AUC between a given range for 
# x across k groups. Marginal densities are displayed separately.
# Arguments to be specified are: a continuous variable (x), 
# a grouping variable (group), and the min and max values of the range that is
# to be integrated.

# Packages
library(ISLR) # Hitters Data set
library(dplyr) 
library(magrittr) 
library(ggplot2)
library(forcats) 
library(sfsmisc)

# Data 
data <- ISLR::Hitters %>%
  mutate(League = fct_recode(League,
                             "American" = "A", 
                             "National" = "N"))

# Function

fun <- function(x, group, min, max) {
  data <- data.frame(x = x,
                     group = group)
  
  p1 <- ggplot(data = data, aes(x = x)) +
    geom_density() +
    facet_grid(~group)
  
  build <- ggplot_build(p1)[["data"]][[1]]
  build[, "sort"] <- ifelse(build[, "x"] >= min & build[, "x"] <= max, "target",
                            ifelse(build[, "x"] > max, "upper", "lower"))
  build[, "min"] <- 0
  build[, "group"] <- NULL
  build[, "group"] <- build[, "PANEL"]
  
  levels(build[, "group"]) <- levels(data[, "group"])
  
  prop <- list()
  
  for (i in levels(build[, "group"])) {
    subset_x <- subset(build[,"x"],
                       build[, "group"] == i & build[, "sort"] %in% "target")
    
    subset_y <- subset(build[,"y"], 
                       build[, "group"] == i & build[, "sort"] %in% "target")
    
    prop[["total"]][i] <- integrate.xy(subset_x, subset_y)
  }
  
  text_df <- data.frame(group = names(prop[["total"]]),
                        value = round(as.numeric(unname(prop[["total"]])),
                                      digits = 3))
  text_df[, "text"] <- paste("~", text_df[, "value"]*100, 
                             "% of the distribution falls in-between", min, "&", max)
  
  data2 <- merge(data, text_df, by = "group")
  text_plt <- merge(build, text_df, by = "group")
  
  plots <- list()
  
  for (i in levels(data2[, "group"])) {
    plots[[i]] <- ggplot(data = subset(data2, group == i), aes(x = x)) +
      geom_density() +
      geom_text(data = subset(text_plt, group == i),
                aes(x = median(x),
                    y = median(y),
                    label = text,
                    color = "red")) +
      geom_ribbon(data = subset(build, group == i & sort %in% "target"), 
                  aes(x = x,
                      ymin = min,
                      ymax = y), 
                  fill = "red",
                  alpha = .3) +
      geom_ribbon(data = subset(build, group == i & sort %in% "lower"), 
                  aes(x = x,
                      ymin = min,
                      ymax = y),
                  fill = "blue",
                  alpha = .3) +
      geom_ribbon(data = subset(build, group == i & sort %in% "upper"), 
                  aes(x = x,
                      ymin = min,
                      ymax = y),
                  fill = "blue",
                  alpha = .3) +
      theme(legend.position = "none") +
      ylab("Density") +
      ggtitle(paste("Marginal Density Plot for", i, sep = " "))
  }
  return(plots)
}

# Example
foo <- fun(x = data$RBI, 
           group = data$League, 
           min = 4, 
           max = 12)

foo$National
