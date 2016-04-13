####################################################################################################
#
# TITLE:       Three Dice Rolls
#
# DESCRIPTION: See https://www.youtube.com/watch?v=xHh0ui5mi_E
#
####################################################################################################

if (!require(pacman))
    install.packages(pacman)

pacman::p_load('car',
               'dplyr',
               'ggplot2',
               'magrittr',
               'plyr',
               'scatterplot3d',
               'rgl',
               'tidyr')



# This function takes an integer number of dice then returns a dataframe which represents all
# possible unique combinations of that many dice. The dataframe will have ndice columns where each
# column is a single dice.
getCombinations <- function(ndice) {
    # Create a list of dice
    dice <- replicate(ndice, seq_len(6), simplify=F)

    # Use each dice as a parameter to expand.grid
    combinations <- do.call(expand.grid, dice)

    names(combinations) <- paste0('roll', seq_len(ndice))

    # Remove duplicates
    distinct(combinations)
}



## Calculate 1 Die from 3 Roles ####

rolls <- getCombinations(ndice=3) %>%
    mutate(total = roll1 + roll2 + roll3, # Get the sum of the rolls
           calculated_single_die_sum = (total %% 6) + 1) # Mod the sum with 6 and add 1 to get the value of a single die

# Ensure that the calculated distribution matches the expected distribution of one die roll (uniform)
# Below results in 36 observations for each of the values 1 to 6. This means we have a uniform
# distrubition, as required.
table(rolls$calculated_single_die_sum)




## Try to Calculate 2 Dice from 3 Roles ####


# Get every possible combination of rolls of 3 dice. At this point, we can't differentiate between
# dice, so we can't say "this is die 1" and "this is die 2".
rolls <- getCombinations(ndice=3)

# Next, we simply extract the min, median and max of the three dice. Thus assigning a label to each
# of the dice. If you have 2 or more dice with the same value, this is not an issue as this does not
# impact our ability to calculate the min, max and median.
labeled_rolls <- data.frame(label_1=apply(rolls, 1, min),
                            label_2=apply(rolls, 1, median),
                            label_3=apply(rolls, 1, max))

# Now that we have labeled the values, we calculate the total of each roll
labeled_rolls$total <- apply(rolls, 1, sum)

# We mod the total with 3 and add 1. This will give us a uniform distribution of values between 1
# and 3
labeled_rolls$calculated_one_two_or_three <- (labeled_rolls$total %% 3) + 1

# We can check to make sure we have a uniform distribution of 1s, 2s and 3s
table(labeled_rolls$calculated_one_two_or_three)


# Now we look at calculated_one_two_or_three and modify the total in the following way. If it is a
# 1, we exclude the value of the smallest die; if it is a 2, we exclude the value of the median die;
# and if it is a 3, we exclude the value of the largest die.
# labeled_rolls$calculated_two_dice_sum <- labeled_rolls$total - labeled_rolls[,labeled_rolls$calculated_one_two_or_three]
# labeled_rolls$calculated_two_dice_sum <- labeled_rolls$total - labeled_rolls[,paste0('label_', labeled_rolls$calculated_one_two_or_three)]

labeled_rolls$calculated_two_dice_sum <- labeled_rolls$total -
    ifelse(labeled_rolls$calculated_one_two_or_three==1,
           labeled_rolls$label_1,
           ifelse(labeled_rolls$calculated_one_two_or_three==2,
                  labeled_rolls$label_2,
                  labeled_rolls$label_3))


calculated_two_dice_distribution <- table(labeled_rolls$calculated_two_dice_sum)/nrow(labeled_rolls)


## Compare the Distribution of Values from the Two Dice Calculated from Three Compared with an Actual Two Dice Distribution ####

two_dice_sums <- apply(getCombinations(ndice=2), 1, sum)

# two_dice_distribution is the target variable. If we map the 3 dice distribution to this, we have
# won
two_dice_distribution <- table(two_dice_sums)/length(two_dice_sums)


all(calculated_two_dice_distribution==two_dice_distribution)






## Try to Calculate 2 Dice from 3 Roles - Attempt 2 ####

rolls <- getCombinations(ndice=3) %>%
    mutate(total = roll1 + roll2 + roll3, # Get the sum of the rolls
           calculated_two_dice_sum = (total %/% 3)) # Mod the sum with 6 and add 1 to get the value of a single die

table(rolls$calculated_two_dice_sum)/nrow(rolls)
table(rolls$total)


table(two_dice_sums)/length(two_dice_sums)

# Basically, if we remove the FIRST roll, we win. But how do we know the FIRST roll?
table(rolls$total - rolls$roll1)/nrow(rolls)
# We can't!



## Okay, Let's Try Symbolic Regression ####

# min, med, max are the predictors
# The fit is the euclidian distance between the simulated distribution and the actual distribution

# BUT WHAT WOULD THE DEPENDENT VARIABLE BE??!!

labeled_rolls$comparison_group <- rep(names(two_dice_distribution), two_dice_distribution*nrow(labeled_rolls))

View(labeled_rolls[order(paste0(labeled_rolls$label_1, labeled_rolls$label_2, labeled_rolls$label_3)),])

two_dice_distribution

rep(c(1, 2), c(3, 4))


rep(names(two_dice_distribution), two_dice_distribution*nrow(labeled_rolls))


# HERE!
labeled_rolls2 <- labeled_rolls[order(paste0(labeled_rolls$label_1, labeled_rolls$label_2, labeled_rolls$label_3)),]

labeled_rolls2$comparison_group <- rep(names(two_dice_distribution), two_dice_distribution*nrow(labeled_rolls))

View(labeled_rolls2)


label_table <- table(paste(labeled_rolls$label_1, labeled_rolls$label_2, labeled_rolls$label_3)) %>%
    data.frame
label_table %>% View

label_table$Freq %>% table %>% View


table(labeled_rolls2$comparison_group) %>% View



labeled_rolls2[,1:3]


scatterplot3d(x=labeled_rolls2$label_1, y=labeled_rolls2$label_2, z=labeled_rolls2$label_3)

plot3d(wt, disp, mpg, col="red", size=3)



# Color them based on their frequency
label_table_plot <- label_table %>%
    data.frame %>%
    separate(col=Var1, into=c('label_1', 'label_2', 'label_3'), sep=' ', convert=T)

# FIXME: These colors make 0 sense...
plot3d(label_table_plot[,1:3], col=label_table_plot$Freq, size=10)


# plot3d(labeled_rolls2[,1:3], col=rainbow(nrow(label_table_plot)), size=10)


scatter3d(x=label_table_plot$label_1, y=label_table_plot$label_2, z=label_table_plot$label_3, point.col=label_table_plot$Freq)
scatter3d(x=label_table_plot$label_1, y=label_table_plot$label_2, z=label_table_plot$label_3)




## Plot 2 ####

rolls_two_dice <- getCombinations(ndice=2) %>%
    mutate(total = roll1 + roll2, # Get the sum of the rolls
           calculated_two_dice_sum = (total %/% 3),
           label_1=pmin(roll1, roll2),
           label_2=pmax(roll1, roll2)) # Mod the sum with 6 and add 1 to get the value of a single die

ggplot(data=rolls_two_dice, aes(x=roll1, y=roll2, col=total)) + geom_point()
ggplot(data=rolls_two_dice, aes(x=label_1, y=label_2, col=total)) + geom_point() +  scale_colour_gradientn(colours = terrain.colors(10))
ggplot(data=rolls_two_dice, aes(x=label_1, y=label_2, col=total)) + geom_point() +  scale_colour_gradient(low='green',high='red')


rolls_two_dice$total <- rolls_two_dice$total %>% as.factor
ggplot(data=rolls_two_dice, aes(x=label_1, y=label_2, col=total)) + geom_point(size=8)




two_dice_frequency <- table(paste(rolls_two_dice$label_1, rolls_two_dice$label_2)) %>%
    data.frame %>%
    separate(col=Var1, into=c('label_1', 'label_2'), sep=' ', convert=T) %>%
    mutate(Freq=as.factor(Freq))
ggplot(data=two_dice_frequency, aes(x=label_1, y=label_2, col=Freq)) + geom_point(size=8)






