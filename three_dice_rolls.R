####################################################################################################
#
# TITLE:       Three Dice Rolls
#
# DESCRIPTION: See https://www.youtube.com/watch?v=xHh0ui5mi_E
#
####################################################################################################

if (!require(pacman))
    install.packages(pacman)

pacman::p_load('dplyr',
               'plyr',
               'tidyr',
               'magrittr')



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












