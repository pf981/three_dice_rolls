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




## Simulate 1 Die Roll ####


# rolls <- runif(n=100000, min=1, max=6)
# hist(rolls)
rolls <- sample.int(6, 10^6, replace=T)
hist(rolls)


rollDie <- function(n=10^6) {
    sample.int(6, n, replace=T)
}




## Simulate 3 Dice Rolls ####


rolls <- data.frame(one=rollDie(),
                    two=rollDie(),
                    three=rollDie()) %>%
    mutate(total=one+two+three,
           calculated_single=(total%%6) + 1,
           calculated_one_in_three=(calculated_single%%3)+1,
           calculated_double=total - ifelse(calculated_one_in_three==1,
                                            one,
                                            ifelse(calculated_one_in_three==2,
                                                   two,
                                                   three)))

hist(rolls$calculated_single)
table(rolls$calculated_single)/nrow(rolls)

hist(rolls$calculated_double)
table(rolls$calculated_double)/nrow(rolls)


table(rolls$calculated_one_in_three)/nrow(rolls)


## Simulate 2 Dice Rolls ####


rolls <- data.frame(one=rollDie(),
                    two=rollDie()) %>%
    mutate(total=one+two)

hist(rolls$total)
table(rolls$total)/nrow(rolls)







faces <- rep.int(seq_len(6), 3)

combn(faces, 1)
combn(faces, 2)%>% View



combn(faces, 1)



expand.grid(seq_len(6), seq_len(6)) %>%
    distinct


getCombinations <- function(ndice) {
    # Create a list of dice
    dice <- replicate(ndice, seq_len(6), simplify=F)

    # Use each dice as a parameter to expand.grid
    combinations <- do.call(expand.grid, dice)

    names(combinations) <- paste0('roll', seq_len(ndice))

    # Remove duplicates
    distinct(combinations)
}

replicate(2, seq_len(6))




## Try to Calculate 1 Die from 3 Roles ####

rolls <- getCombinations(ndice=3) %>%
    mutate(total = roll1 + roll2 + roll3,
           calculated_single = (total %% 6) + 1)

# Below results in 36 observations for each of the values 1 to 6. This means we have a uniform
# distrubition, as required.
table(rolls$calculated_single)




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


table(labeled_rolls$calculated_two_dice_sum)/nrow(labeled_rolls)


two_dice_sums <- apply(getCombinations(ndice=2), 1, sum)
table(two_dice_combinations)/nrow(two_dice_combinations)












