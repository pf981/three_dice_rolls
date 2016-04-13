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


# Roll 3 dice. At this point, we can't differentiate between dice, so we can't say "this is dice 1"
# and "this is dice 2".
rolls <- getCombinations(ndice=3)

# In order to label each dice 1 to 3, we order them and label them smallest to largest. If you have
# 2 or more dice with the same value, this is not an issue as they are equivalent, so it doesn't
# matter which one you label to be "smaller".
# rolls %<>%
#     rowwise %>%
#     mutate(label1 = min(roll1, roll2, roll3),
#            label2 = median(c(roll1, roll2, roll3)),
#            label3 = max(roll1, roll2, roll3))
#
#
# rolls %<>%
#     summarise(label1 = min(roll1, roll2, roll3),
#            label2 = median(c(roll1, roll2, roll3)),
#            label3 = max(roll1, roll2, roll3))
#
# rolls %<>%
#     mutate(label1 = pmin(roll1, roll2, roll3),
#            label2 = median(c(roll1, roll2, roll3)),
#            label3 = pmax(roll1, roll2, roll3))
# rolls
#
#
# rolls %<>%
#     mutate_each(label1 = funs(min), roll1, roll2, roll3)
# rolls
#
# rolls %<>%
#     mutate(label1 = pmin(roll1, roll2, roll3),
#            label3 = pmax(roll1, roll2, roll3))
# rolls

labeled_rolls <- data.frame(label_1=apply(rolls, 1, min),
                            label_2=apply(rolls, 1, median),
                            label_3=apply(rolls, 1, max))



rolls$label1 <- rowm

adply(rolls, 1, sort)

rolls %<>%
    sort()




rolls <- getCombinations(ndice=3) %>%
    mutate(total = roll1 + roll2 + roll3,
           calculated_one_in_three = (total %% 3) + 1)

# Below results in 36 observations for each of the values 1 to 6. This means we have a uniform
# distrubition, as required.
table(rolls$calculated_one_in_three)






by_species <- iris %>% group_by(Species)
by_species %>% summarise_each(funs(length))
by_species %>% summarise_each(funs(mean))
by_species %>% summarise_each(funs(mean), Petal.Width)



