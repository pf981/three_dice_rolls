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
               'tidyr')




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

    # Remove duplicates
    distinct(combinations)
}

replicate(2, seq_len(6))

getCombinations(ndice=3)


















