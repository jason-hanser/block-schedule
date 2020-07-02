

library(dplyr)
library(tidyr)
library(tibble)
library(stringr)


#####################################
#####################################
######## INITIAL POPULATION  ########
#####################################
#####################################

###################################
######## SETTING VARIABLES ########
###################################

set.seed(1958)

temp_progeny   <- 1000
temp_survivors <- 100



##############################
######## LOADING DATA ########
##############################

courses      <- read.csv("sample data\\courses.csv", stringsAsFactors = FALSE)
course_regs  <- read.csv("sample data\\course_regs.csv", stringsAsFactors = FALSE)




###############################
######## DATA CLEANING ########
###############################

## Tidying course data

courses %>%
  gather("BLOCK", "BLOCK_IND", 5:8) %>%
  arrange(CRSE_ID,
          BLOCK) %>%
  mutate(row_name =  paste0(CRSE_ID, "_", BLOCK)) %>%
  column_to_rownames(var = "row_name") -> courses 



#####################################
######## MATRIX CONSTRUCTION ########
#####################################

matrix_random_numbers   <- matrix(data     = runif(n = temp_progeny*nrow(courses)),
                                  ncol     = temp_progeny,
                                  nrow     = nrow(courses),
                                  dimnames = list(row.names(courses),
                                                  paste0("SCHD_", 1:temp_progeny)))

matrix_block_contraints <- matrix(data     = courses$BLOCK_IND,
                                  ncol     = temp_progeny,
                                  nrow     = nrow(courses),
                                  dimnames = list(row.names(courses),
                                                  paste0("SCHD_", 1:temp_progeny)))

matrix_unscheduled_courses <- matrix(data     = 1,
                                     ncol     = temp_progeny,
                                     nrow     = nrow(courses),
                                     dimnames = list(row.names(courses),
                                                       paste0("SCHD_", 1:temp_progeny)))

matrix_final_schedule   <- matrix(data     = NA,
                                  ncol     = temp_progeny,
                                  nrow     = nrow(courses),
                                  dimnames = list(paste0(row.names(courses)),
                                                  paste0("SCHD_", 1:temp_progeny)))




##################################
######## INITIAL SCHEDULE ######## 
##################################

## the scheduling loop

for (i in 1:4) {
  
  ## Since some course sections have fewer scheduling options, we need to schedule those sections first. To
  ## do this we create a matrix to priotize the section for each professor that has the fewest scheduling
  ## options. 
  
  courses %>%
    cbind(matrix_random_numbers*matrix_block_contraints*matrix_unscheduled_courses) %>%
    group_by(PROF_ID, 
             CRSE_ID) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(sum(. > 0))) %>%
    group_by(PROF_ID) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(ifelse(suppressWarnings(min(.[.>0])) == ., 1, 0))) %>%
    ungroup() %>%
    select_at(.vars = vars(matches("^SCHD_"))) %>%
    as.matrix() -> matrix_course_scheduling_priority
  
  
  ## Scheduling priority course sections, by selecting the block for each section the with the max value -
  ## based - in part - on matrix of random numbers
  
  courses %>% 
    cbind(matrix_random_numbers*matrix_block_contraints*matrix_unscheduled_courses*matrix_course_scheduling_priority) %>%
    group_by(PROF_ID) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(case_when(. == suppressWarnings(max(.[.>0])) ~ 1))) -> temp_schedule      
  
  
  ## Once a course section is scheduled in a particular block, we need to prevent additional courses taught
  ## by the same professor from being scheduled in that same block. 
  
  temp_schedule %>%
    group_by(PROF_ID,
             BLOCK) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(case_when(is.na(.) == TRUE & sum(., na.rm = TRUE) == 1 ~ 0,
                                     TRUE ~ .))) %>%
    ungroup() -> temp_schedule
  
  
  ## Similairly, once a course section has been scheduled, we need to prevent that section from being 
  ## scheduled in another block
  
  temp_schedule %>%
    group_by(PROF_ID,
             CRSE_ID) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(case_when(is.na(.) == TRUE & sum(., na.rm = TRUE) == 1 ~ 0,
                                     TRUE ~ .))) %>%
    ungroup() %>%
    select_at(.vars = vars(matches("^SCHD_"))) %>%
    as.matrix() -> temp_schedule
  
  
  ## Filling in the final schedule matrix with the newly scheduled courses (i.e. temp schedule)
  
  matrix_final_schedule[is.na(matrix_final_schedule)] <- temp_schedule[which(is.na(matrix_final_schedule))]
  
  
  ## Filling in unscheduled course matrix
  
  matrix_unscheduled_courses <- matrix_final_schedule
  matrix_unscheduled_courses[is.na(matrix_unscheduled_courses) == FALSE] <- 0
  matrix_unscheduled_courses[is.na(matrix_unscheduled_courses) == TRUE]  <- 1
  
  
  ## cleaning up the loop environment
  
  rm(temp_schedule, matrix_course_scheduling_priority)
  print(i)
  
  
  ## Breaking the loops once the schedule is done
  
  if (sum(matrix_unscheduled_courses) == 0) {
    
    print("The schedules are finished and a solution was found.")
    break
    
  }
  
}
rm(i, matrix_block_contraints, matrix_unscheduled_courses, matrix_random_numbers)




##################################
######## SCHEDULE CLEANUP ########
##################################

## De-dpulicating schedules

matrix_final_schedule <- t(unique(t(matrix_final_schedule)))


## Removing any schedules where a section was scheduled more multiple blocks

schedules <- schedules[, which(colSums(schedules) == length(unique((courses$CRN))))]




##################################
######## FITNESS FUNCTION ########
##################################

##

matrix_cost <- matrix(data = 0,
                      ncol = ncol(matrix_final_schedule),
                      nrow = length(unique(course_regs$STUDENT_ID)),
                      dimnames = list(1:length(unique(course_regs$STUDENT_ID)),
                                      colnames(matrix_final_schedule)))


##

for (i in unique(courses$BLOCK)) {
  
  ## Getting a list of course sections (i.e. CRSE_ID's) within block i for each schedule
  
  courses %>%
    cbind(matrix_final_schedule) %>%
    filter(BLOCK == i) %>%
    summarise_at(.vars = vars(matches("^SCHD_")),
                 .funs = funs(list(CRSE_ID[.==1]))
    ) -> temp_block_crse_id
  
  ## Getting a list of unique courses within block i for each schedule
  
  courses %>%
    cbind(matrix_final_schedule) %>%
    filter(BLOCK == i) %>%
    summarise_at(.vars = vars(matches("^SCHD_")),
                 .funs = funs(list(unique(CRSE[.==1])))
    ) -> temp_block_crse
  
  ## Checking if each students has a course section in block i for each schedule
  
  course_regs %>%
    select(STUDENT_ID,
           CLASS,
           CRSE_ID) %>%
    cbind(matrix(data = NA,
                 ncol = ncol(matrix_final_schedule),
                 nrow = nrow(.),
                 dimnames = list(paste0(.$STUDENT_ID, "_", .$CRSE_ID), 
                                 colnames(matrix_final_schedule)))) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(!as.numeric(CRSE_ID %in% unlist(temp_block_crse_id$.)))) %>%
    group_by(STUDENT_ID,
             CLASS) %>%
    summarise_at(.vars = vars(matches("^SCHD_")),
                 .funs = funs(as.numeric(min(.)))) %>%
    ungroup() -> matrix_regs_block_crse_id
  
  ## Since we want to minimize course resistration conflicts for upperclassmen, we weight
  ## those conflicts more heavily
  
  matrix_regs_block_crse_id %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(case_when(CLASS == "SR" ~ .*3,
                                     CLASS == "JR" ~ .*1.5,
                                     TRUE ~ .))) %>%
    select(-STUDENT_ID,
           -CLASS) %>%
    as.matrix() -> matrix_regs_block_crse_id
  
  ## Counting the number of courses for each student in block i for each schedule and then transforming 
  ## that number into a score
  
  course_regs %>%
    select(STUDENT_ID,
           CRSE) %>%
    cbind(matrix(data = NA,
                 ncol = ncol(matrix_final_schedule),
                 nrow = nrow(.),
                 dimnames = list(paste0(.$STUDENT_ID, "_", .$CRSE_ID), 
                                 colnames(matrix_final_schedule)))) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(as.numeric(CRSE %in% unlist(temp_block_crse$.)))) %>%
    group_by(STUDENT_ID) %>%
    summarise_at(.vars = vars(matches("^SCHD_")),
                 .funs = funs(1/(1+sum(.)))) %>%
    ungroup() %>%
    select(-STUDENT_ID) %>%
    as.matrix() -> matrix_regs_block_crse
  
  ## Calculating the final score for each student for block i and adding it to the master cost matrix
  
  matrix_cost <- matrix_cost+(matrix_regs_block_crse_id*matrix_regs_block_crse)
  
  ## Cleaning up loop enviroment
  
  rm(temp_block_crse_id, temp_block_crse, 
     matrix_regs_block_crse_id, matrix_regs_block_crse)
  
  print(i)
  
}

rm(i)




#############################
#### SELECTION EVENT ########
#############################


## Ranking potential schedules by fitness function

data.frame(SCHD          = colnames(matrix_cost),
           FITNESS       = colSums(matrix_cost),
           TOT_CRSE_REGS = nrow(course_regs),
           MODELS_BULIT  = ncol(matrix_cost),
           stringsAsFactors = FALSE) %>%
  arrange(FITNESS) %>%
  filter(row_number() <= temp_survivors) -> temp_best_schds



## Selecting the schedules with the best performance

matrix_final_schedule %>%
  as.data.frame() %>%
  select(temp_best_schds$SCHD) %>%
  rename_all(.funs = funs(paste0("SCHD_", 1:temp_survivors))) %>%
  merge(x  = courses,
        by = 0) %>%
  column_to_rownames("Row.names") -> final_schedules




##################################
#### WRITING DATA TO FILE ########
##################################

## Writing selected schedules to file

final_schedules %>%
  write.csv(file = "output/best_schedules.csv")


## Writing summary stats for the best schedule

temp_best_schds %>%
  filter(row_number() == 1) %>%
  write.csv(file = "output/fitness_over_time.csv",
            row.names = FALSE)


rm(list=ls())





















###############################
###############################
######## EVOLVING LOOP ########
###############################
###############################

for (i in 1:1000) {
  
  ###############################################
  ######## SETTING VARIABLES & FUNCTIONS ########
  ###############################################
  
  ## Setting up variables
  
  temp_survivors <- 100
  
  temp_crossover <- 70
  temp_migration <- 100
  temp_mutations <- 5
  
  temp_crossover_freq <- rpois(n = 100, lambda = 2)
  temp_mutation_freq  <- rpois(n = 100, lambda = 4)
  
  
  ## Function to sample profs to mutate

  sample_subj <- function(temp_var) {sample(x     = unique(courses$SUBJ),
                                             size = sample(x    = temp_var,
                                                           size = 1)+1,
                                             replace = FALSE)
  }
  
    
  sample_profs <- function(temp_var) {sample(x    = unique(courses$PROF_ID),
                                             size = sample(x    = temp_var,
                                                           size = 1)+1,
                                             replace = FALSE)
  }
  

  
  ##############################
  ######## LOADING DATA ########
  ##############################
  
  ## Loading the data
  
  read.csv("output/best_schedules.csv", stringsAsFactors = FALSE) %>%
    column_to_rownames("X") -> initial_schedules
  
  read.csv("sample data/course_regs.csv", 
           stringsAsFactors = FALSE) -> course_regs
  
  read.csv("output/fitness_over_time.csv", 
           stringsAsFactors = FALSE) -> best_schd_summary_stats

  
  
  ###############################
  ######## DATA CLEANING ########
  ###############################
  
  ## Getting a list of courses
  
  initial_schedules %>%
    select_at(.vars = vars(-matches("^SCHD_"))) -> courses
  
  
  ## creating the initial schedule matrix
  
  initial_schedules %>%
    select_at(.vars = vars(matches("^SCHD_"))) %>%
    as.matrix() -> initial_schedules
  


  
  #################################
  ######## CROSSOVER EVENT ########
  #################################
  
  ## Duplicating the schedules & renaming columns
  
  crossover_schedules <- do.call(cbind, replicate(temp_crossover, initial_schedules, simplify=FALSE))
  
  colnames(crossover_schedules) <- paste0("SCHD_C_", 1:ncol(crossover_schedules))
  
  
  ## Selecting disciplines for cross over events

  courses %>%
    cbind(crossover_schedules) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(ifelse(SUBJ %in% sample_subj(temp_crossover_freq), 1, 0))) %>%
    group_by(PROF_ID) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(max(.))) %>%
    ungroup() %>%
    select_at(.vars = vars(matches("^SCHD_"))) %>%
    as.matrix() -> matrix_crossover
  
  
  ## Deleting prof schedules where crossover events are to occur
  
  crossover_schedules[matrix_crossover == 1] <- NA
  
  
  ## Preforming crossover

  matrix_crossover <- initial_schedules[, sample(x       = ncol(initial_schedules), 
                                                 size    = ncol(crossover_schedules),
                                                 replace = TRUE)]
  
  crossover_schedules[is.na(crossover_schedules)] <- matrix_crossover[which(is.na(crossover_schedules))]

  rm(matrix_crossover, temp_crossover, temp_crossover_freq, sample_subj)

    
  
  ##################################
  ######## POINT MUTATTIONS ########
  ##################################
  
  ## Duplicating the schedules & renaming columns
  
  mutation_schedules <- do.call(cbind, replicate(floor(temp_mutations), initial_schedules, simplify=FALSE))
  
  colnames(mutation_schedules) <- paste0("SCHD_M_", 1:ncol(mutation_schedules))
  
  
  ## Selecting profs for point mutations
  
  courses %>%
    cbind(mutation_schedules) %>%
    mutate_at(.vars = vars(matches("^SCHD_")),
              .funs = funs(ifelse(PROF_ID %in% sample_profs(temp_mutation_freq), 1, 0))) %>%
    select_at(.vars = vars(matches("^SCHD_"))) %>%
    as.matrix() -> matrix_mutations
  
  
  ## Deleting prof schedules where mutations occur
  
  mutation_schedules[matrix_mutations == 1] <- NA
  
  
  rm(matrix_mutations, temp_mutations, temp_mutation_freq, sample_profs)
  

  
  #################################
  ######## MIGRATION EVENT ########
  #################################
  
  ## Creating new schedules for migration event
  
  migration_schedules <- matrix(data = NA,
                                ncol = temp_migration,
                                nrow = nrow(initial_schedules),
                                dimnames = list(row.names(initial_schedules),
                                                paste0("SCHD_N_", 1:temp_migration)))
  
  rm(temp_migration)
  
  
  ####################################
  ######## SCHEDULING COURSES ########
  ####################################
  
  ## We don't need to solve the cross-over or initial schedules, but we do need to solve the 
  ## mutated and migration schedules - because they containing N/A values
  
  matrix_solve <- cbind(mutation_schedules, migration_schedules)
  
  rm(migration_schedules, mutation_schedules)
  
  
  ## Creating some matricies to schedule courses

  matrix_random_numbers   <- matrix(data     = runif(n = matrix_solve),
                                    ncol     = ncol(matrix_solve),
                                    nrow     = nrow(matrix_solve),
                                    dimnames = list(row.names(matrix_solve),
                                                    colnames(matrix_solve)))
  
  matrix_block_contraints <- matrix(data     = courses$BLOCK_IND,
                                    ncol     = ncol(matrix_solve),
                                    nrow     = nrow(matrix_solve),
                                    dimnames = list(row.names(matrix_solve),
                                                    colnames(matrix_solve)))
  
  matrix_unscheduled_courses <- matrix(data     = 0,
                                       ncol     = ncol(matrix_solve),
                                       nrow     = nrow(matrix_solve),
                                       dimnames = list(row.names(matrix_solve),
                                                       colnames(matrix_solve)))
  
  matrix_unscheduled_courses[which(is.na(matrix_solve))] <- 1
  
  
  ## Running the loop to schedule courses
  
  for (j in 1:4) {
    
    ## Generating a matrix for the courses that need to be scheduled first - because 
    ## they have fewer open slots
    
    courses %>%
      cbind(matrix_random_numbers*matrix_block_contraints*matrix_unscheduled_courses) %>%
      group_by(PROF_ID, 
               CRSE_ID) %>%
      mutate_at(.vars = vars(matches("^SCHD_")),
                .funs = funs(sum(. > 0))) %>%
      group_by(PROF_ID) %>%
      mutate_at(.vars = vars(matches("^SCHD_")),
                .funs = funs(ifelse(suppressWarnings(min(.[.>0])) == ., 1, 0))) %>%
      ungroup() %>%
      select_at(.vars = vars(matches("^SCHD_"))) %>%
      as.matrix() -> matrix_course_scheduling_priority  
    
    
    ## Scheduling priority courses            
    
    courses %>% 
      cbind(matrix_random_numbers*matrix_block_contraints*matrix_unscheduled_courses*matrix_course_scheduling_priority) %>%
      group_by(PROF_ID) %>%
      mutate_at(.vars = vars(matches("^SCHD_")),
                .funs = funs(case_when(. == suppressWarnings(max(.[.>0])) ~ 1))) -> temp_schedule
    
    
    ## Blocking off filled blocks for each prof
    
    temp_schedule %>%
      group_by(PROF_ID,
               BLOCK) %>%
      mutate_at(.vars = vars(matches("^SCHD_")),
                .funs = funs(case_when(is.na(.) == TRUE & sum(., na.rm = TRUE) == 1 ~ 0,
                                       TRUE ~ .))) %>%
      ungroup() -> temp_schedule
    
    
    ## Blocking off courses (in other blocks) that have been set
    
    temp_schedule %>%
      group_by(PROF_ID,
               CRSE_ID) %>%
      mutate_at(.vars = vars(matches("^SCHD_")),
                .funs = funs(case_when(is.na(.) == TRUE & sum(., na.rm = TRUE) == 1 ~ 0,
                                       TRUE ~ .))) %>%
      ungroup() %>%
      select_at(.vars = vars(matches("^SCHD_"))) %>%
      as.matrix() -> temp_schedule
    
    
    ## Filling in new schedules with newly scheduled courses
    
    matrix_solve[is.na(matrix_solve)] <- temp_schedule[which(is.na(matrix_solve))]
    
    
    ## Filling in which courses/blocks have still yet to be filled
    
    matrix_unscheduled_courses <- matrix_solve
    matrix_unscheduled_courses[is.na(matrix_unscheduled_courses) == FALSE] <- 0
    matrix_unscheduled_courses[is.na(matrix_unscheduled_courses) == TRUE]  <- 1
    
    
    ##
    
    rm(temp_schedule, matrix_course_scheduling_priority)
    print(j)
    
    
    ## Breaking the loops once the schedule is done
    
    if (sum(matrix_unscheduled_courses) == 0) {
      
      print("The schedules are finished and a solution was found.")
      break
      
    }
    
    
  }
  rm(j, matrix_block_contraints, matrix_unscheduled_courses, matrix_random_numbers)
  
  
  

  #########################################################
  ######## RECOMBING ALL SCHEDULES & DEPUPLICATING ########
  #########################################################
  
  ## Recombining the schedules
  
  schedules <- cbind(initial_schedules,
                     crossover_schedules,
                     matrix_solve)
  
  rm(initial_schedules,
     crossover_schedules,
     matrix_solve)
  
  
  ## Depulicating schedules
  
  schedules <- t(unique(t(schedules)))
  
  ## Removing any schedules where a section was scheduled more multiple blocks
  
  schedules <- schedules[, which(colSums(schedules) == length(unique((courses$CRN))))]
  
  
  
  
  ##################################
  ######## FITNESS FUNCTION ########
  ##################################
  
  ##
  
  matrix_cost <- matrix(data = 0,
                        ncol = ncol(schedules),
                        nrow = length(unique(course_regs$STUDENT_ID)),
                        dimnames = list(1:length(unique(course_regs$STUDENT_ID)),
                                        colnames(schedules)))
  
  
  ##
  
  for (k in unique(courses$BLOCK)) {
    
    ## getting a list of CRSE_IDs within block i for each schedule
    
    courses %>%
      cbind(schedules) %>%
      filter(BLOCK == k) %>%
      summarise_at(.vars = vars(matches("^SCHD_")),
                   .funs = funs(list(CRSE_ID[.==1]))
                   ) -> temp_block_CRSE_ID
    
    
    ## getting a list of unique crses within block i for each schedule
    
    courses %>%
      cbind(schedules) %>%
      filter(BLOCK == k) %>%
      summarise_at(.vars = vars(matches("^SCHD_")),
                   .funs = funs(list(unique(CRSE[.==1])))
                   ) -> temp_block_crse
    
    
    ## determining whether no exact matches exist for each student for block i of each schedule
    
    course_regs %>%
      select(STUDENT_ID,
             CLASS,
             CRSE_ID) %>%
      cbind(matrix(data = NA,
                   ncol = ncol(schedules),
                   nrow = nrow(.),
                   dimnames = list(paste0(.$STUDENT_ID, "_", .$CRSE_ID), 
                                   colnames(schedules)))) %>%
      mutate_at(.vars = vars(matches("^SCHD_")),
                .funs = funs(as.numeric(!CRSE_ID %in% unlist(temp_block_CRSE_ID$.)))) %>%
      group_by(STUDENT_ID,
               CLASS) %>%
      summarise_at(.vars = vars(matches("^SCHD_")),
                   .funs = funs(min)) %>%
      ungroup() -> matrix_regs_block_CRSE_ID
    
    
    ## calculating weights
    
    matrix_regs_block_CRSE_ID %>%
      mutate_at(.vars = vars(matches("^SCHD_")),
                .funs = funs(case_when(CLASS == "SR" ~ .*3,
                                       CLASS == "JR" ~ .*1.5,
                                       TRUE ~ .))) %>%
      select(-STUDENT_ID,
             -CLASS) %>%
      as.matrix() -> matrix_regs_block_CRSE_ID
    
    
    ##
    
    course_regs %>%
      select(STUDENT_ID,
             CRSE) %>%
      cbind(matrix(data = NA,
                   ncol = ncol(schedules),
                   nrow = nrow(.),
                   dimnames = list(paste0(.$STUDENT_ID, "_", .$CRSE_ID), 
                                   colnames(schedules)))) %>%
      mutate_at(.vars = vars(matches("^SCHD_")),
                .funs = funs(as.numeric(CRSE %in% unlist(temp_block_crse$.)))) %>%
      group_by(STUDENT_ID) %>%
      summarise_at(.vars = vars(matches("^SCHD_")),
                   .funs = funs(1/(1+sum(.)))) %>%
      ungroup() %>%
      select(-STUDENT_ID) %>%
      as.matrix() -> matrix_regs_block_crse
    
    
    ##
    
    matrix_cost <- matrix_cost+(matrix_regs_block_CRSE_ID*matrix_regs_block_crse)
    
    
    ## Cleaning up loop enviroment
    
    rm(temp_block_CRSE_ID, temp_block_crse, 
       matrix_regs_block_CRSE_ID, matrix_regs_block_crse)

    print(k)
    
    
  }
  rm(k)
  

  
  
  ###################################
  ######## THINNING THE HERD ########
  ###################################
  
  ## Ranking potential schedules by fitness function
  
  data.frame(SCHD          = colnames(matrix_cost),
             FITNESS       = colSums(matrix_cost),
             TOT_CRSE_REGS = nrow(course_regs),
             MODELS_BULIT  = ncol(matrix_cost),
             stringsAsFactors = FALSE) %>%
    arrange(FITNESS) %>%
    filter(row_number() <= temp_survivors) -> temp_best_schds
  
  best_schd_summary_stats <- rbind(best_schd_summary_stats, temp_best_schds[1, ])
  
  
  ## Selecting the schedules with the best performance
  
  schedules %>%
    as.data.frame() %>%
    select(temp_best_schds$SCHD) %>%
    rename_all(.funs = funs(paste0("SCHD_", 1:temp_survivors))) %>%
    merge(x  = courses,
          by = 0) %>%
    column_to_rownames("Row.names") -> final_schedules
  
  
  
  
  ##################################
  #### WRITING DATA TO FILE ########
  ##################################
  
  ## Writing selected schedules to file
  
  final_schedules %>%
    write.csv(file = "output/best_schedules.csv")
  
  
  ## Writing summary stats for the best schedule
  
  best_schd_summary_stats %>%
    write.csv(file = "output/fitness_over_time.csv",
              row.names = FALSE)
  
  
  ## End of loop cleanup
  
  rm(courses, course_regs,
     schedules, final_schedules,
     temp_survivors, matrix_cost,
     best_schd_summary_stats, temp_best_schds)
  
  gc()
  print(paste0("Generation ", i, " is complete."))
  
  
}
rm(i)








