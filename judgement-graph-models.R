library(tidyverse)
library(statnet)
library(texreg) 

### load data ------------------------------------------------------------
wd <- getwd()
files <- list.files(path = file.path(wd, "Daten"), pattern = "*.csv", full.names=TRUE)
files

# load individual csv files and combine them in one data frame
# vroom packages speeds up the loading process 
library(vroom)
process_data <- sapply(files, vroom, simplify=FALSE) %>% 
  bind_rows(.id = "id")


### prepare data ---------------------------------------------------------

# Every second line contains the entry "schueler == 0". That is an artefact of the setup. 
# Participants first select an item and in a second recorded step they choose a student which leads to a correct
# or incorrect response. That means only every second row contains information item, student and a solution. 

process_data <- process_data %>% 
  # keeping only rowts with all three relevant information
  filter(richtig != -1) %>% 
  # cleaning the id column keeping only the number
  # removing the data path which is the same for every entry
  mutate(id = str_remove(id, "/Users/mmueckshoff/Documents/Analytics/judgement-graph-models/Daten/"),
         # removing the suffix ".csv" leaving only the number
         id = str_remove(id, ".csv"),
         id = as.numeric(id))

# create a function to turn row-wise data in a data frame into an adjacency matrix indication transitions 
# from entries in one row to the next row

create_adjacency <- function(x){
  # 6 students * 72 items makes 72 total combinations, including an extra start and finish point 
  y <- matrix(0,74,74)
  # starting in row 1 of each data frame 
  zeile <- 1
  spalte <- (x$schueler[1]-1)*12 + x$frage[1]+1
  y[zeile,spalte] <- 1
  
  for (i in 1:(nrow(x)-1)){
    zeile <- (x$schueler[i]-1)*12 + x$frage[i]+1
    spalte <- (x$schueler[i+1]-1)*12 + x$frage[i+1]+1
    y[zeile,spalte] <- y[zeile,spalte]+1
  }
  zeile <- (x$schueler[dim(x)[1]]-1)*12 + x$frage[dim(x)[1]]+1
  spalte <- 74
  y[zeile,spalte] <- 1
  y
}

# nest participant data in "process_data" to have one data frame per participant 
data_nested <- process_data %>% 
  group_by(id) %>% 
  nest()

# create adjacency matrices for each participant 
data_nested <- data_nested %>% 
  mutate(matrix = map(data, create_adjacency))

# create group adjacency matrices based on accurate vs inaccurate judgements 
accurate_group <- c(3, 7, 9, 13, 170, 178, 181, 359, 670, 676, 839, 850, 
                    1102, 1103,1105, 1107, 1108, 1115, 1117, 1119, 1120, 1121, 1123, 1124, 1125)
inaccurate_group <- c(356, 503, 509, 511, 844, 1104, 1106, 1110, 1112, 1116, 1118, 1122)

data_nested_accurate <- data_nested %>% 
  filter(id %in% accurate_group)

data_nested_inaccurate <- data_nested %>% 
  filter(id %in% inaccurate_group)

# summing up adjacency matrics in each nested data frame to create one group adjacency matrix 
matrix_accurate <- purrr::reduce(data_nested_accurate$matrix, `+`)
matrix_inaccurate <-  purrr::reduce(data_nested_inaccurate$matrix, `+`)

# for this particular analysis I don't need the extra start and end entry 
matrix_accurate <- matrix_accurate[2:73,2:73]
matrix_inaccurate <- matrix_inaccurate[2:73,2:73]

# setting row and column names for the matrices 
names <- c( "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", 
            "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
            "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
            "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
            "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
            "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12")

rownames(matrix_accurate) <- names
colnames(matrix_accurate) <- names

rownames(matrix_inaccurate) <- names
colnames(matrix_inaccurate) <- names


### prepare attribute files ----------------------------------------------

attributes <- read_csv("attributes.csv")

# manually adding a student variable in the attributes data frame 
attributes$student <- c(1,1,1,1,1,1,1,1,1,1,1,1,
                        2,2,2,2,2,2,2,2,2,2,2,2,
                        3,3,3,3,3,3,3,3,3,3,3,3,
                        4,4,4,4,4,4,4,4,4,4,4,4,
                        5,5,5,5,5,5,5,5,5,5,5,5,
                        6,6,6,6,6,6,6,6,6,6,6,6)

# assigning row names in the attributes file for consistency 
rownames(attributes) <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", 
                          "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                          "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                          "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                          "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                          "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12")

### create network objects -----------------------------------------------
network_hi <- network(matrix_accurate, directed = TRUE, weighted = TRUE)
network_low <- network(matrix_inaccurate, directed = TRUE, weighted = TRUE)

# set network_hi attributes
set.vertex.attribute(network_hi, 'gender', as.character(attributes$gender))
set.vertex.attribute(network_hi, 'ses', as.character(attributes$ses))
set.vertex.attribute(network_hi, 'item', attributes$item)
set.vertex.attribute(network_hi, 'student', attributes$student)

network_hi

# set network_low attributes
set.vertex.attribute(network_low, 'gender', as.character(attributes$gender))
set.vertex.attribute(network_low, 'ses', as.character(attributes$ses))
set.vertex.attribute(network_low, 'item', attributes$item)
set.vertex.attribute(network_low, 'student', attributes$student)

network_low


### building the network models ------------------------------------------

## create baseline models only with an edges term 
model_high_baseline <- ergm(network_hi ~ edges)
summary(model_high_baseline) 

model_low_baseline <-  ergm(network_low ~ edges)
summary(model_low_baseline)

## include student and item variables in the models 
model_high_student_item <- ergm(network_hi ~ edges +
                                  nodefactor('gender') +
                                  nodefactor('ses') + 
                                  nodefactor('student') + 
                                  nodefactor('item', base = 2))

model_low_student_item <- ergm(network_low ~ edges +
                                  nodefactor('gender') +
                                  nodefactor('ses') + 
                                  nodefactor('student') + 
                                  nodefactor('item', base = 2))

# comparing the two models 
screenreg(list(model_high_student_item, model_low_student_item))

## include behavioral terms (nodematch) and a term for local clustering (triangle)
model_high_behavioral <- ergm(network_hi ~ edges + 
                                triangle + 
                                nodematch('gender') + 
                                nodematch('ses')+
                                nodematch('student')+
                                nodematch('item') +
                                nodefactor('gender') +
                                nodefactor('ses') + 
                                nodefactor('student') + 
                                nodefactor('item', base = 2))

model_low_behavioral <- ergm(network_low ~ edges + 
                                triangle + 
                                nodematch('gender') + 
                                nodematch('ses')+
                                nodematch('student')+
                                nodematch('item') +
                                nodefactor('gender') +
                                nodefactor('ses') + 
                                nodefactor('student') + 
                                nodefactor('item', base = 2))

# compare the two models 
screenreg(list(model_high_behavioral, model_low_behavioral))

# assessing and comparing goodness of fit 
gof_model_high_behavioral <- btergm::gof(model_high_behavioral, nsim = 50)
plot(gof_model_high_behavioral)

gof_model_low_behavioral <- btergm::gof(model_low_behavioral, nsim = 50)
plot(gof_model_low_behavioral)


