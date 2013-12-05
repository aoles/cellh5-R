

source('cellh5.R')
library("grid")

file_ = H5Fopen('data/_all_positions.ch5')
#ch5.print_file_info(file_)

gdef = ch5.definition(file_)

plates <- ch5.plates(file_)
for ( i in 1:length(plates)) {
    print(paste("loading plate: ", plates[i]))
    positions <- ch5.positions(file_, plates[i])
#     for (j in 1:length(postions)) {
#          ch5.timelapse(postions[i])
#    }
}

print(paste("Number of plates: ", length(positions)))