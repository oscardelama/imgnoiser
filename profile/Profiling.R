# lineprof profiling
library(lineprof)
source('./profile/channel_nbr-profile.R')
test.data <- build.test.data()
t <- lineprof(prof.test(test.data))
t
shine(t)

# Rprof profiling
file.name <- "./profile/channel_nbr-profile.txt"
source('./profile/channel_nbr-profile.R')
test.data <- build.test.data()
Rprof(file.name)
prof.test(test.data)
Rprof(NULL)
summaryRprof(file.name)


# Rprof profiling hvdvm load
nikon.channel.names <- c("greenB","blue","red","greenR")
nobj <- hvdvm$new(nikon.channel.names)
prof.file <- "./profile/loadHvdvm.prof.txt"
Rprof(prof.file)
nobj$load('./data-raw/iso100White.csv', './data-raw/')
Rprof(NULL)
summaryRprof(prof.file)
