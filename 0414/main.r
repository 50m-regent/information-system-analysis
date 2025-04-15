library(dplyr)

data = read.csv("leafN.csv")
data$Species = as.factor(data$Species)


stats = data |>
    group_by(Species) |>
    summarise(
        mean_Nconc = mean(Nconc),
        sd_Nconc = sd(Nconc),
        median_Nconc = median(Nconc)
    )

print("stats")
print(stats)

plot(Nconc~Species, data=data)

print("t-test")
t.test(Nconc~Species, var.qual=T, data=data)