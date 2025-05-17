
data = read.csv("german_credit_data.csv")

set.seed(1001)

sample = sample.int(n = nrow(data), size = floor(0.75*nrow(data)), replace = F)

train = data[sample,]
test = data[-sample,]

write.csv(test, "Test_Credit_Data.csv", row.names = F)
write.csv(train, "Train_Credit_Data.csv", row.names = F)
