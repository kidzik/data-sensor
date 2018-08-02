data = read.csv2("../micropython-fusion/data/sensor.txt",sep = "\t",stringsAsFactors = FALSE)
data = data.frame(data[,-(2:14)])
for (var in names(data))
  data[[var]] = as.numeric((data[[var]]))

names(data)

mat2eul = function(matrow){
  c(atan2(matrow[6],matrow[9]),
  atan2(-matrow[3], sqrt(matrow[6]**2 + matrow[9]**2)),
  atan2(matrow[2],matrow[1]))
}
r = 180*t(apply(data[,11:19], 1, mat2eul))/pi
colnames(r) = c("x","y","z")
data = cbind(data, r)

output = read.csv2("../micropython-fusion/data/output.txt",header = TRUE,sep = ',',stringsAsFactors = FALSE)[,-1]
output = data.frame(output)
for (var in names(output))
  output[[var]] = as.numeric((output[[var]]))

cor(data[,13],output[,1])
plot(r[,1],output[,3])

### SOME DATA SCIENCE RANDOM CHECK
library("randomForest")
train.obs = sample(nrow(data))[1:5000]
test.obs = sample(nrow(data))[1:5000]

coef = 22
model = randomForest(data[train.obs,2:7],data[train.obs,coef])

preds = predict(model, data[test.obs,2:7])
cor(preds, data[test.obs,coef])
plot(preds, data[test.obs,coef])
