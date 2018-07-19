# 信用卡评分系统
# 学习网址：https://mp.weixin.qq.com/s?__biz=MzA3OTAxMDQzNQ==&mid=2650615258&idx=1&sn=67c531786a45a22adef347ec473430a8&chksm=87b38236b0c40b20ef84cd3b97b73c825ed1ffe2558ecdfd9d963681b59366e1ef25b995f3b3&scene=0#rd

# 获得工作路径
getwd()
# 修改绝对工作路径
setwd("E://Learn//GitHub//Python//credit-card-scoring")
setwd("./data/")

# 导入库
library(rJava)
library(plyr)
library(smbinning)
library(prettyR)

# 导入训练数据集
train1<-read.csv("Training.csv", stringsAsFactors = T)

# ------数据探索分析------

# 查看缺失值
train1$EDUCATION<-NULL
# 移除缺失值
train2<-na.omit(train1)

# 针对异常值过大的变量，采用盖帽法;
# 该方法用99分位点值代替极大异常值，有1分位点值代替极小异常值
block<-function(x, lower = T, upper = T)
{
  if(lower)
  {
    q1<-quantile(x, 0.01)
    x[x <= q1]<-q1
  }
  if(upper)
  {
    q99<-quantile(x, 0.99)
    x[x > q99]<-q99
  }
  return(x)
}

# 删除不需要的列
isnull<-function(data)
{
  result = vector()
  for (i in 1:ncol(data))
  {
    x = data[[i]]
    if(class(x) == "factor")
    {
      flag = ifelse(length(levels(x)) == 1, TRUE, FALSE)
    }
    else if(class(x) == "integer")
    {
      flag = ifelse(sum(x) == 0, TRUE, FALSE)
    }
    else flag = FALSE
    result[i] = flag
  }
  return(result)
}
# 可删除的列名
colnames(train2[, isnull(train2)])
train2[, isnull(train2)]<-NULL
# 数据统计
summary(train2)
# 观察各列属性与值
str(train2)

# ------数据清洗------
# 新建评分卡模型分数，将TARGET_LABEL_BAD.1的0、1值交换
train2$TARGET_LABEL_good<-as.numeric(ifelse(train2$TARGET_LABEL_BAD.1 == 1, 0, 1))
# 观察TARGET_LABEL_BAD.1的0、1值数目
table(as.numeric(train2$TARGET_LABEL_BAD.1))
# 观察评分卡模型分数的0、1值数目
table(as.numeric(train2$TARGET_LABEL_good))

# 新建数据集train3，不含train2的第1、2、24、20、21列数据
train3<-train2[, -c(1, 2, 24, 20, 21)]
# 观察train3列名
colnames(train3)
# train3中含有num、int等数值型与factor因子型
# 观察数据类型与值
str(train3)
# 将选中列改为因子型数据
train3[, c("AREA_CODE_RESIDENCIAL_PHONE", "SHOP_RANK", "PROFESSION_CODE")]<-lapply(train3[, c("AREA_CODE_RESIDENCIAL_PHONE", "SHOP_RANK", "PROFESSION_CODE")], as.factor)
# 观察数据类型与值
str(train3)

# 判断哪些变量是因子型
isfactor = vector()
for (i in 1:ncol(train3))
{
  isfactor[i] = is.factor(train3[[i]])
}
# 判断哪些变量是数值型
isnumeric = vector()
for (i in 1:ncol(train3))
{
  isnumeric[i] = is.numeric(train3[[i]])
}
# 新建因子型与数值型数据集
factorval = colnames(train3[isfactor])
numericval = colnames(train3[isnumeric])

# ------数据分箱------

## 数值型变量分箱
# 首先查看数据分布情况，因变量TARGET_LABEL_good为二分类，自变量为数值型
# 用t检验来检验两分布是否有显著性差别，有显著性差别才能进行分箱，否则分箱结果无意义
# 然后采用smbinning包CART回归树进行属性划分实现分箱

# 1)用AGE数值型变量进行分箱
# 绘制数据分布线箱图
boxplot(AGE~TARGET_LABEL_good, data = train3, horizontal = T, frame = F, col = "blue", main = "Distribution")
# t检验(原假设为两分类组的均值相等，若结果表明原假设被拒绝，则两分布具有显著性差别，可分箱)
t.test(AGE~TARGET_LABEL_good, data = train3)
# CART回归树属性划分分箱
AGE<-smbinning(train3, "TARGET_LABEL_good", "AGE")
AGE$ctree
AGE$iv
# 对AGE的WoE值画图，若分布呈现单调趋势，则分箱结果良好，可纳入模型
smbinning.plot(AGE, option = "WoE", sub = "年龄")
creditivs<-c("AGE" = AGE$iv)

# 2)用PAYMENT_DAY数值型变量进行分箱
boxplot(PAYMENT_DAY~TARGET_LABEL_good, data = train3, horizontal = T, frame = F, col = "blue", main = "Distribution")
t.test(PAYMENT_DAY~TARGET_LABEL_good, data = train3)
PAYMENT_DAY<-smbinning(train3, "TARGET_LABEL_good", "PAYMENT_DAY")
PAYMENT_DAY$ctree
PAYMENT_DAY$iv
smbinning.plot(PAYMENT_DAY, option = "WoE", sub = "付款日")
creditivs<-c(creditivs, "PAYMENT_DAY" = PAYMENT_DAY$iv)

# 3)用MONTHS_IN_RESIDENCE数值型变量进行分箱
boxplot(MONTHS_IN_RESIDENCE~TARGET_LABEL_good, data = train3, horizontal = T, frame = F, col = "blue", main = "Distribution")
t.test(MONTHS_IN_RESIDENCE~TARGET_LABEL_good, data = train3)
MONTHS_IN_RESIDENCE<-smbinning(train3, "TARGET_LABEL_good", "MONTHS_IN_RESIDENCE")
MONTHS_IN_RESIDENCE$ctree
MONTHS_IN_RESIDENCE$iv
smbinning.plot(MONTHS_IN_RESIDENCE, option = "WoE", sub = "住房时间")
creditivs<-c(creditivs, "MONTHS_IN_RESIDENCE" = MONTHS_IN_RESIDENCE$iv)

# 4)用MONTHS_IN_THE_JOB数值型变量进行分箱
boxplot(MONTHS_IN_THE_JOB~TARGET_LABEL_good, data = train3, horizontal = T, frame = F, col = "blue", main = "Distribution")
t.test(MONTHS_IN_THE_JOB~TARGET_LABEL_good, data = train3)
MONTHS_IN_THE_JOB<-smbinning(train3, "TARGET_LABEL_good", "MONTHS_IN_THE_JOB")
MONTHS_IN_THE_JOB$ctree
MONTHS_IN_THE_JOB$iv
smbinning.plot(MONTHS_IN_THE_JOB, option = "WoE", sub = "现职做多久")  #非单调
creditivs<-c(creditivs, "MONTHS_IN_THE_JOB" = MONTHS_IN_THE_JOB$iv)

# 5)用MATE_INCOME数值型变量进行分箱
boxplot(MATE_INCOME~TARGET_LABEL_good, data = train3, horizontal = T, frame = F, col = "blue", main = "Distribution")
t.test(MATE_INCOME~TARGET_LABEL_good, data = train3)
# 无法分箱
MATE_INCOME<-smbinning(train3, "TARGET_LABEL_good", "MATE_INCOME")

# 6)用PERSONAL_NET_INCOME数值型变量进行分箱
boxplot(PERSONAL_NET_INCOME~TARGET_LABEL_good, data = train3, horizontal = T, frame = F, col = "blue", main = "Distribution")
t.test(PERSONAL_NET_INCOME~TARGET_LABEL_good, data = train3)
# 无法分箱
PERSONAL_NET_INCOME<-smbinning(train3, "TARGET_LABEL_good", "PERSONAL_NET_INCOME")

# 7)用QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION数值型变量进行分箱
boxplot(QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION~TARGET_LABEL_good, data = train3, horizontal = T, frame = F, col = "blue", main = "Distribution")
t.test(QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION~TARGET_LABEL_good, data = train3)
# 无法分箱
QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION<-smbinning(train3, "TARGET_LABEL_good", "QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION")

## 因子型变量分箱
# 首先查看数据分布情况，因变量TARGET_LABEL_good为二分类，自变量为因子型
# 用卡方检验来检验两分布是否有显著性差别，有显著性差别才能进行分箱，否则分箱结果无意义
# 然后采用smbinning包原属性值属性划分

# 1)用SEX因子型变量进行分箱
# 因子型数据数值化表示：1表示女性，0表示男性
train3$SEX = factor(ifelse(train3$SEX == ""|train3$SEX == "F", 1, 0), levels = c(1, 0), labels = c("F", "M"))
table(sex = train3$SEX, score = train3$TARGET_LABEL_good)
# 卡方检验，若结果表明拒绝原假设，则两样本有显著性差别，可进行分箱
xtab(~SEX + TARGET_LABEL_good, data = train3, chisq = T)
# 分箱
SEX<-smbinning.factor(train3, "TARGET_LABEL_good", "SEX")
SEX$iv
# 对AGE的WoE值画图
smbinning.plot(SEX, option = "WoE", sub = "性别")
creditivs<-c(creditivs, "SEX" = SEX$iv)

# 2)用MARITAL_STATUS因子型变量进行分箱
table(MARITAL_STATUS = train3$MARITAL_STATUS, score = train3$TARGET_LABEL_good)
xtab(~MARITAL_STATUS + TARGET_LABEL_good, data = train3, chisq = T)
MARITAL_STATUS<-smbinning.factor(train3, "TARGET_LABEL_good", "MARITAL_STATUS")
MARITAL_STATUS$iv
smbinning.plot(MARITAL_STATUS, option = "WoE", sub = "婚姻情况")
creditivs<-c(creditivs, "MARITAL_STATUS" = MARITAL_STATUS$iv)

# 3)用FLAG_RESIDENCIAL_PHONE因子型变量进行分箱
table(FLAG_RESIDENCIAL_PHONE = train3$FLAG_RESIDENCIAL_PHONE, score = train3$TARGET_LABEL_good)
xtab(~FLAG_RESIDENCIAL_PHONE + TARGET_LABEL_good, data = train3, chisq = T)
FLAG_RESIDENCIAL_PHONE<-smbinning.factor(train3, "TARGET_LABEL_good", "FLAG_RESIDENCIAL_PHONE")
FLAG_RESIDENCIAL_PHONE$iv
smbinning.plot(FLAG_RESIDENCIAL_PHONE, option = "WoE", sub = "是否有家住电话")
creditivs<-c(creditivs, "FLAG_RESIDENCIAL_PHONE" = FLAG_RESIDENCIAL_PHONE$iv)

# 4)用AREA_CODE_RESIDENCIAL_PHONE因子型变量进行分箱(手机区码)
table(AREA_CODE_RESIDENCIAL_PHONE = train3$AREA_CODE_RESIDENCIAL_PHONE, score = train3$TARGET_LABEL_good)
xtab(~AREA_CODE_RESIDENCIAL_PHONE + TARGET_LABEL_good, data = train3, chisq = T) # 无法分箱

# 5)用SHOP_RANK因子型变量进行分箱
table(SHOP_RANK = train3$SHOP_RANK, score = train3$TARGET_LABEL_good)
xtab(~SHOP_RANK + TARGET_LABEL_good, data = train3, chisq = T)
SHOP_RANK<-smbinning.factor(train3, "TARGET_LABEL_good", "SHOP_RANK")
SHOP_RANK$iv
smbinning.plot(SHOP_RANK, option = "WoE", sub = "商店等级")
creditivs<-c(creditivs, "SHOP_RANK" = SHOP_RANK$iv)

# 6)用RESIDENCE_TYPE因子型变量进行分箱
table(RESIDENCE_TYPE = train3$RESIDENCE_TYPE, score = train3$TARGET_LABEL_good)
xtab(~RESIDENCE_TYPE + TARGET_LABEL_good, data = train3, chisq = T)
RESIDENCE_TYPE<-smbinning.factor(train3, "TARGET_LABEL_good", "RESIDENCE_TYPE")
RESIDENCE_TYPE$iv
smbinning.plot(RESIDENCE_TYPE, option = "WoE", sub = "住房类型")
creditivs<-c(creditivs, "RESIDENCE_TYPE" = RESIDENCE_TYPE$iv)

# 7)用FLAG_MOTHERS_NAME因子型变量进行分箱
table(FLAG_MOTHERS_NAME = train3$FLAG_MOTHERS_NAME, score = train3$TARGET_LABEL_good)
xtab(~FLAG_MOTHERS_NAME + TARGET_LABEL_good, data = train3, chisq = T)
FLAG_MOTHERS_NAME<-smbinning.factor(train3, "TARGET_LABEL_good", "FLAG_MOTHERS_NAME")
FLAG_MOTHERS_NAME$iv
smbinning.plot(FLAG_MOTHERS_NAME, option = "WoE", sub = "是否填写母亲姓名")
creditivs<-c(creditivs, "FLAG_MOTHERS_NAME" = FLAG_MOTHERS_NAME$iv)

# 8)用FLAG_FATHERS_NAME因子型变量进行分箱
table(FLAG_FATHERS_NAME = train3$FLAG_FATHERS_NAME, score = train3$TARGET_LABEL_good)
xtab(~FLAG_FATHERS_NAME + TARGET_LABEL_good, data = train3, chisq = T)
FLAG_FATHERS_NAME<-smbinning.factor(train3, "TARGET_LABEL_good", "FLAG_FATHERS_NAME")
FLAG_FATHERS_NAME$iv
smbinning.plot(FLAG_FATHERS_NAME, option = "WoE", sub = "是否填写父亲姓名")
creditivs<-c(creditivs, "FLAG_FATHERS_NAME" = FLAG_FATHERS_NAME$iv)

# 9)用FLAG_RESIDENCE_TOWN.WORKING_TOWN因子型变量进行分箱
table(FLAG_RESIDENCE_TOWN.WORKING_TOWN = train3$FLAG_RESIDENCE_TOWN.WORKING_TOWN, score = train3$TARGET_LABEL_good)
xtab(~FLAG_RESIDENCE_TOWN.WORKING_TOWN + TARGET_LABEL_good, data = train3, chisq = T) # 无法分箱

# 10)用FLAG_RESIDENCE_STATE.WORKING_STATE因子型变量进行分箱
table(FLAG_RESIDENCE_STATE.WORKING_STATE = train3$FLAG_RESIDENCE_STATE.WORKING_STATE, score = train3$TARGET_LABEL_good)
xtab(~FLAG_RESIDENCE_STATE.WORKING_STATE + TARGET_LABEL_good, data = train3, chisq = T) # 无法分箱

# 11)用PROFESSION_CODE因子型变量进行分箱
table(PROFESSION_CODE = train3$PROFESSION_CODE, score = train3$TARGET_LABEL_good)
xtab(~PROFESSION_CODE + TARGET_LABEL_good, data = train3, chisq = T) # 无法分箱

# 12)用FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS因子型变量进行分箱
table(FLAG_RESIDENCIAL_ADDRESS = train3$FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS, score = train3$TARGET_LABEL_good)
xtab(~FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS + TARGET_LABEL_good, data = train3, chisq = T) # 无法分箱

# 13)将MATE_INCOME数值型变量转化为因子型变量进行分箱
train3$MATE_INCOME<-factor(ifelse(train3$MATE_INCOME == 0, 0, 1), levels = c(1, 0), labels = c("Y", "N"))
table(MATE_INCOME = train3$MATE_INCOME, score = train3$TARGET_LABEL_good)
xtab(~MATE_INCOME + TARGET_LABEL_good, data = train3, chisq = T)
MATE_INCOME<-smbinning.factor(train3, "TARGET_LABEL_good", "MATE_INCOME")
MATE_INCOME$iv
smbinning.plot(MATE_INCOME, option = "WoE", sub = "伴侣是否有收入")
creditivs<-c(creditivs, "MATE_INCOME" = MATE_INCOME$iv)

# 14)将QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION数值型变量转化为因子型变量进行分箱
train3$QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION<-factor(ifelse(train3$QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION == 0, 0, 1), levels = c(1, 0), labels = c("Y", "N"))
table(FLAG_ADDITIONAL_CARDS = train3$QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION, score = train3$TARGET_LABEL_good)
xtab(~QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION + TARGET_LABEL_good, data = train3, chisq = T)
FLAG_ADDITIONAL_CARDS<-smbinning.factor(train3, "TARGET_LABEL_good", "QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION")
FLAG_ADDITIONAL_CARDS$iv
smbinning.plot(FLAG_ADDITIONAL_CARDS, option = "WoE", sub = "是否有其他信用卡")
creditivs<-c(creditivs, "FLAG_ADDITIONAL_CARDS" = FLAG_ADDITIONAL_CARDS$iv)

# 15)将PERSONAL_NET_INCOME数值型变量转化为因子型变量进行分箱
# 分级
train3$PERSONAL_NET_INCOME[train3$PERSONAL_NET_INCOME > 0 & train3$PERSONAL_NET_INCOME <= 250] = 1
train3$PERSONAL_NET_INCOME[train3$PERSONAL_NET_INCOME > 250 & train3$PERSONAL_NET_INCOME <= 400] = 2
train3$PERSONAL_NET_INCOME[train3$PERSONAL_NET_INCOME > 400 & train3$PERSONAL_NET_INCOME <= 700] = 3
train3$PERSONAL_NET_INCOME[train3$PERSONAL_NET_INCOME > 700 & train3$PERSONAL_NET_INCOME <= 100000] = 4
train3$PERSONAL_NET_INCOME[train3$PERSONAL_NET_INCOME > 100000] = 5
# 无法分箱
table(PERSONAL_NET_INCOME = train3$PERSONAL_NET_INCOME, score = train3$TARGET_LABEL_good)
xtab(~PERSONAL_NET_INCOME + TARGET_LABEL_good, data = train3, chisq = T)
PERSONAL_NET_INCOME<-smbinning.factor(train3, "TARGET_LABEL_good", "PERSONAL_NET_INCOME")
PERSONAL_NET_INCOME$iv
smbinning.plot(PERSONAL_NET_INCOME, option = "WoE", sub = "个人收入")
creditivs<-c(creditivs, "PERSONAL_NET_INCOME" = PERSONAL_NET_INCOME$iv)


# ------建立评分卡------
# 观察creditivs数据
creditivs
# 一般认为IV>=0.02的变量对构建评分卡具有一定的帮助
vars<-c(names(creditivs[creditivs >= 0.02]))
# 获得IV>=0.02的变量名称
# 结果表示年龄、婚姻状态、工作时长、是否有自用手机等变量对预测结果影响较大
vars
creditivs[vars]

# 生成分箱结果
# 数值型分箱变量用函数smbinning.gen()，因子型变量用函数smbinning.factor.gen()
data1<-train3
data1<-smbinning.gen(data1, AGE, "glage")
data1<-smbinning.factor.gen(data1, SEX, "glSEX")
data1<-smbinning.factor.gen(data1, MARITAL_STATUS, "glmarital")
data1<-smbinning.factor.gen(data1, FLAG_RESIDENCIAL_PHONE, "glphone")
data1<-smbinning.factor.gen(data1, MATE_INCOME, "glmincome")
data1<-smbinning.factor.gen(data1, MONTHS_IN_THE_JOB, "glmonth")

# 分箱后生成的新列并因变量得到data2数据集
data2<-data1[, c(21:26, 20)]

# 生成评分卡模型
cred_mod<-glm(TARGET_LABEL_good~. , data = data2, family = binomial())
summary(cred_mod)
cre_scal<-smbinning.scaling(cred_mod, pdo = 90, score = 1000, odds = 90)
cre_scal$minmaxscore

scaledcard<-cre_scal$logitscaled[[1]][-1, c(1, 2, 6)]
scaledcard[, 1]<-c(rep("年龄", 7), rep("性别", 2), rep("婚姻状态", 5), rep("是否有手机", 2), rep("伴侣是否有收入", 2), rep("工作时长（月）", 7))
write.csv(scaledcard, "card.csv", row.names = F) # 保存评分卡

# ------模型验证------
data3<-smbinning.scoring.gen(smbscaled = cre_scal, dataset = data2)
boxplot(Score~TARGET_LABEL_good, data = data3, horizontal = T, frame = F, col = "blue", main = "Distribution", xlab = "cutoff")
t.test(Score~TARGET_LABEL_good, data = data3)
View(data3)
smbinning.metrics(dataset = data3, prediction = "Score", actualclass = "TARGET_LABEL_good", report = 1)
# 建议cutoff = 615