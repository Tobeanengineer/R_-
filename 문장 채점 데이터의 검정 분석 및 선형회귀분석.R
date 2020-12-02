library(moments)

sent_info <- read.csv('rating_writing.csv')

head(sent_info)

# grammer - contents / contents - fluency / grammer - fluency 간 검정 및 선형회귀분석

# 정규분포 여부 확인
grammer <- sent_info$grammar
contents <- sent_info$contents
fluency <- sent_info$fluency

par(mfrow = c(1, 2))
boxplot(grammer, col = 'yellow')
hist(grammer, prob = T, col = 'yellow', main = '', xlab = '')
lines(density(grammer))
boxplot(contents, col = 'green')
hist(contents, prob = T, col = 'green', main = '', xlab = '')
lines(density(contents))
boxplot(fluency, col = 'skyblue')
hist(fluency, prob = T, col = 'skyblue', main = '', xlab = '')
lines(density(fluency))

# calculate skewness and kurtosis
cat('skewness of grammer:', skewness(grammer), 'kurtosis of grammer:', kurtosis(grammer), '\n')
cat('skewness of contents', skewness(contents), 'kurtosis of contents:', kurtosis(contents), '\n')
cat('skewness of fluency:', skewness(fluency), 'kurtosis of fluency:', kurtosis(fluency), '\n')

# 1) paired t-test
t.test(grammer, fluency, paired = T)
t.test(grammer, contents, paired = T)
t.test(contents, fluency, paired = T)
## 데이터가 정규성을 따르지 않으므로 t.test는 사용 불가


# 2) shapiro 검정을 통한 정규성 파악
shapiro.test(grammer)
shapiro.test(contents)
shapiro.test(fluency)

# 3) wilcox 검정을 통한 비모수 통계
wilcox.test(grammer, contents, paired = T)
wilcox.test(grammer, fluency, paired = T)
wilcox.test(contents, fluency, paired = T) # 유의성이 떨어짐 -> 차이가 없음


# 4) 선형회귀분석
lm.grammer <- lm(contents~grammer)
summary(lm.grammer)

lm.grammer2 <- lm(fluency~grammer)
summary(lm.grammer2)

lm.grammer3 <- lm(fluency~contents)
summary(lm.grammer3)

par(mfrow = c(1, 1))
plot(grammer, contents, main = 'Grammer and Contents')
abline(lm.grammer, col = 'red')

plot(grammer, fluency, main = 'Grammer and fluency')
abline(lm.grammer2, col = 'red')

plot(contents, fluency, main = 'contents and fluency')
abline(lm.grammer3, col = 'red')

# 5) 상관관계
cor(grammer, contents)
cor(grammer, fluency)
cor(contents, fluency)
