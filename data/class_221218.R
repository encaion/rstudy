getwd()

df = data.frame(xx = 1:5,
                yy = 1:5)

library("ggplot2")
ggplot(data = df, mapping = aes(x = xx, y = yy)) + 
  geom_point()

ggplot() + 
  geom_point(data = df, mapping = aes(x = xx, y = yy))

ggplot(data = df, aes(x = xx, y = yy)) + 
  geom_point()

ggplot() + 
  geom_point(data = df, aes(x = xx, y = yy))

# ggplot() + 
#   geom_point(df, aes(x = xx, y = yy))

ggplot() + 
  geom_point(data = df, aes(x = xx, y = yy)) + 
  geom_line(data = df, aes(x = xx, y = yy))

ggplot(data = df, aes(x = xx, y = yy)) + 
  geom_point() + 
  geom_line()

library("ggplot2")
df = read.csv("data_auc_main_interpolated_with_AUC2.csv")
head(df)

n = 1
df_sub = data.frame(x = c(0, 2, 30, 180, 365) / 365,
                    y = as.numeric(df[n, 13:17]))

ggplot(data = df_sub,
            aes(x = x, y = y)) + 
  geom_area(alpha = 0.2) + 
  geom_line(linewidth = 2) +
  geom_point(size = 4) +
  annotate(geom = "text", x = 0.9, y = 1, 
           label = paste0("AUC: ", round(df[n, "AUC"], 3))) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 1.1),
                     breaks = seq(0, 1, 0.2),
                     labels = paste0(0:5, "k")) +
  labs(x = "지연", y = "무차별 금액", title = "title",
       subtitle = "subtitle",
       caption = "caption") +
  theme_bw() + 
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))

for(n in 1:nrow(df)){
  df_sub = data.frame(x = c(0, 2, 30, 180, 365) / 365,
                      y = as.numeric(df[n, 13:17]))
  
  gg = ggplot(data = df_sub,
              aes(x = x, y = y)) + 
    geom_area(alpha = 0.2) + 
    geom_line(linewidth = 2) +
    geom_point(size = 4) +
    annotate(geom = "text", x = 0.9, y = 1, label = paste0("AUC: ", round(df[n, "AUC"], 3))) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 1.1),
                       breaks = seq(0, 1, 0.2)) +
    labs(x = "지연", y = "무차별 금액") +
    theme_bw() + 
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 15))
  
  file_name = glue("plots/AUC_plot_{no}_{id}.png",
                   no = sprintf("%02d", n),
                   id = df[n, "id"])
  ggsave(filename = file_name,
         plot = gg)
}
?ggsave


df1 = data.frame(xx = 1:10,
                 yy = 1:10)

ggplot() + 
  geom_col(data = df1,
           aes(x = xx, y = yy, fill = yy)) + 
  theme(legend.position = "none")

ggplot() + 
  geom_col(data = df1,
           aes(x = xx, y = yy, fill = yy)) + 
  coord_flip() +
  theme(legend.position = "none")

ggplot() + 
  geom_col(data = df1,
           aes(x = xx, y = yy, fill = yy)) + 
  coord_polar(theta = "x") +
  theme(legend.position = "none")

ggplot() + 
  geom_col(data = df1,
           aes(x = xx, y = yy, fill = yy),
           width = 1)

ggplot() + 
  geom_col(data = df1,
           aes(x = xx, y = yy, fill = yy),
           width = 1.01) +
  coord_polar(theta = "x") +
  theme(legend.position = "none")

# install.packages("plotly")
library("plotly")
gg = ggplot() + 
  geom_point(data = df1,
             aes(x = xx, y = yy),
             size = 5)
gg

ggplotly(gg)

dia = as.data.frame(diamonds)
head(dia, 2)

ggplot() + 
  geom_point(data = dia,
           aes(x = carat, y = price),
           alpha = 0.3) + 
  facet_grid(rows = vars(cut))

ggplot() + 
  geom_point(data = dia,
             aes(x = carat, y = price),
             alpha = 0.3) + 
  facet_grid(cols = vars(cut),
             scales = "free_x")

ggplot() + 
  geom_point(data = dia,
             aes(x = carat, y = price),
             alpha = 0.3) + 
  scale_y_continuous(limits = c(0, 17000)) + 
  facet_grid(cols = vars(cut),
             scales = "free_x") + 
  theme(strip.placement = "outside",
        strip.text = element_text(size = 12, color = "#EEEEEE",
                                  face = "bold"),
        strip.background = element_rect(fill = "#63B8AA"))


df2 = data.frame(type = c("abc", "zip", "col", "and"),
                 value = c(200, 100, 300, 400))

ggplot() + 
  geom_col(data = df2,
           aes(x = type, y = value, fill = type))

ggplot() + 
  geom_col(data = df2,
           aes(x = reorder(type, value), 
               y = value, fill = type))

ggplot() + 
  geom_col(data = df2,
           aes(x = reorder(type, value, decreasing = TRUE), 
               y = value, fill = type))

ggplot() + 
  geom_col(data = df2,
           aes(x = type, y = value, fill = type)) + 
  labs(fill = "asdf") # 범례를 그리는데 사용된 인자.


ggplot() + 
  geom_col(data = df2,
           aes(x = type, y = value, fill = type)) +
  guides(fill = "none")

#### 사용자 정의 함수 ####

udf1 = function(x){
  return(x + 2)
}
udf1(3)

udf2 = function(x = 100){
  return(x + 2)
}
udf2()

udf3 = function(x = 100){
  yy <<- x * 100 # 전역변수로 등록
  return(x + 2)
}
udf3()

udf4 = function(x = 100){
  return(list(yy = x * 100,
              xx = x + 2))
}
udf4(123)

set.seed(123)
df_s1 = as.data.frame(matrix(sample(1:100, 40), ncol = 4))
df_s1[, "type"] = paste0("m", 1:10)
df_s2 = as.data.frame(matrix(sample(1:100, 40), ncol = 4))
df_s2[, "type"] = paste0("k", 1:10)
colnames(df_s1)[1:4] = paste0("s", 1:4)
colnames(df_s2)[1:4] = paste0("s", 1:4)
head(df_s1)
head(df_s2)

df_s2[, "type"] = df_s1$type

df_s1_melt = melt(df_s1, id.vars = "type")
df_s2_melt = melt(df_s2, id.vars = "type")
head(df_s1_melt)
head(df_s2_melt)

data.frame(aa = 1:6,
           bb = 1:2)

library("readxl")
excel_sheets("충동성_DDT_AUC계산용.xlsx")
