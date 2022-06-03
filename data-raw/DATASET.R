##################################################################
# 2022/05/26 ok.xmonad@gmail.com
# kotoを用いて、メインとシリアルデータを作成
##################################################################

# パスはパッケージのトップディレクトリからの相対パス
# 要kotoライブラリ
# https://github.com/syunsuke/koto

# 地価公示データ入手先
# 国土数値情報ダウンロードサービス
# https://nlftp.mlit.go.jp/ksj/ 

library(tidyverse)
library(koto)

# 令和４年のデータ
target_csvfile <- "data-raw/L01-2022P-2K.csv"

k_data <- read_kouji_csv(target_csvfile) %>% 
  
  select(date,
         wareki,
         std_number,
         price,
         mod_rate,
         `年次`,
         `都道府県`,
         `所在地コード`,
         `市区町村名`,
         `所在並びに地番`,
         `住居表示`,
         `地積`,
         genkyo,
         `利用状況表示`,
         `建物構造`,
         `周辺の土地の利用の現況`,
         sisetu,
         youto,
         bouka,
         `建ぺい率`,`容積率`,
         `駅名`,
         `駅距離`,
         `前面道路区分`,
         `前面道路の方位区分`,
         `前面道路の幅員`,
         `側道区分`,
         long,
         lat,
         `用途`,`連番`,
         bind_id) %>%
  
  rename(`価格時点` = date,
         `和暦` = wareki,
         `標準地番号` = std_number,
         `標準地価格` = price,
         `対前年比` = mod_rate,
         `利用の現況` = genkyo,
         `用途地域` = youto,
         `防火区分` = bouka)

s_data <- make_serial_price_data(target_csvfile)

usethis::use_data(k_data, overwrite = TRUE)
usethis::use_data(s_data, overwrite = TRUE)
