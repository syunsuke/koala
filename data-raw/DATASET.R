##################################################################
# 地価公示全国全年データ 作成スクリプト
# 2023/09/28 ok.xmonad@gmail.com
# 国土交通省が公開している国土数値情報データの中になる
# 地価公示データを収集して
# kotoのツールで読み込み同一地点IDを付したもの
# data.frame(tibble)として作成
##################################################################

# パスはパッケージのトップディレクトリからの相対パス

library(tidyverse)
library(hash)
library(koto)

start_time <- proc.time()


# 全国の地価公示URLリスト
url_list <- url_list_all_kouji()
#url_list <- url_list_kouji(27)

# データのダウンロード
download_by_urls(url_list, "data-raw/dl_data")


# zipファイルパスのリストを作成
zipfiles <- dir("data-raw/dl_data", full.names = TRUE)


# csvファイルの抜き出し
pickup_from_zip(zipfiles, "\\.csv", "data-raw/csvfiles")


# csvファイルの読み込み
csvfiles <- dir("data-raw/csvfiles", full.names = TRUE)
chikakouji_expdata <- read_kouji_csv(csvfiles)


chikakouji_all_data <-
  chikakouji_expdata %>%
  add_pointid_col() %>%
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
         bind_id,
         point_id) %>%

  rename(`価格時点` = date,
         `和暦` = wareki,
         `標準地番号` = std_number,
         `標準地価格` = price,
         `対前年比` = mod_rate,
         `利用の現況` = genkyo,
         `用途地域` = youto,
         `防火区分` = bouka)


print(proc.time() - start_time)

save(chikakouji_all_data, file = "data/chikakouji_all_data.RData")
#usethis::use_data(chikakouji_all_data, overwrite = TRUE)
