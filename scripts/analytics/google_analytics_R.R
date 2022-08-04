
# https://code.markedmondson.me/googleAnalyticsR/
# https://michalbrys.gitbooks.io/r-google-analytics/content/chapter3/library_rga.html

install.packages("googleAnalyticsR")

library(googleAnalyticsR)

googleAuthR::gar_set_client("gcp_client.json")
ga_auth()

my_accounts <- ga_account_list()

my_accounts$accountId


options(googleAuthR.client_id)
