app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")
Sys.sleep(120)
app$snapshot()
