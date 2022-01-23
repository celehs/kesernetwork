app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")
Sys.sleep(50)
app$snapshot()
