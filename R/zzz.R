#.GimmPath <- paste(.Library, "/gimmR/doc/", sep="")
#.GimmPath <- NA
#.PosthocPath<-NA
#print(.Library)
#print(.GimmPath)

#".PosthocPath" <- paste(.Library, "/gimmR/doc/", sep="")


#.First.lib <- function(lib, pkg) {
.onLoad <- function(lib, pkg) {
#      .GimmPath <- paste(.Library, "/gimmR/doc/", sep="")
     for(cpath in .libPaths()){
      cfpath<-paste(cpath, "/gimmR/doc/", sep="")
      if(file.exists(paste(cfpath,"gimm", sep=""))) 
	assign(".GimmPath",cfpath,envir=sys.frame())
      if(file.exists(paste(cfpath,"posthoc", sep=""))) 
	assign(".PosthocPath",cfpath,envir=sys.frame())
     }
print(.GimmPath)
print(.PosthocPath)
#      assign(".PosthocPath",paste(.Library, "/gimmR/doc/", sep=""),envir=sys.frame())
#     assign(".GeneDataTable",NULL,envir=sys.frame())
#     export(.GimmPath)
print(sys.frame())
#cat(".First.Lib call\n",.GimmPath,"\n")

#     .PosthocPath <- paste(.Library, "/gimmR/doc/", sep="")
}
#.onLoad <- function(lib, pkg) {
#     .GimmPath <- paste(.Library, "/gimmR/doc/", sep="")
#cat(".First.Lib call\n",.GimmPath)
#.PosthocPath <- paste(.Library, "/gimmR/doc/", sep="")
#}
#.onAttach <- function(lib, pkg) {
#     .GimmPath <- paste(.Library, "/gimmR/doc/", sep="")
#cat(".First.Lib call\n",.GimmPath)
#     .PosthocPath <- paste(.Library, "/gimmR/doc/", sep="")
#}
