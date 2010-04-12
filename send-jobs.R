system("ssh gusl@cypress.stat.ubc.ca 'cd sbm-slave; mkdir cypress; cp config.csv cypress; cd cypress; Rscript ../run-multiple.R' &")

system("ssh gusl@grouse.stat.ubc.ca 'cd sbm-slave; mkdir grouse; cp config.csv grouse; cd grouse; Rscript ../run-multiple.R' &")


##create a directory for each job
#ssh gusl@cypress.stat.ubc.ca 'cd sbm-slave; mkdir cyp; cp config.csv cyp; cd cyp; Rscript ../run-multiple'
#ssh gusl@grouse.stat.ubc.ca 'cd sbm-slave; mkdir gro; cd cp config.csv gro; Rscript ../run-multiple'



##it's waiting for ssh to return
## can I make system spawn a process and come back?



