# kernlab is another R package that solves QP problems. here's the call that I used to solve our frontier MV constrution using kernlab and the same matrices/vectors that we pass to quadprog. I inserted this right below the solver.QP() call
# the purpose of the exercise was to compare with quadprog

ipopResults <- ipop(rep(0,numAssets),solverObject[["coVar"]], t(constraintsLhs), constraintsRhs, rep(0,numAssets),rep(1,numAssets), c(rep(0, constraintsNumEqs),rep(100,length(constraintsRhs)-constraintsNumEqs)))
solution <- primal(ipopResults) # yes, accessor functions are needed to get to the slots in the result