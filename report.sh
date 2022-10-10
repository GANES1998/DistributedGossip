ALGORITHMS=(gossip push_sum)
TOPOLOGIES=(full line twoD twoDImperfect)
ACTORS=(100 200 400 800 1600)

rm report.out

for ALGO in "${ALGORITHMS[@]}"
do
  for TOPO in "${TOPOLOGIES[@]}"
  do
    for ACTOR in "${ACTORS[@]}"
    do
      echo "------------- Stating ${ALGO} - ${TOPO} - ${ACTOR} ACTORS ------------------------------------------------" >> report.out
      sh project2.sh ${ACTOR} ${TOPO} ${ALGO} >> report.out
      echo "------------- Completed ${ALGO} - ${TOPO} - ${ACTOR} ACTORS----------------------------------------------" >> report.out
      echo "=========================================================================================================" >> report.out
    done
  done
done