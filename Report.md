<h2 style="text-align:center">Project Report</h2>

**Group Member**
1. Ganeson Ravichandran, g.ravichandran@ufl.edu
2. Sai Pavan Kalyan Munaga, saipavank.munaga@ufl.edu

Implementation Details:

*Gossip*

    1. Each Gossip Actor coninuously sends its message to choosen random worker.
    2. When it receives a message from other worker, considers it as its secret message and updates it to its seen list
    3. When any actor receives same secret for 10 times, It 

*Push Sum*

    1. When a actor doesn't see any change in Sum / Weight beyond 10^-10 for consecutive 3 times, then we converge the actor.
    2. We terminate the actor, after all nodes in the network terminate.




