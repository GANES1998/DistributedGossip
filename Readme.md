## Erlang Bitcoin Mining Simulation.

This is the actor model based bitcoin mining system in erlang submitted in response
to [project 1](https://ufl.instructure.com/courses/467300/assignments/5383669)
of [COP5615](https://ufl.instructure.com/courses/467300).

#### Team

| Name                     | Gator Email             | UF Id     | Github username |
|--------------------------|-------------------------|-----------|-----------------|
| Ravichandran, Ganeson    | g.ravichandran@ufl.edu  | 10798982 | GANES1998       |
| Munaga, Sai Pavan Kalyan | saipavank.munaga@ufl.edu | 88769245 | saipavankalyan  |

#### Architecture

```mermaid
graph TD;

subgraph supervisor
A[User Ip] -.-> |Actors, Topology, Algorithm| B[Spawn];
B --> BA{Algorithm};
B --> C[Send Global State];
C --> D[Send Secret/Initial S,W];
D --> E[Wait for convergence]
subgraph convergence
E[Wait for convergence] --> F[First Convergance];
F -.-> |Collect convergance| F;
F --> G{No Convergance for 5 sec}; 
G --> |No| F;
end 
G --> |Yes| H[Collect Metrics];
H -.-> I(Convergence Time, Convergance Ratio);
end

subgraph worker
BA -.-> |gossip| J;
J[Wait Global Mapping] --> K{Check Convergance};
K -.-> |Converged| E;
K --> L((Wait for message));
L --> N[Choose Neighbour];
N --> O[Find Neighbor Pid];
O --> P(Send Rumer);
P -.-> K; 
L --> |Message| M[Add to Message List];
M --> MA[Update Current Secret Value];
MA --> N;
end

subgraph psworker
BA -.-> |push sum| JPS;
JPS[Wait Global Mapping] --> KPS{Check Convergance};
KPS -.-> |Converged| E;
KPS --> NPS{Wait for Message};
NPS --> |Sum & Weight| OPS[Compute Cumulative Sum & Weight];
NPS -.-> |Timeout| QPS;
OPS --> PPS[Choose One Neighbor];
PPS --> QPS[Find Neightbor Pid];
QPS --> RPS(Send Half Weight & Sum);
RPS -.-> KPS;
end

```

#### Steps of Execution

1. Fill the [variables.env](variables.env) with appropriate values.

   | Variable Name         | Use                                                                                       |
   |-----------------------|-------------------------------------------------------------------------------------------|
   | CONVERGE_LENGTH       | Defaults to 5 for push sum and 10 for gossip                                              |
   | ERLANG_BIN            | Absolute path to erlang bin.                                                              |

2. Execute ```./project2.sh ActorCount Topology Algorithm``` where
      * ActorCount - Number of Actors - For 2d topologies, the nearest perfect square is taken
      * Topology - one of 
        * full - Star chosen at random
        * line - position + or - 1 if possible
        * twoD - one of all 8 possible neighbours of a position in a 2D grid.
        * twoDImperfect - twoD plus another node chosen at random.
      * Algorithm
        * push_sum
        * gossip


![img.png](doc/assets/sample_io.png)

#### Answers

*2. What is working*

- Gossip
  - All four topologies are working upto 20K processes and 99+% of the actors are converging
- Push Sum.
  - All four topologies are working upto 5K processes and 99+% of the actors are converging.

*3. What is the largest network you managed to deal with for each type of topology and algorithm?*

| Algorithm | Topology      | Largest Network |
|-----------|---------------|-----------------|
| Gossip    | full          | 20K             |
| Gossip    | line          | 20K             |
| Gossip    | twoD          | 25K             |
| Gossip    | twoDImperfect | 25K             |
| Push Sum  | full          | 5K              |
| Push Sum  | line          | 5K              |
| Push Sum  | twoD          | 5K              |
| Push Sum  | twoDImperfect | 5K              |





