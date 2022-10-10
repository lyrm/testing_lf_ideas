## False Sharing
### Goal
Trying to have a basic measure of false sharing impact in run time.

### How to use
#### Command line :
`dune exec ./src/false_sharing.exe -- -d [NB_DOMAIN] -r [NB_RUN] -t [ID_TEST]`

#### Parameters
- [NB_DOMAIN] : number of domain
- [NB_RUN] : number of time the test is launched
- [ID_TEST] : between 1 to 4 (see below)

#### Returns
(median, average) run times in milliseconds. 

#### Available tests: 
- 1- with false sharing
- 2- just to see what happens: counters are `int Atomic.t`
- 3- trying to avoid false sharing with padding
- 4- trying to avoid false sharing with references

### Results
~20% in run time between test 1 (false sharing) and tests 3 and 4 (with 2 domains).

## Counter
### Goal 
Playing with `Domain.DLS` to implement a counter and comparing performances with a basic `Atomic.t` counter.

### Implementation
The idea is that each domain has its local counter. Every time they accessed it, they try once to push their local value in the global counter. This way there are no `CAS` loops in the main function (`incr`, `get`). A `flush` function (a `CAS` loop) enables to get the final counter value. 

### Command line
`dune exec ./src/counter.exe`

### Results 
There is a ~50% of improvement in run time with 4 domains with the counter using `DLS` versus an `int Atomic.t` counter.
