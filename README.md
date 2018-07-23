# reactionator
A shiny-dashboard webapp for simulating chemical reaction kinetics

## currently working on
### data input
number of species and reactions is put in using simple numeric inputs
reactions are defined using the stoichiometric matrix by filling in the stoichiometric coefficients. The reactions are then parsed into a more readbable format. For example, this table:

| reaction        | A           | B  |  C  |
| -------------: |-------------:| -----:| -----: |
| 1      | -2 | -1 | 1 |
| 2      | 2      |   1 | -1 |

Should be parsed into the equations
2A + B -> C and C -> 2A + B

Furthermore simple numeric inputs for starting concentrations and reaction rate constants are needed.

## to-do
  * Implement calculation
  * graphical output
  * input checks

## completed features
Nothing yet :(

## future plans
  * chemical reactions network graph
  * consider heat generation of reaction, add cooling and heating to reactor
  



