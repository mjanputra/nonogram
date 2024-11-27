# Nonogram Solver in OCaml

This OCaml script provides a solver for Nonogram puzzles. Nonograms are logic puzzles where you fill in cells on a grid according to provided clues.

## How It Works

The script defines a range of functions to solve Nonogram puzzles:

- **Cell States**: Defines the possible states of a cell (`Filled` or `Empty`).
- **Printing the Grid**: A function to print the solution grid.
- **Generating Sequences**: Generates all possible sequences for a line given the clues.
- **Swapping the Grid**: Transposes the grid (rows to columns and vice versa).
- **Matching Sequences**: Checks if a sequence matches the known cells.
- **Merging Sequences**: Merges possible sequences to find definite cell states.
- **Solving the Puzzle**: Main function to solve the Nonogram puzzle using the provided row and column clues.

## Example Usage

The script includes an example usage with predefined row and column clues. You can modify these clues to solve different puzzles.

```ocaml
let () =
  let row_clues = [
    [3];
    [2; 1];
    [3; 2];
    [2; 2];
    [6];
    [1; 5];
    [6];
    [1];
    [2]
  ] in

  let col_clues = [
    [1; 2];
    [3; 1];
    [1; 5];
    [7; 1];
    [5];
    [3];
    [4];
    [3]
  ] in

  find_solution row_clues col_clues

## Running the Solver

To run the solver, compile and execute the v2nonograms.ml file

ocamlc -o nonogram_solver v2nonograms.ml
./nonogram_solver
