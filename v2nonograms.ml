(* Nonogram Solver in OCaml *)

(* Define the possible states of a cell *)
type cell_state = Filled | Empty

(* Function to print the solution grid *)
let print_grid grid =
  List.iter (fun row ->
    List.iter (fun cell ->
      match cell with
      | Filled -> print_string "X"  (* Print 'X' for filled cells *)
      | Empty  -> print_string "_"  (* Print '_' for empty cells *)
    ) row;
    print_newline ()  (* Move to the next line after each row *)
  ) grid

(* Generate all possible sequences for a line given the clues *)
let rec generate_sequences clues length =
  match clues with
  | [] -> [List.init length (fun _ -> Empty)]  (* If no clues, the line is all empty *)
  | clue :: rest ->
      let min_spaces = if rest = [] then 0 else 1 in  (* Minimum spaces needed after a block *)
      let total_clues_length = clue + min_spaces * (List.length rest) in
      let max_start = length - total_clues_length in  (* Maximum starting index for the block *)
      let rec place start acc =
        if start > max_start then acc  (* If start exceeds max_start, return accumulated sequences *)
        else
          let leading_empty = List.init start (fun _ -> Empty)  (* Leading empty cells before the block *)
          and block = List.init clue (fun _ -> Filled)    (* The filled cells for the current clue *)
          and empty_space = if rest = [] then [] else [Empty]  (* Space after the block if needed *)
          and remaining_length = length - start - clue - min_spaces in  (* Remaining length after placing block *)
          let rest_sequences = generate_sequences rest remaining_length in  (* Generate sequences for remaining clues *)
          let sequences = List.map (fun seq ->
            leading_empty @ block @ empty_space @ seq  (* Combine the parts to form a full sequence *)
          ) rest_sequences in
          place (start + 1) (acc @ sequences)  (* Recurse with next starting position *)
      in
      place 0 []  (* Start placing blocks from position 0 *)

(* Swap a grid (list of lists) *)
let swap grid =
  let rec swap_helper acc grid =
    if List.exists (fun row -> row = []) grid then List.rev acc  (* If any row is empty, we're done *)
    else
      let heads = List.map List.hd grid  (* Extract the first element from each row *)
      and tails = List.map List.tl grid  (* Extract the remaining elements from each row *)
      in
      swap_helper (heads :: acc) tails  (* Accumulate the heads and recurse with the tails *)
  in
  swap_helper [] grid  (* Initialize with an empty accumulator *)

(* Check if a sequence matches the known cells *)
let matches known sequence =
  List.for_all2 (fun k s ->
    match k with
    | Some state -> state = s  (* If the cell state is known, it must match *)
    | None -> true             (* If the cell state is unknown, any state is acceptable *)
  ) known sequence

(* Merge sequences to find definite cell states *)
let merge_sequences sequences =
  let length = List.length (List.hd sequences) in  (* Length of the sequences *)
  List.init length (fun idx ->
    let states = List.map (fun seq -> List.nth seq idx) sequences in  (* Get the states at position idx *)
    if List.for_all ((=) (List.hd states)) states then  (* If all states are the same at idx *)
      Some (List.hd states)  (* The cell state is definite *)
    else
      None  (* The cell state is ambiguous *)
  )

(* Solve the Nonogram puzzle *)
let find_solution row_clues col_clues =
  let num_rows = List.length row_clues in  (* Number of rows in the grid *)
  let num_cols = List.length col_clues in  (* Number of columns in the grid *)

  (* Initialize the grid with None (unknown cells) *)
  let initial_grid = List.init num_rows (fun _ ->
    List.init num_cols (fun _ -> None)
  ) in

  (* Recursive function to solve the puzzle *)
  let rec solve grid =
    (* Function to update a single line (row or column) based on the clues *)
    let update_line clues line =
      let possible_sequences = generate_sequences clues (List.length line) in  (* All possible sequences for the line *)
      let valid_sequences = List.filter (matches line) possible_sequences in  (* Sequences that match known cells *)
      if valid_sequences = [] then failwith "No solution"  (* No valid sequences, puzzle is unsolvable *)
      else merge_sequences valid_sequences  (* Merge sequences to find definite cell states *)
    in

    (* Function to update the entire grid *)
    let rec update_grid grid =
      let updated_rows = List.map2 update_line row_clues grid in  (* Update all rows *)
      let swapd_grid = swap updated_rows in  (* swap the grid to work with columns *)
      let updated_cols = List.map2 update_line col_clues swapd_grid in  (* Update all columns *)
      let new_grid = swap updated_cols in  (* swap back to the original orientation *)
      if new_grid = grid then grid  (* If no changes were made, return the grid *)
      else update_grid new_grid  (* Otherwise, continue updating *)
    in

    try
      let updated_grid = update_grid grid in  (* Attempt to update the grid *)
      if List.for_all (List.for_all (fun cell -> cell <> None)) updated_grid then
        Some (List.map (List.map Option.get) updated_grid)  (* Puzzle solved, return the solution *)
      else
        (* Find the first unknown cell to apply backtracking *)
        let rec find_unknown grid i =
          match grid with
          | [] -> None  (* No unknown cells found *)
          | row :: rest ->
              let rec find_in_row row j =
                match row with
                | [] -> find_unknown rest (i + 1)  (* Move to the next row *)
                | cell :: cells ->
                    if cell = None then Some (i, j)  (* Found an unknown cell at position (i, j) *)
                    else find_in_row cells (j + 1)  (* Continue searching in the current row *)
              in
              find_in_row row 0  (* Start searching from the first column *)
        in
        match find_unknown updated_grid 0 with
        | None -> Some (List.map (List.map Option.get) updated_grid)  (* No unknown cells, return the solution *)
        | Some (i, j) ->
            (* Try filling the unknown cell with 'Filled' *)
            let try_cell value =
              let new_grid = List.mapi (fun row_idx row ->
                if row_idx = i then
                  List.mapi (fun col_idx cell ->
                    if col_idx = j then Some value else cell  (* Set the cell at (i, j) to 'value' *)
                  ) row
                else row  (* Keep other rows unchanged *)
              ) updated_grid in
              solve new_grid  (* Recursively attempt to solve with the new grid *)
            in
            (match try_cell Filled with
             | Some solution -> Some solution  (* If successful, return the solution *)
             | None -> try_cell Empty)         (* Otherwise, try with 'Empty' *)
    with Failure _ -> None  (* If an exception occurs (no solution), return None *)
  in

  match solve initial_grid with
  | Some solution -> print_grid solution  (* If a solution is found, print the grid *)
  | None -> print_endline "No solution"  (* Otherwise, print an error message *)


(* Example usage *)
let () =
  (* Define the clues for rows and columns *)
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

  (* Solve the puzzle *)
  find_solution row_clues col_clues
