
let initial_board_vals = [4; 4; 4; 4; 4; 4; 0; 4; 4; 4; 4; 4; 4; 0];;
let board_spacers = ["\n|   | "; " | "; " | "; " | "; " | "; " | "; " | "; " | \n|   |-----------------------|   | \n| "; " | "; " | "; " | "; " | "; " | "; " | "; " |   |";" |"];;

let rec filledBoardFunc (board_vals: string list) (board_spacers: string list) (return_board: string) = 
  match board_spacers with
  | [] -> return_board
  | h1::t1 ->
    match board_vals with
      |h2::t2 -> 
        if List.length t2 == 7 then
          let rTail = List.rev t2 in
          filledBoardFunc (rTail) (t1) (return_board ^ h1 ^ h2)
        else
          filledBoardFunc (t2) (t1) (return_board ^ h1 ^ h2)
      |[] -> 
        return_board ^ h1

let convertIntToStr x = string_of_int x

let prettyPrint (x: string) =
  "\n             Player 1" ^
  "\n      1   2   3   4   5   6" ^
  "\n---------------->----------------" ^
  x ^
  "----------------<----------------\n" ^
  "      6   5   4   3   2   1\n" ^
  "             Player 2\n"

let rec moveMarbles value index (board: int list) = 
  if value == 0 then
    board
  else
    if index == 14 then
      moveMarbles (value+1) (-1) (board)
    else
      let boardInc = List.mapi (fun i x -> if i == index+1 then x+1 else x) board in
      moveMarbles (value-1) (index + 1) (boardInc)



let rec game_func (gameBoard: int list)(t: bool) = 
  match gameBoard with
    | 0::0::0::0::0::0::_ -> 
      let strBoard = List.map convertIntToStr gameBoard in 
      prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n" )
    | _::_::_::_::_::_::_::0::0::0::0::0::0::_ -> 
      let strBoard = List.map convertIntToStr gameBoard in 
      prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n" )
    | _ ->
      if t == true then
        (*player 1 turn*)
        let strBoard = List.map convertIntToStr gameBoard in
        let () = print_string(prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n" )) in
        let () = print_string("Player 1, Select the marbles you would like to move (1-6): ") in
        let cellPicked = read_int() in
        let newBoard = moveMarbles (List.nth gameBoard (cellPicked - 1))(cellPicked - 1)(List.mapi (fun i x -> if i == cellPicked -1 then 0 else x) gameBoard) in
        if List.nth gameBoard (cellPicked-1) + cellPicked-1 == 6 then
          let () = print_string("Player 1 goes again!") in
          game_func newBoard t
        else
          game_func newBoard (not t)
      else
        (*player 2 turn*)
        let strBoard = List.map convertIntToStr gameBoard in 
        let () = print_string(prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n" )) in
        let () = print_string("Player 2, Select the marbles you would like to move (1-6): ") in
        let cellPicked = read_int() in
        let newBoard = moveMarbles (List.nth gameBoard (cellPicked + 6))(cellPicked + 6)(List.mapi (fun i x -> if i == cellPicked+6 then 0 else x) gameBoard) in
        if List.nth gameBoard (cellPicked+6) + cellPicked+6 == 13 then
          let () = print_string("Player 2 goes again!") in
          game_func newBoard t
        else
          game_func newBoard (not t);;


game_func initial_board_vals true

(* let rec run_game n t = 

  if t == true then
    let () = print_string("Player 1, select which marbles you want to move: ") in 
      let cellPicked = read_int() in  *)


(* let test = read_int();;
print_int test; *)