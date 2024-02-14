
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
let prettyPrint (x: string) =
  "\n             Player 1" ^
  "\n      1   2   3   4   5   6" ^
  "\n---------------->----------------" ^
  x ^
  "----------------<----------------\n" ^
  "      6   5   4   3   2   1\n" ^
  "             Player 2\n"

let rec moveMarbles (value) (index) (board: int list) (turn: bool) = 
  if value == 0 then
    board
  else
    if (turn == true) && (index == 12) then
      moveMarbles (value) (-1) (board) (turn)
    else
      if (turn == false) && (index == 5) then
        moveMarbles (value) (6) (board) (turn)
      else 
        if index == 14 then
          moveMarbles (value+1) (-1) (board) (turn)
        else
          let boardInc = List.mapi (fun i x -> if i == index+1 then x+1 else x) board in
          moveMarbles (value-1) (index + 1) (boardInc) (turn)

let endGameDepositLogic i x player1Total player2Total = 
  if x != -2 then
    if i == 6 then
      player1Total
    else
      if i == 13 then
        player2Total
      else
        0
  else
    0


let depositEndGameMarbles (board: int list) = 
  let player1Board = List.filteri (fun i x -> i < 7 && x != -2) board in
  let player1Total = List.fold_left (fun acc x -> acc + x) 0 player1Board in

  let player2Board = List.filteri (fun i x -> i > 6 && x != -2) board in
  let player2Total = List.fold_left (fun acc x -> acc + x) 0 player2Board in 

  List.mapi (fun i x -> endGameDepositLogic i x player1Total player2Total ) board

let endGame (gameBoard: int list) = 
  let finalBoard = depositEndGameMarbles gameBoard in 
  let player1Score = 
    match finalBoard with
    | _::_::_::_::_::_::x::_ -> x 
    | _ -> 0 in
  let player2Score = 
    match finalBoard with
    | _::_::_::_::_::_::_::0::0::0::0::0::0::x::_ -> x
    | _ -> 0 in

  if player1Score > player2Score then
    let () = print_string("Player 1 won!!!") in 
    let strBoard = List.map string_of_int (depositEndGameMarbles gameBoard) in 

    let () = print_string(prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n")) in
    true
  else 
    if player2Score > player1Score then
      let () = print_string("Player 2 won!!!") in 
      let strBoard = List.map string_of_int (depositEndGameMarbles gameBoard) in 
  
      let () = print_string(prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n")) in
      true
    else
      let () = print_string("Draw :(") in 
      let strBoard = List.map string_of_int (depositEndGameMarbles gameBoard) in 
  
      let () = print_string(prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n")) in 
      true

let rec game_func (gameBoard: int list)(t: bool) = 
  let int_of_string_default str = 
    try int_of_string str
    with Failure _ -> 0 in
    
  match gameBoard with
    | 0::0::0::0::0::0::_ -> 
      endGame gameBoard
    | _::_::_::_::_::_::_::0::0::0::0::0::0::_ -> 
      endGame gameBoard
    | _ ->
      if t == true then
        (*player 1 turn*)
        let strBoard = List.map string_of_int gameBoard in
        let () = print_string(prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n" )) in
        let () = print_string("Player 1, Select the marbles you would like to move (1--6): ") in
        let input = read_line() in

        let cellPicked = int_of_string_default input in

        if cellPicked < 1 || cellPicked > 6 then
          let() = print_string("Please choose an integer between 1 and 6") in 
          game_func gameBoard t
        else
          let newBoard = moveMarbles (List.nth gameBoard (cellPicked - 1))(cellPicked - 1)(List.mapi (fun i x -> if i == cellPicked -1 then 0 else x) gameBoard)(t) in
          if (List.nth gameBoard (cellPicked-1)) + cellPicked-1 == 6 then
            let () = print_string("Player 1 goes again!") in
            game_func newBoard t
          else
            game_func newBoard (not t)
      else
        (*player 2 turn*)
        let strBoard = List.map string_of_int gameBoard in 
        let () = print_string(prettyPrint (filledBoardFunc (strBoard) (board_spacers) ("") ^ "\n" )) in
        let () = print_string("Player 2, Select the marbles you would like to move (1-6): ") in
        let input = read_line() in

        let cellPicked = int_of_string_default input in 

        if cellPicked < 1 || cellPicked > 6 then
          game_func gameBoard t
        else
          let newBoard = moveMarbles (List.nth gameBoard (cellPicked + 6))(cellPicked + 6)(List.mapi (fun i x -> if i == cellPicked+6 then 0 else x) gameBoard)(t) in
          if (List.nth gameBoard (cellPicked+6)) + cellPicked+6 == 13 then
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