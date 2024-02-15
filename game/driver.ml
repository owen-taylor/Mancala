let initial_board_vals = [4; 4; 4; 4; 4; 4; 0; 4; 4; 4; 4; 4; 4; 0];;
let board_spacers = ["\n|   | "; " | "; " | "; " | "; " | "; " | "; " | "; " | \n|   |-----------------------|   | \n| "; " | "; " | "; " | "; " | "; " | "; " | "; " |   |";" |"];;

let rec fillBoard (board_vals: string list) (board_spacers: string list) (return_board: string) = 
  match board_spacers with
  | [] -> return_board
  | h1::t1 ->
    match board_vals with
      |h2::t2 -> 
        if List.length t2 == 7 then
          let rTail = List.rev t2 in
          fillBoard (rTail) (t1) (return_board ^ h1 ^ h2)
        else
          fillBoard (t2) (t1) (return_board ^ h1 ^ h2)
      |[] -> 
        return_board ^ h1;;

let printBoard (board) =
  let strBoard = List.map string_of_int board in
  let filledBoard = fillBoard (strBoard) (board_spacers) ("") ^ "\n" in
  "\n             Player 1" ^
  "\n      1   2   3   4   5   6" ^
  "\n---------------->----------------" ^
  filledBoard ^
  "----------------<----------------\n" ^
  "      6   5   4   3   2   1\n" ^
  "             Player 2\n";;

let int_of_string_default str = 
  try int_of_string str
  with Failure _ -> -10;;

let printTurn board player1Turn = 
    let playerString = if player1Turn then "Player 1" else "Player 2" in
    let directions = playerString ^ ", Select the marbles you would like to move (1--6): " in 
  
    let printableBoard = printBoard board in
  
    let () = print_string printableBoard in 
    let () = print_string directions in 
  
    int_of_string_default (read_line());;

let setGoalsToTotalScore i x player1Total player2Total = 
  if x != -2 then
    if i == 6 then
      player1Total
    else
      if i == 13 then
        player2Total
      else
        0
  else
    0;;

let depositLeftoverMarbles (board: int list) = 
  let player1Board = List.filteri (fun i x -> i < 7 && x != -2) board in
  let player1Total = List.fold_left (fun acc x -> acc + x) 0 player1Board in

  let player2Board = List.filteri (fun i x -> i > 6 && x != -2) board in
  let player2Total = List.fold_left (fun acc x -> acc + x) 0 player2Board in 

  List.mapi (fun i x -> setGoalsToTotalScore i x player1Total player2Total ) board;;

let gameNotOver board = 
  match board with
  | 0::0::0::0::0::0::_ -> false
  | _::_::_::_::_::_::_::0::0::0::0::0::0::_ -> false
  | _ -> true;;

let endGame (gameBoard: int list) = 
  let finalBoard = depositLeftoverMarbles gameBoard in 
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
    let () = print_string(printBoard (depositLeftoverMarbles gameBoard)) in
    true
    
  else 
    if player2Score > player1Score then
      let () = print_string("Player 2 won!!!") in 
      let () = print_string(printBoard (depositLeftoverMarbles gameBoard)) in
      true

    else
      let () = print_string("Draw :(") in 
      let () = print_string(printBoard (depositLeftoverMarbles gameBoard)) in 
      true;;

let zeroOutCell board cell =
  List.mapi (fun i x -> if i == cell then 0 else x) board;;

let notValidCell cellNumber player1Turn = 
  if player1Turn then 
    not (cellNumber+1 >=1 && cellNumber+1 <= 6) 
  else 
    not (cellNumber-6 >=1 && cellNumber-6 <= 6);;

let normalizeLappedBoard cell = 
  if cell > 13 then
    cell - ((cell / 14) * 14)
  else
    cell

let playerLandedInGoal cellNumber player1Turn = 
  if player1Turn then
    if cellNumber == 6 then
      true
    else
      false
  else
    if cellNumber == 13 then
      true
    else
      false;;

let playerLandedInEmptySpace board lastCell player1Turn = 
  if lastCell <= 12 then
    let numberOfMarblesInLastCell = List.nth board lastCell in
    let numberOfMarblesInOpponentCell = List.nth board (12-lastCell) in

    if player1Turn && lastCell >= 0 && lastCell <= 5 && numberOfMarblesInOpponentCell > 0 then
      if numberOfMarblesInLastCell == 0 then
        true
      else
        false
    else
      if lastCell >= 7 && lastCell <= 12 && numberOfMarblesInOpponentCell > 0 then
        if numberOfMarblesInLastCell == 0 then
          true
        else
          false
      else
        false
  else
    false;;

let moveStealToGoal board goal playerCell opponentCell = 
  let stealValue = (List.nth board opponentCell) + 1 in
  let boardWithStealInGoal = List.mapi (fun i x -> if i == goal then x + stealValue else x) board in 
  List.mapi (fun i x -> if i == playerCell || i == opponentCell then 0 else x) boardWithStealInGoal


let steal board newCell player1Turn = 
  let goal = if player1Turn then 6 else 13 in 
  let opponentCell = 12-newCell in
  moveStealToGoal board goal newCell opponentCell;;

let rec moveMarbles (marblesLeft) (index) (board: int list) (player1Turn: bool) = 
  if marblesLeft == 0 then
    board
  else
    if (player1Turn == true) && (index == 13) then
      moveMarbles (marblesLeft) (0) (board) (player1Turn)
    else
      if (player1Turn == false) && (index == 6) then
        moveMarbles (marblesLeft) (7) (board) (player1Turn)
      else 
        if index == 14 then
          moveMarbles (marblesLeft) (0) (board) (player1Turn)
        else
          let boardWithAddedMarble = List.mapi (fun i x -> if i == index then x+1 else x) board in
          moveMarbles (marblesLeft-1) (index + 1) (boardWithAddedMarble) (player1Turn);;

let rec playGame (gameBoard: int list)(player1Turn: bool) = 
    
  if gameNotOver gameBoard then
    let pickedCell = (printTurn gameBoard player1Turn) + if player1Turn then -1 else 6 in

    if notValidCell pickedCell player1Turn then
      let() = print_string("Please choose an integer between 1 and 6") in 
      playGame gameBoard player1Turn

    else
        let numberOfMarblesPicked = List.nth gameBoard pickedCell in
        let lastCell = normalizeLappedBoard (pickedCell + numberOfMarblesPicked) in
        let zeroedOutCell = zeroOutCell gameBoard pickedCell in

        let newBoard = moveMarbles numberOfMarblesPicked (pickedCell+1) zeroedOutCell player1Turn in

        if playerLandedInGoal lastCell player1Turn then
          let () = if player1Turn then print_string("Player 1 goes again!") else print_string("Player 2 goes again!") in
          playGame newBoard player1Turn

        else
          if playerLandedInEmptySpace gameBoard lastCell player1Turn then
            let () = if player1Turn then print_string("Player 1 Stole!") else print_string("Player 2 Stole") in
            playGame (steal newBoard lastCell player1Turn) (not player1Turn)
          else
            playGame newBoard (not player1Turn)
  else
    endGame gameBoard;;

playGame initial_board_vals true
