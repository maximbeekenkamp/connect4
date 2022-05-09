open! CS17SetupGame;   
open Game; 

module Connect4 = {
  /* player 1 is P1, player 2 is P2 */
  type whichPlayer =
    | P1
    | P2;

  /* either a player has won, it's a draw, or it's ongoing */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  /* a state has a status and the representation of the board, which
    is a list of column lists */
  type state =
    | State(status, list(list(int)));

  /* a move is a type that contains an integer representing the column the
    player wishes to place their stone in */
  type move =
    | Move(int);

  /*
  Input: player, a whichPlayer
  Output: an integer representing the player
  */
  let playerToID: whichPlayer => int =
    player =>
      switch (player) {
      | P1 => 1
      | P2 => 2
      };

  /*
  Input: a whichPlayer
  Output: the other whichPlayer
  */
  let otherPlayer: whichPlayer => whichPlayer =
    player =>
      switch (player) {
      | P1 => P2
      | P2 => P1
      };

  /*
  Input: width, an integer > 0 representing the width of the board
          column, a list of 0s representing an empty column of the board
  Output: a list of column lists of 0 that represents the empty board!

  OI 2, [0, 0]
  RI 1, [0, 0]
  RO [[0, 0]]
  OO [[0, 0], [0, 0]]

  OI 1, [0, 0]
  RI 0, [0, 0]
  RO []
  OO [[0, 0]]
  */

  let rec makeRows: (int, list(int)) => list(list(int)) =
    (width, column) =>
      switch (width) {
      | 0 => []
      | _ => [column, ...makeRows(width - 1, column)]
      };

  /*
  Input: height, an integer > 0 representing the height of the board
  Output: a list of 0s that represents an empty column of the board

  OI 2
  RI 1
  RO [0]
  OO [0, 0]

  OI 1
  RI 0
  RO []
  OO [0]

  */

  let rec makeColumn: int => list(int) =
    height =>
      switch (height) {
      | 0 => []
      | _ => [0, ...makeColumn(height - 1)]
      };

  /*
  Input: width, an integer representing the width of the board
          height, an integer representing the height of the board
  Output: a list of column lists of 0 that represents the empty board!
  */

  let makeBoard: (int, int) => list(list(int)) =
    (width, height) =>
      if (width > 0 && height > 0) {
        makeRows(width, makeColumn(height));
      } else {
        failwith("makeBoard: cannot make board w dims < 1");
      };

  /*
  Input: s, a string that represents the board dimensions
  Output: the initial state of the game! P1 goes first
  */

  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let boardHeight = getBoardHeight(boardDims);
      let boardWidth = getBoardWidth(boardDims);
      /*the board is represented as a LIST OF COLUMNS!!!!!!!!!!!*/
      if (boardWidth < 4 || boardHeight < 4) {
        failwith("initialState: domain error (can't be smaller than 4x4)");
      } else {
        State(Ongoing(P1), makeBoard(boardWidth, boardHeight));
      };
    };

  /*
  Input: a whichPlayers
  Output: a string representing the whichPlayer
  */
  let stringOfPlayer: whichPlayer => string =
    p =>
      switch (p) {
      | P1 => "P1"
      | P2 => "P2"
      };

  /*
  Input: a move
  Output: a string explaining which move was selected
  */
  let stringOfMove: move => string =
    inMove => {
      let Move(m) = inMove;
      switch (m) {
      | m =>
        "\nPlayer chose to place their token in column "
        ++ string_of_int(m + 1)
        ++ "."
      };
    };

  /*
  Input: board, a list of int lists
          count, a integer that counts
          rowIndex, an integer representing the desired row Index.
                    this needs to be within the bounds of the board.
  Output: no output, but prints a string of the contents of that row within the board to the terminal

  OI [[0, 1], [0, 2], [0, 4]], 0, 1
  RI [[0, 1], [0, 2], [0, 4]], 1, 1
  RO nothing, command line would get 1  2  4
  OO nothing, command line would get 2  4

  OI [[0, 1], [0, 2], [0, 4]], 1, 1
  RI [[0, 1], [0, 2], [0, 4]], 2, 1
  RO nothing, command line would get 2  4
  OO nothing, command line would get 4
  */

  let rec anyRowToStringHelper: (list(list(int)), int, int, int) => unit =
    (board, count, rowIndex, width) =>
      if (count < width) {
        print_string(
          string_of_int(List.nth(List.nth(board, count), rowIndex)) ++ "  ",
        );
        anyRowToStringHelper(board, count + 1, rowIndex, width);
      } else {
        print_string("");
      };

  /*
  Input: board, a list of int lists
          rowIndex, an integer representing the desired row Index.
                    this needs to be within the bounds of the board.
  Output: no output, but prints a string of the contents of that row within the board to the terminal
  */
  let anyRowToString: (list(list(int)), int) => unit =
    (board, rowIndex) =>
      anyRowToStringHelper(board, 0, rowIndex, List.length(board));

  /*
  Input: board, a list of int lists
          count, an integer that acts as a counter
          height, an integer representing the total height of the board

  Output: the whole board as a string output to the terminal

  Recursive Diagrams where the terminal output is depicted as a string...

  OI: [[1], [2], [3]], 1, 3
  RI: [[1], [2], [3]], 2, 3
  RO: "3 \n \n"
  OO: "2 \n 3 \n \n"

  OI: [1], [2], [3]], 2, 3
  RI: [1], [2], [3]], 3, 3
  RO: "\n"
  OO: "3 \n \n"
  */
  let rec stateToString: (list(list(int)), int, int) => unit =
    (board, count, height) =>
      if (count < height) {
        let _x = anyRowToString(board, count);
        print_string("\n");
        stateToString(board, count + 1, height);
      } else {
        print_string("\n");
      };

  /*
  Input: inState, a state
  Output: a string representing/responding to the inputted state
  */
  let stringOfState: state => string =
    inState =>
      switch (inState) {
      | State(Win(p), board) =>
        stateToString(board, 0, List.length(List.nth(board, 0)));
        stringOfPlayer(p) ++ " has four in a row and wins!";
      | State(Draw, board) =>
        stateToString(board, 0, List.length(List.nth(board, 0)));
        "There are no more spots on the board. It is a draw.";
      | State(Ongoing(p), board) =>
        stateToString(board, 0, List.length(List.nth(board, 0)));
        "It is " ++ stringOfPlayer(p) ++ "'s turn. The board is displayed above.";
      };

  /*
  Input: board, a list of int lists that represents the board
          rowIndex, an integer representing the desired row to be output
  Output: a list of ints representing the row at the desired index
  */
  let anyRow: (list(list(int)), int) => list(int) =
    (board, rowIndex) => List.map(x => List.nth(x, rowIndex), board);

  /* takes in the top row as a list, outputs a list of moves
    Input:
        topRowBoard, an int list representing the top row of the board
        count, an integer that counts how many times the recursive function
                has run, starting at 0
    Output:
        a list of legal moves based on the top row input

    OI [0, 0, 1], 0
    RI [0, 1], 1
    RO [Move(1)]
    OO [Move(0), Move(1)]

    OI [0, 1], 1
    RI [1], 2
    RO []
    OO [Move(1)]
      */
  let rec legalMovesHelper: (list(int), int) => list(move) =
    (topRowBoard, count) =>
      switch (topRowBoard) {
      | [] => []
      | [hd, ...tl] =>
        switch (hd) {
        | 0 => [Move(count), ...legalMovesHelper(tl, count + 1)]
        | _ => legalMovesHelper(tl, count + 1)
        }
      };

  /* produces the list of legal moves at a state
    Input:
        currentState, a state
    Output:
        a list of moves that are legal
    */
  let legalMoves: state => list(move) =
    currentState =>
      switch (currentState) {
      | State(_, board) => legalMovesHelper(anyRow(board, 0), 0)
      };

  /* returns the status of the game at the given state
    Input:
        inState, a state
    Output:
        the status of the game
    */
  let gameStatus: state => status =
    inState => {
      let State(p, _) = inState;
      p;
    };

  /*
  Input: column, an list of integers representing the column
                 that was selected to have a player's stone placed in
                 this list should not be a full column (it needs to have 
                 at least one open space)
         index, the current index for recursion. this will always start at 0
                and iterate upwards until it hits the first non-0 element
         columnHeight, the length of the column input
  Output: an integer representing the row index in which the stone
          will fall when it is subject to gravity

  OI [0, 0, 2, 1], 0, 4
  RI [0, 0, 2, 1], 1, 4
  RO 1
  OO 1

  OI [0, 0, 2, 1], 1, 4
  RI [0, 0, 2, 1], 2, 4
  RO 1
  OO 1

  */
  let rec gravity: (list(int), int, int) => int =
    (column, index, columnHeight) =>
      if (index == columnHeight - 1) {
        index;
      } else if (List.nth(column, index + 1) > 0) {
        index;
      } else {
        gravity(column, index + 1, columnHeight);
      };

  /*
  Input: x, a list of anything! this allows us to use it to split
            and add a new item to a list(int) OR column to a list(list(int))
          index, an integer representing the index at which
                the thingToAdd should be added to x. 
                The index must be less than the length of x.
          count, an integer that starts at 0 and counts
          thingToAdd, an item of the same type as x contains that
                      will be added to x in the correct index

  Output: a list('a) where the thingToAdd has been swapped out with
          what was originally in x at the inputted index.

  OI: [0, 1, 2, 3, 4], 3, 0, 5
  RI: [1, 2, 3, 4], 3, 1, 5
  RO: [2, 5, 4]
  OO: [0, 1, 2, 5, 4]

  OI: [2, 3, 4], 1, 0, 5
  RI: [3, 4], 1, 1, 5
  RO: [5, 4]
  OO: [2, 5, 4]

  OI: [0, 0, 4], 2, 0, 5
  RI: [0, 4], 2, 1, 5
  RO: [0, 5]
  OO: [0, 0, 5]

  */
  let rec splitListAdd: (list('a), int, int, 'a) => list('a) =
    (x, index, count, thingToAdd) =>
      switch (x, index - count) {
      | ([], _) => failwith("splitListAdd: empty list input.")
      | ([_hd, ...tl], 0) => [thingToAdd, ...tl]
      | ([hd, ...tl], _) => [
          hd,
          ...splitListAdd(tl, index, count + 1, thingToAdd),
        ]
      };

  /* Input: currentBoard, an list of int lists representing the board
            inMove, the move that was input
            playerID, an int representing the player's stone
    Output: a new list of int lists representing the board after the player's move */

  let newBoard: (list(list(int)), move, int) => list(list(int)) =
    (currentBoard, inMove, playerID) => {
      let Move(x) = inMove;
      let columnOfChoice = List.nth(currentBoard, x);
      let newColumn =
        splitListAdd(columnOfChoice, gravity(columnOfChoice, 0, List.length(columnOfChoice)), 0, playerID);
      splitListAdd(currentBoard, x, 0, newColumn);
    };

  /*
  Input: hvd, a list of ints representing a row, a column, or a diagonal on the board
          rI, an index representing the index at which the stone was dropped
              into hvd
  Output:
          a boolean representing whether or not putting the stone in resulted in victory
          on the axis that is being assessed (h, v, or d)
  */

  let victoryHelper: (list(int), int) => bool =
    (hvd, index) => {
      let x = List.nth(hvd, index);
      if (List.length(hvd) < 4) {
        false;
      } else if (x == 0) {
        false;
      } else {
        x == List.nth(hvd, index + 1)
        && x == List.nth(hvd, index + 2)
        && x == List.nth(hvd, index + 3);
      };
    };

  /*
  Input: resultBoard, a list of int lists that represents the board after the last move
          columnIndex, an int that is the index representing the column at which the
                      stone was placed
          rowIndex, an int that is the index representing the row at which the stone was placed
          heightOfBoard, an int representing the total height of the entire board
  Output: a boolean representing whether or not the last move caused a victory on the vertical
          axis
  */
  let verticalVictory: (list(list(int)), int, int, int) => bool =
    (resultBoard, columnIndex, rowIndex, heightOfBoard) =>
      if (heightOfBoard - rowIndex > 3) {
        victoryHelper(List.nth(resultBoard, columnIndex), rowIndex);
      } else {
        false;
      };

  /*
  Here is where the horizontal/diagonal victory checkers begin. Rather than
  transposing the board, as was advised in class, we chose to implement a strategy
  that I think is more efficient when boards become large. 
  
  If we are transposing the board for horizontal and diagonal victories, that's a lot
  of operations after every move. By not transposing the board, our program is faster.
  
  Also, we decided not to search the whole diagonal/horizontal for potential victories.
  What if the board is 1000x1000? Then scanning the row takes forever and is pointless because
  for every move, there are only really 4 'potential' victories on each axis (except vertical 
  where there is only one). It could either be X111 1X11 11X1 or 111X where X is the most recently 
  placed 1. Therefore, this implementation of checking for victory on horizontals is efficient

  Input: index, an index
  Output: the index at which to start seeking the leftmost
          token that the player maay have placed, accounting for
          the edges of the board
  */
  let startingColumnIndex: int => int =
    index =>
      switch (index) {
      | 0 => 0
      | 1 => 0
      | 2 => 0
      | x => x - 3
      };

  /*
  Input: row, a list of ints representing a row
          startingCI, an int that is the starting column index
          cI, an int that is the index at which the stone was placed
          widthOfBoard, an int representing the total width of board
  Output: a bool representing whether or not there is a victory generated
          by placing the stone in the slot. for a normal entry (provided the
          placement is 3 units from the walls), there will be 4 potential methods
          of winning

  OI [1, 2, 1, 1, 1, 1, 2, 2, 2], 0, 2, 9
  RI [1, 2, 1, 1, 1, 1, 2, 2, 2], 1, 2, 9
  RO True
  OO True

  OI [1, 2, 1, 1, 1, 1, 2, 2, 2], 1, 2, 9
  RI [1, 2, 1, 1, 1, 1, 2, 2, 2], 2, 2, 9
  RO True
  OO True
  */

  let rec horzVictoryHelper: (list(int), int, int, int) => bool =
    (row, startingCI, cI, widthOfBoard) =>
      if (widthOfBoard
          - startingCI > 3
          && startingCI <= cI
          && widthOfBoard >= 4) {
        victoryHelper(row, startingCI)
        || horzVictoryHelper(row, startingCI + 1, cI, widthOfBoard);
      } else {
        false;
      };

  /*
  Input:
      resultBoard, an list of int lists representing the full board after the move
      columnIndex, an integer representing the column index where the stone was placed
      rowIndex, an integer representing the row index where the stone was placed
  Output:
      a boolean representing whether or not there was a victory in the row where it was placed
      all of our helper functions and if clauses within thehelper functions make our code
      more efficient on board of massive dimensions. We wouldn't want to look through the whole
      row if the row was 100 units long. We would only want to assess, at most, the four potential
      winning positions based on where the stone was placed.
  */
  let horzVictory: (list(list(int)), int, int) => bool =
    (resultBoard, columnIndex, rowIndex) => {
      let row = anyRow(resultBoard, rowIndex);
      horzVictoryHelper(
        row,
        startingColumnIndex(columnIndex),
        columnIndex,
        List.length(resultBoard),
      );
    };

  /*
  Input:
    board, a list of int lists representing the game board
    cI, an integer that is the column index of the desired value
    rI, an integer that is the row index of the desired value
  Output:
    if non-negative indices that are within the bounds of the board, the item on the board at
      column cI and row rI wrapped in a Some() option type. otherwise None
  */
  let diagonalHelper: (list(list(int)), int, int) => option(int) =
    (board, cI, rI) =>
      if (cI < 0 || rI < 0) {
        None;
      } else if (cI >= List.length(board) || rI >= List.length(List.hd(board))) {
        None;
      } else {
        Some(List.nth(List.nth(board, cI), rI));
      };

  /*
  Input:
    board, a list of int lists representing the game board
    cI, an integer that is the column index of the desired value
    rI, an integer that is the row index of the desired value
    cDir, an integer that determines whether to move left or right
          through the columns, should be 1 or -1
    rDir, an integer that determines whether to move up or down
          through the rows, should be 1 or -1
  Output:
    a list of integers representing the line starting from the input cI rI and moving
        in the direction specified by cDir and rDir. this allows to get diagonals moving in both
        directions

  OI [[1, 2, 3], [4, 5, 6], [7, 8, 9]], 0, 0, 1, 1
  RI [[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 1, 1, 1
  RO [1, 5, 9]
  OO [5, 9]

  OI [[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 1, 1, 1
  RI [[1, 2, 3], [4, 5, 6], [7, 8, 9]], 2, 2, 1, 1
  RO [5, 9]
  OO [9]

  */

  let rec diagonalMaker: (list(list(int)), int, int, int, int) => list(int) =
    (board, cI, rI, cDir, rDir) => {
      let diagonalItem = diagonalHelper(board, cI, rI);
      switch (diagonalItem) {
      | None => []
      | Some(x) => [
          x,
          ...diagonalMaker(board, cI + cDir, rI + rDir, cDir, rDir),
        ]
      };
    };

  /*
  Input:
    board, a list of int lists representing the game board
    cI, an integer that is the column index of the stone placed
    rI, an integer that is the row index of the stone placed
  Output:
    a int list that is the diagonal that runs from NW to SE in the actual board
    that passes through that inputted stone

  here is a depiction of what NWtoSE means on the a 4x4 grid if we selected row and column index @ 0, 0
  this is not a depiction of column lists, this is the 'real' board
  the Xs would be output as a list from L to R

  X 0 0 0
  0 X 0 0
  0 0 X 0
  0 0 0 X
  */
  let diagonalNWtoSE: (list(list(int)), int, int) => list('a) =
    (board, cI, rI) =>
      List.append(
        List.rev(diagonalMaker(board, cI - 1, rI - 1, -1, -1)),
        /*subtracting 1 to make sure we don't double
          count the input stone*/
        diagonalMaker(board, cI, rI, 1, 1),
      );

  /*
  Input:
    board, a list of int lists representing the game board
    cI, an integer that is the column index of the stone placed
    rI, an integer that is the row index of the stone placed
  Output:
    a int list that is the diagonal that runs from NW to SE in the actual board
    that passes through that inputted stone

  here is a depiction of what NEtoSW means on the a 4x4 grid if we selected row and column index @ 4, 0
  this is not a depiction of column lists, this is the 'real' board
  the Xs would be output as a list from L to R

  0 0 0 X
  0 0 X 0
  0 X 0 0
  X 0 0 0
  */
  let diagonalNEtoSW: (list(list(int)), int, int) => list('a) =
    (board, cI, rI) =>
      List.append(
        List.rev(diagonalMaker(board, cI, rI, 1, -1)),
        diagonalMaker(board, cI - 1, rI + 1, -1, 1),
        /*subtracting 1 to make sure we don't double
          count the input stone*/
      );

  /*
  Input:
    board, a list of int lists representing the game board
    cI, an integer that is the column index of the stone placed
    rI, an integer that is the row index of the stone placed
  Output:
    a bool representing if there is a victory on either diagonal axis
  */
  let diagonalVictory: (list(list(int)), int, int) => bool =
    (board, cI, rI) => {
      let diagonal1 = diagonalNWtoSE(board, cI, rI);
      let diagonal2 = diagonalNEtoSW(board, cI, rI);
      horzVictoryHelper(
        diagonal1,
        0,
        List.length(diagonal1),
        List.length(diagonal1),
      )
      || horzVictoryHelper(
          diagonal2,
          0,
          List.length(diagonal2),
          List.length(diagonal2),
        );
    };

  /* takes the board and move before move has already been made
    Input:
        inState, the current state
        inMove, the current move
    Output:
        a bool representing if there is a victory on the horizontal,
        vertical, or diagonal axes
    */
  let winningBoard: (state, move) => bool =
    (inState, inMove) =>
      switch (inState, inMove) {
      | (State(Ongoing(player), board), Move(cI)) =>
        let col = List.nth(board, cI);
        let rI = gravity(col, 0, List.length(col));
        let nextBoard = newBoard(board, inMove, playerToID(player));
        /* the if statements below aren't necessary because they are if true output true else false.
          they are only there if you really want to implement printing the victory mode on the command line
          this is currently commented out because it becomes annoying with the AI's minimax triggering states
          of potential victory and outputting the strings, but this is useful when
          you are playing a human vs. human game*/
        (
          if (horzVictory(nextBoard, cI, rI)) {
            true /*print_string("Victory on horizontal axis or more. \n");*/;
          } else {
            false;
          }
        )
        || (
          if (verticalVictory(
                nextBoard,
                cI,
                rI,
                List.length(List.nth(board, 0)),
              )) {
            true /*print_string("Victory on vertical axis or more. \n");*/;
          } else {
            false;
          }
        )
        || (
          if (diagonalVictory(nextBoard, cI, rI)) {
            true /*print_string("Victory on diagonal axis. \n");*/;
          } else {
            false;
          }
        );
      | (_, _) => false
      };

  /* for transforming human player input into
      internal representation of move
      Input: str, a string
              inState, the state
      Output: a move representing the input move,
              fails if string is nonsense or illegal
    */

  let moveOfString: (string, state) => move =
    (str, inState) =>
      switch (int_of_string(str) - 1) {
      | num =>
        if (List.mem(Move(num), legalMoves(inState))) {
          Move(num);
        } else {
          failwith(
            "Not a legal move. Did you go past the max index? Or is that column full?",
          );
        }
      };

  /*
  Input:
    inState, a state
    inMove, a move
  Output:
    the new state of the board after applying the move
  */
  let nextState: (state, move) => state =
    (inState, inMove) =>
      switch (inState, inMove) {
      | (State(Ongoing(player), currentBoard), _) =>
        let nextBoard = newBoard(currentBoard, inMove, playerToID(player));
        if (winningBoard(inState, inMove)) {
          State(Win(player), nextBoard);
        } else if (legalMoves(State(Ongoing(P2), nextBoard)) == []) {
          State(Draw, nextBoard);
        } else {
          State(Ongoing(otherPlayer(player)), nextBoard);
        };
      | _ => failwith("nextState called on a non-ongoing game.")
      };

  /*
  Input:
    sublist, a list of integers
    mainlist, a non-empty list of integers that is of longer length than mainlist
  Output:
    a boolean representing whether or not the mainlist STARTS with the sublist

  OI: [0, 1, 2], [0, 1, 2, 3, 4]
  RI: [1, 2], [1, 2, 3, 4]
  RO: true
  OO: true

  OI: [2], [2, 3, 4]
  RI: [], [3, 4]
  RO: true
  OO: true
  */
  let rec patternSearchHelper: (list(int), list(int)) => bool =
    (sublist, mainlist) =>
      switch (sublist, mainlist) {
        | ([], []) => true
        | ([], _) => true
        | ([shd, ...stl], [mhd, ...mtl]) =>
          if (shd == mhd) {
            patternSearchHelper(stl, mtl);
          } else {
            false;
          }
        | (_, []) => false
    };

  /*
  Input:
    sublist, a list of integers
    mainlist, a  list of integers
    sublistLength, an int that is the length of the sublist
    mainlistLength, an int that is the length of the mainlist
  Output:
    a bool representing whether or not the mainlist contains the sublist

  OI: [1, 2], [0, 1, 2, 3]
  RI: [1, 2], [1, 2, 3]
  RO: true
  OO: true

  OI: [1, 2, 3, 4], [5, 1, 2, 3, 4]
  RI: [1, 2, 3, 4], [1, 2, 3, 4]
  RO: true
  OO: true
  */
  let rec patternSearch: (list(int), list(int), int, int) => bool =
    (sublist, mainlist, sublistLength, mainlistLength) =>
    if (sublistLength > mainlistLength) {
        false;
      } else {
      switch (mainlist) {
      | [] => false
      | [_hd, ...tl] =>
        patternSearchHelper(sublist, mainlist) || patternSearch(sublist, tl, sublistLength, mainlistLength - 1)
      }};

  /*
  Input:
    aloi, a list of integers
    pID, an integer representing the playerID
  Output:
    an integer score representing the value of a the list based on our hierarchy of patterns
    that are sort of displayed below. in this case, pID is 1.

  patterns and their points
  1 1 1 1 +2000 (this is a win)
  0 1 1 1 0 +1000 (this is a guaranteed win for P1 on the next turn)
  0 1 1 1 or 1 1 1 0 +50
  0 1 1 0 +20
  0 1 1 or 1 1 0 +15
  0 1 0 + 5
  0 1 or 1 0 + 1
  */

  let valueOfList: (list(int), int) => int =
    (aloi, pID) =>
    {let aloiLength = List.length(aloi);
      if (patternSearch([pID, pID, pID, pID], aloi, 4, aloiLength)) {
        2000;
      } else if (patternSearch([0, pID, pID, pID, 0], aloi, 5, aloiLength)) {
        1900;
      } else if (patternSearch([0, pID, pID, pID], aloi, 4, aloiLength)
                || patternSearch([pID, pID, pID, 0], aloi, 4, aloiLength)) {
        200;
      } else if (patternSearch([0, pID, pID, 0], aloi, 4, aloiLength)) {
        20;
      } else if (patternSearch([0, pID, pID], aloi, 3, aloiLength)
                || patternSearch([pID, pID, 0], aloi, 3, aloiLength)) {
        15;
      } else if (patternSearch([0, pID, 0], aloi, 3, aloiLength)) {
        5;
      } else {
        0;
      }
      };

  /*
  Input:
    board, a list of int lists representing the board
    pID, an integer representing the playerID
  Output:
    the value of the board when each column (hence vertValue name) is put through
    the pattern search

  OI: [[0, 0, 0, 1], [0, 1, 1, 1], [0, 2, 2, 1]], 1
  RI: [[0, 1, 1, 1], [0, 2, 2, 1]], 1
  RO: 50
  OO: 50

  OI: [[0, 1, 1, 1], [0, 2, 2, 1]], 1
  RI: [[0, 2, 2, 1]], 1
  RO: 0
  OO: 50
  */
  let rec vertValueBoard: (list(list(int)), int) => int =
    (board, pID) =>
      switch (board) {
      | [] => 0
      | [hd, ...tl] => valueOfList(hd, pID) + vertValueBoard(tl, pID)
      };

  /*
  Input:
    board, a list of int lists representing the board
    rI, an integer representing the row index of the board
    pID, an integer representing the playerID
  Output:
    the value of the board when each row (hence horzValue name) is put through
    the pattern search

  OI: [[0, 0, 0, 1], [0, 1, 1, 1], [0, 1, 2, 1]], 0, 1
  RI: [[0, 0, 0, 1], [0, 1, 1, 1], [0, 1, 2, 1]], 1, 1
  RO: 20
  OO: 20

  OI: [[0, 0, 0, 1], [0, 1, 1, 1], [0, 1, 2, 1]], 1, 1
  RI: [[0, 0, 0, 1], [0, 1, 1, 1], [0, 1, 2, 1]], 2, 1
  RO: 20
  OO: 0
  */
  let rec horzValueBoard: (list(list(int)), int, int, int) => int =
    (board, rI, pID, boardHeight) =>
      if (rI < boardHeight - 1) {
        valueOfList(anyRow(board, rI), pID)
        + horzValueBoard(board, rI + 1, pID, boardHeight);
      } else {
        0;
      };

  /*
  Input:
    board, a list of int lists representing the board
    cI, an integer representing the column index of the board
    rI, an integer representing the row index of the board
    pID, an integer representing the playerID
  Output:
    the value of the board when each diagonal that runs NWtoSE on my screen (hence function name) is put through
    the pattern search
    this function runs across each row and then once it hits the final row, it starts running down the columns
    until the column index is <0 this ensures that the total val includes all the corners too


  this diagram below shows the order in which the diagValueBoardNWtoSE function
  would iterate through all the diagonals.

  as a list
  [
  [X X X 7]
  [X X X 6]
  [X X X 5]
  [1 2 3 4]
  ]

  as the real board
  X X X 1
  X X X 2
  X X X 3
  7 6 5 4

  let f =
    [[0, 0, 1, 1],
    [0, 2, 1, 2],
    [0, 2, 2, 1],
    [0, 0, 2, 2]];

  OI: f, 3, 2, 2
  RI: f, 3, 3, 2
  RO: 50
  OO: 65

  OI: f, 3, 3, 2
  RI: f, 2, 3, 2
  RO: 50
  OO: 0
  */
  let rec diagValueBoardNWtoSE: (list(list(int)), int, int, int, int) => int =
    (board, cI, rI, pID, boardHeight) =>
      if (rI < boardHeight - 1) {
        valueOfList(diagonalNWtoSE(board, cI, rI), pID)
        + diagValueBoardNWtoSE(board, cI, rI + 1, pID, boardHeight);
      } else if (cI >= 0) {
        valueOfList(diagonalNWtoSE(board, cI, rI), pID)
        + diagValueBoardNWtoSE(board, cI - 1, rI, pID, boardHeight);
      } else {
        0;
      };

  /*
  Input:
    board, a list of int lists representing the board
    cI, an integer representing the column index of the board
    rI, an integer representing the row index of the board
    pID, an integer representing the playerID
  Output:
    the value of the board when each diagonal that runs NEtoSW on my screen (hence function name) is put through
    the pattern search
    this function runs across each row and then once it hits the final row, it starts running up the columns
    until it hits the column index equivalent to the corner of the board
    this ensures that the total val includes all the corners too

  this diagram below shows the order in which the diagValueBoardNEtoSW function
  would iterate through all the diagonals
  as a list of columns
  [
  [1 2 3 4]
  [X X X 5]
  [X X X 6]
  [X X X 7]
  ]

  as the real board
  1 X X X
  2 X X X
  3 X X X
  4 5 6 7

  let f =
    [[0, 0, 1, 1],
    [0, 2, 1, 2],
    [0, 2, 2, 1],
    [0, 0, 2, 2]];

  OI: f, 0, 2, 2
  RI: f, 0, 3, 2
  RO: 15
  OO: 15

  OI: f, 0, 3, 2
  RI: f, 1, 3, 2
  RO: 15
  OO: 15
  */
  let rec diagValueBoardNEtoSW: (list(list(int)), int, int, int, int, int) => int =
    (board, cI, rI, pID, boardHeight, boardWidth) =>
      if (rI < boardHeight - 1) {
        valueOfList(diagonalNEtoSW(board, cI, rI), pID)
        + diagValueBoardNEtoSW(board, cI, rI + 1, pID, boardHeight, boardWidth);
      } else if (cI < boardWidth - 1) {
        valueOfList(diagonalNEtoSW(board, cI, rI), pID)
        + diagValueBoardNEtoSW(board, cI + 1, rI, pID, boardHeight, boardWidth);
      } else {
        0;
      };

  /*
  Input:
    board, a list of int lists representing the board
    pID, an integer representing the playerID
  Output:
    the total diagonal value of the board is found by calling the
  */
  let diagValueBoard: (list(list(int)), int) => int =
    (board, pID) =>
      diagValueBoardNEtoSW(board, 0, 0, pID, List.length(List.hd(board)), List.length(board))
      + diagValueBoardNWtoSE(board, List.length(board) - 1, 0, pID, List.length(List.hd(board)));

  /* estimates the value of a given state (static evaluation)
    Input:
      inState, a state
    Output:
      the value of that state to each player
      very negative means good for P2
      very positive means good for P1
    */
  let estimateValue: state => float =
    inState =>
      switch (inState) {
      /* P2 and P1 are flipped in the first two switch cases because the AI Player uses nextState on each move, changing the ongoing to the next player */
      | State(Ongoing(P2), board) =>
        float_of_int(
          diagValueBoard(board, 1)
          + vertValueBoard(board, 1)
          + horzValueBoard(board, 0, 1, List.length(List.hd(board))),
        )
      | State(Ongoing(P1), board) =>
        (-1.)
        *. float_of_int(
            diagValueBoard(board, 2)
            + vertValueBoard(board, 2)
            + horzValueBoard(board, 0, 2, List.length(List.hd(board))),
          )
      | State(Win(P1), _) => 2000.
      | State(Win(P2), _) => (-2000.)
      | State(Draw, _) => 0.
      };
};

module MyGame : Game = Connect4;
open Connect4;


/* board initialization for tests */

let empty3x3 = [ [0, 0, 0], [0, 0, 0], [0, 0, 0] ];
let firstmove3x3 = [ [0, 0, 1], [0, 0, 0], [0, 0, 0] ];
let a1 = [ [0, 2, 1], [0, 1, 6], [1, 2, 1] ];
let a2 = [ [0, 2, 1], [0, 1, 6], [1, 2, 1], [2, 2, 1] ];
let a21 = [ [1, 2, 1, 1], [0, 1, 2, 2], [1, 2, 1, 1], [1, 2, 1, 2] ];
let a222 = [ [1, 2, 1, 1], [2, 1, 2, 2], [1, 2, 1, 1], [1, 2, 1, 2] ];
let a212 = [ [1, 2, 1, 1], [1, 1, 2, 2], [1, 2, 1, 1], [1, 2, 1, 2] ];
let a25 = [
  [0, 0, 0, 0],
  [0, 2, 2, 2],
  [0, 1, 1, 2],
  [0, 2, 2, 2],
];
let a3 = [
  [0, 0, 0, 2],
  [0, 2, 2, 2],
  [0, 1, 1, 2],
  [0, 2, 2, 2],
];
let a31 = [
  [0, 0, 0, 1],
  [0, 2, 2, 2],
  [0, 1, 1, 2],
  [0, 2, 2, 2],
];
let a4 = [
  [0, 0, 0, 2],
  [0, 0, 0, 2],
  [0, 0, 0, 2],
  [0, 2, 2, 2],
  [0, 1, 1, 1],
  [0, 2, 2, 2],
];
let a5 = [
  [0, 0, 0, 2],
  [0, 0, 0, 2],
  [0, 0, 0, 0],
  [2, 2, 2, 2],
  [0, 1, 1, 1],
  [0, 2, 2, 2],
];
let a6 = [
  [1, 2, 1, 2],
  [1, 2, 2, 1],
  [2, 1, 1, 2],
  [1, 2, 2, 2],
];

let diagwin4x4 = [
  [1, 2, 1, 2], 
  [0, 1, 2, 1], 
  [0, 0, 1, 2], 
  [0, 0, 0, 1]
  ];

let diagwin4x4v2 = [
  [0, 2, 1, 2], 
  [0, 1, 2, 1], 
  [0, 2, 1, 2], 
  [2, 1, 1, 1]
  ];

let diagwin6x6 = [
  [1, 2, 1, 2, 1, 2],
  [0, 0, 2, 1, 2, 1],
  [0, 0, 1, 2, 1, 1],
  [0, 0, 0, 1, 2, 1],
  [0, 0, 0, 0, 1, 2],
  [0, 0, 0, 0, 0, 1],
];

let diagwin4x4v3 = [
  [0, 0, 0, 1],
  [0, 2, 1, 1],
  [0, 1, 2, 2],
  [1, 1, 2, 1]
]

let almostDiagWin = [
  [0, 0, 0, 0, 2], 
  [0, 2, 1, 1, 1], 
  [0, 0, 0, 2, 2],
  [0, 0, 2, 1, 1], 
  [0, 2, 1, 1, 2],
  [0, 2, 1, 2, 1], 
  [0, 0, 0, 1, 2],
  [0, 0, 0, 0, 1]
];

let board3x7 = [
  [1, 1, 2],
  [0, 2, 1],
  [0, 2, 1],
  [0, 2, 2],
  [0, 0, 1],
  [0, 1, 1],
  [0, 0, 2]
];

let z = [
[0, 0, 0, 1, 1], 
[0, 1, 1, 2, 2],
[0, 0, 2, 1, 2],
[0, 2, 2, 2, 1],
[0, 0, 0, 2, 1]];

let board6x3 = 
[
[0, 0, 1, 1, 2, 2],
[0, 1, 2, 2, 2, 1],
[0, 0, 0, 1, 1, 1]
]

let board4x7 = 
[
[0, 1, 1, 2],
[1, 2, 2, 1],
[0, 0, 0, 1],
[1, 2, 2, 2],
[0, 1, 2, 2],
[0, 0, 2, 1],
[0, 0, 0, 1]
]

let a50 = 
[
[0, 0, 0, 0, 0, 0],
[2, 1, 2, 1, 2, 1],
[1, 2, 1, 2, 1, 2],
[2, 1, 2, 1, 2, 1]
]

let a60 =
[
  [2, 2, 1, 2],
  [1, 1, 2, 1],
  [2, 2, 1, 2],
  [0, 1, 1, 1]
];

let a601 =
[
  [2, 2, 1, 2],
  [1, 1, 2, 1],
  [2, 2, 1, 2],
  [1, 1, 1, 1]
];

let a602 =
[
  [2, 2, 1, 2],
  [1, 1, 2, 1],
  [2, 2, 1, 2],
  [2, 1, 1, 1]
];

let a333 = [
  [1, 2, 2, 2],
  [0, 2, 2, 1],
  [1, 2, 1, 2],
  [1, 1, 2, 2],
  [0, 2, 2, 2],
];

/* test cases */

checkExpect(playerToID(P1), 1, "playerToID: P1");
checkExpect(playerToID(P2), 2, "playerToID: P2");

checkExpect(otherPlayer(P1), P2, "otherPlayer: P1");
checkExpect(otherPlayer(P2), P1, "otherPlayer: P2");

checkExpect(makeRows(3, [0]), [[0], [0], [0]], "makeRows: 3 rows of 0")
checkExpect(makeRows(5, []), [[], [], [], [], []], "makeRows: empty lists");

checkExpect(makeColumn(5), [0, 0, 0, 0, 0], "makeColumn: 5 0s");
checkExpect(makeColumn(7), [0, 0, 0, 0, 0, 0, 0], "makeColumn: 7 0s");
checkExpect(makeColumn(0), [], "makeColumn: 0 0s");

checkExpect(makeBoard(3, 3), empty3x3, "makeBoard: 3x3");
checkExpect(makeBoard(1, 1), [[0]], "makeBoard: 0x0");
checkError(() => makeBoard(0, 0), "makeBoard: cannot make board w dims < 1");

checkError(() =>
  initialState("1 1"),
  "initialState: domain error (can't be smaller than 4x4)"
);

checkError(() =>
  initialState("3 10"),
  "initialState: domain error (can't be smaller than 4x4)"
);

checkError(() =>
  initialState("10 -10"),
  "initialState: domain error (can't be smaller than 4x4)"
);

checkExpect(initialState("4 4"), State(Ongoing(P1), [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]), "initialState: 4x4 board");
checkExpect(initialState("4 5"), State(Ongoing(P1), [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]), "initialState: 4x5 board");
checkExpect(initialState("5 4"), State(Ongoing(P1), [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]), "initialState: 5x4 board");

checkExpect(stringOfPlayer(P1), "P1", "stringOfPlayer: P1");
checkExpect(stringOfPlayer(P1), "P1", "stringOfPlayer: P2");

checkExpect(stringOfMove(Move(1)), "\nPlayer chose to place their token in column 2.", "stringOfMove: sample move");
checkExpect(stringOfMove(Move(0)), "\nPlayer chose to place their token in column 1.", "stringOfMove: sample move");

/*
checkExpect(stringOfState(State(Ongoing(P1), [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]])), "It is P1's turn. The board is displayed above.");
checkExpect(stringOfState(State(Ongoing(P1), [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]])), "It is P2's turn. The board is displayed above.");
checkExpect(stringOfState(State(Win(P1), [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [1, 1, 1, 1]])), "P1 has four in a row and wins!");
checkExpect(stringOfState(State(Win(P2), [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [2, 2, 2, 2]])), "P2 has four in a row and wins!");
*/

checkExpect(anyRow(a1, 0), [0, 0, 1], "anyRow: top row 3x3");
checkExpect(anyRow(a2, 0), [0, 0, 1, 2], "anyRow: top row 3x4");
checkExpect(anyRow(a2, 0), [0, 0, 1, 2], "anyRow: top row 3x4");
checkExpect(anyRow(board3x7, 1), [1, 2, 2, 2, 0, 1, 0], "anyRow: second row 3x7");

checkExpect(
  legalMovesHelper([0, 0, 1, 2], 0),
  [Move(0), Move(1)],
  "legalMovesHelper: top row has 2 zeroes",
);

checkExpect(
  legalMovesHelper([1, 2, 0, 0, 0], 0),
  [Move(2), Move(3), Move(4)],
  "legalMovesHelper: top row has 3 zeroes",
);

checkExpect(
  legalMovesHelper([1, 2, 1, 2, 1, 2], 0),
  [],
  "legalMovesHelper: top row has no zeroes",
);

checkExpect(
  legalMoves(State(Ongoing(P1), a1)),
  [Move(0), Move(1)],
  "legalMoves: two options",
);

checkExpect(
  legalMoves(State(Ongoing(P1), a6)),
  [],
  "legalMoves: no options",
);
checkExpect(
  legalMoves(State(Ongoing(P1), board3x7)), 
  [Move(1), Move(2), Move(3), Move(4), Move(5), Move(6)],
  "legalMoves: weird dimensions"
)
checkExpect(legalMoves(State(Ongoing(P1), a6)), [], "legalMoves: no options"); 


checkExpect(
  gameStatus(State(Ongoing(P1), a1)),
  Ongoing(P1),
  "gameStatus: onGoing",
);
checkExpect(gameStatus(State(Win(P2), a4)), Win(P2), "gameStatus: Win");
checkExpect(gameStatus(State(Draw, a6)), Draw, "gameStatus: Draw");
checkExpect(gameStatus(State(Ongoing(P2), board3x7)), Ongoing(P2), "gameStatus: Ongoing w/ uneven grid");

checkExpect(gravity([0, 0, 2, 1], 0, 4), 1, "gravity: sinks to correct pos");
checkExpect(gravity([0, 0, 0, 0], 0, 4), 3, "gravity: sinks to bottom");
checkExpect(gravity([0, 1, 2, 1], 0, 4), 0, "gravity: sinks to 0th index");
checkExpect(gravity([0], 0, 1), 0, "gravity: sinks to 0th index in a one item list");

checkExpect(splitListAdd([0, 0, 2, 1], 1, 0, 1), [0, 1, 2, 1], "splitListAdd: adding a 1 to a column");
checkExpect(splitListAdd([0, 0, 2, 1], 2, 1, 1), [0, 1, 2, 1], "splitListAdd: adding a 1 to a column");
checkExpect(splitListAdd([1, 0, 2, 1], 0, 0, 1), [1, 0, 2, 1], "splitListAdd: adding a 1 to a column");
checkExpect(splitListAdd([[0], [0], [0], [1]], 2, 0, [2]), [[0], [0], [2], [1]], "splitListAdd: adding a column to a board");
checkError(() => splitListAdd([], 1, 2, 1), "splitListAdd: empty list input.");

checkExpect(
  newBoard(a25, Move(0), playerToID(P2)),
  a3,
  "newBoard: making the winning move",
);

checkExpect(
  newBoard(empty3x3, Move(0), playerToID(P1)),
  firstmove3x3,
  "newBoard: making the first move",
);

checkExpect(
  newBoard(board3x7, Move(1), playerToID(P2)), 
  [
  [1, 1, 2],
  [2, 2, 1],
  [0, 2, 1],
  [0, 2, 2],
  [0, 0, 1],
  [0, 1, 1],
  [0, 0, 2]], 
  "newBoard: irregularly shaped board"
);

checkExpect(
  victoryHelper([2, 1, 1, 1, 2], 0),
  false,
  "victoryHelper: item placed next to three of other player",
);
checkExpect(
  victoryHelper([2, 0, 0, 0, 0], 0),
  false,
  "victoryHelper: item placed next to three of other player",
);

checkExpect(
  victoryHelper([0, 1, 1, 1, 1], 1),
  true,
  "victoryHelper: winning configuration",
);

checkExpect(
  victoryHelper([2, 1, 1, 1, 2], 0),
  false,
  "victoryHelper: item placed next to three of other player",
);
checkExpect(
  victoryHelper([0, 1, 1, 1, 1], 1),
  true,
  "victoryHelper: success",
);

checkExpect(verticalVictory(a5, 3, 0, 4), true, "verticalVictory: 4x4");
checkExpect(verticalVictory(a5, 4, 1, 4), false, "verticalVictory: 4x4");
checkExpect(verticalVictory(board3x7, 0, 0, 3), false, "verticalVictory: 3x7 height-rowIndex < 3");

checkExpect(
  startingColumnIndex(2),
  0,
  "startingColumnIndex: under 4? gets 0",
);
checkExpect(
  startingColumnIndex(3),
  0,
  "startingColumnIndex: under 4? gets 0",
);
checkExpect(
  startingColumnIndex(1),
  0,
  "startingColumnIndex: under 4? gets 0",
);
checkExpect(
  startingColumnIndex(4),
  1,
  "startingColumnIndex: over 4? gets index-3",
);

checkExpect(
  horzVictoryHelper([2, 1, 1, 1, 2], 3, 3, 5),
  false,
  "horzVictoryHelper: don't bother checking bc index doesn't allow for a four to be made from the starting index as the leftmost stone",
);
checkExpect(
  horzVictoryHelper([0, 1, 1, 1, 1], 1, 4, 5),
  true,
  "horzVictoryHelper: check with HelperHelper and find success",
);
checkExpect(
  horzVictoryHelper(
    [0, 0, 2, 1, 1, 1, 1, 0, 2, 1, 0],
    startingColumnIndex(6),
    6,
    11,
  ),
  true,
  "horzVictoryHelper: longer list",
);

checkExpect(horzVictory(a3, 0, 3), true, "horzVictory: 4x4");
checkExpect(horzVictory(a4, 2, 3), true, "horzVictory: 4x6");
checkExpect(horzVictory(a5, 1, 3), false, "horzVictory: 4x6");

checkExpect(diagonalHelper(diagwin4x4, 4, 4), None, "diagonalHelper: out of bounds");
checkExpect(diagonalHelper(diagwin4x4, -4, 4), None, "diagonalHelper: negative");
checkExpect(diagonalHelper(diagwin4x4, 0, 1), Some(2), "diagonalHelper: return option(int)");
checkExpect(diagonalHelper(diagwin4x4, 2, 3), Some(2), "diagonalHelper: return option(int)");

checkExpect(diagonalMaker(diagwin4x4, 1, 2, 1, 1), [2, 2], "diagonalMaker: 4x4 test");
checkExpect(diagonalMaker(diagwin4x4, 1, 2, -1, -1), [2, 2], "diagonalMaker: 4x4 test");
checkExpect(diagonalMaker(diagwin6x6, 1, 4, -1, 1), [2, 2], "diagonalMaker: 6x6 test");
checkExpect(diagonalMaker(diagwin6x6, 1, 4, 1, -1), [2, 2, 0, 0, 0], "diagonalMaker: 6x6 test");

checkExpect(diagonalNWtoSE(diagwin4x4, 0, 1), [2, 2, 2], "diagonalNWtoSE: 4x4 test");
checkExpect(diagonalNWtoSE(diagwin4x4, 1, 2), [2, 2, 2], "diagonalNWtoSE: 4x4 test");
checkExpect(diagonalNWtoSE(diagwin6x6, 0, 0), [1, 0, 1, 1, 1, 1], "diagonalNWtoSE: 6x6 test");
checkExpect(diagonalNWtoSE(diagwin6x6, 5, 5), [1, 0, 1, 1, 1, 1], "diagonalNWtoSE: 6x6 test");
checkExpect(diagonalNWtoSE(board3x7, 4, 0), [0, 1, 2], "diagonalNWtoSE: 3x7 test");
checkExpect(diagonalNWtoSE(board3x7, 5, 0), [0, 0], "diagonalNWtoSE: 3x7 test adjacent to corner");
checkExpect(diagonalNWtoSE(board3x7, 6, 0), [0], "diagonalNWtoSE: 3x7 test in corner");

checkExpect(diagonalNEtoSW(diagwin6x6, 0, 5), List.rev([2, 2, 2, 0, 0, 0]), "diagonalNEtoSW: 4x4 test");
checkExpect(diagonalNEtoSW(diagwin6x6, 1, 4), List.rev([2, 2, 2, 0, 0, 0]), "diagonalNEtoSW: 4x4 test");
checkExpect(diagonalNEtoSW(board3x7, 2, 0), [0, 2, 2], "diagonalNEtoSW: 3x7 test");
checkExpect(diagonalNEtoSW(board3x7, 1, 0), [0, 1], "diagonalNEtoSW: 3x7 test adjacent to corner");
checkExpect(diagonalNEtoSW(board3x7, 0, 0), [1], "diagonalNEtoSW: 3x7 test in corner");
checkExpect(diagonalNEtoSW(diagwin4x4, 1, 1), [0, 1, 1], "diagonalNEtoSW: 4x4 test");

checkExpect(diagonalVictory(diagwin4x4, 0, 0), true, "diagonalVictory: 4x4 spot 1");
checkExpect(diagonalVictory(diagwin4x4, 1, 1), true, "diagonalVictory: 4x4 spot 2");
checkExpect(diagonalVictory(diagwin4x4, 2, 2), true, "diagonalVictory: 4x4 spot 3");
checkExpect(diagonalVictory(diagwin4x4, 3, 3), true, "diagonalVictory: 4x4 spot 4");
checkExpect(diagonalVictory(diagwin4x4, 3, 2), false, "diagonalVictory: 4x4 spot 4");

checkExpect(diagonalVictory(diagwin4x4v2, 3, 0), true, "diagonalVictory: 4x4 spot 1 other axis diagonal");
checkExpect(diagonalVictory(diagwin4x4v2, 2, 1), true, "diagonalVictory: 4x4 spot 1 other axis diagonal");
checkExpect(diagonalVictory(diagwin4x4v2, 1, 2), true, "diagonalVictory: 4x4 spot 1 other axis diagonal");
checkExpect(diagonalVictory(diagwin4x4v2, 0, 3), true, "diagonalVictory: 4x4 spot 1 other axis diagonal");
checkExpect(diagonalVictory(diagwin4x4v2, 0, 0), false, "diagonalVictory: 4x4 spot 1 other axis diagonal");

checkExpect(diagonalVictory(diagwin6x6, 2, 2), true, "diagonalVictory: 6x6");

checkExpect(winningBoard(State(Ongoing(P1), diagwin6x6), Move(1)), true, "winningBoard: diagonal win");
checkExpect(winningBoard(State(Ongoing(P2), almostDiagWin), Move(5)), true, "winningBoard: diagonal win");
checkExpect(winningBoard(State(Ongoing(P2), almostDiagWin), Move(0)), false, "winningBoard: no win");
checkExpect(winningBoard(State(Ongoing(P2), board3x7), Move(4)), true, "winningBoard: horizontal victory");
checkExpect(winningBoard(State(Draw, board3x7), Move(0)), false, "winningBoard: draw state");
checkExpect(winningBoard(State(Ongoing(P2), z), Move(3)), true, "winningBoard: vertical victory state");
checkExpect(winningBoard(State(Ongoing(P1), a60), Move(3)), true, "winningBoard: final move");
checkExpect(winningBoard(State(Ongoing(P2), a60), Move(3)), true, "winningBoard: final move");
checkExpect(winningBoard(State(Ongoing(P2), a50), Move(0)), true, "winningBoard: diagonal win");

checkExpect(
  moveOfString("2", State(Ongoing(P1), a1)),
  Move(1),
  "moveOfString: permittedMove",
);

checkError(
  () => moveOfString("3", State(Ongoing(P1), a1)),
  "Not a legal move. Did you go past the max index? Or is that column full?",
);

checkExpect(nextState(State(Ongoing(P2), a25), Move(0)), State(Win(P2), a3), "nextState: winning move");
checkExpect(nextState(State(Ongoing(P2), a21), Move(1)), State(Draw, a222), "nextState: draw"); 
checkExpect(nextState(State(Ongoing(P1), a21), Move(1)), State(Win(P1), a212), "nextState: win"); 
checkExpect(nextState(State(Ongoing(P1), a25), Move(0)), State(Ongoing(P2), a31), "nextState: no win"); 
checkExpect(nextState(State(Ongoing(P2), [[0, 0, 0, 0], [1, 2, 2, 2], [0, 1, 1, 2], [0, 2, 2, 2]]), Move(2)), 
            State(Ongoing(P1), [[0, 0, 0, 0], [1, 2, 2, 2], [2, 1, 1, 2], [0, 2, 2, 2]]), "nextState: ongoing");
checkExpect(nextState(State(Ongoing(P2), board3x7), Move(2)), 
            State(Ongoing(P1), [[1, 1, 2],
                                [0, 2, 1],
                                [2, 2, 1],
                                [0, 2, 2],
                                [0, 0, 1],
                                [0, 1, 1],
                                [0, 0, 2]]), "nextState: ongoing on irregular size board");
checkExpect(nextState(State(Ongoing(P2), board3x7), Move(4)), 
            State(Win(P2), [[1, 1, 2],
                                [0, 2, 1],
                                [0, 2, 1],
                                [0, 2, 2],
                                [0, 2, 1],
                                [0, 1, 1],
                                [0, 0, 2]]), "nextState: win on irregular size board");
checkExpect(nextState(State(Ongoing(P2), a60), Move(3)), State(Win(P2), a602), "nextState: vert win with final piece.")
checkExpect(nextState(State(Ongoing(P1), a60), Move(3)), State(Win(P1), a601), "nextState: diag win with final piece.")


checkError(() => nextState(State(Win(P1), board3x7), Move(0)), "nextState called on a non-ongoing game.");
checkError(() => nextState(State(Draw, board3x7), Move(0)), "nextState called on a non-ongoing game.");

checkExpect(patternSearchHelper([], [1, 2, 3]), true, "patternSearchHelper: empty sublist, nonempty mainlist");
checkExpect(patternSearchHelper([1, 2], [1, 2, 3]), true, "patternSearchHelper: one element sublist, nonempty mainlist");
checkExpect(patternSearchHelper([3, 2], [1, 2, 3]), false, "patternSearchHelper: reversed sublist, nonempty mainlist");
checkExpect(patternSearchHelper([], []), true, "patternSearchHelper: received an empty mainlist to scan.");

checkExpect(patternSearch([3, 2], [1, 2, 3], 2, 3), false, "patternSearch: reversed sublist, nonempty mainlist");
checkExpect(patternSearch([2, 3], [1, 2, 3], 2, 3), true, "patternSearch: contained sublist, nonempty mainlist");
checkExpect(patternSearch([3], [1, 2, 3], 1, 3), true, "patternSearch: one element sublist, nonempty mainlist");
checkExpect(patternSearch([], [1, 2, 3], 0, 3), true, "patternSearch: empty sublist, nonempty mainlist");

checkExpect(valueOfList([1, 1, 1, 1], 1), 2000, "valueOfList: checking pattern matching");
checkExpect(valueOfList([0, 1, 1, 1, 0], 1), 1900, "valueOfList: checking pattern matching");
checkExpect(valueOfList([1, 1, 1, 0], 1), 200, "valueOfList: checking pattern matching");
checkExpect(valueOfList([0, 1, 1, 1], 1), 200, "valueOfList: checking pattern matching");
checkExpect(valueOfList([0, 1, 1, 0], 1), 20, "valueOfList: checking pattern matching");
checkExpect(valueOfList([0, 1, 1], 1), 15, "valueOfList: checking pattern matching");
checkExpect(valueOfList([1, 1, 0], 1), 15, "valueOfList: checking pattern matching");
checkExpect(valueOfList([0, 2, 0], 2), 5, "valueOfList: checking pattern matching");

checkExpect(vertValueBoard(z, 1), 30, "vertValueBoard: check P1");
checkExpect(vertValueBoard(z, 2), 200, "vertValueBoard: check P2");

checkExpect(horzValueBoard(z, 0, 1, List.length(List.hd(z))), 5, "horzValueBoard: check P1");
checkExpect(horzValueBoard(z, 0, 2, List.length(List.hd(z))), 20, "horzValueBoard: check P2");

checkExpect(diagValueBoardNWtoSE(z, List.length(z) - 1, 0, 1, List.length(List.hd(z))), 200, "diagValueBoardNWtoSE: check P1");
checkExpect(diagValueBoardNWtoSE(z, List.length(z) - 1, 0, 2, List.length(List.hd(z))), 35, "diagValueBoardNWtoSE: check P2"); 

checkExpect(diagValueBoardNEtoSW(z, 0, 0, 1, List.length(List.hd(z)), List.length(z)), 20, "diagValueBoardNEtoSW: check P1");
checkExpect(diagValueBoardNEtoSW(z, 0, 0, 2, List.length(List.hd(z)), List.length(z)), 215, "diagValueBoardNEtoSW: check P2");

checkExpect(diagValueBoard(z, 1), 220, "diagValueBoard: combining both diagVals for P1");
checkExpect(diagValueBoard(z, 2), 250, "diagValueBoard: combining both diagVals for P2");

checkExpect(estimateValue(State(Ongoing(P2), z)), 255., "estimateValue: combining all boardVals for P1");
checkExpect(estimateValue(State(Ongoing(P1), z)), -470., "estimateValue: combining all boardVals for P2");

checkExpect(diagValueBoard(board6x3, 1), 20, "diagValueBoard: 6 rows 3 columns p1");
checkExpect(vertValueBoard(board6x3, 1), 215, "vertValueBoard: 6 rows 3 columns p1");
checkExpect(horzValueBoard(board6x3, 0, 1, List.length(List.hd(board6x3))), 5, "horzValueBoard: 6 rows 3 columns p1");

checkExpect(diagValueBoard(board6x3, 2), 15, "diagValueBoard: 6 rows 3 columns p2");
checkExpect(vertValueBoard(board6x3, 2), 0, "vertValueBoard: 6 rows 3 columns p2");
checkExpect(horzValueBoard(board6x3, 0, 2, List.length(List.hd(board6x3))), 0, "horzValueBoard: 6 rows 3 columns p2");

checkExpect(estimateValue(State(Ongoing(P2), board6x3)), 240., "estimateValue: 6 rows 3 columns p1");
checkExpect(estimateValue(State(Ongoing(P1), board6x3)), -15., "estimateValue: 6 rows 3 columns p2");

checkExpect(diagValueBoard(board4x7, 1), 0, "diagValueBoard: 4 rows 7 columns p1");
checkExpect(vertValueBoard(board4x7, 1), 15, "vertValueBoard: 4 rows 7 columns p1");
checkExpect(horzValueBoard(board4x7, 0, 1, List.length(List.hd(board4x7))), 5, "horzValueBoard: 4 rows 7 columns p1");

checkExpect(diagValueBoard(board4x7, 2), 85, "diagValueBoard: 4 rows 7 columns p2");
checkExpect(vertValueBoard(board4x7, 2), 0, "vertValueBoard: 4 rows 7 columns p2");
checkExpect(horzValueBoard(board4x7, 0, 2, List.length(List.hd(board4x7))), 1900, "horzValueBoard: 4 rows 7 columns p2");

checkExpect(estimateValue(State(Ongoing(P2), board4x7)), 20., "estimateValue: 4 rows 7 columns p1");
checkExpect(estimateValue(State(Ongoing(P1), board4x7)), -1985., "estimateValue: 4 rows 7 columns p2");

/*MAXIM TEST*/

let a3 = [
  [0, 0, 0, 2],
  [0, 2, 2, 2],
  [0, 1, 1, 2],
  [0, 2, 2, 2],
];

let a33 = [
  [0, 0, 0, 1, 2, 1],
  [0, 0, 1, 2, 2, 1],
  [0, 1, 2, 2, 2, 1],
  [1, 2, 1, 2, 1, 1],
  [0, 0, 2, 2, 2, 1],
  [0, 0, 2, 2, 2, 2],
];

checkExpect(makeRows(0, [0, 1]), [], "makeRows: empty board");
checkExpect(makeRows(6, [0, 0]), [[0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0]], "makeRows: 6 width column 2 0s");

checkExpect(makeColumn(0),[], "makeColumn: empty board");
checkExpect(makeColumn(6), [0, 0, 0, 0, 0, 0], "makeColumn: height of 6");

checkError(() => makeBoard(0, 0), "makeBoard: cannot make board w dims < 1");
checkExpect(makeBoard(5,7), [
[0, 0, 0, 0, 0, 0, 0],
[0, 0, 0, 0, 0, 0, 0], 
[0, 0, 0, 0, 0, 0, 0], 
[0, 0, 0, 0, 0, 0, 0], 
[0, 0, 0, 0, 0, 0, 0]], "makeBoard: 5x7 board of zeros");

checkExpect(anyRow([[1]], 0), [1], "anyRow: 1x1 board");
checkExpect(anyRow(a3, 0), [0, 0, 0, 0], "anyRow: 0th row of board a3");
checkExpect(anyRow(a3, 1), [0, 2, 1, 2], "anyRow: 1st row of board a3");
checkExpect(anyRow(a3, 2), [0, 2, 1, 2], "anyRow: 2nd row of board a3");
checkExpect(anyRow(a3, 3), [2, 2, 2, 2], "anyRow: 3rd row of board a3");

checkExpect(legalMovesHelper(anyRow(a3, 0), 0), [Move(0), Move(1), Move(2), Move(3)], "legalMovesHelper: 4 available moves");	
checkExpect(legalMovesHelper(anyRow(a3, 1), 0), [Move(0)], "legalMovesHelper: 1 available move");	
checkExpect(legalMovesHelper(anyRow(a3, 3), 0), [], "legalMovesHelper: 0 available moves");	
checkExpect(legalMovesHelper([0,1,1,0,2,0], 0), [Move(0), Move(3), Move(5)], "legalMovesHelper: 3 available moves");

checkExpect(legalMoves(State(Ongoing(P1),a3)), [Move(0), Move(1), Move(2), Move(3)], "legalMoves: 4 available moves");	

checkExpect(gameStatus(State(Ongoing(P1),a3)), Ongoing(P1), "gameStatus: ongoing game");	
checkExpect(gameStatus(State(Win(P2),a3)), Win(P2), "gameStatus: won game");	
checkExpect(gameStatus(State(Draw,a3)), Draw, "gameStatus: drawn game");	

checkExpect(gravity(anyRow(a3, 0), 0, 4), 3, "gravity: 0th row of board a3 (gravity will be applied to columns primarily)");
checkExpect(gravity(anyRow(a3, 1), 0, 4), 0, "gravity: 1st row of board a3 (gravity will be applied to columns primarily)");
checkExpect(gravity([0,0,2,1,2,1,1], 0, 7), 1, "gravity: 2nd position free");

checkExpect(splitListAdd([0,1,2,3,4,5],1,0,99), [0,99,2,3,4,5],"splitListAdd: int into 1 space");
checkExpect(splitListAdd(a3, 1, 0, [0, 1, 2, 1]), [
[0, 0, 0, 2], 
[0, 1, 2, 1], 
[0, 1, 1, 2], 
[0, 2, 2, 2]], "splitListAdd: list(int) into a3 board");
checkExpect(splitListAdd([0, 1, 2, 3, 4, 5], 0, 0, 99), [99, 1, 2, 3, 4, 5], "splitListAdd: int into 0 space");
checkExpect(splitListAdd([0, 1, 2, 3, 4, 5], 5, 0, 99), [0, 1, 2, 3, 4, 99], "splitListAdd: int into 5 space");

checkExpect(newBoard(a3,Move(0),1), [
[0, 0, 1, 2], 
[0, 2, 2, 2], 
[0, 1, 1, 2], 
[0, 2, 2, 2]], "newBoard: insert into column 0");
checkExpect(newBoard(a3, Move(1), 2), [
[0, 0, 0, 2], 
[2, 2, 2, 2], 
[0, 1, 1, 2], 
[0, 2, 2, 2]], "newBoard: insert into column 1");

checkExpect(newBoard(a3, Move(2), 1), [
[0, 0, 0, 2], 
[0, 2, 2, 2], 
[1, 1, 1, 2], 
[0, 2, 2, 2]], "newBoard: insert into column 2");

checkExpect(newBoard(a3, Move(3), 2), [
[0, 0, 0, 2], 
[0, 2, 2, 2], 
[0, 1, 1, 2], 
[2, 2, 2, 2]], "newBoard: insert into column 3");

checkExpect(newBoard(splitListAdd(a3, 0, 0, [0, 0, 0, 0]), Move(0), 1), [
[0, 0, 0, 1], 
[0, 2, 2, 2], 
[0, 1, 1, 2], 
[0, 2, 2, 2]], "newBoard: insert into column 1 (empty)");

checkExpect(verticalVictory(splitListAdd(a3, 0, 0, [2, 2, 2, 2]), 0, 0, 4), true, "verticalVictory: Victory 4 high");
checkExpect(verticalVictory(a3, 3, 1, 4), false, "verticalVictory: no victory 4 high");
checkExpect(verticalVictory(a33, 5, 2, 6), true, "verticalVictory: victory 6 high");

checkExpect(startingColumnIndex(0), 0, "startingColumnIndex: 0");
checkExpect(startingColumnIndex(1), 0, "startingColumnIndex: 1");
checkExpect(startingColumnIndex(2), 0, "startingColumnIndex: 2");
checkExpect(startingColumnIndex(3), 0, "startingColumnIndex: 3");
checkExpect(startingColumnIndex(99), 96, "startingColumnIndex: 99");

checkExpect(horzVictoryHelper([1, 2, 1, 1, 1, 1, 2, 2, 2], 0, 2, 9),true,"horzVictoryHelper: 9 wide, position 1")
checkExpect(horzVictoryHelper([1, 2, 1, 1, 1, 1, 2, 2, 2], 0, 3, 9),true,"horzVictoryHelper: 9 wide, position 2")
checkExpect(horzVictoryHelper([1, 2, 1, 1, 1, 1, 2, 2, 2], 0, 4, 9),true,"horzVictoryHelper: 9 wide, position 3")
checkExpect(horzVictoryHelper([1, 2, 1, 1, 1, 1, 2, 2, 2], 0, 5, 9),true,"horzVictoryHelper: 9 wide, position 4")
checkExpect(horzVictoryHelper([1, 2, 2, 1, 1, 1, 2, 2, 2], 0, 6, 9),false,"horzVictoryHelper: 9 wide false")


checkExpect(horzVictory(a33, 0, 5), true, "horzVictory: a33 victory position 0 (1)");
checkExpect(horzVictory(a33, 1, 5), true, "horzVictory: a33 victory position 1 (1)");
checkExpect(horzVictory(a33, 2, 5), true, "horzVictory: a33 victory position 2 (1)");
checkExpect(horzVictory(a33, 3, 5), true, "horzVictory: a33 victory position 3 (1)");
checkExpect(horzVictory(a33, 4, 5), true, "horzVictory: a33 victory position 4 (1)");
checkExpect(horzVictory(a33, 1, 3), true, "horzVictory: a33 victory position 0 (2)");
checkExpect(horzVictory(a33, 2, 3), true, "horzVictory: a33 victory position 1 (2)");
checkExpect(horzVictory(a33, 3, 3), true, "horzVictory: a33 victory position 2 (2)");
checkExpect(horzVictory(a33, 4, 3), true, "horzVictory: a33 victory position 3 (2)");
checkExpect(horzVictory(a33, 5, 3), true, "horzVictory: a33 victory position 4 (2)");
checkExpect(horzVictory(a33, 0, 0), false, "horzVictory: a33 false");

checkExpect(diagonalHelper(a33, 0, 3), Some(1), "diagonalHelper: a33 position 1");
checkExpect(diagonalHelper(a33, 0, 0), Some(0), "diagonalHelper: a33 position 2");
checkExpect(diagonalHelper(a33, 5, 5), Some(2), "diagonalHelper: a33 position 3");

checkExpect(diagonalMaker(a33, 0, 0, 1, 1), [0, 0, 2, 2, 2, 2], "diagonalMaker: a33 (0,0) long diagonal");
checkExpect(diagonalMaker(a33, 5, 0, 1, -1), [0], "diagonalMaker: a33 (5,0) boundary check");
checkExpect(diagonalMaker(a33, 5, 0, -1, 1), [0, 0, 1, 2, 2, 1], "diagonalMaker: a33 (5,0) long diagonal, opposite");

checkExpect(diagonalNWtoSE(a33, 0, 0), [0, 0, 2, 2, 2, 2], "diagonalNWtoSE: diagonal 0");
checkExpect(diagonalNWtoSE(a33, 1, 0), [0, 1, 1, 2, 2], "diagonalNWtoSE: diagonal 1");
checkExpect(diagonalNWtoSE(a33, 2, 0), [0, 2, 2, 2], "diagonalNWtoSE: diagonal 2");
checkExpect(diagonalNWtoSE(a33, 3, 0), [1, 0, 2], "diagonalNWtoSE: diagonal 3");
checkExpect(diagonalNWtoSE(a33, 4, 0), [0, 0], "diagonalNWtoSE: diagonal 4");
checkExpect(diagonalNWtoSE(a33, 5, 0), [0], "diagonalNWtoSE: diagonal 5");

checkExpect(diagonalNEtoSW(a33, 5, 0), [0, 0, 1, 2, 2, 1], "diagonalNEtoSW: diagonal 0");
checkExpect(diagonalNEtoSW(a33, 4, 0), [0, 2, 2, 2, 2], "diagonalNEtoSW: diagonal 1");
checkExpect(diagonalNEtoSW(a33, 3, 0), [1, 1, 1, 1], "diagonalNEtoSW: diagonal 2");
checkExpect(diagonalNEtoSW(a33, 2, 0), [0, 0, 0], "diagonalNEtoSW: diagonal 3");
checkExpect(diagonalNEtoSW(a33, 1, 0), [0, 0], "diagonalNEtoSW: diagonal 4");
checkExpect(diagonalNEtoSW(a33, 0, 0), [0], "diagonalNEtoSW: diagonal 5");

checkExpect(diagonalVictory(a33, 0, 3), true, "diagonalVictory: (1) 1");
checkExpect(diagonalVictory(a33, 1, 2), true, "diagonalVictory: (1) 2");
checkExpect(diagonalVictory(a33, 2, 1), true, "diagonalVictory: (1) 3");
checkExpect(diagonalVictory(a33, 3, 0), true, "diagonalVictory: (1) 4");
checkExpect(diagonalVictory(a33, 2, 2), true, "diagonalVictory: (2) 1");
checkExpect(diagonalVictory(a33, 3, 3), true, "diagonalVictory: (2) 2");
checkExpect(diagonalVictory(a33, 4, 4), true, "diagonalVictory: (2) 3");
checkExpect(diagonalVictory(a33, 5, 5), true,  "diagonalVictory: (2) 4");

checkExpect(winningBoard(State(Ongoing(P1), a3), Move(3)), false, "winningBoard: a3 P1 blocks");
checkExpect(winningBoard(State(Ongoing(P2), a3), Move(3)), true, "winningBoard: a3 P2 wins vert");
checkExpect(winningBoard(State(Ongoing(P1), a333), Move(1)), true, "winningBoard: a333 P1 wins horz");
checkExpect(winningBoard(State(Ongoing(P2), a333), Move(1)), true, "winningBoard: a333 P2 wins diag");
checkExpect(winningBoard(State(Ongoing(P1), a333), Move(4)), true, "winningBoard: a333 P1 wins diag");
  
checkExpect(estimateValue((State(Ongoing(P1), a3))), -420., "estimateValue: P1 board a3");
checkExpect(estimateValue((State(Ongoing(P2), a3))), 15., "estimateValue: P2 board a3");
checkExpect(estimateValue((State(Ongoing(P1), a333))), -415., "estimateValue: P1 board a333");
checkExpect(estimateValue((State(Ongoing(P2), a333))), 220., "estimateValue: P2 board a333");