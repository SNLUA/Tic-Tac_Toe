# Tic-Tac_Toe
Tic-Tac-Toe using RL

Implementation of Tic Tac Toe using basic RL methods

  - What are the total number of states?

    * There are totally 9 box's in tic tac toe, and each box can be filled with
      3 possible ways which are o, x, " "(blank) respectively.

    * 3 ^ 9 = 19,683(all possible states). NOTE: Avoid the illegal moves in the
      code.

  - What are the possible actions?

    * Fill the board with either o, x. NOTE: check whether that particular
      position is filled or not.

  - How the actions should be rewarded?

    * Win = 1, Loss = 0, Draw = 0.5.
