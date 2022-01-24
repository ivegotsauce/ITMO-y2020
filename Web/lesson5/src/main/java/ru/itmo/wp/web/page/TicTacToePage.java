package ru.itmo.wp.web.page;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class TicTacToePage {
    public static class State {
        private String[][] cells = new String[3][3];
        private int[][] board = new int[3][3];
        private String phase = "RUNNING";
        private boolean crossesMove = true;


        public int[][] getBoard() {
            return board;
        }

        public void setBoard(int[][] board) {
            this.board = board;
        }
        public boolean getCrossesMove() {
            return crossesMove;
        }

        public String getPhase() {
            return phase;
        }

        public void setPhase(String phase) {
            this.phase = phase;
        }

        public void setCrossesMove(boolean crossesMove) {
            this.crossesMove = crossesMove;
        }

        public int getSize() {
            return 3;
        }

        public String[][] getCells() {
            return cells;
        }

        public void setCells(String[][] cells) {
            this.cells = cells;
        }
    }

    private void action(HttpServletRequest request, Map<String, Object> view) {
        State currentState = (State) request.getSession().getAttribute("state");
        if (currentState == null) {
            currentState = new State();
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    currentState.getBoard()[i][j] = 0;
                }
            }
            request.getSession().setAttribute("state", currentState);
        }
        view.put("state", currentState);
    }

    private void onMove(HttpServletRequest request, Map<String, Object> view) {
        State currentState = (State) request.getSession().getAttribute("state");
        if (currentState.getPhase().equals("RUNNING")) {
            boolean crossesMove = currentState.getCrossesMove();
            Map.Entry<String, String[]> param = request.getParameterMap().entrySet().stream()
                    .filter(i -> i.getKey().startsWith("cell_")).findFirst().orElse(null);
            if (param == null || param.getKey().length() != 7) {
                doNothing(view, currentState);
                return;
            }

            String[][] cells = currentState.getCells();
            int turn = crossesMove ? 1 : -1;
            int[][] board = currentState.getBoard();
            String cellNum = param.getKey().substring(5);

            int row, col;
            try {
                row = Integer.parseInt(cellNum.substring(0, 1));
                col = Integer.parseInt(cellNum.substring(1));
                if (row > 2 || col > 2) {
                    doNothing(view, currentState);
                    return;
                }
            } catch (NumberFormatException e) {
                doNothing(view, currentState);
                return;
            }

            if (board[row][col] != 0) {
                doNothing(view, currentState);
                return;
            }
            board[row][col] = turn;
            if (allUsed(board)) {
                currentState.setPhase("DRAW");
            }
            if (checkWinner(turn, board)) {
                currentState.setPhase("WON_" + (crossesMove ? "X" : "O"));
            }
            currentState.setCrossesMove(!crossesMove);
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    if (board[i][j] == 1) {
                        cells[i][j] = "X";
                    }
                    if (board[i][j] == -1) {
                        cells[i][j] = "O";
                    }
                }
            }
        }
        view.put("state", currentState);
    }

    void doNothing(Map<String, Object> view, State currentState) {
        view.put("state", currentState);
    }

    private void newGame(HttpServletRequest request, Map<String, Object> view) {
        State newState = new State();
        request.getSession().setAttribute("state", newState);
        view.put("state", newState);
    }

    private boolean allUsed(int[][] board) {
        int cellsCount = 0;
        for (int u = 0; u < 3; u++) {
            for (int v = 0; v < 3; v++) {
                if (board[u][v] != 0) {
                    cellsCount++;
                }
            }
        }
        return cellsCount == 9;
    }

    private boolean checkWinner(int turn, int[][] board) {
        int countDiag1 = 0;
        int countDiag2 = 0;
        for (int u = 0; u < 3; u++) {
            int rowCount = 0;
            int colCount = 0;
            for (int v = 0; v < 3; v++) {
                if (board[u][v] == turn) {
                    rowCount++;
                }
                if (board[v][u] == turn) {
                    colCount++;
                }
            }
            if (rowCount == 3 || colCount == 3) {
                return true;
            }
            if (board[u][u] == turn) {
                countDiag1++;
            }
            if (board[u][2 - u] == turn) {
                countDiag2++;
            }
        }
        return countDiag1 == 3 || countDiag2 == 3;
    }
}
