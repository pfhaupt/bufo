package main.tok;

public class Location {
    private final int col, row;
    public Location(int _row, int _col) {
        row = _row;
        col = _col;
    }

    public String toString() {
        return String.format("%d:%d", row, col);
    }
}
