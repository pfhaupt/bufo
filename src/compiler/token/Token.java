package compiler.token;

public class Token {
    private final String word;
    private final Location location;
    public Token(int row, int col, String _word) {
        word = _word;
        location = new Location(row, col);
    }

    public String toString() {
        return String.format("%s: %s", location, word);
    }

    public boolean isEmptyToken() {
        return word.equals("");
    }
}
