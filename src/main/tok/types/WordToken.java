package main.tok.types;

import main.tok.Token;

public class WordToken extends Token {
    public WordToken(int row, int col, String word) {
        super(row, col, word);
    }
}
