package compiler.token.types;

import compiler.token.Token;

public class WordToken extends Token {
    public WordToken(int row, int col, String word) {
        super(row, col, word);
    }
}
