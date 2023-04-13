package compiler.token.types;

import compiler.token.Token;

public class SeparatorToken extends Token {
    public SeparatorToken(int row, int col, String word) {
        super(row, col, word);
    }
}
