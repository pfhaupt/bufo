package compiler.token.types;

import compiler.token.Token;

public class NumberToken extends Token {
    public NumberToken(int row, int col, String word) {
        super(row, col, word);
    }
}
