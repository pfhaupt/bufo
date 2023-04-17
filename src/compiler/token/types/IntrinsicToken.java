package compiler.token.types;

import compiler.token.Token;

public class IntrinsicToken extends Token {
    public IntrinsicToken(int row, int col, String word) {
        super(row, col, word);
    }
}
