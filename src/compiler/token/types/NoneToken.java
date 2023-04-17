package compiler.token.types;

import compiler.token.Token;

public class NoneToken extends Token {
    public NoneToken(int row, int col) {
        super(row, col, "");
    }
}
