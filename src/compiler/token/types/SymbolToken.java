package compiler.token.types;

import compiler.token.Token;

public class SymbolToken extends Token {
    public SymbolToken(int row, int col, String word) {
        super(row, col, word);
    }
}
