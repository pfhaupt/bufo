package compiler.token.types;

import compiler.token.Token;

public class IntrinsicToken extends Token {
    private int precedence;
    public IntrinsicToken(int row, int col, String word, int _precedence) {
        super(row, col, word);
        precedence = _precedence;
    }

    public void setPrecedence(int _precedence) {
        precedence = _precedence;
    }

    public int getPrecedence() {
        return precedence;
    }
}
