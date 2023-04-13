package compiler.parser;

import compiler.token.Token;

public class TokenNode {
    private final Token token;
    private TokenNode parent = null;
    private TokenNode left = null;
    private TokenNode right = null;

    public TokenNode(Token _token) {
        token = _token;
    }

    public TokenNode(Token _token, TokenNode _parent, TokenNode _left, TokenNode _right) {
        token = _token;
        parent = _parent;
        left = _left;
        right = _right;
    }

    public String toString() {
        return String.format("Token: %s\nLeft: %s\nRight: %s", token, left, right);
    }

    public TokenNode getParent() {
        return parent;
    }

    public void setParent(TokenNode newParent) {
        parent = newParent;
    }

    public TokenNode getRightChild() {
        return right;
    }

    public void setRightChild(TokenNode newRight) {
        right = newRight;
    }

    public TokenNode getLeftChild() {
        return left;
    }

    public Token getToken() {
        return token;
    }
}
