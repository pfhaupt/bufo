package compiler.parser;

import compiler.token.types.IntrinsicToken;
import compiler.token.Token;
import compiler.token.types.BracketToken;
import compiler.token.types.TypeToken;
import util.TreePrinter;
import util.Utility;

import java.util.ArrayList;

public class AST {
    private final TokenNode root;

    private AST(TokenNode _root) {
        root = _root;
    }

    public String toString() {
        printTree(root);
        return "";
    }

    private static void insertNode(TokenNode root, Token token) {
        if (root.getRightChild() != null) {
            Utility.printCompilerError("Please think of a good name for this error, right child of root is not null");
        }
        root.setRightChild(new TokenNode(token, root, null, null));
    }

    private static boolean checkNode(TokenNode node) {
        // TODO: Keep in mind that later on the trees can get much more complicated than this
        if (node.getRightChild().getToken() instanceof IntrinsicToken) {
            return false;
        }
        if (node.getToken() instanceof TypeToken) {
            return false;
        }
        return true;
    }

    private static TokenNode inlineNode(TokenNode root) {
        TokenNode result = new TokenNode(root.getRightChild().getToken(), root.getParent(), root, null);
        root.setRightChild(null);
        root.setParent(result);
        if (root.getParent().getParent() != null) {
            root.getParent().getParent().setRightChild(root.getParent());
        }
        return result;
    }

    private static void printTree(TokenNode root) {
        printTree(root, 0);
    }

    private static void printTree(TokenNode root, int depth) {
        if (root == null) return;
        System.out.println(root.getToken());
        depth += 2;
        if (root.getLeftChild() != null) {
            System.out.print(" ".repeat(depth) + "L: ");
            printTree(root.getLeftChild(), depth);
        }
        if (root.getRightChild() != null) {
            System.out.print(" ".repeat(depth) + "R: ");
            printTree(root.getRightChild(), depth);
        }
    }

    public static AST generateTree(ArrayList<Token> code) {
        TokenNode currentNode = new TokenNode(code.get(0));
        int currentTokenIndex = 1;
        int openBrackets = 0;
        while (currentTokenIndex < code.size()) {
            Token currentToken = code.get(currentTokenIndex++);
            if (currentToken instanceof BracketToken) {
                String bracket = currentToken.getWord();
                if (bracket.equals("(")) {
                    openBrackets++;
                } else if (bracket.equals(")")) {
                    if (openBrackets <= 0) {
                        Utility.printCompilerError("Please think of a good name for this error, too many closing brackets");
                    }
                    openBrackets--;
                }
            } else {
                insertNode(currentNode, currentToken);
                if (currentNode.getToken() instanceof IntrinsicToken) {
                    Utility.printCompilerWarning("Please think about precedence, why is this so difficult");
                }
                boolean validTree = checkNode(currentNode);
                if (!validTree) {
                    currentNode = inlineNode(currentNode);
                } else {
                    currentNode = currentNode.getRightChild();
                }
            }
        }
        if (openBrackets > 0) {
            Utility.printCompilerError("Please think of a good name for this error, too many opening brackets");
        }

        while (currentNode.getParent() != null) currentNode = currentNode.getParent();

        return new AST(currentNode);
    }
}
