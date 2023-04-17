package compiler.parser;

import compiler.Compiler;
import compiler.token.types.IntrinsicToken;
import compiler.token.Token;
import compiler.token.types.BracketToken;
import compiler.token.types.TypeToken;
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

    private static final String[] operatorPrecedence = Compiler.getOperatorPrecedence();

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
        return generateTreeRecursive(code);
    }

    private static int findNextToken(ArrayList<Token> code, String currentPrecedence) {
        int bracketCounter = 0;
        String openBracket = "{(";
        String closingBracket = "})";
        for (int i = code.size() - 1; i >= 0; i--) {
            String word = code.get(i).getWord();
            if (openBracket.contains(word)) {
                bracketCounter++;
                if (bracketCounter > 0) {
                    Utility.printCompilerErrorWithMessage("Unmatched brackets",
                            String.format("Could not find matching bracket %s",
                                    code.get(i)));
                }
            } else if (closingBracket.contains(word)) {
                bracketCounter--;
            } else if (bracketCounter == 0 && currentPrecedence.contains(word)) {
                return i;
            }
        }
        if (bracketCounter != 0) {
            Utility.printCompilerErrorWithMessage("Unmatched brackets",
                    "Could not find matching bracket in parsing step.");
        }
        return -1;
    }

    private static void removeBrackets(ArrayList<Token> code) {
        int counter = 0;
        boolean removeBrackets = true;
        for (int i = 0; i < code.size(); i++) {
            Token t = code.get(i);
            if (t instanceof BracketToken) {
                if ("({".contains(t.getWord())) {
                    counter++;
                } else {
                    counter--;
                    if (counter == 0 && i != code.size() - 1) {
                        removeBrackets = false;
                        break;
                    }
                }
            }
        }
        if (removeBrackets) {
            Token first = code.get(0);
            Token last = code.get(code.size() - 1);
            if (first instanceof BracketToken && last instanceof  BracketToken) {
                code.remove(last);
                code.remove(first);
            }
        }
    }

    private static TokenNode generateSubtree(ArrayList<Token> code) {
        removeBrackets(code);
        if (code.size() == 1) {
            Token last = code.get(0);
            return new TokenNode(last);
        } else if (code.size() == 2) {
            Utility.printCompilerWarning("Please overthink this again.");
            Token type = code.get(0);
            Token val = code.get(1);
            return new TokenNode(val, null, new TokenNode(type), null);
        }
        for (String currentPrecedence : operatorPrecedence) {
            int index = findNextToken(code, currentPrecedence);
            if (index == -1) continue;
            TokenNode left = generateSubtree(Utility.splitList(code, 0, index));
            TokenNode right = generateSubtree(Utility.splitList(code, index + 1, code.size()));
            return new TokenNode(code.get(index), null, left, right);
        }
        System.out.println(code);
        Utility.printNotImplementedError("recursively generating the AST");
        return null;

    }

    private static AST generateTreeRecursive(ArrayList<Token> code) {
        AST result = new AST(generateSubtree(code));
        System.out.println(result);
        return result;
    }

    private static AST generateTreeIterative(ArrayList<Token> code) {
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
