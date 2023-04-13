package compiler.parser;

import compiler.token.Token;
import compiler.token.types.SeparatorToken;
import util.Utility;

import java.util.ArrayList;
import java.util.HashMap;

public class Parser {
    private static HashMap<String, String> flags;
    private static Token[] tokens;
    private static final ArrayList<AST> parsedCode = new ArrayList<>();

    public static void initialize(HashMap<String, String> _flags) {
        flags = _flags;
    }

    public static void run(Token[] token) {
        tokens = token;
        int currentTokenIndex = 0;
        ArrayList<Token> buffer = new ArrayList<>();
        while (currentTokenIndex < tokens.length) {
            Token currentToken = tokens[currentTokenIndex];
            if (currentToken instanceof SeparatorToken) {
                if (buffer.size() > 0) {
                    AST instruction = AST.generateTree(buffer);
                    System.out.println(instruction);
                    parsedCode.add(instruction);
                    buffer.clear();
                }
            } else {
                buffer.add(currentToken);
            }
            currentTokenIndex++;
        }
        Utility.printNotImplementedError("parsing anything");
    }
}
