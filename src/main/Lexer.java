package main;

import main.tok.types.BracketToken;
import main.tok.types.NoneToken;
import main.tok.Token;
import main.tok.types.NumberToken;
import main.tok.types.WordToken;
import util.Utility;

import java.util.ArrayList;
import java.util.HashMap;

public class Lexer {
    private static final ArrayList<Token> tokens = new ArrayList<>();
    private static String original;
    private static int row = 1, col = 1;
    private static char currentChar;
    private static int charIndex = 0;
    public static void initialize(String sourceCode, HashMap<String, String> flags) {
        original = sourceCode;
        currentChar = original.charAt(charIndex);
        charIndex = 1;
    }

    private static char nextCharacter() {
        if (charIndex < original.length()) {
            currentChar = original.charAt(charIndex++);
        } else {
            currentChar = 0;
        }
        col++;
        return currentChar;
    }

    private static Token nextToken() {
        /*
            Given a string of "word1 word2 word3 ...", next should return the following:
            next() -> word1
            next() -> word2
            next() -> word3
            next() -> ...
         */
        StringBuilder buffer = new StringBuilder();
        if (Character.isWhitespace(currentChar)) {
            if (currentChar == '\n') {
                col = 0;
                row++;
            }
            currentChar = nextCharacter();
            return new NoneToken(row, col, "");
        } else if (Character.isAlphabetic(currentChar)) {
            while (charIndex < original.length() && Character.isAlphabetic(currentChar)) {
                buffer.append(currentChar);
                currentChar = nextCharacter();
            }
            return new WordToken(row, col, buffer.toString());
        } else if (Character.isDigit(currentChar)) {
            while (charIndex < original.length() && Character.isDigit(currentChar)) {
                buffer.append(currentChar);
                currentChar = nextCharacter();
            }
            return new NumberToken(row, col, buffer.toString());
        } else if (currentChar == '{' || currentChar == '}') {
            buffer.append(currentChar);
            currentChar = nextCharacter();
            return new BracketToken(row, col, buffer.toString());
        } else if (currentChar == '(' || currentChar == ')') {
            buffer.append(currentChar);
            currentChar = nextCharacter();
            return new BracketToken(row, col, buffer.toString());
        } else {
            Utility.printCompilerWarning("Can't parse `" + currentChar + "` yet, it's ignored for now.");
            currentChar = nextCharacter();
            return new NoneToken(row, col, "");
        }
    }

    public static void run() {
        Token token = nextToken();
        while (charIndex < original.length()) {
            if (!token.isEmptyToken()) {
                tokens.add(token);
            }
            token = nextToken();
        }
        for (Token t : tokens) {
            System.out.println(t);
        }
        Utility.printNotImplementedError("running the lexer");
    }

    public static Token[] getTokens() {
        Utility.printNotImplementedError("getting the lexer output");
        return null;
    }
}
