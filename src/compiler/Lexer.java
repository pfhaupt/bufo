package compiler;

import compiler.token.types.IntrinsicToken;
import compiler.token.types.*;
import compiler.token.Token;
import util.Utility;

import java.util.*;

public class Lexer {
    private static final ArrayList<Token> tokens = new ArrayList<>();

    private static final Character[] supportedCharArray = new Character[] {'=', '+', '/', '*', '-'};
    private static final Set<Character> supportedChars = new HashSet<>(List.of(supportedCharArray));

    private static final String[] supportedTypeArray = new String[] {"int", "float"};
    private static final Set<String> supportedTypes = new HashSet<>(List.of(supportedTypeArray));

    private static final HashMap<String, Integer> operatorPrecedence = new HashMap<>();

    private static String original;
    private static int row = 1, col = 0;
    private static char currentChar;
    private static int charIndex = 0;

    public static void initialize(HashMap<String, String> flags) {
        operatorPrecedence.put("+", 1);
        operatorPrecedence.put("-", 1);
        operatorPrecedence.put("*", 10);
        operatorPrecedence.put("/", 10);
        operatorPrecedence.put("=", 999999999);
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
        StringBuilder buffer = new StringBuilder();
        if (Character.isWhitespace(currentChar)) {
            if (currentChar == '\n') {
                col = 0;
                row++;
            }
            currentChar = nextCharacter();
            return new NoneToken(row, col);
        } else if (Character.isAlphabetic(currentChar)) {
            while (charIndex < original.length() && Character.isAlphabetic(currentChar)) {
                buffer.append(currentChar);
                currentChar = nextCharacter();
            }
            if (supportedTypes.contains(buffer.toString())) {
                return new TypeToken(row, col, buffer.toString());
            }
            return new WordToken(row, col, buffer.toString());
        } else if (Character.isDigit(currentChar)) {
            while (charIndex < original.length() && Character.isDigit(currentChar)) {
                buffer.append(currentChar);
                currentChar = nextCharacter();
            }
            return new NumberToken(row, col, buffer.toString());
        } else {
            char currentChar = Lexer.currentChar;
            buffer.append(currentChar);
            Lexer.currentChar = nextCharacter();
            if (currentChar == '{' || currentChar == '}') {
                return new BracketToken(row, col, buffer.toString());
            } else if (currentChar == '(' || currentChar == ')') {
                return new BracketToken(row, col, buffer.toString());
            } else if (currentChar == ';') {
                return new SeparatorToken(row, col, buffer.toString());
            } else if (supportedChars.contains(currentChar)) {
                return new IntrinsicToken(row, col, buffer.toString(), operatorPrecedence.get(currentChar + ""));
            } else {
                Utility.printCompilerErrorWithMessage(
                        String.format("Can't lex `%s` yet!", currentChar),
                        String.format("  Found here: row %d, col %d", row, col)
                );
                // Unreachable because the above function exits the program, but Java doesn't know that and I need to return *something*.
                return new NoneToken(-1, -1);
            }
        }
    }

    public static void run(String sourceCode) {
        original = sourceCode;
        currentChar = nextCharacter();
        while (charIndex < original.length()) {
            Token token = nextToken();
            if (!token.isEmptyToken()) {
                tokens.add(token);
            }
        }
    }

    public static Token[] getTokens() {
        return tokens.toArray(new Token[0]);
    }
}
