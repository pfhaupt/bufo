package compiler.parser;

import compiler.token.Token;
import util.Utility;

import java.util.HashMap;

public class Parser {
    private static HashMap<String, String> flags;
    private static Token[] tokens;
    public static void initialize(HashMap<String, String> _flags) {
        flags = _flags;
    }

    public static void run(Token[] token) {
        tokens = token;
        Utility.printNotImplementedError("parsing anything");
    }
}
