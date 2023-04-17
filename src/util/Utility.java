package util;

import compiler.token.Token;

import java.util.ArrayList;

public class Utility {
    public static void printCompilerWarning(String warning) {
        System.out.println("WARNING: " + warning);
    }

    public static void printCompilerError(String error) {
        System.err.println("ERROR: " + error);
        System.exit(1);
    }

    public static void printCompilerErrorWithMessage(String error, String info) {
        System.err.println("ERROR: " + error + "\n" + info);
        System.exit(3);
    }

    public static void printNotImplementedError(String msg) {
        System.err.println("Oops! I have not implemented " + msg + " yet.");
        System.exit(2);
    }

    public static ArrayList<Token> splitList(ArrayList<Token> list, int start, int end) {
        ArrayList<Token> result = new ArrayList<>();
        for (int i = start; i < end; i++) {
            result.add(list.get(i));
        }
        return result;
    }
}
