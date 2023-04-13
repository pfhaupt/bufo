package util;

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
}
