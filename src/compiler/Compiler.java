package compiler;

import compiler.parser.Parser;
import compiler.token.Token;
import util.Utility;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class Compiler {
    private static String filename;
    private static String sourceCode;
    private static Token[] lexedCode;
    private static final String FILE_EXTENSION = ".bu";
    private static HashMap<String, String> flags;

    private static final String[] operatorPrecedence = new String[] {"=", "+-", "*/"};


    public static void initialize(HashMap<String, String> _flags) {
        flags = _flags;
        filename = flags.get("input");
        if (!filename.endsWith(FILE_EXTENSION)) {
            Utility.printCompilerErrorWithMessage(
                    String.format("File %s does not end with `%s`!", filename, FILE_EXTENSION),
                    "Aborting compilation!");
        }
        sourceCode = loadFile(filename);
        Lexer.initialize(flags);
        Parser.initialize(flags);
    }

    public static String[] getOperatorPrecedence() {
        return operatorPrecedence;
    }

    public static void run() {
        Lexer.run(sourceCode);
        lexedCode = Lexer.getTokens();
        Parser.run(lexedCode);
        Utility.printNotImplementedError("actually compiling anything");
    }

    private static String loadFile(String filename) {
        StringBuilder sourceCode = new StringBuilder();
        try {
            BufferedReader br = new BufferedReader(new FileReader(filename));
            try {
                String line = br.readLine();
                while (line != null) {
                    sourceCode.append(line).append("\n");
                    line = br.readLine();
                }
            } catch (IOException e) {
                Utility.printCompilerError("Something went wrong when reading the source file!");
            }
        } catch (FileNotFoundException e) {
            Utility.printCompilerError(String.format("Could not find file `%s`!", filename));
        }
        return sourceCode.toString();
    }
}
