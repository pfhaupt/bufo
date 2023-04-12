package heart;

import util.Utility;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class Compiler {
    private static String filename;
    private static final String FILE_EXTENSION = ".bu";
    private static HashMap<String, String> flags;

    public static void initialize(HashMap<String, String> _flags) {
        flags = _flags;
        filename = flags.get("input");
        if (!filename.startsWith("./")) {
            filename = "./"+ filename;
        }
        if (!filename.endsWith(FILE_EXTENSION)) {
            Utility.printCompilerErrorWithMessage(
                    String.format("File %s does not end with `%s`!", filename, FILE_EXTENSION),
                    "Aborting compilation!");
        }
    }

    public static void run() {
        String sourceCode = loadFile(filename);
        System.out.println(sourceCode);
        Utility.printNotImplementedError("actually compiling/running/parsing anything");
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
