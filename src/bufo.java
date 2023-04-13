import compiler.Compiler;
import compiler.Flag;

import java.util.HashMap;

public class bufo {
    public static void main(String[] args) {
        HashMap<String, String> parsedFlags = Flag.parseFlags(args);
        Compiler.initialize(parsedFlags);
        Compiler.run();
    }
}
