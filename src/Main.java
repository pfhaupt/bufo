import java.util.HashMap;

public class Main {
    public static void main(String[] args) {
        HashMap<String, String> parsedFlags = Flag.parseFlags(args);
        Compiler.initialize(parsedFlags);
        System.out.println(parsedFlags);
    }
}