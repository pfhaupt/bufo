import java.util.Map;

public class Main {
    public static void main(String[] args) {
        Map<String, String> parsedFlags = Flag.parseFlags(args);
        System.out.println(parsedFlags);
    }
}