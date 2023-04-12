import util.Utility;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;


public class Flag {
    private static Flag[] validFlags;

    private final String name;
    private final String[] options;
    private int selected = 0;

    private Flag(String name, String[] options) {
        this.name = name;
        this.options = options;
    }

    private static void initFlags() {
        /*
        All those flags are taken from my other project "Haupt", no idea which I'll use this time around
        so they're just commented out for now.
         */
        validFlags = new Flag[]{
                new Flag("input", new String[]{"file"}),
                //new Flag("warn", new String[]{"none", "all"}),
                //new Flag("optimize", new String[]{"none", "all"}),
                //new Flag("run", new String[]{"false", "true"}),
                //new Flag("typed", new String[]{"true", "false"}),
                //new Flag("silenced", new String[]{"false", "true"}),
                //new Flag("asm", new String[]{"discard", "keep"}),
        };
    }

    private static void showHelp(String cmd) {
        String helpString = "";
        switch (cmd) {
            case "flags":
                Utility.printNotImplementedError("showing help for flags");
                break;
            case "mode":
                Utility.printNotImplementedError("showing help for mode");
                break;
            case "warn":
                Utility.printNotImplementedError("showing help for warn");
                break;
            case "optimize":
                Utility.printNotImplementedError("showing help for optimize");
                break;
            case "run":
                Utility.printNotImplementedError("showing help for run");
                break;
            case "typed":
                Utility.printNotImplementedError("showing help for typed");
                break;
            case "silenced":
                Utility.printNotImplementedError("showing help for silenced");
                break;
            case "asm":
                Utility.printNotImplementedError("showing help for asm");
                break;
            case "all":
                Utility.printNotImplementedError("showing help for all flags");
                break;
            default:
                for (Flag flag : validFlags) {
                    if (cmd.equals(flag.name)) {
                        Utility.printCompilerError("If this fails, that means that the flag does exist, but has no help-text yet.");
                    }
                }
                Utility.printCompilerErrorWithMessage(
                        String.format("Unknown flag `%s`.", cmd),
                        "  Can't show help.");
        }
        helpString = "Flag `" + cmd + "`:\n" + helpString + "\n";
        System.out.println(helpString);
    }

    public static Map<String, String> parseFlags(String[] args) {
        if (validFlags == null) initFlags();

        if (args.length == 0) Utility.printCompilerError("Not enough arguments");

        boolean found_input = false;
        String input_name = "";

        for (String arg : args) {
            String[] splitArg = arg.split("=");
            if (splitArg.length != 2) {
                Utility.printCompilerErrorWithMessage(
                        String.format("Unknown flag `%s`.", arg),
                        "All flags follow the form `flag=mode`.");
            }
            String arg_flag = splitArg[0];
            String arg_mode = splitArg[1];
            boolean valid_flag = false;
            if (arg_flag.equals("input")) {
                found_input = true;
                input_name = arg_mode;
            } else if (arg_flag.equals("help")) {
                showHelp(arg_mode);
                System.exit(0);
            } else {
                for (Flag flag : validFlags) {
                    if (arg_flag.equals(flag.name)) {
                        boolean valid_mode = false;
                        for (int i = 0; i < flag.options.length; i++) {
                            if (arg_mode.equals(flag.options[i])) {
                                flag.selected = i;
                                valid_mode = true;
                                break;
                            }
                        }
                        if (!valid_mode) {
                            Utility.printCompilerErrorWithMessage(
                                    String.format("Unknown mode `%s` for argument `%s`", arg_mode, arg_flag),
                                    String.format("  Run `java Main help=%s` for supported modes.", arg_flag));
                        }
                        valid_flag = true;
                        break;
                    }
                }
                if (!valid_flag) {
                    Utility.printCompilerErrorWithMessage(
                            String.format("Unknown flag `%s`", arg_flag),
                            "  Run `java Main help=flags` for supported flags."
                    );
                }
            }
        }

        if (!found_input) {
            Utility.printCompilerErrorWithMessage(
                    "No input specified!",
                    "Please specify the input file with `input=file`."
            );
        }
        Map<String, String> usedFlags = new HashMap<>();
        for (Flag flag : validFlags) {
            usedFlags.put(flag.name, flag.options[flag.selected]);
        }
        usedFlags.put("input", input_name);
        return usedFlags;
    }
}
