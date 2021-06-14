import java.io.*;
import java.util.Map;
import static java.util.Map.entry;

public class Parser
{
    private BufferedReader br;
    private String[] currLineParts;
    private String nextLine;
    private CommandType currCommandType;

    // Initialise static hashsets to categorise commands
    private static Map<String, CommandType> commandsMap;
    static
    {
        commandsMap = Map.ofEntries(entry("add", CommandType.C_ARITHMETIC), entry("sub", CommandType.C_ARITHMETIC),
                entry("neg", CommandType.C_ARITHMETIC), entry("eq", CommandType.C_ARITHMETIC),
                entry("gt", CommandType.C_ARITHMETIC), entry("lt", CommandType.C_ARITHMETIC),
                entry("and", CommandType.C_ARITHMETIC), entry("or", CommandType.C_ARITHMETIC),
                entry("not", CommandType.C_ARITHMETIC), entry("push", CommandType.C_PUSH),
                entry("pop", CommandType.C_POP));
    }

    public Parser(BufferedReader br) throws IOException
    {
        this.br = br;
        readNextCommand();
    }

    public boolean hasMoreCommands()
    {
        return (nextLine != null);
    }

    /**
     * Advances to the next line in the file
     */
    public void advance() throws IOException
    {
        if (!hasMoreCommands())
        {
            throw new UnsupportedOperationException("Attempted to advance without remaining commands");
        }
        currLineParts = nextLine.split("\\s+");
        determineCurrCommandType();
        readNextCommand();
    }

    /**
     * Gets the command type of the current line
     * 
     * @return The {@code CommandType} of the current line
     */
    public CommandType commandType()
    {
        return currCommandType;
    }

    
    /**
     * Get arg1 of the current line
     * @return In case of C_ARITHMETIC, the command itself; otherwise the first argument
     * @throws UnsupportedOperationException - If arg1() is called 
     * when the current line is a C_RETURN command
     */
    public String arg1()
    {
        // No breaks needed as immediately throwing/returning in all cases
        switch (currCommandType) {
            case C_RETURN:
                throw new UnsupportedOperationException("Attempted to get arg1 on a C_RETURN command");
            case C_ARITHMETIC:
                return currLineParts[0];
            default:
                return currLineParts[1];
        }
    }
    
    
    public int arg2()
    {
        if (currCommandType == CommandType.C_PUSH ||
            currCommandType == CommandType.C_POP ||
            currCommandType == CommandType.C_FUNCTION ||
            currCommandType == CommandType.C_CALL)
        {
            return Integer.parseInt(currLineParts[2]);
        }
        else 
        {
            throw new UnsupportedOperationException("Attempted to get arg2 on " + currCommandType + " Command");
        }
    }

    public void dump() throws IOException
    {
        while (hasMoreCommands())
        {
            advance();
            System.out.println(currLineParts[0] + "\t" + commandType());
        }
    }

    /**
     * Lookup the commandsMap to set the currCommandType
     */
    private void determineCurrCommandType()
    {
        currCommandType = commandsMap.get(currLineParts[0]);
    }

    /**
     * Reads and cleans up the next non-empty line
     * 
     * @throws IOException
     */
    private void readNextCommand() throws IOException // read next non-empty line
    {
        nextLine = br.readLine();

        // Skip empty lines unless EOF
        while (nextLine != null)
        {
            // Strip leading and trailing whitespace, and // comments
            if (!(nextLine = nextLine.replaceAll("^\\s+|\\s+$|//.*", "")).equals(""))
            {
                break;
            }
            nextLine = br.readLine();
        }
    }
}
