import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Parser
{
    
    public static enum CommandType
    {
        A_COMMAND,
        C_COMMAND,
        L_COMMAND
    }
    
    String currLine, nextLine, path;
    private BufferedReader br;

    public Parser(String inputPath) throws IOException
    {
        path = inputPath;
        br = new BufferedReader(new FileReader(inputPath));
        readNextCommand();
    }

    // Check if there is still input (next line is not EOF)
    public boolean hasMoreCommands()
    {
        return (nextLine != null);
    }
    
    // Advance to the next line in the input file
    public void advance() throws IOException
    {
        currLine = nextLine;
        readNextCommand();
    }
    
    // Reset the reader back to the beginning of the file
    public void reset() throws IOException
    {
        br.close();
        currLine = null;
        br = new BufferedReader(new FileReader(path)); 
        readNextCommand();
    }
    
    public CommandType commandType()
    {
        if (currLine.matches("\\(.+\\)")) return CommandType.L_COMMAND; 
        
        if (currLine.matches("@.+")) return CommandType.A_COMMAND;
        
        return CommandType.C_COMMAND;
    }
    
    public String symbol()
    {
        return currLine.replaceAll("[@()]", "");
    }
    
    public String dest() 
    {
        if (currLine.indexOf('=') == -1)
        {
            return null;
        }
        else
        {
            return currLine.split("=", 2)[0];
        }
    }
    
    public String comp()
    {
        return currLine.replaceAll(".*=|;.*", "");
    }
    
    public String jump()
    {
        if (currLine.indexOf(';') == -1)
        {
            return null;
        }
        else
        {
            return currLine.split(";", 2)[1];
        }
    }
    
    private void readNextCommand() throws IOException // read next non-empty line
    {
        nextLine = br.readLine();
        
        // Skip empty lines unless EOF
        while (nextLine != null)
        {
            // Replace whitespace and comments with nothing. Continue reading if line ends up being empty
            if (!(nextLine = nextLine.replaceAll("\\s|//.*", "")).equals(""))
            {
                break;
            }
            nextLine = br.readLine();
        }
    }
}
