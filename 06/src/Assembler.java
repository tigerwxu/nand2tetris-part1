import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class Assembler
{

    public static void main(String[] args) throws IOException
    {
        if (args.length < 1)
        {
            System.err.println("Usage: Assembler <filepath>");
            return;
        }

        Parser parser = new Parser(args[0]);
        
        // Create file to write output to
        String outName = new File(args[0]).getName().replaceFirst("\\..*", ".hack");
        File out = new File(outName);
        out.delete();
        out.createNewFile();
        
        BufferedWriter bw = new BufferedWriter(new FileWriter(outName));
        
        // First pass: populate the SymbolTable
        SymbolTable table = new SymbolTable(); // Constructor loads predefined symbols
        int currAddress = 0;
        while (parser.hasMoreCommands())
        {
            parser.advance();
            switch (parser.commandType()) {
                case A_COMMAND:
                    currAddress++;
                    break;
                case C_COMMAND:
                    currAddress++;
                    break;
                case L_COMMAND:
                    table.addEntry(parser.symbol(), currAddress);
                    break;
            }
        }
        
        // Second pass: reset the parser to the beginning of the file
        parser.reset();
        int nextEmptyAddress = 16;
        while (parser.hasMoreCommands())
        {
            parser.advance();
            switch (parser.commandType()) {
                case A_COMMAND:
                    String addr = parser.symbol();
                    if (addr.matches("[0-9]+")) // if @Xxx is a direct number
                    {
                        bw.write(toBin16String(addr));
                    }
                    else
                    {
                        if (table.contains(addr)) // if @Xxx was previously defined
                        {
                            bw.write(toBin16String(Integer.toString(table.getAddress(addr))));
                        }
                        else // if @Xxx is a new variable
                        {
                            table.addEntry(addr, nextEmptyAddress);
                            bw.write(toBin16String(Integer.toString(nextEmptyAddress)));
                            nextEmptyAddress++; 
                        }
                    }
                    bw.write('\n');
                    break;
                case C_COMMAND:
                    bw.write("111" + Code.comp(parser.comp()) + Code.dest(parser.dest()) + Code.jump(parser.jump()));
                    bw.write('\n');
                    break;
                case L_COMMAND:
                    break;
            }
        }
        
        // Flush and close output before exiting
        bw.close();
    }

    public static String toBin16String(String s)
    {
        return String.format("%16s", Integer.toBinaryString(Integer.parseInt(s))).replaceAll(" ", "0");
    }

}
