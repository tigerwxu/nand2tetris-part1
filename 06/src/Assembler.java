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

        while (parser.hasMoreCommands())
        {
            parser.advance();
            switch (parser.commandType()) {
                case A_COMMAND:
                    bw.write(toBin16String(parser.symbol()));
                    break;
                case C_COMMAND:
                    bw.write("111" + Code.comp(parser.comp()) + Code.dest(parser.dest()) + Code.jump(parser.jump()));
                    break;
                case L_COMMAND:
                    break;
            }
            bw.write('\n');
        }
        
        // Flush and close output before exiting
        bw.close();
    }
    
    public static String toBin16String(String s)
    {
        return String.format("%16s", Integer.toBinaryString(Integer.parseInt(s))).replaceAll(" ", "0");
    }
   
}
