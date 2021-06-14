import java.io.*;

public class VMtranslator
{

    public static void main(String[] args) throws Exception
    {
        // Validate the command line argument
        if (args.length < 1)
        {
            System.err.println("Usage: VMtranslator <vm file/folder path>");
            return;
        }
        File path = new File(args[0]);
        File outpath = null;
        if (!path.exists())
        {
            System.err.println("ERROR: The specified path does not exist");
        }

        // Build an array of all the vm file names
        String[] vmFileNames = null;
        if (path.isDirectory()) // If a folder, then list all .vm files
        {
            vmFileNames = path.list(new FilenameFilter()
            {
                public boolean accept(File f, String s)
                {
                    return s.endsWith(".vm");
                }
            });
            // Check we actually have .vm files
            if (vmFileNames.length == 0) 
            {
                System.err.println("ERROR: The specified path contains no .vm files");
                return;
            }
            
            // Setup output file
            outpath = new File(path, "out.asm");
        }
        else if (path.isFile()) // Otherwise, just use the path as is
        {
            vmFileNames = new String[] { "" };
            if (!path.getName().endsWith(".vm"))
            {   
                System.err.println("ERROR: The specified file is not a .vm file");
                return;
            }
            // Setup output file
            outpath = new File(path.getPath().replaceAll("\\.vm$", ".asm"));
        }
        
        
        // Loop through each .vm file in the path given
        CodeWriter cw = new CodeWriter(outpath);
        for (String vmFileName : vmFileNames)
        {
            cw.fname = vmFileName; // TODO: Fix single file mode giving empty fname
            String currFile = path.toString() + File.separator + vmFileName;
            Parser parser = new Parser(new BufferedReader(new FileReader(currFile)));
            while (parser.hasMoreCommands())
            {
                parser.advance();
                CommandType currCType;
                switch (currCType = parser.commandType()) {
                    case C_ARITHMETIC:
                        cw.writeArithmetic(parser.arg1());
                        break;
                    case C_PUSH: case C_POP:
                        cw.writePushPop(currCType, parser.arg1(), parser.arg2());
                        break;
                }
            }
            
        }
        cw.close();
    }

}
