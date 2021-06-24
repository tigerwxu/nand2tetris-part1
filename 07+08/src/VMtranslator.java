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
            return;
        }

        // Build an array of all the vm file names
        String[] vmFileNames = null;
        if (path.isDirectory()) // If the user passes a directory, then list all .vm files in that directory
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
            
            // Setup output file name as directory name + .asm extension
            outpath = new File(path, (path.getName() + ".asm"));
        }
        else if (path.isFile()) // Otherwise, if the user passes a file, then get that one file
        {
            vmFileNames = new String[] { path.getName() }; // 
            path = path.getParentFile(); // Set path to parent directory of file
            
            // Check the file is indeed a .vm file
            if (!vmFileNames[0].endsWith(".vm"))
            {   
                System.err.println("ERROR: The specified file is not a .vm file");
                return;
            }
            // Setup output file as file name with .vm replaced with .asm
            outpath = new File(path, vmFileNames[0].replaceAll("\\.vm$", ".asm"));
        }
        
        
        // Loop through each .vm file in the path given
        CodeWriter cw = new CodeWriter(outpath);
        for (String vmFileName : vmFileNames)
        {
            cw.setFileName(vmFileName.replaceAll("\\.vm$", "")); // Set filename as current file name without .vm extension
            String vmFilePath = path.toString() + File.separator + vmFileName;
            Parser parser = new Parser(new BufferedReader(new FileReader(vmFilePath)));
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
                    case C_LABEL:
                        cw.writeLabel(parser.arg1());
                        break;
                    case C_GOTO:
                        cw.writeGoto(parser.arg1());
                        break;
                    case C_IF:
                        cw.writeIf(parser.arg1());
                        break;
					case C_CALL:
						cw.writeCall(parser.arg1(), parser.arg2());
						break;
					case C_FUNCTION:
						cw.writeFunction(parser.arg1(), parser.arg2());
						break;
					case C_RETURN:
						cw.writeReturn();
						break;
                }
            }
        }
        cw.close();
    }
}
