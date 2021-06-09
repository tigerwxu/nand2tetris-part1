import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class CodeWriter
{
    private BufferedWriter bw;

    // Arithmetic on the top two elements of the stack
    // Binary: Pop y; Pop x; Push x <operator> y
    // y = *(SP-1), x = *(SP-2)
    // Note SP points to the next open slot on the stack (1 more than the top of the stack)
    private static final String ADD = 
        "@SP\n" +       
        "AM=M-1\n" +    // Decrement the stack pointer, go to top element of stack (y)
        "D=M\n" +       // D holds the value of y
        "A=A-1\n" +     // Go to next element of stack (x)
        "M=D+M\n";      // Replace x with y + x. SP has already been decremented
                        // (Note that the .hack platform does not support M+D, as = to D+M from commutativity)
    private static final String SUB = 
        "@SP\n" +       
        "AM=M-1\n" + 
        "D=M\n" + 
        "A=A-1\n" + 
        "M=M-D\n";      // Same as ADD, except we replace x with x - y
                        // (this time order does matter; the .hack platform supports both M-D and D-M)
    private static final String AND = 
        "@SP\n" + 
        "AM=M-1\n" + 
        "D=M\n" + 
        "A=A-1\n" + 
        "M=D&M\n";      // Same as ADD, except we replace x with y & x (commutative op)
    
    private static final String OR = 
        "@SP\n" + 
        "AM=M-1\n" + 
        "D=M\n" + 
        "A=A-1\n" + 
        "M=D|M\n";      // Same as ADD, except we replace x with y | x (commutative op)
    
    // Arithmetic on the top element of the stack
    // Unary: Pop y; Push <operator> y
    // y = *(SP - 1)
    private static final String NOT = 
        "@SP\n" +
        "A=M-1\n" +
        "M=!M\n";
            
    private static final String NEG = 
        "@SP\n" +
        "A=M-1\n" +
        "M=-M\n";

    public CodeWriter(File fname) throws IOException
    {
        bw = new BufferedWriter(new FileWriter(fname));
    }

    public void setFileName(File fname) throws IOException
    {
        close();
        bw = new BufferedWriter(new FileWriter(fname));
    }

    public void writeArithmetic(String command) throws IOException
    {
        switch (command) {
            case "add":
                bw.write(ADD);
                break;
            case "sub":
                bw.write(SUB);
                break;
            // eq, gt, lt require a bit more logic
            case "eq":
                eq();
                break;
            case "gt":
                gt();
                break;
            case "lt":
                lt();
                break;
            case "and":
                bw.write(AND);
                break;
            case "or":
                bw.write(OR);
                break;
            case "not":
                bw.write(NOT);
                break;
            case "neg":
                bw.write(NEG);
                break;
        }
    }
    
    // Variables to give comparison jumps unique labels
    private int i = 0, j = 0, k = 0;
    private void eq() throws IOException
    {
        bw.write(
            "@SP\n" +
            "AM=M-1\n" +
            "D=M\n" +       
            "A=A-1\n" +
            "D=M-D\n" +                   // Set D=x-y
            "@$EQ." + i + "\n" +
            "D;JEQ\n" +                   // Jump to $EQ.i if x-y=0 (i.e. x = y)
            "@SP\n" +                     // Otherwise, set *SP to 0 (false)
            "A=M-1\n" +
            "M=0\n" +
            "@$END.EQ." + i + "\n" +      // Jump past $EQ.i part to end of block
            "0;JMP\n" +
            "($EQ." + i + ")\n" +   
            "@SP\n" +
            "A=M-1\n" +
            "M=-1\n"  +                   // Set *SP to true (-1 in 2's complement, or 0xFF)
            "($END.EQ." + i + ")\n"
        );
        this.i++;
    }
    
    // lt and gt are similar to eq, except on their branching condition
    // Might refactor this to generate lt, gt, and eq dynamically rather than hard-code.
    private void lt() throws IOException
    {
        bw.write(
            "@SP\n" +
            "AM=M-1\n" +
            "D=M\n" +       
            "A=A-1\n" +
            "D=M-D\n" +
            "@$LT." + j + "\n" +
            "D;JLT\n" +
            "@SP\n" +
            "A=M-1\n" +
            "M=0\n" +
            "@$END.LT." + j + "\n" +
            "0;JMP\n" +
            "($LT." + j + ")\n" +   
            "@SP\n" +
            "A=M-1\n" +
            "M=-1\n"  +
            "($END.LT." + j + ")\n"
        );
        this.j++;
    }
    
    private void gt() throws IOException
    {
        bw.write(
            "@SP\n" +
            "AM=M-1\n" +
            "D=M\n" +       
            "A=A-1\n" +
            "D=M-D\n" +
            "@$GT." + k + "\n" +
            "D;JGT\n" +
            "@SP\n" +
            "A=M-1\n" +
            "M=0\n" +
            "@$END.GT." + k + "\n" +
            "0;JMP\n" +
            "($GT." + k + ")\n" +   
            "@SP\n" +
            "A=M-1\n" +
            "M=-1\n"  +
            "($END.GT." + k + ")\n" 
        );
        this.k++;
    }

    public void writePushPop(CommandType command, String segment, int index) throws IOException
    {
        switch (command) {
            case C_PUSH:
                getValueToPush(segment, index);
                bw.write("@SP\n" +
                         "AM=M+1\n" + // Increment stack pointer, and go there
                         "A=A-1\n" +  // Go back one address to original SP
                         "M=D\n"      // Store D in there
                        );
                break;
            case C_POP:
                //
                break;
            default:
                throw new RuntimeException();
        }
    }

    /**
     * Generates assembly to set D register to the value to push
     * @param segment - name of the segment
     * @param i - index added to base address of segment
     * @throws IOException 
     */
    private void getValueToPush(String segment, int i) throws IOException
    {
        String asm = "";
        switch (segment) {
            case "constant":
                asm = "@" + i + "\n" +
                      "D=A\n";
                break;
        }
        bw.write(asm);
    }
    
    public void close() throws IOException
    {
        bw.close();
    }
}
