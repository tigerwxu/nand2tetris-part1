import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

public class CodeWriter
{
    public String currFileName;
    private String currFunction = "";
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
            case C_PUSH: // Push value from memory segment onto stack using D register
                getValueToPush(segment, index);
                bw.write("@SP\n" +
                         "AM=M+1\n" + // Increment stack pointer, and go there
                         "A=A-1\n" +  // Go back one address to original SP
                         "M=D\n"      // Store D in there
                        );
                break;
            case C_POP: // Pop value from stack into memory segment
                storePoppedValue(segment, index);
                break;
            default:
                throw new RuntimeException();
        }
    }

    // NOTE: these hack assembly predefined symbols could be replaced with literal constants
    // I.E.  1, 2, 3, 4, instead of LCL, ARG, THIS, THAT
    private Map<String, String> baseRegs = Map.ofEntries(Map.entry("local", "LCL"),
                                                          Map.entry("argument", "ARG"),
                                                          Map.entry("this", "THIS"),
                                                          Map.entry("that", "THAT"));
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
            
            // Use an A instruction to load a constant into D
            case "constant":
                asm = "@" + i + "\n" +
                      "D=A\n";
                break;
                
            // These segments have their base address stored in pointers
            case "local": case "argument": case "this": case "that":
                String baseReg = baseRegs.get(segment);
                asm = "@" + baseReg + "\n" + // Go to base address pointer
                      "D=M\n" +              // Store base address value
                      "@" + i + "\n" +       
                      "A=D+A\n" +            // Add offset to base address and go there
                      "D=M\n";               // Get value and put into D
                break;
                
            // pointer and temp are fixed at ram[3] and ram[5]
            case "pointer":
                asm = "@" + (i + 3) + "\n" +
                      "D=M\n";
                break;
            case "temp":
                asm = "@" + (i + 5) + "\n" +
                      "D=M\n";
                break;
            // static segment is emulated by exploiting symbolic references,
            // which is a feature of the hack assembly language.
            case "static":
                asm = "@" + currFileName + "." + i + "\n" +
                      "D=M\n";
                break;
            default:
                throw new RuntimeException("Invalid Push memory segment: " + segment);
        }
        bw.write(asm);
    }
    
    
    /**
     * Generates assembly to pop stack value into segment[i]
     * @param segment - name of the segment
     * @param i - index added to base address of segment
     */
    private void storePoppedValue(String segment, int i) throws IOException
    {
        String asm = "";
        switch (segment) {
            // Shouldn't ever try to pop into the constant segment
            case "constant":
                throw new RuntimeException("Illegal instruction: Attempted to pop into constant segment");
                
            // These segments have their base addresses stored in pointers
            case "local": case "argument": case "this": case "that":
                String baseReg = baseRegs.get(segment);
                asm = "@" + baseReg + "\n" + // First calculate target address
                      "D=M\n" +
                      "@" + i + "\n" +
                      "D=D+A\n" +  // D is now the target address
                      "@R13\n" +
                      "M=D\n" +    // Temporarily store target address in R13
                      "@SP\n" +    // Go to stack pointer
                      "AM=M-1\n" + // Decrement stack pointer and go where it's pointing
                      "D=M\n" +    // D now stores the popped value
                      "@R13\n" +
                      "A=M\n" +    // Point A back to the target address
                      "M=D\n";     // Store popped value in M
                break;
            //
            case "pointer":
                asm = "@SP\n" +    // Go to stack pointer
                      "AM=M-1\n" +
                      "D=M\n" +    // Store popped value in D
                      "@" + (i + 3) + "\n" + // Go to segment
                      "M=D\n";     // Store in segment
               break;
            case "temp":
                asm = "@SP\n" +    // Go to stack pointer
                      "AM=M-1\n" +
                      "D=M\n" +    // Store popped value in D
                      "@" + (i + 5) + "\n" + // Go to segment
                      "M=D\n";     // Store in segment  
                break;
            //    
            case "static":    
                asm = "@SP\n" +    // Go to stack pointer
                      "AM=M-1\n" +
                      "D=M\n" +    // Store popped value in D
                      "@" + currFileName + "." + i + "\n" + // @filename.i
                      "M=D\n";
                break;
            default:
                throw new RuntimeException("Invalid Pop memory segment: " + segment);
        }
        bw.write(asm);
    }
    
    public void close() throws IOException
    {
        bw.close();
    }
    
    /**
     * Writes bootstrap code for initialisation
     * @throws IOException
     */
    public void writeInit() throws IOException
    {
        // Set SP to 256 as per standard mapping
        bw.write("@256\n" +
                 "D=A\n" +
                 "@SP\n" +
                 "M=D\n");
    }
    
    /**
     * Writes assembly to label current instruction
     * @param label - name of the label
     * @throws IOException
     */
    public void writeLabel(String label) throws IOException
    {
        // Label current address as (file.function$label) to scope to current function
        bw.write("(" + currFileName + "." + currFunction + "$" + label  + ")\n");
    }
    
    /**
     * Writes assembly to jump to a label
     * @param label - name of the label
     * @throws IOException
     */
    public void writeGoto(String label) throws IOException
    {
        // Unconditonally jump to @file.function$label
        bw.write("@" + currFileName + "." + currFunction + "$" + label + "\n" + 
                 "0; JMP\n");
    }
    
    /**
     * Writes assembly to pop the first item off the stack and jump if it is true
     * @param label - name of the label
     * @throws IOException
     */
    public void writeIf(String label) throws IOException
    {
        // Pop item off stack, jump if it is true
        bw.write("@SP\n" +
                 "AM=M-1\n" +  // Decrement address and go where it's pointing
                 "D=M\n" +     // Store M in D, as M will change when addressing the jump target
                 "@" + currFileName + "." + currFunction + "$" + label + "\n" + // @file.function$label
                 "D; JNE\n" ); // Jump if the stack value was true (not 0)
    }
}
