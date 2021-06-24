import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

public class CodeWriter
{
	// Keep track of the current file name, the current function name within this file,
	// and how many return labels have been generated in the current function
    private String currFileName;
    private String currFunction = "Sys.init";	// Initialised so bootstrapper will call Sys.init
    private int nthReturn = 0; // nthReturn is 0 only for the initial call to Sys.init
    						   // after that it resets to 1 on seeing a new function
    						   // this is to avoid a naming conflict since it is reset on every new function
    						   // including Sys.init
    
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

    public CodeWriter(File fpath) throws IOException
    {
        bw = new BufferedWriter(new FileWriter(fpath));
        this.writeInit(); // write bootstrap instructions before currFunction is overwritten
    }

    public void setFileName(String fname) throws IOException
    {
        this.currFileName = fname;
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

    /**
     * Generates assembly to push or pop between the stack and a memory segment
     * @param command - A push or pop command
     * @param segment - Name of the memory segment to push from or pop to
     * @param index - Positive offset to the base segment address
     * @throws RuntimeException If commandType was not a C_PUSH or C_POP
     * 
     */
    public void writePushPop(CommandType command, String segment, int index) throws IOException
    {
        switch (command) {
            case C_PUSH: // Push value from memory segment onto stack using D register
                getValueToPush(segment, index);
                pushD();
                break;
            case C_POP: // Pop value from stack into memory segment
                storePoppedValue(segment, index);
                break;
            default:
                throw new RuntimeException();
        }
    }
    
    /**
     * Pushes the contents of the D register onto the stack
     */
    private void pushD() throws IOException
    {
        bw.write("@SP\n" +
                "AM=M+1\n" + // Increment stack pointer, and go there
                "A=A-1\n" +  // Go back one address to original SP
                "M=D\n"      // Store D in there
        		);
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
                      "@R15\n" +
                      "M=D\n" +    // Temporarily store target address in R15
                      "@SP\n" +    // Go to stack pointer
                      "AM=M-1\n" + // Decrement stack pointer and go where it's pointing
                      "D=M\n" +    // D now stores the popped value
                      "@R15\n" +
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
     * NOTE: The authors of Nand2Tetris assume that writeInit is not implemented except for
     * the final three tests, FibonacciElement, NestedCall and StaticsTest
     * @throws IOException
     */
    private void writeInit() throws IOException
    {
        // Set SP to 256 as per standard mapping
        bw.write("@256\n" +
                 "D=A\n" +
                 "@SP\n" +
                 "M=D\n");
        
        // Call Sys.init
        this.writeCall(currFunction, 0);
        
    }
    
    /**
     * Writes assembly to label current instruction
     * @param label - name of the label
     * @throws IOException
     */
    public void writeLabel(String label) throws IOException
    {
        // Label current address as (file.function$label) to scope to current function
    	// currFileName is not required as function lines in the vm code are already required
    	// to be prefixed with the name of the file they belong in
        bw.write("(" + currFunction + "$" + label  + ")\n");
    }
    
    /**
     * Writes assembly to jump to a label
     * @param label - name of the label to jump to
     * @throws IOException
     */
    public void writeGoto(String label) throws IOException
    {
        // Unconditionally jump to @file.function$label
    	// Once again, currFileName is not needed
        bw.write("@" + currFunction + "$" + label + "\n" + 
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
                 "@" + currFunction + "$" + label + "\n" + // @file.function$label
                 "D; JNE\n" ); // Jump if the stack value was true (not 0)
    }
    
    /**
     * Writes assembly to label a function and allocate space on the system stack for its local variables
     * @param functionName - name of this function
     * @param nVars - number of arguments to this function
     * @throws IOException
     */
    public void writeFunction(String functionName, int nVars) throws IOException
    {
    	// Since we're in a new function definition, change the current function name to this function
    	// and reset the incrementing return label counter
    	this.currFunction = functionName; 
    	this.nthReturn = 1; // nthReturn starts at 1 as 0 is reserved for the bootstrapper
    	
    	// Label current address
    	bw.write("(" + this.currFunction + ")\n");
    	
    	// Set LCL to point to base of local segment, which is where SP currently points
    	bw.write("@SP\n" +
    			 "D=M\n" +
    			 "@LCL\n" + 
    			 "M=D\n");
    	
    	
    	// Push constant 0 onto stack nVars times to make space for locals
    	// This code runs first when the function is called
    	for (int i = 0; i < nVars; i++)
    	{
    		this.writePushPop(CommandType.C_PUSH, "constant", 0);
    	}
    }

    public void writeReturn() throws IOException
    {
    	// First store LCL of the current stack frame in R13
    	bw.write("@LCL\n" +
    			 "D=M\n" +
    			 "@R13\n" +
    			 "M=D\n"
    			);
    	
    	// Next store the return address in R14 before it might be overwritten WRONG storing address not val
    	bw.write("@R14\n" +	
    			 "M=D\n" +	  // D still holds LCL from before
    			 "@5\n" +
    			 "D=A\n" +
    			 "@R14\n" +
    			 "A=M-D\n" +  //  Go to LCL - 5
    			 "D=M\n" +    // Store return address in D
    			 "@R14\n" +
    			 "M=D\n");    // Store return address in R14
    	
    	// Store the return value in ARG[0], where calling function expects
    	// THIS OVERWRITES THE RETURN ADDRESS IF THE FUNCTION HAS 0 ARGUMENTS!
    	// We could do this.writePushPop(CommandType.C_POP, "argument", 0), but since the offset is 0 we can hard code in less instructions
    	bw.write("@SP\n" +
    			 "A=M-1\n" + // Set address to SP-1, which holds the return value
    			 "D=M\n" +	 // Store return value in D
    			 "@ARG\n" +
    			 "A=M\n" +   // Go to *ARG
    			 "M=D\n");	 // Store return value in *ARG

    	bw.write(// reposition SP to ARG+1, so it's in the correct place after return to caller
    			"@ARG\n" +
    			"D=M+1\n" +
    			"@SP\n" +
    			"M=D\n" +

				// restore THAT
				"@R13\n" + // R13 = LCL, which is one address higher than THAT
				"AM=M-1\n" + // Decrement R13 so it points to THAT on the stack frame
				"D=M\n" +	// Store THAT from stack in D
				"@THAT\n" + // Restore value from the stack
				"M=D\n" +
				// repeat to restore THIS, ARG, LCL
				"@R13\n" +
				"AM=M-1\n" + // Decrement R13 so it points to THIS on the stack frame
				"D=M\n" +	// Store THIS from stack in D
				"@THIS\n" +
				"M=D\n" +				
				"@R13\n" +
				"AM=M-1\n" + // Decrement R13 so it points to ARG on the stack frame
				"D=M\n" +	// Store ARG from stack in D
				"@ARG\n" +
				"M=D\n" +				
				"@R13\n" +
				"AM=M-1\n" + // Decrement R13 so it points to LCL on the stack frame
				"D=M\n" +	// Store LCL from stack in D
				"@LCL\n" +
				"M=D\n" +				
				
				// Jump to return address
				"@R14\n" +
				"A=M\n" +
    			"0;JMP\n");  
    }

    /**
     * Generates assembly to push the return address, LCL, ARG, THIS, and THAT onto the stack,
     * points ARG to the bottom of the new stack frame at (SP - nArgs - 5),
     * and jumps to the called function
     * @param functionName - The name of the function to call
     * @param nArgs - The number of arguments the called function expects
     * @throws IOException
     */
    public void writeCall(String functionName, int nArgs) throws IOException
    {
    	// In order to track the return address, we have to push a label onto the stack
    	// which points to the end of this call code, in the form file.function$ret.i;
    	String returnLabel = this.currFunction + "$ret." + this.nthReturn;
    	bw.write("@" + returnLabel + "\n" +
    			"D=A\n");	 // Store return label in D and push onto the stack
        pushD();
    	
    	
    	// Next push the caller's LCL, ARG, THIS, THAT
    	bw.write("@LCL\n" +
    			 "D=M\n");
    	pushD();
    	bw.write("@ARG\n" +
   			 	 "D=M\n");
	   	pushD();
		bw.write("@THIS\n" +
				 "D=M\n");
		pushD();
		bw.write("@THAT\n" +
				 "D=M\n");
		pushD();
		
		// Set a new ARG = (SP - 5 - nArgs)
		// ARG[0] will point at the first argument,
		// or the return address if there are no arguments
		bw.write("@SP\n" +
				"D=M\n" +
				"@" + (nArgs + 5) + "\n" +
				"D=D-A\n" +
				"@ARG\n" +
				"M=D\n");
		
		// Jump to the called function at file.functionName$
		bw.write("@" + functionName + "\n" +
				 "0;JMP\n");
		
		// Write return label for the next instruction for the called function to return to
		bw.write("(" + returnLabel + ")\n");
		
		// Increment the number of return labels there have been in this function
		this.nthReturn++;
    }
}
