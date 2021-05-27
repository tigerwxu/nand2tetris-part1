import java.util.HashMap;

public class Code
{
    private static HashMap<String, String> destTable = new HashMap<String, String>();
    private static HashMap<String, String> compTable = new HashMap<String, String>();
    private static HashMap<String, String> jumpTable = new HashMap<String, String>();

    static // Initialise tables
    {
        String[] compMnems = new String[] {"0", "1", "-1", "D", "A",
                                               "!D", "!A", "-D","-A", "D+1",
                                               "A+1", "D-1", "A-1", "D+A","D-A",
                                               "A-D", "D&A", "D|A", "M", "!M",
                                               "-M", "M+1", "M-1", "D+M", "D-M",
                                               "M-D","D&M", "D|M"};
        String[] compCodes = new String[] {"0101010", "0111111", "0111010", "0001100", "0110000",
                                           "0001101", "0110001", "0001111", "0110011", "0011111",
                                           "0110111", "0001110", "0110010", "0000010", "0010011",
                                           "0000111", "0000000", "0010101", "1110000", "1110001",
                                           "1110011", "1110111", "1110010", "1000010", "1010011",
                                           "1000111", "1000000", "1010101"};
        for (int i = 0; i < compMnems.length; i++)
        {
            compTable.put(compMnems[i], compCodes[i]);
        }
                
        // Code table for dest and jump are the same
        String[] djCodes = new String[] {"000", "001", "010", "011", "100", "101", "110", "111"};
        String[] destMnems = new String[] {null, "M", "D", "MD", "A", "AM", "AD", "AMD"};
        String[] jumpMnems = new String[] {null, "JGT", "JEQ", "JGE", "JLT", "JNE", "JLE", "JMP"};
        for (int i = 0; i < destMnems.length; i++)
        {
            destTable.put(destMnems[i], djCodes[i]);
            jumpTable.put(jumpMnems[i], djCodes[i]);
        }
        
        
    }

    public static String dest(String mnemonic)
    {
        return destTable.get(mnemonic);
    }

    public static String comp(String mnemonic)
    {
        return compTable.get(mnemonic);
    }
    
    public static String jump(String mnemonic)
    {
        return jumpTable.get(mnemonic);
    }
}
