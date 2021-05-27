import java.util.HashMap;

public class SymbolTable
{
    HashMap<String, Integer> table;

    public SymbolTable()
    {
        table = new HashMap<String, Integer>();
        String[] predefSymbols = new String[] {"SP", "LCL", "ARG", "THIS", "THAT", "SCREEN", "KBD",
                                               "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8",
                                               "R9", "R10", "R11", "R12", "R13", "R14", "R15"};
        int[] predefAddresses = new int[] {0, 1, 2, 3, 4, 16384, 24576, 0, 1, 2, 3, 4, 5, 6, 7,
                                           8, 9, 10, 11, 12, 13 ,14 ,15};
        
        for (int i = 0; i < predefSymbols.length; i++)
        {
            table.put(predefSymbols[i], predefAddresses[i]);
        }
    }
    
    public void addEntry(String symbol, int address)
    {
        table.put(symbol, address);
    }
    
    public boolean contains(String symbol)
    {
        return table.containsKey(symbol);
    }
    
    public int getAddress(String symbol)
    {
        return table.get(symbol);
    }
}
