// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
//
// This program only needs to handle arguments that satisfy
// R0 >= 0, R1 >= 0, and R0*R1 < 32768.

// Psuedocode:
// int result = 0;
// int i = 0;
// while (i < R1)
// {
//     result += R0;
//     i++;
// }

    // Initialise result to 0
    @R2
    M=0

    // Initialise i to 0
    @i
    M=0

(LOOP)
    // Check if need to break out of LOOP
    @i
    D=M
    @R1
    D=D-M
    @END
    D;JGE // Break if i-R1 >= 0 (i.e. i >= R1)

    // Sum another R0 to R2
    @R0
    D=M
    @R2
    M=D+M // R2 = R0 + R2

    // Increment i
    @i
    M=M+1
    @LOOP
    0;JMP

(END)
    @END
    0;JMP