// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Psuedocode:
// while (true) 
// {
//     if (keyPressed)
//     {
//         blackenScreen()
//     }
//     else
//     {
//         whitenScreen()
//     }
// }

(LOOP)
    // Set color to 0
    @color
    M=0

    // Check if a key is pressed, in which case change color to -1
    @KBD
    D=M
    @SETBLACK
    D;JNE // Jump if KEYBOARD != 0 (a key is pressed)

(FILLSCREEN)
    @i
    M=0
(FILLSCREENLOOP)
    // Check if we have set all pixels
    @i
    D=M
    @8192
    D=D-A
    @LOOP
    D;JGE // Return to LOOP if i >= 8192

    // Set current register to 0
    @i
    D=M
    @SCREEN
    D=A+D
    @currline
    M=D // Save address KDB+i (base+offset)
    @color
    D=M
    @currline
    A=M
    M=D // Set all pixels in currline to color
    
    // Increment i and loop
    @i
    M=M+1
    @FILLSCREENLOOP
    0;JMP

(SETBLACK)
    @color
    M=-1
    @FILLSCREEN
    0;JMP