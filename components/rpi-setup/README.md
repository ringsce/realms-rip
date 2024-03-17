# AssemblyFunction Test for Raspberry Pi 3+ Lazarus IDE

## Objective
This markdown provides a guide to test the `AssemblyFunction` within Lazarus IDE on a Raspberry Pi 3+.

## Prerequisites
- Raspberry Pi 3+ set up and running Raspbian or compatible operating system.
- Lazarus IDE installed on the Raspberry Pi.
- Basic knowledge of assembly language for ARM processors.

## Procedure

### 1. Open Lazarus IDE
Launch Lazarus IDE on your Raspberry Pi.

### 2. Create a New Project
Create a new project in Lazarus. Choose "Console Application" or any suitable project type.

### 3. Write Assembly Code
Write your assembly code within the `AssemblyFunction` block. Be cautious, as assembly directly interacts with hardware. Example:

```pascal
procedure AssemblyFunction; assembler;
asm
.global _start @starting main
@address linker


_start:
    mov RO, #1 @1=StdOut
    ldr R1, =testing @ string to print
    mov R2, #13 @length
    mov R7, #4 @linux call
    svc 0 @call to print

@set up the parameters to exit the program
@and call linux to do it

    mov R0, #0 @return 0 code
    mov R7, #1 @return service code 1
                @ ends call
    svc 0 @call to terminate

.data
testing: .ascii "testing\n"
end;
```


Remember to replace the placeholder assembly code with your actual assembly routines, ensuring they are appropriate for your hardware setup and intended functionality. Always exercise caution when working with low-level code like assembly, especially on hardware platforms like the Raspberry Pi.
