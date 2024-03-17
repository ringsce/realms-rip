program setup;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

procedure AssemblyFunction; assembler;
asm
  // Your assembly code goes here
  // for test purposes
.global _start @starting main
@address linker


_start:
    mov RO, #1 @1=StdOut
    ldr R1, =testing @ string to print
    mov R2, #13 @length
    mov R7, #4 @linux call
    svc 0 @call to print

@set up the parameters to exit the program
@and casll linux to do it

    mov R0, #0 @return 0 code
    mov R7, #1 @return service code 1
                @ ends call
    svc 0 @call to terminate

.data
testing: .ascii "testing\n"

end;

begin
  AssemblyFunction; // Call your assembly function
end.
