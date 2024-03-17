program SaveKeyToFile;

uses
  SysUtils;

var
  KeyInput: string;
  KeyFile: TextFile;

begin
  // Prompt the user to input a key
  Write('Enter a 16-character key: ');
  ReadLn(KeyInput);

  // Validate the length of the key
  if Length(KeyInput) <> 16 then
  begin
    Writeln('Error: Key must be exactly 16 characters long.');
    Halt(1); // Exit with an error code
  end;

  // Save the key to a file
  AssignFile(KeyFile, 'keyfile.key'); // Assign file name
  try
    Rewrite(KeyFile); // Open file for writing (creates a new file)
    Write(KeyFile, KeyInput); // Write the key to the file
    Writeln('Key saved to keyfile.key successfully.');
  finally
    CloseFile(KeyFile); // Close the file
  end;
end.

