program LimitOggWithAssets;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

procedure AddFileToPK3(const PK3FileName, FileName: string);
var
  PK3File: TFileStream;
  SourceFile: TFileStream;
  Header: AnsiString;
begin
  PK3File := TFileStream.Create(PK3FileName, fmOpenReadWrite or fmShareDenyWrite);
  try
    PK3File.Position := PK3File.Size; // Move to the end of the PK3 file

    // Create and write the header for the file entry
    Header := Format('%-56s', [ExtractFileName(FileName)]) + #0;
    PK3File.WriteBuffer(Header[1], Length(Header));

    // Write the file content
    SourceFile := TFileStream.Create(FileName, fmOpenRead);
    try
      PK3File.CopyFrom(SourceFile, 0);
    finally
      SourceFile.Free;
    end;
  finally
    PK3File.Free;
  end;
end;

procedure DecompressFile(const SourceFile, DestFile: string);
var
  SourceStream: TFileStream;
  DestStream: TFileStream;
  ByteCount: Integer;
  CurrentByte: Byte;
  i: Integer;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead);
  try
    DestStream := TFileStream.Create(DestFile, fmCreate);
    try
      ByteCount := 0;
      CurrentByte := 0;

      while SourceStream.Position < SourceStream.Size do
      begin
        // Read count and byte
        SourceStream.Read(ByteCount, SizeOf(Integer));
        SourceStream.Read(CurrentByte, SizeOf(Byte));

        // Write the byte 'ByteCount' times to the destination stream
        for i := 1 to ByteCount do
          DestStream.Write(CurrentByte, SizeOf(Byte));
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

const
  PK3FilePath = 'example.pk3';
  SoundFolderPath = 'sounds/';
  AssetFolderPath = 'assets/';
  MaxSoundTracks = 10;
  MaxAssets = 10;

var
  PK3File: TFileStream;
  SoundCount: Integer;
  AssetCount: Integer;
  FileName: string;
  SoundLoaded: Integer; // Counter for loaded sound tracks
  AssetLoaded: Integer; // Counter for loaded assets

begin
  // Initialize sound and asset counts
  SoundCount := 0;
  AssetCount := 0;
  SoundLoaded := 0;
  AssetLoaded := 0;

  FileName := ''; // Initializing FileName as an empty string

  // Open the PK3 file
  PK3File := TFileStream.Create(PK3FilePath, fmOpenRead or fmShareDenyNone);
  try
    // Loop through each file in the PK3 archive
    while PK3File.Position < PK3File.Size do
    begin
      // Read the next file name
      SetLength(FileName, 56);
      PK3File.ReadBuffer(FileName[1], 56);

      // Trim null characters from the end of the file name
      FileName := TrimRight(FileName);

      // Check if the file is in the sounds folder and is an OGG file
      if (Pos(SoundFolderPath, FileName) = 1) and (LowerCase(ExtractFileExt(FileName)) = '.ogg') then
      begin
        // Check if the sound count exceeds the limit
        if SoundCount >= MaxSoundTracks then
        begin
          Writeln('Maximum sound tracks reached. Additional tracks will not be loaded.');
          Break;
        end;

        // Decompress the file content
        DecompressFile(PK3FilePath, 'output_sound_' + IntToStr(SoundLoaded) + '.ogg');

        // Load the sound file (do something with it)
        // For demonstration, we just print the filename
        Writeln('Loaded sound: ', FileName);

        // Increment the sound count and loaded sound counter
        Inc(SoundCount);
        Inc(SoundLoaded);
        if SoundLoaded >= MaxSoundTracks then
          Break;
      end
      else if (Pos(AssetFolderPath, FileName) = 1) then
      begin
        // Check if the asset count exceeds the limit
        if AssetCount >= MaxAssets then
        begin
          Writeln('Maximum assets reached. Additional assets will not be loaded.');
          Break;
        end;

        // Decompress the asset content
        DecompressFile(PK3FilePath, 'output_asset_' + IntToStr(AssetLoaded) + '.xyz');

        // Load the asset file (do something with it)
        // For demonstration, we just print the filename
        Writeln('Loaded asset: ', FileName);

        // Increment the asset count and loaded asset counter
        Inc(AssetCount);
        Inc(AssetLoaded);
        if AssetLoaded >= MaxAssets then
          Break;
      end
      else
      begin
        // Skip the file content
        PK3File.Position := PK3File.Position + 32; // Skip the compressed size
        PK3File.Position := PK3File.Position + 4;  // Skip the uncompressed size
      end;
    end;
  finally
    PK3File.Free;
  end;

  // Display the total number of sound tracks and assets loaded
  Writeln('Total sound tracks loaded: ', SoundCount);
  Writeln('Total assets loaded: ', AssetCount);

  // Wait for user input before exiting
  Readln;
end.

