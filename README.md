# TextureMaterialFinder

TextureMaterialFinder is a tool that searches the P:\DZ\Structures directory for vanilla textures and materials. It displays image previews and any linked textures/materials.

## Current Features
- Searches P:\DZ\Structures for textures and materials.
- Displays image previews and linked textures/materials.

## Future Improvements
- Enhance search and filter capabilities.
- Expand search to include the entire P:\ drive.

## Requirements
- Convert PAA files to PNG format using Pal2PacE. A batch script is provided below.

## Batch Script for PAA to PNG Conversion

To convert your PAA files to PNG format, use the following batch script:

```batch
@echo off
setlocal enabledelayedexpansion

REM Set the path to Pal2PacE executable
set "PAL2PACE_PATH=C:\Path\To\Pal2PacE.exe"

REM Set the current directory
set "CURRENT_DIR=%cd%"

REM Initialize the PAA file counter
set "PAA_COUNT=0"

REM Function to process each PAA file
for /r "%CURRENT_DIR%" %%f in (*.paa) do (
    set "input_file=%%f"
    set "output_file=%%~dpnf.png"
    
    REM Increment the PAA file counter
    set /a PAA_COUNT+=1
    
    REM Convert PAA to PNG using Pal2PacE
    echo Converting "!input_file!" to "!output_file!"
    "%PAL2PACE_PATH%" "!input_file!" "!output_file!"
)

REM Display the total number of PAA files found and converted
echo Conversion complete! Total PAA files converted: %PAA_COUNT%
pause
