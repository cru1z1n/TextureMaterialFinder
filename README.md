# TextureMaterialFinder
searches pdrive for for vanilla textures/materials displays image preview and any linked textures/materials

currently only searches P:\DZ\Structures
once i improve the search and filter ill add the rest of the pdrive


your gonna need to also convert your paa's to png i created a bat script todo this using paltopacE

https://github.com/DayZ-RF/DayZ-Modding-Features/tree/main/Paa2Png

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
