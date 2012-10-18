@ECHO OFF

@REM Clean up .beam files
cd ebin

for %%i in (*.beam) do del %%i

cd..

@REM compile all files

for %%i in (src/*.erl) do erlc -o ebin/ "src/%%i"