:loop
@cls
if exist bin\main.exe del /F bin\main.exe
@gprbuild main.gpr -p
@if exist bin\main.exe (
  cd bin
  main.exe Replace 3.0 BN2.csv
  cd ..
) else (
  echo "No main.exe try again?"
)

@pause
goto loop
