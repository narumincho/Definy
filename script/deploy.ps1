$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Compile Hosting Files And Upload Firebase Server";

Write-Output "Delete Dist Folder";
Remove-Item .\distribution -Recurse;

$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Compile Elm And Minify ...";
$Host.UI.RawUI.ForegroundColor = "Gray";

.\node_modules\.bin\parcel.ps1 build .\source\call\call.ts --out-dir distribution --out-file main.js

.\node_modules\.bin\tsc.ps1 --project .\source\serviceworker\tsconfig.json

Copy-Item .\source\assets .\distribution -Recurse

Write-Output "Upload to Firebase ...";
$Host.UI.RawUI.ForegroundColor = "Gray";
firebase.ps1 deploy --project definy-lang;
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Upload to Firebase OK";
Write-Output "Complete!";