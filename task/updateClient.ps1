$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Compile Client Code And Upload Firebase Server";
Set-Location -Path ./client/source;

Write-Output "Compile Elm ...";
$Host.UI.RawUI.ForegroundColor = "Gray";
elm.exe make ./elm-source/Main.elm --output ../beforeMinifiy.js --optimize;
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Compile Elm OK";

Write-Output "Call Compile ...";
$Host.UI.RawUI.ForegroundColor = "Gray";
tsc.cmd;
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Call Compile OK";

Set-Location -Path ./serviceworker;
Write-Output "ServiceWorker Compile ...";
$Host.UI.RawUI.ForegroundColor = "Gray";
tsc.cmd;
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "ServiceWorker Compile OK";

Set-Location -Path ../../;
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Minify JavaScript ...";
$Host.UI.RawUI.ForegroundColor = "Gray";
uglifyjs.cmd ./beforeMinifiy.js -o distribution/main.js;
Remove-Item ./beforeMinifiy.js;
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Minify JavaScript OK";

Write-Output "Copy robots.txt ...";
$Host.UI.RawUI.ForegroundColor = "Gray";
Copy-Item -Path ./source/robots.txt -Destination ./distribution/robots.txt
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Copy robots.txt OK";

Write-Output "Copy assets ...";
$Host.UI.RawUI.ForegroundColor = "Gray";
Copy-Item -Path ./source/assets/ -Destination ./distribution/ -Recurse -Force
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Copy assets OK";

Write-Output "Upload to Firebase ...";
$Host.UI.RawUI.ForegroundColor = "Gray";
firebase.ps1 deploy --project definy-lang --only hosting;
$Host.UI.RawUI.ForegroundColor = "Yellow";
Write-Output "Upload to Firebase OK";
Write-Output "Complete!";
