@echo off
cd "G:/My Drive/My_Finances/My Business/Southern_Travels/customer_quote" 

REM Check if the folder is already a Git repository
if not exist .git (
    git init
    echo Initialized Git repository.
) else (
    echo Git repository already exists.
)

REM Check if the remote origin is already set
git remote | find "origin" >nul
if errorlevel 1 (
    git remote add origin https://github.com/r4miao/southern_tour.git
    echo Remote origin added.
) else (
    echo Remote origin already exists.
)

REM Make an initial commit if none exists
git add .
git commit -m "Initial commit" 2>nul || echo Commit already exists.

REM Ensure the branch is named 'master'
git branch -M master

REM Pull and push
git pull origin master --allow-unrelated-histories
git push origin master
pause
