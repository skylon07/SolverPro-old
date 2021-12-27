# SolverPro
SolverPro is a simple command-line application that provides tools for organizing and solving multivariate systems of equations. Tired of subscripting variables in physics problems to remember which force goes where? Objects can help with that! What about finding symbolic (or even numerical) solutions from a system of equations? That's a score for equivelance relations! SolverPro aims to be the ideal math-help, whether it be for tutoring, homework (don't cheat!), job/project related calculations, or even  just for messing around.

## Installation
Installation should be as easy as a download and a click (maybe a blink too, if you include looking for the right OS). If you run into any issues, please [log them here](#logging-issues).

### Mac
Latest Mac version (installer): <a href="https://raw.githubusercontent.com/skylon07/SolverPro/main/dist/latest/SolverPro.pkg" download>Download</a>

Latest Mac version (distributable): <a href="https://raw.githubusercontent.com/skylon07/SolverPro/main/dist/latest/SolverPro.dmg" download>Download</a>

### Windows
Latest Windows version (installer): <a href="https://raw.githubusercontent.com/skylon07/SolverPro/main/dist/latest/SolverPro_install.exe" download>Download</a>

Latest Windows version (distributable): <a href="https://raw.githubusercontent.com/skylon07/SolverPro/main/dist/latest/SolverPro.exe" download>Download</a>

### Manual Compilation
If trying the above doesn't work (or you just want to compile the program yourself), first [download the repository](https://github.com/skylon07/SolverPro/archive/refs/heads/main.zip). Running "main.py" (`cd path/to/SolverPro-main/ && python3 main.py`) will do the trick, however you can also use `pyinstaller` to create a standalone application for your specific computer configuration. To do this, either [install pyinstaller yourself](https://www.pyinstaller.org/) or use `pipenv install` (cd-ed in SolverPro-main) and run `pyinstaller --onefile main.py`. This will generate the bundled application in a "dist" folder.

## Help
There are many files provided to aid in understanding SolverPro and how it works. All of these files are located in a folder named "docs". If you want to:

* See a list of features, open "features.txt"
* See some examples, especially for design decisions, open "examples.txt"
* Find why you're getting an error, open "why am I getting this error.txt"

And if you're feeling *really* crazy, you can also explore the "dev-notes" folder too.

If you have any other questions, feel free to [post an issue here](#logging-issues). Who knows, it might just end up in the docs!

## Logging issues
If you have a problem with SolverPro, a bug report, or even a question, [this is where you should do it](https://github.com/skylon07/SolverPro/issues/new). However, before posting, please make sure your issue isn't already listed below (and if you've done this kind of thing before, please [search current issues](https://github.com/skylon07/SolverPro/issues?q=is%3Aissue) too).

### Known issues
* Systems of equations don't give numeric values

	```
	|> a - b = 2
	|> a + b = 4
	|> a
		(Does not print 3)
	|> b
		(Does not print 1)
	```
