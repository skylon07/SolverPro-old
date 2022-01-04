# SolverPro
SolverPro is a simple command-line application that provides tools for organizing and solving multivariate systems of equations. Tired of subscripting variables in physics problems to remember which force goes where? Objects can help with that! What about finding symbolic (or even numerical) solutions from a system of equations? That's a score for equivelance relations! SolverPro aims to be the ideal math-help, whether it be for tutoring, homework (don't cheat!), job/project related calculations, or even  just for messing around.

## Installation
Installation should be as easy as a download and a click (maybe a blink too, if you include looking for the right OS). If you run into any issues, please [log them here](#logging-issues).

### Mac
Latest: Version 0.1.0 (downloader): [Download](https://github.com/skylon07/SolverPro/releases/download/v0.1.0/SolverPro_downloader_mac.tgz)

Latest: Version 0.1.0 (app): [Download](https://github.com/skylon07/SolverPro/releases/download/v0.1.0/SolverPro.tgz)

**IF YOU CAN'T RUN THE DOWNLOADER**, try right-clicking the file and clicking the "Open" option, then the "Open" button. (If you're worried about viruses, you can open this script in a text editor by adding '.txt' to the end of the filename, to verify it isn't evil. ;)

If you would rather download the app directly, use the "app" download link instead. Something to note, however; Because Apple makes it a headache to sign apps, you will have to run `xattr -d com.apple.quarantine path/to/SolverPro.app` to "fix" the app. Too technical? There's a "fixErrors" script that will run this command for you. Just make sure **not to move the files after downloading**, and run it with the right-click-trick mentioned above. (Again, you can verify that's all the script does by opening it in any text editor.)

### Windows
Latest: Version 0.1.0 (exe): [Download](https://github.com/skylon07/SolverPro/releases/download/v0.1.0/SolverPro.exe)

**IF YOU CAN'T RUN THE EXECUTABLE**, try finding a "Run Anyway" button.

- For Windows 10, this is shown on the bottom after you click "More info"

### Manual Compilation
Warning: This method is pretty in-depth and not for the faint of heart. If you're sure you want to do this, then first download the source code for the latest stable version:

- [Download (zip)](https://github.com/skylon07/SolverPro/archive/refs/tags/v0.1.0.zip)
- [Download (tar)](https://github.com/skylon07/SolverPro/archive/refs/tags/v0.1.0.tar.gz)

After that, running "main.py" (`cd path/to/SolverPro-main/ && python3 main.py`) will do the trick (make sure you have python v3 installed). If you want to build your own installation, however, you can use `pyinstaller` to create a standalone application for your specific computer configuration. To do this, either [install pyinstaller yourself](https://www.pyinstaller.org/) or use `pipenv install` (make sure to `cd` to SolverPro-main) and run `pyinstaller --cF main.py`. This will generate the bundled application in a "dist" folder.

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
* Making relations overrides defined variables (fixed; will be in v0.1.1)

	```
	|> a := 4
	|> a + b = 6
	|> b
		6 - a
		(Expected 2)
	```

* Systems of equations don't give numeric values

	```
	|> a - b = 2
	|> a + b = 4
	|> a
		(Does not print 3)
	|> b
		(Does not print 1)
	```
