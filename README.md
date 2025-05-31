# Manki

Manki is a library and commandline tool for producing bare-bones flashcards 
to be imported in [Anki](https://apps.ankiweb.net/).

This should be useful to anyone set on learning a foreign language using flashcards.
If you are like me, you prefer working with text files rather than excel files, the 
Anki GUI or whatnot. Manki's purpose is to generate flashcards (via Anki) out of a 
text file.

Manki converts text files to a csv format ready for import in the Anki app.
You'll write a lightly marked up text like:

```
Here is a cell/field with @words to be converted to sound@.
---
And this is another cell/field with ~a cloze deletion~1
---
Third cell, with a reference to the audio string in first cell: |>
```

This will be converted to a csv string (or file) marked up for Anki use. 
So any `@blah@` markup disappears, and the insert marker `|>` will be 
replaced by a `[sound:blah.mp3]` string (the Anki's markup for sound files).
The Manki cloze markup `~foo~1` becomes the Anki string `{{c1:foo}}`. 
And three dashes on their own lines delimit what will become the Anki 
fields. In the csv string they will be replaced by commas.

Note: Manki does _not_ produce the sound files for you. It generates
a reference to sound files which can be added to the Anki collection
independently, for instance, using 
[Google Cloud's Text-to-Speech](https://cloud.google.com/text-to-speech/)
or similar services.
A list of audio strings picked up from the input file can be
obtained with the `--audios` flag on the command line or using
the functions provided by the library.

## Manki markup

Manki is more constrained than other light markup languages in the sense that not every stream of characters is a valid Manki document.

* cloze deletion: `~check~1 this ~out~1` converts to the Anki string `{{c1:check}} this {{c1:out}}`
* `*bold*` and `*italic*` convert to html `<b>bold</b>` and `<i>italic</i>` (recognised as such by Anki)
* `@audio string@3` converts to a bare string with no markup (`audio string`) but can be collected and inserted in the same block; if you don't want `audio string` to occur in the output file (yet need the string in order to produce audio) use one or two dashes before the starting marker: `-@audio string`.
* the `3|>` inserts a coindexed audio string, and converts to something like `[[sound:audio_strings]]` (assuming the audio string just above)
* indices (on cloze-deletion markers, audio markers and insert markers) are optional and should be used for cross-referencing.
* two or more dashes on a new line (`\n---\n`) separate one field (csv cell) from another; no start or end delimiters are allowed; so `\n--\ncell one\n--\ncell two\n--\n` is wrong; use instead: `cell one\n--\ncell two`
* blocks in the input file (which will become csv rows, which in turn will become flash cards after import) are separated from one another by two or more newlines. Example: `row one, cell one\n--\nrow two, cell two\n\nrow two, cell one\n--\nrowtwo, cell two`

Note: at the moment the Manki tags _can't_ overlap. For instance, `*_bold italic_*` is invalid.
One exception is the audio marker `@` which can wrap around any other tags `@*bold*, _italic_, etc.@`.
This is particularly useful if you plan to convert cloze-deleted words to a sound file: `@~hidden but pronounceable~@`.

Check out `/test/test_files/example.txt` to get a input file sample using the Manki markup. `example.csv`,
in the same directory, is the output that Manki's commandline tool produces out of the `example.txt` input file.

## Run

You'll find the executable in Releases.

To convert `file.txt` to `file.csv`, run:

```$ manki-exe -f file.txt -o file.csv```

You can then import `file.csv` in Anki. There is a 
[thread on Stack Exchange](https://superuser.com/questions/698902/can-i-create-an-anki-deck-from-a-csv-file)
that explains how to import csv files. 
And the [Anki Manual](https://apps.ankiweb.net/docs/manual.html) provides more detail.

Run `manki --help` for more information about the commandline options.

Note: Manki has not yet been tested on Windows and macOS.

## Development

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) - Haskell build tool
- Git

### Download and Setup

Clone the repository and navigate to the project directory:

```bash
git clone https://github.com/reinasta/manki.git
cd manki
```

### Build

Build the project using Stack:

```bash
# Install dependencies and build the project
stack build

# Build with optimizations (for release)
stack build --optimization
```

### Test

Run the test suite to ensure everything works correctly:

```bash
# Run all tests
stack test

# Run tests with verbose output
stack test --verbose

# Run specific test module
stack test --test-arguments="--match AudioSpec"

# Run specific `it` test block in the ParserSpec module (targeting functionality in the Parser module)
stack test --test-arguments "--match \"/Parser/mixed math and text/parses inline math with escaped dollar signs within math content/\" --seed 800542947"
```

### Development Usage

During development, you can test the application with various inputs:

```bash
# Run the executable directly with Stack
echo "Hello ~world~1" | stack exec manki-exe -- --csv

# Test with a file
echo "Here is @audio text@.\n---\nInsert audio here: |>" > test.txt
stack exec manki-exe -- -f test.txt --csv

# Get JSON output
stack exec manki-exe -- -f test.txt --json

# Extract audio strings
stack exec manki-exe -- -f test.txt --audios

# Get native AST representation
stack exec manki-exe -- -f test.txt --manki
```

### Testing with Example Files

The project includes example files for testing:

```bash
# Test with the provided example
stack exec manki-exe -- -f test/test_files/example.txt --csv

# Compare with expected output
stack exec manki-exe -- -f test/test_files/example.txt --csv > output.csv
diff output.csv test/test_files/example.csv
```

### Quick Testing Examples

Here are some quick examples for testing different markup features:

```bash
# Cloze deletion
echo "This is a ~cloze deletion~1 test." | stack exec manki-exe -- --csv

# Audio markup
echo "@pronunciation text@\n---\nInsert here: |>" | stack exec manki-exe -- --csv

# Bold and italic
echo "*Bold text* and _italic text_." | stack exec manki-exe -- --csv

# Multiple fields
echo "Field 1\n---\nField 2\n---\nField 3" | stack exec manki-exe -- --csv

# Multiple cards (separated by blank lines)
echo "Card 1, Field 1\n---\nCard 1, Field 2\n\nCard 2, Field 1\n---\nCard 2, Field 2" | stack exec manki-exe -- --csv
```

### Clean Build

If you encounter issues, try a clean build:

```bash
stack clean
stack build
```
