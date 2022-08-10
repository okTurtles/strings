# String Extractor

The String Extractor is a program that runs on your computer to help manage translations.

## Help translate a language
You can contribute by directly making changes to the `.strings` file for the language of your choice. Those files are located in the `strings/` directory. If the language of your choice is not present yet, follow the instructions in [Add a language](#Add-a-language).

Feel free to make edits directly on Github!

Example (`french.strings`):

**before**
```
/* MISSING TRANSLATION - frontend/views/pages/Home.vue */
"Logout" = "Logout";

/* MISSING TRANSLATION - frontend/views/containers/contributions/ContributionsWidget.vue */
"You and {count} other members are contributing." = "You and {count} other members are contributing.";
```

**after**
```
/* frontend/views/pages/Home.vue */
"Logout" = "Déconnexion";

/* frontend/views/containers/contributions/ContributionsWidget.vue */
"You and {count} other members are contributing." = "Vous, ainsi que {count} autres membres, contribuez.";
```
Notes:
- The original English text is on the left and must not be changed.
- Removing `MISSING TRANSLATION` is not necessary.
- Text is enclosed within double quotes.
- Do not modify text enclosed within `{` and `}`, but you can move it.
- To represent the double quote character itself (`"`), use `\"`.
- To represent the backslash character itself (`\`), use `\\`.

## Add a language

1. Make a copy of `english.strings` and rename it to e.g. `french.strings`
2. Start translating `french.strings`!

## Developers

Download the latest version of the String Extractor [here](https://github.com/okTurtles/strings/releases).

Place it at the root of the repository.

Simply run it before submitting a Pull Request!
```sh
# MacOS
./strings.mac src/
# getting a 'code cannot be verified' error? Run this:
# xattr -d com.apple.quarantine strings.mac

# Linux
tar xzvf strings.linux.tar.gz # unzip
./strings.linux src/
```
- **MacOS**: Monterey or newer
- **Linux** and **WSL**
  - Ubuntu 20.04 or newer
  - Debian 11 (Bullseye) or newer
  - You'll need to install your Linux distribution's `musl` package: `apt-get install musl`
  - Make sure the `lib` directory stays in the same directory as `strings.linux`
