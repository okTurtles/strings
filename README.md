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

## Astro

The extractor also scans `.astro` files ([Astro framework](https://astro.build)). It extracts:

- The text inside `<I18n>...</I18n>` (or `<i18n>...</i18n>`) components, used as the translation key — including components nested inside JSX expressions such as `{cond && <I18n is:raw>...</I18n>}` or `{items.map(() => <I18n is:raw>...</I18n>)}`.
- `L('...')` calls found in the frontmatter (`--- ... ---`), in `{...}` expressions (including JSX), in `args={...}`, and in `<script>` blocks. Astro code is always treated as TypeScript (TSX).

Because Astro compiles `{...}` in element children as JavaScript expressions, add the [`is:raw`](https://docs.astro.build/en/reference/directives-reference/#israw) directive whenever the text contains `{placeholder}` syntax. With `is:raw`, the component receives the literal text — the same string as in `.vue` files, so the app and the website share one set of translations:

```astro
---
import I18n from '../components/I18n.astro'
const title = L('Create a group')
---

<h1>{L('Welcome to Group Income')}</h1>

<I18n is:raw>Logout</I18n>

<I18n is:raw args={{ name: groupName, ...LTags("strong") }}>
  Yes, I want to {strong_}delete {name} permanently{_strong}.
</I18n>
```

If an `<I18n>` element contains `{placeholders}` but is missing `is:raw`, the extractor prints a warning (the string is still extracted), because Astro would otherwise evaluate the placeholders as expressions.

A minimal `I18n.astro` component looks like this (note: the runtime lookup function is deliberately not named `L` — `L` is reserved for string literals, which is what the extractor scans for, and it is never called with a variable):

```astro
---
import { translate } from '../utils/translations'
const { args } = Astro.props
const text = await Astro.slots.render('default')
---

<Fragment set:html={translate(text.trim(), args)} />
```

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
