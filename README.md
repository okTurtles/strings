# The Strings Translation Utility

The `strings` utility extracts user-facing strings from source files to make it easy to both translate and dislay them.

Unlike some i18n translation software, strings allows developers to write their strings directly in-place instead of having to manage some separate list of special "keys" to then reference wherever the string is supposed to show up. With strings, you just write your software like normal, and use special markers to indicate that this is a localized string. strings will then extract this string and place it into localization files for easy localization. You don't need special software, just Github and an LLM is all you need to translate your entire app.

It works together with two client-side markers that your app defines: the **`L()` function** and the **`<i18n>`/`<I18n>` tag**. These play a double role:

1. **At extraction time**, `strings` scans your source files (`.js`, `.ts`, `.html`, `.vue`, `.pug`, `.astro`) for string *literals* wrapped by `L('...')` or enclosed in `<i18n>...</i18n>` tags, and collects them into `strings/english.strings` and `strings/english.json` (English currently acts as the default language). Only literals are extracted — variables and other expressions are ignored.
2. **At runtime**, those same markers act as *lookup functions*: the app calls `L('Logout')` (or renders `<i18n>Logout</i18n>`) to look up the English text in the translation mappings for the user's language and display e.g. `"Déconnexion"`.

The workflow:

1. Developers wrap user-facing strings in `L('...')` or `<i18n>` tags.
2. Running `strings` generates/updates `strings/english.strings` (and `.json`).
3. `english.strings` is copied to e.g. `french.strings`, and translators update the right-hand side of each mapping.
4. At runtime, `L()` / `<i18n>` use those mappings to retrieve and display the translated string.

> **Note:** An npm library providing the client-side `L()` function and `i18n`/`I18n` components will be published soon. Until then, see [group-income](https://github.com/okTurtles/group-income) for a real-world implementation.

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

## Usage Examples

`strings` extracts string literals labeled by `L()` and `i18n`/`I18n` tags. Here is what that looks like in each supported file type.

### JavaScript / TypeScript

`L('...')` calls are extracted from `.js` and `.ts` files:

```js
const message = L('Welcome back!')
```

```
/* app.js */
"Welcome back!" = "Welcome back!";
```

### Vue

In `.vue` files, `<i18n>` tag contents are extracted from the template (HTML or Pug), and `L('...')` calls from the script block:

```vue
<template lang="html">
  <button>
    <i18n>Logout</i18n>
  </button>
</template>

<script>
export default {
  computed: {
    greeting () {
      return L('Good morning, {name}!', { name: this.name })
    }
  }
}
</script>
```

With a Pug template:

```vue
<template lang="pug">
p.notice
  i18n You and {count} other members are contributing.
</template>
```

```
/* Home.vue */
"Good morning, {name}!" = "Good morning, {name}!";

/* Home.vue */
"Logout" = "Logout";

/* Widget.vue */
"You and {count} other members are contributing." = "You and {count} other members are contributing.";
```

Note how `{name}` and `{count}` placeholders are part of the extracted string: the translator moves them around freely, and the runtime `L()` / `i18n` implementation substitutes the values.

Plain `.html` and `.pug` files are scanned the same way as the corresponding Vue template languages.

### Astro

The extractor also scans `.astro` files ([Astro framework](https://astro.build)). It extracts:

- The text inside `<I18n>...</I18n>` (or `<i18n>...</i18n>`) components, used as the translation key — including components nested inside JSX expressions such as `{cond && <I18n is:raw>...</I18n>}` or `{items.map(() => <I18n is:raw>...</I18n>)}`.
- `L('...')` calls found in the frontmatter (`--- ... ---`), in `{...}` expressions (including JSX), in `args={...}`, and in `<script>` blocks. Astro code is always treated as TypeScript (TSX).

Because Astro compiles `{...}` in element children as JavaScript expressions, add the [`is:raw`](https://docs.astro.build/en/reference/directives-reference/#israw) directive whenever the text contains `{placeholder}` syntax. With `is:raw`, the component receives the literal text — the same string as in `.vue` files, so the app and the website share one set of translations:

```astro
---
import { I18n, L, LTags } from '<future-strings-package>'
const title = L('Create a group')
---

<title>{title}</title>

<I18n tag="h1">Welcome to Group Income</I18n>

<I18n is:raw>Logout</I18n>

<I18n is:raw args={{ name: groupName, ...LTags("strong") }}>
  Yes, I want to {strong_}delete {name} permanently{_strong}.
</I18n>
```

If an `<I18n>` element contains `{placeholders}` but is missing `is:raw`, the extractor prints a warning (the string is still extracted), because Astro would otherwise evaluate the placeholders as expressions.

Running the extractor over the example above adds these entries to `strings/english.strings`:

```
/* pages/example.astro */
"Create a group" = "Create a group";

/* pages/example.astro */
"Logout" = "Logout";

/* pages/example.astro */
"Welcome to Group Income" = "Welcome to Group Income";

/* pages/example.astro */
"Yes, I want to {strong_}delete {name} permanently{_strong}." = "Yes, I want to {strong_}delete {name} permanently{_strong}.";
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
./strings src/
```
- **MacOS**: Monterey or newer
- **Linux** and **WSL**
  - Ubuntu 20.04 or newer
  - Debian 11 (Bullseye) or newer
  - You'll need to install your Linux distribution's `musl` package: `apt-get install musl`
  - Make sure the `lib` directory stays in the same directory as `strings.linux`
