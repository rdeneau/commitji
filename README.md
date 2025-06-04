# Commitji

User interface to facilitate the creation of conventional commit messages, a kind of lightweight 
[Commitizen](https://commitizen.github.io/cz-cli/) UI combined with [Gitmoji](https://gitmoji.dev/).

The idea is to allow the user to choose:

- a prefix (`feat`, `fix`...) and an emoji amongst a short list of relevant emojis (e.g. `:sparkles:` for `feat`)
- or an emoji (`:lipstick:` 💄) and a prefix amongst a short list of relevant prefixes (e.g. `feat` or `fix`)

The selection is made in a simple text zone, where the user starts typing the prefix (`fe`) or an emoji code (`:a`).

Underneath, a notification area gives hints about what the user can type:

- When the text zone is empty: the list of available prefixes with their description,
  as well as the character `:` to trigger to the selection of the emoji, and emojis.
- In emoji input mode, the emoji list shrinks to match the input.

As soon as the input matches a single prefix or emoji, it is auto-completed in a label below the text box,
the input is cleared, and the mode switches from prefix to emoji, or vice versa.

When you have both the prefix and the emoji, you can validate this combination by pressing `[Enter]`.
The corresponding text is then sent to the application behind which has the focus, for instance an IDE or a console.

At any time, you can press:

- `[Escape]` to close the UI,
- `[Backspace]` to undo the last character inputted, or, \
  if the input is already empty, to clear the eventual last selected prefix or emoji.