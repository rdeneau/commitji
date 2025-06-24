# Commitji

![Commitji](Commitji.png)

> Kind of lightweight [Commitizen](https://commitizen.github.io/cz-cli/) combined with [Gitmoji](https://gitmoji.dev/)
> to facilitate writing conventional commit messages closed to [Angular Commit Message Conventions](https://github.com/angular/angular/blob/main/contributing-docs/commit-message-guidelines.md)
> supported by [semantic release](https://semantic-release.gitbook.io/semantic-release).

## Usage

### Steps

The tool display a series of questions, mainly to allow you to choose:

- a prefix (`feat`, `fix`...) and an emoji amongst a short list of relevant emojis (e.g. `sparkles` ✨ for `feat`)
- or an emoji (`lipstick` 💄) and a prefix amongst a short list of relevant prefixes (e.g. `feat` or `fix`)

The default mode is to start with the prefix selection. You can switch to emoji selection by pressing `[:]`.

When you have both the prefix and the emoji, you can indicate if it's a breaking change.

Afterward, the corresponding semantic version change is indicated (Major, Minor, Patch, None),
and the commit message template is displayed. You can copy it to the clipboard by pressing `[Enter]`.

### Behaviours

The selection is made in various ways:

- You can navigate through the list of available choices, using the arrow keys `[↓]/[↑]` to change the selected choice.
- And/or you can start typing the number of the choice to select (when numbers are displayed: for prefixes and emojis).
- And/or you can start typing some characters: the list of choices is filtered to match the input.
  - If a single choice matches the input, the step is auto-completed, the input is cleared, and you go to the next step.
  - If multiple choices match:
    - The portions that match the input are highlighted.
    - You can complete the step by pressing `[Enter]`.

The search is case-insensitive, and has 2 modes:

- Quick search: the input is matched against the beginning of the choice code.
- Full-text search: the input is matched against the whole choice text.
- You can switch to full-text search by pressing `[Alt]+[F]` (or `[Ctrl]+[F]` when it's not intercepted by the host).
- You can exit the full-text search by pressing `[Escape]`.

At any time, you can press:

- `[Alt]+[Z]` or `[Backspace]` to undo the last action (if any).
- `[Ctrl]+[C]` to close the tool.

💡 These keystrokes are indicated in the hints panel that is updated to match the input and the selection.

## Installation

🚧 TODO

## Additional resources 🔗

- [Emojipedia](https://emojipedia.org/)