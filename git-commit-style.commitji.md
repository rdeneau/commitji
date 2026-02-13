# Git Commit Style - Commitji

## Description

[Commitji](https://github.com/rdeneau/commitji) is a dotnet tool to facilitate writing conventional commit.

The *Commitji* git commit style is a variant of [Conventional Commits](https://www.conventionalcommits.org):

- Keep the template (see below)
- Adjust the commit message prefixes, keeping the main ones (`feat`, `fix`) to remain compatible with [semantic release](https://semantic-release.gitbook.io/semantic-release).
- Add an emoji between the prefix and the description. The emoji list is taken from [Gitmoji](https://gitmoji.dev/), with minor adjustments.

## Template

```txt
{Prefix}: {Emoji} Short description of the change (max 50 characters)

- Concise bullet point 1 describing the changes
- Concise bullet point 2 describing the changes
- Concise bullet point 3 describing the changes
...
```

## Emoji list

The prefix drives which emojis is available. Here a list of suggested emojis and their related prefixes:

`{Prefix}: {Emoji}` follows the [Commitji](https://github.com/rdeneau/commitji) standard:

| Emoji | Codes                                                | Purpose                                                       | Prefixes                    |
| ----- | ---------------------------------------------------- | ------------------------------------------------------------- | --------------------------- |
| 🩹    | `adhesive_bandage`                                   | Simple fix for a non-critical issue.                          | `fix`                       |
| ✈️    | `airplane`                                           | Improve offline support.                                      | `feat`                      |
| ⚗️    | `alembic`                                            | Perform experiments.                                          | `chore`, `feat`, `wip`      |
| 👽️    | `alien`                                              | Update code due to external API changes.                      | `chore`                     |
| 🚑    | `ambulance`                                          | Critical hotfix.                                              | `fix`                       |
| ⬇️    | `arrow_down`                                         | Downgrade dependencies.                                       | `chore`                     |
| ⬆️    | `arrow_up`                                           | Upgrade dependencies.                                         | `chore`                     |
| 🎨    | `artist_palette`                                     | Improve structure / format of the code.                       | `refactor`, `tidy`          |
| 🍻    | `beers`                                              | Write code drunkenly.                                         | `wip`                       |
| 🍱    | `bento`                                              | Add or update assets.                                         | `chore`                     |
| 🔖    | `bookmark`                                           | Release / Version tags.                                       | `chore`                     |
| 💥    | `boom`, `collision`                                  | Introduce breaking changes.                                   | `feat`, `fix`               |
| 🧱    | `bricks`                                             | Infrastructure related changes.                               | `chore`, `feat`             |
| 🧹    | `broom`, `sweep`                                     | Clean up code (remove dead code, auto-format) or files.       | `chore`, `refactor`, `tidy` |
| 🐛    | `bug`                                                | Fix a bug.                                                    | `fix`                       |
| 🏗️    | `building_construction`                              | Make architectural changes.                                   | `refactor`                  |
| 💡    | `bulb`, `idea`, `light_bulb`                         | Add or update comments in source code.                        | `docs`                      |
| 👥    | `busts_in_silhouette`, `users`                       | Add or update contributor(s).                                 | `docs`                      |
| 📸    | `camera_flash`                                       | Add or update snapshots.                                      | `chore`, `docs`, `test`     |
| 🗃️    | `card_file_box`                                      | Perform database related changes.                             | `chore`, `feat`             |
| 📈    | `chart_increasing`,`up_pointing_graph`               | Add or update analytics or track code.                        | `feat`, `fix`               |
| ✅    | `check_mark`, `green_tick`                           | Add, update, or pass tests.                                   | `test`                      |
| 🚸    | `children_crossing`                                  | Improve user experience / usability.                          | `feat`                      |
| 🔐    | `closed_lock_with_key`                               | Add or update secrets.                                        | `chore`                     |
| 🤡    | `clown_face`                                         | Mock things.                                                  | `test`                      |
| ⚰️    | `coffin`, `casket`, `funeral`                        | Remove dead code.                                             | `refactor`                  |
| 🚧    | `construction`, `wip`                                | Work in progress (wip), not yet finalized.                    | `feat`, `wip`               |
| 👷    | `construction_worker`                                | Add or update CI build system.                                | `chore`                     |
| 💫    | `dizzy`                                              | Add or update animations and transitions. #UI                 | `feat`, `fix`               |
| 🥚    | `egg`                                                | Add or update an easter egg.                                  | `feat`                      |
| 🔥    | `fire`, `flame`                                      | Remove code or files.                                         | `refactor`                  |
| 🌐    | `globe_with_meridians`                               | Internationalization and localization.                        | `feat`                      |
| 🥅    | `goal_net`                                           | Catch errors.                                                 | `feat`                      |
| 💚    | `green_heart`                                        | Fix CI Build.                                                 | `chore`                     |
| 🔨    | `hammer`, `claw_hammer`, `tool`                      | Add or update development scripts.                            | `chore`                     |
| ➖    | `heavy_minus_sign`                                   | Remove a dependency.                                          | `chore`                     |
| ➕    | `heavy_plus_sign`                                    | Add a dependency.                                             | `chore`                     |
| 📱    | `iphone`, `mobile_phone`                             | Work on responsive design. #UI                                | `feat`, `fix`               |
| 🏷️    | `label`                                              | Add or update types.                                          | `feat`                      |
| 💄    | `lipstick`                                           | Change the UI visually but not it's behaviour. #style         | `feat`, `fix`               |
| 🔒️    | `lock`                                               | Fix security or privacy issues.                               | `fix`                       |
| 🔊    | `loud_sound`                                         | Add or update logs.                                           | `chore`                     |
| 🔍️    | `mag`                                                | Improve SEO.                                                  | `chore`                     |
| 📝    | `memo`                                               | Add or update documentation.                                  | `docs`                      |
| 💸    | `money_with_wings`                                   | Add sponsorships or money related infrastructure.             | `docs`                      |
| 🧐    | `monocle_face`                                       | Data exploration/inspection.                                  | `chore`, `wip`              |
| 🔇    | `mute`                                               | Remove logs.                                                  | `chore`                     |
| 👔    | `necktie`                                            | Add or update business logic.                                 | `feat`                      |
| 📦    | `package`                                            | Add or update compiled files or packages.                     | `chore`                     |
| 📄    | `page_facing_up`                                     | Add or update license.                                        | `docs`                      |
| 🛂    | `passport_control`                                   | Work on code related to authorization, roles and permissions. | `feat`                      |
| ✏️    | `pencil`                                             | Fix typos.                                                    | `chore`                     |
| 💩    | `poop`, `dirt`, `shit`                               | Write bad code that needs to be improved.                     | `feat`, `wip`               |
| 📌    | `pushpin`                                            | Pin dependencies to specific versions.                        | `chore`                     |
| ♻️    | `recycle`, `recycling_symbol`                        | Refactor code: without changing its behavior.                 | `refactor`                  |
| ⏪    | `rewind`, `fast_reverse_button`                      | Revert changes.                                               | `chore`, `revert`           |
| 🚀    | `rocket`, `space_shuttle`                            | Deploy stuff.                                                 | `chore`                     |
| 🚨    | `rotating_light`, `emergency_light`                  | Fix compiler / linter warnings.                               | `fix`                       |
| 🦺    | `safety_vest`                                        | Add or update code related to validation.                     | `feat`                      |
| 🌱    | `seedling`, `spring`, `sprout`                       | Add or update seed files.                                     | `chore`, `feat`, `test`     |
| 🙈    | `see_no_evil`, `monkey_covering_eyes`                | Add or update a .gitignore file.                              | `chore`                     |
| ✨    | `sparkles`, `glitter`, `shiny`                       | Introduces a new feature.                                     | `feat`                      |
| 💬    | `speech_balloon`, `chat_bubble`                      | Add or update text and literals.                              | `chore`                     |
| 🩺    | `stethoscope`                                        | Add or update healthcheck.                                    | `chore`                     |
| 🎉    | `tada`, `party_popper`                               | Begin a project.                                              | `wip`                       |
| 🧑‍💻    | `technologist`                                       | Improve developer experience.                                 | `refactor`                  |
| 🧪    | `test_tube`                                          | Add a (failing) test.                                         | `test`                      |
| 🧵    | `thread`                                             | Add or update code related to multithreading or concurrency.  | `perf`                      |
| 🚩    | `triangular_flag_on_post`, `red_flag`                | Add, update, or remove feature flags.                         | `chore`, `feat`             |
| 📐    | `triangular_ruler`, `triangle_ruler`                 | Format code.                                                  | `tidy`                      |
| 🚚    | `truck`, `delivery_truck`                            | Move or rename resources (e.g.: files, paths, routes).        | `refactor`                  |
| 🔀    | `twisted_rightwards_arrows`, `shuffle_tracks_button` | Merge branches.                                               | `chore`                     |
| 🗑️    | `wastebasket`, `trash_can`                           | Deprecate code that needs to be cleaned up.                   | `refactor`, `wip`           |
| ♿️    | `wheelchair`                                         | Improve accessibility.                                        | `feat`                      |
| 🔧    | `wrench`, `spanner`                                  | Add or update configuration files.                            | `chore`                     |
| ⚡️    | `zap`, `thunderbolt`                                 | Improve performance.                                          | `perf`                      |
