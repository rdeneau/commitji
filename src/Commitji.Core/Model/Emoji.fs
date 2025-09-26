namespace Commitji.Core.Model

open Commitji.Core.Helpers

/// <summary>
/// Gitmoji emoji, from https://gitmoji.dev
/// </summary>
type Emoji =
    | AdhesiveBandage //         # 1
    | Airplane //                # 2
    | Alembic //                 # 3
    | Alien //                   # 4
    | Ambulance //               # 5
    | ArrowDown //               # 6
    | ArrowUp //                 # 7
    | ArtistPalette //           # 8
    | Beers //                   # 9
    | Bento //                   # 10
    | Bookmark //                # 11
    | Boom //                    # 12
    | Bricks //                  # 13
    | Broom //                   # 14
    | Bug //                     # 15
    | BuildingConstruction //    # 16
    | Bulb //                    # 17
    | BustsInSilhouette //       # 18
    | CameraFlash //             # 19
    | CardFileBox //             # 20
    | ChartWithUpwardsTrend //   # 21
    | CheckMark //               # 22
    | ChildrenCrossing //        # 23
    | ClosedLockWithKey //       # 24
    | ClownFace //               # 25
    | Coffin //                  # 26
    | Construction //            # 27
    | ConstructionWorker //      # 28
    | Dizzy //                   # 29
    | Egg //                     # 30
    | Fire //                    # 31
    | GlobeWithMeridians //      # 32
    | GoalNet //                 # 33
    | GreenHeart //              # 34
    | Hammer //                  # 35
    | HeavyMinusSign //          # 36
    | HeavyPlusSign //           # 37
    | Iphone //                  # 38
    | Label //                   # 39
    | Lipstick //                # 40
    | Lock //                    # 41
    | LoudSound //               # 42
    | Mag //                     # 43
    | Memo //                    # 44
    | MoneyWithWings //          # 45
    | MonocleFace //             # 46
    | Mute //                    # 47
    | Necktie //                 # 48
    | Package //                 # 49
    | PageFacingUp //            # 50
    | PassportControl //         # 51
    | Pencil //                  # 52
    | Poop //                    # 53
    | Pushpin //                 # 54
    | Recycle //                 # 55
    | Rewind //                  # 56
    | Rocket //                  # 57
    | RotatingLight //           # 58
    | SafetyVest //              # 59
    | Seedling //                # 60
    | SeeNoEvil //               # 61
    | Sparkles //                # 62
    | SpeechBalloon //           # 63
    | Stethoscope //             # 64
    | Tada //                    # 65
    | Technologist //            # 66
    | TestTube //                # 67
    | Thread //                  # 68
    | TriangularFlagOnPost //    # 69
    | TriangularRuler //         # 70
    | Truck //                   # 71
    | TwistedRightwardsArrows // # 72
    | Wastebasket //             # 73
    | Wheelchair //              # 74
    | Wrench //                  # 75
    | Zap //                     # 76

    member this.Props =
        let props char hint codes = {|
            Char = char
            Code = List.head codes
            Codes = codes
            Hint = hint
        |}

        match this with
        | AdhesiveBandage ->
            props "🩹" "Simple fix for a non-critical issue." [ // ↩
                "adhesive_bandage"
            ]
        | Airplane ->
            props "✈️" "Improve offline support." [ // ↩
                "airplane"
            ]
        | Alembic ->
            props "⚗️" "Perform experiments." [ // ↩
                "alembic"
            ]
        | Alien ->
            props "👽️" "Update code due to external API changes." [ // ↩
                "alien"
            ]
        | Ambulance ->
            props "🚑" "Critical hotfix." [ // ↩
                "ambulance"
            ]
        | ArrowDown ->
            props "⬇️" "Downgrade dependencies." [ // ↩
                "arrow_down"
            ]
        | ArrowUp ->
            props "⬆️" "Upgrade dependencies." [ // ↩
                "arrow_up"
            ]
        | ArtistPalette ->
            props "🎨" "Improve structure / format of the code." [ // ↩
                "artist_palette"
            ]
        | Beers ->
            props "🍻" "Write code drunkenly." [ // ↩
                "beers"
            ]
        | Bento ->
            props "🍱" "Add or update assets." [ // ↩
                "bento"
            ]
        | Bookmark ->
            props "🔖" "Release / Version tags." [ // ↩
                "bookmark"
            ]
        | Boom ->
            props "💥" "Introduce breaking changes." [ // ↩
                "boom"
                "collision"
            ]
        | Bricks ->
            props "🧱" "Infrastructure related changes." [ // ↩
                "bricks"
            ]
        | Broom ->
            props "🧹" "Clean up code (remove dead code, auto-format) or files." [ // ↩
                "broom"
                "sweep"
            ]
        | Bug ->
            props "🐛" "Fix a bug." [ // ↩
                "bug"
            ]
        | BuildingConstruction ->
            props "🏗️" "Make architectural changes." [ // ↩
                "building_construction"
            ]
        | Bulb ->
            props "💡" "Add or update comments in source code." [ // ↩
                "bulb"
                "idea"
                "light_bulb"
            ]
        | BustsInSilhouette ->
            props "👥" "Add or update contributor(s)." [ // ↩
                "busts_in_silhouette"
                "silhouette_of_two_people"
                "shadow"
                "users"
            ]
        | CameraFlash ->
            props "📸" "Add or update snapshots." [ // ↩
                "camera_flash"
            ]
        | CardFileBox ->
            props "🗃️" "Perform database related changes." [ // ↩
                "card_file_box"
            ]
        | ChartWithUpwardsTrend ->
            props "📈" "Add or update analytics or track code." [ // ↩
                "chart_with_upwards_trend"
                "chart_increasing"
                "positive_chart"
                "up_pointing_graph"
            ]
        | CheckMark ->
            props "✅" "Add, update, or pass tests." [ // ↩
                "check_mark"
                "green_tick"
                "white_check_mark"
            ]
        | ChildrenCrossing ->
            props "🚸" "Improve user experience / usability." [ // ↩
                "children_crossing"
            ]
        | ClosedLockWithKey ->
            props "🔐" "Add or update secrets." [ // ↩
                "closed_lock_with_key"
            ]
        | ClownFace ->
            props "🤡" "Mock things." [ // ↩
                "clown_face"
            ]
        | Coffin ->
            props "⚰️" "Remove dead code." [ // ↩
                "coffin"
                "casket"
                "funeral"
            ]
        | Construction ->
            props "🚧" "Work in progress (wip), not yet finalized." [ // ↩
                "construction"
                "wip"
            ]
        | ConstructionWorker ->
            props "👷" "Add or update CI build system." [ // ↩
                "construction_worker"
            ]
        | Dizzy ->
            props "💫" "Add or update animations and transitions. #UI" [ // ↩
                "dizzy"
            ]
        | Egg ->
            props "🥚" "Add or update an easter egg." [ // ↩
                "egg"
            ]
        | Fire ->
            props "🔥" "Remove code or files." [ // ↩
                "fire"
                "flame"
            ]
        | GlobeWithMeridians ->
            props "🌐" "Internationalization and localization." [ // ↩
                "globe_with_meridians"
            ]
        | GoalNet ->
            props "🥅" "Catch errors." [ // ↩
                "goal_net"
            ]
        | GreenHeart ->
            props "💚" "Fix CI Build." [ // ↩
                "green_heart"
            ]
        | Hammer ->
            props "🔨" "Add or update development scripts." [ // ↩
                "hammer"
                "claw_hammer"
                "handyman"
                "tool"
            ]
        | HeavyMinusSign ->
            props "➖" "Remove a dependency." [ // ↩
                "heavy_minus_sign"
            ]
        | HeavyPlusSign ->
            props "➕" "Add a dependency." [ // ↩
                "heavy_plus_sign"
            ]
        | Iphone ->
            props "📱" "Work on responsive design. #UI" [ // ↩
                "iphone"
                "mobile_phone"
            ]
        | Label ->
            props "🏷️" "Add or update types." [ // ↩
                "label"
            ]
        | Lipstick ->
            props "💄" "Change the UI visually but not it's behaviour. #style" [ // ↩
                "lipstick"
            ]
        | Lock ->
            props "🔒️" "Fix security or privacy issues." [ // ↩
                "lock"
            ]
        | LoudSound ->
            props "🔊" "Add or update logs." [ // ↩
                "loud_sound"
            ]
        | Mag ->
            props "🔍️" "Improve SEO." [ // ↩
                "mag"
            ]
        | Memo ->
            props "📝" "Add or update documentation." [ // ↩
                "memo"
            ]
        | MoneyWithWings ->
            props "💸" "Add sponsorships or money related infrastructure." [ // ↩
                "money_with_wings"
            ]
        | MonocleFace ->
            props "🧐" "Data exploration/inspection." [ // ↩
                "monocle_face"
            ]
        | Mute ->
            props "🔇" "Remove logs." [ // ↩
                "mute"
            ]
        | Necktie ->
            props "👔" "Add or update business logic." [ // ↩
                "necktie"
            ]
        | Package ->
            props "📦" "Add or update compiled files or packages." [ // ↩
                "package"
            ]
        | PageFacingUp ->
            props "📄" "Add or update license." [ // ↩
                "page_facing_up"
            ]
        | PassportControl ->
            props "🛂" "Work on code related to authorization, roles and permissions." [ // ↩
                "passport_control"
            ]
        | Pencil ->
            props "✏️" "Fix typos." [ // ↩
                "pencil"
            ]
        | Poop ->
            props "💩" "Write bad code that needs to be improved." [ // ↩
                "poop"
                "dirt"
                "shit"
            ]
        | Pushpin ->
            props "📌" "Pin dependencies to specific versions." [ // ↩
                "pushpin"
            ]
        | Recycle ->
            props "♻️" "Refactor code: without changing its behavior." [ // ↩
                "recycle"
                "recycling_symbol"
            ]
        | Rewind ->
            props "⏪" "Revert changes." [ // ↩
                "rewind"
                "fast_reverse_button"
                "left_pointing_double_triangle"
            ]
        | Rocket ->
            props "🚀" "Deploy stuff." [ // ↩
                "rocket"
                "space_shuttle"
            ]
        | RotatingLight ->
            props "🚨" "Fix compiler / linter warnings." [
                "rotating_light"
                "emergency_light"
                "flashing_light"
                "police_car_light"
                "siren"
            ]
        | SafetyVest ->
            props "🦺" "Add or update code related to validation." [ // ↩
                "safety_vest"
            ]
        | Seedling ->
            props "🌱" "Add or update seed files." [ // ↩
                "seedling"
                "spring"
                "sprout"
            ]
        | SeeNoEvil ->
            props "🙈" "Add or update a .gitignore file." [ // ↩
                "see_no_evil"
                "mizaru"
                "monkey_covering_eyes"
            ]
        | Sparkles ->
            props "✨" "Introduces a new feature." [ // ↩
                "sparkles"
                "glitter"
                "shiny"
            ]
        | SpeechBalloon ->
            props "💬" "Add or update text and literals." [ // ↩
                "speech_balloon"
                "chat_bubble"
            ]
        | Stethoscope ->
            props "🩺" "Add or update healthcheck." [ // ↩
                "stethoscope"
            ]
        | Tada ->
            props "🎉" "Begin a project." [ // ↩
                "tada"
                "party_popper"
            ]
        | Technologist ->
            props "🧑‍💻" "Improve developer experience." [ // ↩
                "technologist"
            ]
        | TestTube ->
            props "🧪" "Add a (failing) test." [ // ↩
                "test_tube"
            ]
        | Thread ->
            props "🧵" "Add or update code related to multithreading or concurrency." [ // ↩
                "thread"
            ]
        | TriangularFlagOnPost ->
            props "🚩" "Add, update, or remove feature flags." [ // ↩
                "triangular_flag_on_post"
                "flag_on_pole"
                "red_flag"
            ]
        | TriangularRuler ->
            props "📐" "Format code." [ // ↩
                "triangular_ruler"
                "triangle_ruler"
            ]
        | Truck ->
            props "🚚" "Move or rename resources (e.g.: files, paths, routes)." [ // ↩
                "truck"
                "delivery_truck"
            ]
        | TwistedRightwardsArrows ->
            props "🔀" "Merge branches." [ // ↩
                "twisted_rightwards_arrows"
                "shuffle_tracks_button"
            ]
        | Wastebasket ->
            props "🗑️" "Deprecate code that needs to be cleaned up." [
                "wastebasket"
                "garbage_can"
                "rubbish_bin"
                "trash_can"
                "wastepaper_basket"
            ]
        | Wheelchair ->
            props "♿️" "Improve accessibility." [ // ↩
                "wheelchair"
                "accessible_bathroom"
            ]
        | Wrench ->
            props "🔧" "Add or update configuration files." [ // ↩
                "wrench"
                "spanner"
            ]
        | Zap ->
            props "⚡️" "Improve performance." [ // ↩
                "zap"
                "high_voltage"
                "lightning_bolt"
                "thunderbolt"
            ]

    member this.Char = this.Props.Char
    member this.Code = this.Props.Code
    member this.Codes = this.Props.Codes
    member this.Hint = this.Props.Hint

[<RequireQualifiedAccess>]
module Emoji =
    let All = Reflection.getEnumLikeUnionCases<Emoji> ()