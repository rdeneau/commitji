namespace Commitji.Core.Model

open Commitji.Core

/// <summary>
/// Gitmoji emoji, from https://gitmoji.dev
/// </summary>
type Emoji =
    | AdhesiveBandage
    | Airplane
    | Alembic
    | Alien
    | Ambulance
    | ArrowDown
    | ArrowUp
    | Art
    | Beers
    | Bento
    | Bookmark
    | Boom
    | Bricks
    | Bug
    | BuildingConstruction
    | Bulb
    | BustsInSilhouette
    | CameraFlash
    | CardFileBox
    | ChartWithUpwardsTrend
    | ChildrenCrossing
    | ClosedLockWithKey
    | ClownFace
    | Coffin
    | Construction
    | ConstructionWorker
    | Dizzy
    | Egg
    | Fire
    | GlobeWithMeridians
    | GoalNet
    | GreenHeart
    | Hammer
    | HeavyMinusSign
    | HeavyPlusSign
    | Iphone
    | Label
    | Lipstick
    | Lock
    | LoudSound
    | Mag
    | Memo
    | MoneyWithWings
    | MonocleFace
    | Mute
    | Necktie
    | Package
    | PageFacingUp
    | PassportControl
    | Pencil2
    | Poop
    | Pushpin
    | Recycle
    | Rewind
    | Rocket
    | RotatingLight
    | SafetyVest
    | Seedling
    | SeeNoEvil
    | Sparkles
    | SpeechBalloon
    | Stethoscope
    | Tada
    | Technologist
    | TestTube
    | Thread
    | TriangularFlagOnPost
    | Truck
    | TwistedRightwardsArrows
    | Wastebasket
    | Wheelchair
    | WhiteCheckMark
    | Wrench
    | Zap

[<RequireQualifiedAccess>]
module Emoji =
    let All = Helpers.Reflection.getEnumLikeUnionCases<Emoji> ()

    let props emoji =
        let char, code, hint =
            match emoji with
            | AdhesiveBandage -> "🩹", "adhesive_bandage", "Simple fix for a non-critical issue."
            | Airplane -> "✈️", "airplane", "Improve offline support."
            | Alembic -> "⚗️", "alembic", "Perform experiments."
            | Alien -> "👽️", "alien", "Update code due to external API changes."
            | Ambulance -> "🚑", "ambulance", "Critical hotfix."
            | ArrowDown -> "⬇️", "arrow_down", "Downgrade dependencies."
            | ArrowUp -> "⬆️", "arrow_up", "Upgrade dependencies."
            | Art -> "🎨", "art", "Improve structure / format of the code."
            | Beers -> "🍻", "beers", "Write code drunkenly."
            | Bento -> "🍱", "bento", "Add or update assets."
            | Bookmark -> "🔖", "bookmark", "Release / Version tags."
            | Boom -> "💥", "boom", "Introduce breaking changes."
            | Bricks -> "🧱", "bricks", "Infrastructure related changes."
            | Bug -> "🐛", "bug", "Fix a bug."
            | BuildingConstruction -> "🏗️", "building_construction", "Make architectural changes."
            | Bulb -> "💡", "bulb", "Add or update comments in source code."
            | BustsInSilhouette -> "👥", "busts_in_silhouette", "Add or update contributor(s)."
            | CameraFlash -> "📸", "camera_flash", "Add or update snapshots."
            | CardFileBox -> "🗃️", "card_file_box", "Perform database related changes."
            | ChartWithUpwardsTrend -> "📈", "chart_with_upwards_trend", "Add or update analytics or track code."
            | ChildrenCrossing -> "🚸", "children_crossing", "Improve user experience / usability."
            | ClosedLockWithKey -> "🔐", "closed_lock_with_key", "Add or update secrets."
            | ClownFace -> "🤡", "clown_face", "Mock things."
            | Coffin -> "⚰️", "coffin", "Remove dead code."
            | Construction -> "🚧", "construction", "Work in progress, not yet finalized."
            | ConstructionWorker -> "👷", "construction_worker", "Add or update CI build system."
            | Dizzy -> "💫", "dizzy", "Add or update animations and transitions."
            | Egg -> "🥚", "egg", "Add or update an easter egg."
            | Fire -> "🔥", "fire", "Remove code or files."
            | GlobeWithMeridians -> "🌐", "globe_with_meridians", "Internationalization and localization."
            | GoalNet -> "🥅", "goal_net", "Catch errors."
            | GreenHeart -> "💚", "green_heart", "Fix CI Build."
            | Hammer -> "🔨", "hammer", "Add or update development scripts."
            | HeavyMinusSign -> "➖", "heavy_minus_sign", "Remove a dependency."
            | HeavyPlusSign -> "➕", "heavy_plus_sign", "Add a dependency."
            | Iphone -> "📱", "iphone", "Work on responsive design."
            | Label -> "🏷️", "label", "Add or update types."
            | Lipstick -> "💄", "lipstick", "Add or update the UI and style files."
            | Lock -> "🔒️", "lock", "Fix security or privacy issues."
            | LoudSound -> "🔊", "loud_sound", "Add or update logs."
            | Mag -> "🔍️", "mag", "Improve SEO."
            | Memo -> "📝", "memo", "Add or update documentation."
            | MoneyWithWings -> "💸", "money_with_wings", "Add sponsorships or money related infrastructure."
            | MonocleFace -> "🧐", "monocle_face", "Data exploration/inspection."
            | Mute -> "🔇", "mute", "Remove logs."
            | Necktie -> "👔", "necktie", "Add or update business logic."
            | Package -> "📦", "package", "Add or update compiled files or packages."
            | PageFacingUp -> "📄", "page_facing_up", "Add or update license."
            | PassportControl -> "🛂", "passport_control", "Work on code related to authorization, roles and permissions."
            | Pencil2 -> "✏️", "pencil2", "Fix typos."
            | Poop -> "💩", "poop", "Write bad code that needs to be improved."
            | Pushpin -> "📌", "pushpin", "Pin dependencies to specific versions."
            | Recycle -> "♻️", "recycle", "Refactor code: without changing its behavior."
            | Rewind -> "⏪", "rewind", "Revert changes."
            | Rocket -> "🚀", "rocket", "Deploy stuff."
            | RotatingLight -> "🚨", "rotating_light", "Fix compiler / linter warnings."
            | SafetyVest -> "🦺", "safety_vest", "Add or update code related to validation."
            | Seedling -> "🌱", "seedling", "Add or update seed files."
            | SeeNoEvil -> "🙈", "see_no_evil", "Add or update a .gitignore file."
            | Sparkles -> "✨", "sparkles", "Introduces a new feature."
            | SpeechBalloon -> "💬", "speech_balloon", "Add or update text and literals."
            | Stethoscope -> "🩺", "stethoscope", "Add or update healthcheck."
            | Tada -> "🎉", "tada", "Begin a project."
            | Technologist -> "🧑‍💻", "technologist", "Improve developer experience."
            | TestTube -> "🧪", "test_tube", "Add a failing test."
            | Thread -> "🧵", "thread", "Add or update code related to multithreading or concurrency."
            | TriangularFlagOnPost -> "🚩", "triangular_flag_on_post", "Add, update, or remove feature flags."
            | Truck -> "🚚", "truck", "Move or rename resources (e.g.: files, paths, routes)."
            | TwistedRightwardsArrows -> "🔀", "twisted_rightwards_arrows", "Merge branches."
            | Wastebasket -> "🗑️", "wastebasket", "Deprecate code that needs to be cleaned up."
            | Wheelchair -> "♿️", "wheelchair", "Improve accessibility."
            | WhiteCheckMark -> "✅", "white_check_mark", "Add, update, or pass tests."
            | Wrench -> "🔧", "wrench", "Add or update configuration files."
            | Zap -> "⚡️", "zap", "Improve performance."

        {|
            Char = char
            Code = code
            Hint = hint
        |}