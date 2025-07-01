module Commitji.Core.Model.Relation

/// List of the supported combination of emojis and prefixes.
let private All = [
    Emoji.Sparkles, Prefix.Feat
    Emoji.Boom, Prefix.Feat
    Emoji.Boom, Prefix.Fix
    Emoji.ChartWithUpwardsTrend, Prefix.Feat
    Emoji.ChartWithUpwardsTrend, Prefix.Fix
    Emoji.GlobeWithMeridians, Prefix.Feat
    Emoji.Wheelchair, Prefix.Feat
    Emoji.Label, Prefix.Feat
    Emoji.TriangularFlagOnPost, Prefix.Feat
    Emoji.TriangularFlagOnPost, Prefix.Chore
    Emoji.GoalNet, Prefix.Feat
    Emoji.PassportControl, Prefix.Feat
    Emoji.Necktie, Prefix.Feat
    Emoji.SafetyVest, Prefix.Feat
    Emoji.Airplane, Prefix.Feat
    Emoji.Bug, Prefix.Fix
    Emoji.Ambulance, Prefix.Fix
    Emoji.Lock, Prefix.Fix
    Emoji.RotatingLight, Prefix.Fix
    Emoji.AdhesiveBandage, Prefix.Fix
    Emoji.Memo, Prefix.Docs
    Emoji.PageFacingUp, Prefix.Docs
    Emoji.Bulb, Prefix.Docs
    Emoji.BustsInSilhouette, Prefix.Docs
    Emoji.MoneyWithWings, Prefix.Docs
    Emoji.Lipstick, Prefix.Feat
    Emoji.Lipstick, Prefix.Fix
    Emoji.ChildrenCrossing, Prefix.Feat
    Emoji.Iphone, Prefix.Feat
    Emoji.Iphone, Prefix.Fix
    Emoji.Dizzy, Prefix.Feat
    Emoji.Dizzy, Prefix.Fix
    Emoji.Recycle, Prefix.Refactor
    Emoji.ArtistPalette, Prefix.Refactor
    Emoji.Fire, Prefix.Refactor
    Emoji.Truck, Prefix.Refactor
    Emoji.BuildingConstruction, Prefix.Refactor
    Emoji.Coffin, Prefix.Refactor
    Emoji.Technologist, Prefix.Refactor
    Emoji.Zap, Prefix.Perf
    Emoji.Thread, Prefix.Perf
    Emoji.CheckMark, Prefix.Test
    Emoji.ClownFace, Prefix.Test
    Emoji.TestTube, Prefix.Test
    Emoji.Hammer, Prefix.Chore
    Emoji.Package, Prefix.Chore
    Emoji.Rocket, Prefix.Chore
    Emoji.Bookmark, Prefix.Chore
    Emoji.GreenHeart, Prefix.Chore
    Emoji.ConstructionWorker, Prefix.Chore
    Emoji.ClosedLockWithKey, Prefix.Chore
    Emoji.ArrowDown, Prefix.Chore
    Emoji.ArrowUp, Prefix.Chore
    Emoji.Pushpin, Prefix.Chore
    Emoji.HeavyPlusSign, Prefix.Chore
    Emoji.HeavyMinusSign, Prefix.Chore
    Emoji.Wrench, Prefix.Chore
    Emoji.Pencil, Prefix.Chore
    Emoji.TwistedRightwardsArrows, Prefix.Chore
    Emoji.Rewind, Prefix.Chore
    Emoji.Rewind, Prefix.Revert
    Emoji.Alien, Prefix.Chore
    Emoji.Bento, Prefix.Chore
    Emoji.SpeechBalloon, Prefix.Chore
    Emoji.LoudSound, Prefix.Chore
    Emoji.Mute, Prefix.Chore
    Emoji.Stethoscope, Prefix.Chore
    Emoji.SeeNoEvil, Prefix.Chore
    Emoji.Mag, Prefix.Chore
    Emoji.Construction, Prefix.Feat
    Emoji.Construction, Prefix.Wip
    Emoji.Tada, Prefix.Wip
    Emoji.Poop, Prefix.Feat
    Emoji.Poop, Prefix.Wip
    Emoji.Beers, Prefix.Wip
    Emoji.Wastebasket, Prefix.Refactor
    Emoji.Wastebasket, Prefix.Wip
    Emoji.Alembic, Prefix.Feat
    Emoji.Alembic, Prefix.Chore
    Emoji.Alembic, Prefix.Wip
    Emoji.CardFileBox, Prefix.Feat
    Emoji.CardFileBox, Prefix.Chore
    Emoji.Egg, Prefix.Feat
    Emoji.CameraFlash, Prefix.Docs
    Emoji.CameraFlash, Prefix.Test
    Emoji.CameraFlash, Prefix.Chore
    Emoji.Seedling, Prefix.Feat
    Emoji.Seedling, Prefix.Test
    Emoji.Seedling, Prefix.Chore
    Emoji.MonocleFace, Prefix.Chore
    Emoji.MonocleFace, Prefix.Wip
    Emoji.Bricks, Prefix.Feat
    Emoji.Bricks, Prefix.Chore
    Emoji.Broom, Prefix.Chore
    Emoji.Broom, Prefix.Refactor
]

let emojisForPrefix selectedPrefix =
    List.sort [
        for emoji, prefix in All do
            if prefix = selectedPrefix then
                emoji
    ]

let prefixesForEmoji selectedEmoji =
    List.sort [
        for emoji, prefix in All do
            if emoji = selectedEmoji then
                prefix
    ]