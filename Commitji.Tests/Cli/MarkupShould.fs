module Commitji.Tests.Cli.MarkupShould

open Commitji.Cli
open Commitji.Core.Model
open Commitji.Core.Model.Search
open Swensen.Unquote
open global.Xunit

module ``highlight segment`` =
    let highlightSegment segmentText segmentState =
        let segment = {
            Id = SegmentId.Code
            Text = segmentText
            State = segmentState
        }

        Markup.highlightSegmentWith (fun s -> $"[{s}]") segment

    [<Theory>]
    [<InlineData("feat", "f", 0, "[f]eat")>]
    [<InlineData("feat", "a", 2, "fe[a]t")>]
    [<InlineData("feat", "t", 3, "fea[t]")>]
    [<InlineData("feat", "fe", 0, "[fe]at")>]
    [<InlineData("feat", "ea", 1, "f[ea]t")>]
    [<InlineData("feat", "at", 2, "fe[at]")>]
    let ``apply highlight on single hit`` text input hit expected =
        let segmentState = SegmentState.Searched([ hit ], String.length input)
        let actual = highlightSegment (SegmentText text) segmentState
        actual =! expected

    [<Theory>]
    [<InlineData("test", "t", 0, 3, "[t]es[t]")>]
    [<InlineData("nette", "t", 2, 3, "ne[t][t]e")>]
    let ``apply highlight on double hit`` text input hit1 hit2 expected =
        let segmentState = SegmentState.Searched([ hit1; hit2 ], String.length input)
        let actual = highlightSegment (SegmentText text) segmentState
        actual =! expected

    [<Theory>]
    [<InlineData("abc", "z")>]
    [<InlineData("abc", "ba")>]
    let ``apply no highlight if not found`` text input =
        let segmentState = SegmentState.Searched([], String.length input)
        let actual = highlightSegment (SegmentText text) segmentState
        actual =! text

    [<Theory>]
    [<InlineData("abc")>]
    [<InlineData("z")>]
    let ``apply no highlight if not searchable`` text =
        let segmentState = SegmentState.NotSearchable
        let actual = highlightSegment (SegmentText text) segmentState
        actual =! text