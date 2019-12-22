namespace FSharp.Data.XRoad.Choices

type IChoiceOf1<'a> =
    abstract TryGetOption1: unit -> Optional.Option<'a>

type IChoiceOf2<'a,'b> =
    abstract TryGetOption1: unit -> Optional.Option<'a>
    abstract TryGetOption2: unit -> Optional.Option<'b>

type IChoiceOf3<'a,'b,'c> =
    abstract TryGetOption1: unit -> Optional.Option<'a>
    abstract TryGetOption2: unit -> Optional.Option<'b>
    abstract TryGetOption3: unit -> Optional.Option<'c>

type IChoiceOf4<'a,'b,'c,'d> =
    abstract TryGetOption1: unit -> Optional.Option<'a>
    abstract TryGetOption2: unit -> Optional.Option<'b>
    abstract TryGetOption3: unit -> Optional.Option<'c>
    abstract TryGetOption4: unit -> Optional.Option<'d>

type IChoiceOf5<'a,'b,'c,'d,'e> =
    abstract TryGetOption1: unit -> Optional.Option<'a>
    abstract TryGetOption2: unit -> Optional.Option<'b>
    abstract TryGetOption3: unit -> Optional.Option<'c>
    abstract TryGetOption4: unit -> Optional.Option<'d>
    abstract TryGetOption5: unit -> Optional.Option<'e>

type IChoiceOf6<'a,'b,'c,'d,'e,'f> =
    abstract TryGetOption1: unit -> Optional.Option<'a>
    abstract TryGetOption2: unit -> Optional.Option<'b>
    abstract TryGetOption3: unit -> Optional.Option<'c>
    abstract TryGetOption4: unit -> Optional.Option<'d>
    abstract TryGetOption5: unit -> Optional.Option<'e>
    abstract TryGetOption6: unit -> Optional.Option<'f>

type IChoiceOf7<'a,'b,'c,'d,'e,'f,'g> =
    abstract TryGetOption1: unit -> Optional.Option<'a>
    abstract TryGetOption2: unit -> Optional.Option<'b>
    abstract TryGetOption3: unit -> Optional.Option<'c>
    abstract TryGetOption4: unit -> Optional.Option<'d>
    abstract TryGetOption5: unit -> Optional.Option<'e>
    abstract TryGetOption6: unit -> Optional.Option<'f>
    abstract TryGetOption7: unit -> Optional.Option<'g>

type IChoiceOf8<'a,'b,'c,'d,'e,'f,'g,'h> =
    abstract TryGetOption1: unit -> Optional.Option<'a>
    abstract TryGetOption2: unit -> Optional.Option<'b>
    abstract TryGetOption3: unit -> Optional.Option<'c>
    abstract TryGetOption4: unit -> Optional.Option<'d>
    abstract TryGetOption5: unit -> Optional.Option<'e>
    abstract TryGetOption6: unit -> Optional.Option<'f>
    abstract TryGetOption7: unit -> Optional.Option<'g>
    abstract TryGetOption8: unit -> Optional.Option<'h>
