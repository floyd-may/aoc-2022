module aoc_2022.OrderedRanges

type Range = {
    Start: int
    End: int
}

module Range =
    let order a b =
        if a.Start <= b.Start
        then a,b else b,a
    let overlaps a b =
        let left, right = order a b
        left.End + 1 >= right.Start
        
    let merge a b =
        let start = min a.Start b.Start
        let endVal = max a.End b.End
        
        { Start = start; End = endVal }
        
    let valid r = r.Start <= r.End
    
    let size r = r.End - r.Start + 1

type OrderedRanges = Range list

module OrderedRanges =
    let rec add r rs =
        match rs with
        | [] -> [r]
        | a :: rest when Range.overlaps r a ->
            let merged = Range.merge r a
            add merged rest
        | [a] ->
            let left, right = Range.order a r
            [left; right]
        | a :: rest ->
            a :: (add r rest)
