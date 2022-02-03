module TimeTests
open TimeHelpers

/// Return time taken by thunk()
/// Run thunk() as many times as is needed
/// for total elapsed time in ms to be  > limitMs.
/// Return average time of all runs.
/// To minimise cache effects run thunk() once before
/// starting to time.
let getTimeOfInMs (limitMs: float) (thunk:Unit -> Unit) =
    thunk()
    let startT = getTimeMs()
    let mutable i = 0
    while getInterval startT < limitMs do
        i <- i+1
        thunk()
    getInterval startT / float i

/// Test various ways to read out all the values of a map.
/// Larger runtime (in ms) is more accurate: suggested range 1000 - 5000.
/// Results all roughly the same.
let testMapRead runTime =

    let viaArrays (m:Map<'a,float>) =
        m |> Map.toArray |> Array.map snd |> Array.sum |> ignore
    let viaLists (m:Map<'a,float>) =
        m |> Map.toList |> List.map snd |> List.sum |> ignore
    let viaListsSumBy (m:Map<'a,float>) =
         m |> Map.toList |> List.sumBy snd |> ignore
    let viaValues (m:Map<'a,float>) =
        m |> Map.values |> Seq.sum |> ignore
    let viaArraysSumBy (m:Map<'a,float>) =
        m |> Map.toArray |> Array.sumBy snd |> ignore

    let viaRandomMapLookups (m:Map<'a,float>) =
        m 
        |> Map.toArray 
        |> Array.map fst
        |> Array.sumBy (fun k -> m[k])
        |> ignore

    let test f =
        let makeMap n = 
            [1.0..(float n)]
            |> List.map (fun n -> n,n*n)
            |> Map.ofList

        let timeIt n = 
            let mapToRead = makeMap n
            $"%5.2f{1000000. * getTimeOfInMs runTime (fun () -> f mapToRead |> ignore) / float n}"

        $"{timeIt 100}     {timeIt 1000}      {timeIt 10000}"

    printfn "\nTesting time in ns to read the values of a Map of given size (per value)"
    printfn $"Siz  in items:   100       1000       10000"
    printfn $"viaArrays:       {test viaArrays}"
    printfn $"viaArraysSumBy:  {test viaArraysSumBy}"
    printfn $"viaLists:        {test viaLists}"
    printfn $"viaListsSumBy:   {test viaListsSumBy}"
    printfn $"viaValues:       {test viaValues}"
    printfn $"viaRamdomLookup: {test viaRandomMapLookups}"


/// test diferent ways of filtering a long generated collection
/// the filtered output is very small, so efficiency depends on how
/// fast the generation and throw away of the long collection is.
/// collect wins because the whole collection is never generated.
/// lists are faster than arrays because [] is handled better than [||].
/// larger runtime (in ms) is more accurate: suggested range 1000 - 5000.
/// n*n is the size of the collection to be filtered.
let testCollections runTime n =
    let useSeq op ()=
        let op = if op then fun n -> n=2 else fun n -> n > 2
        ([| 1 .. n |], [| 1 .. n |])
        ||> Seq.allPairs // n*n pairs
        |> Seq.filter (fun (a, b) -> op (a + b)) // 2 results
        |> Seq.map (fun (a, b) -> a + b)
        |> Seq.sum
        |> ignore

    let useArray op ()=
        let op = if op then fun n -> n=2 else fun n -> n > 2
        ([| 1 .. n |], [| 1 .. n |])
        ||> Array.allPairs
        |> Array.filter (fun (a, b) -> op (a  + b) )
        |> Array.map (fun (a, b) -> a + b)
        |> Array.sum
        |> ignore

    let useList op ()=
        let op = if op then fun n -> n=2 else fun n -> n > 2
        ([ 1 .. n ], [ 1 .. n ])
        ||> List.allPairs
        |> List.filter (fun (a, b) -> op (a + b))
        |> List.map (fun (a, b) -> a + b)
        |> List.sum
        |> ignore

    let useCollectA op () =
        let op = if op then fun n -> n=2 else fun n -> n > 2
        let a, b = ([| 1 .. n |], [| 1 .. n |])

        a
        |> Array.collect
            (fun a' ->
                b
                |> Array.collect
                    (fun b' ->
                        if op (a' + b') then
                            [| a' + b' |]
                        else
                            [||]))
        |> ignore

    let useIterA op ()=
        let op = if op then fun n -> n=2 else fun n -> n > 2
        let a, b = ([| 1 .. n |], [| 1 .. n |])
        let mutable res = []
        a
        |> Array.iter
            (fun a' ->
                b
                |> Array.iter
                    (fun b' ->
                        if op (a' + b') then
                            res <-  a' + b'  :: res))
        |> ignore

    let useIterL op () =
        let op = if op then fun n -> n=2 else fun n -> n > 2
        let a, b = ([ 1 .. n ], [1 .. n ])
        let mutable res = []
        a
        |> List.iter
            (fun a' ->
                b
                |> List.iter
                    (fun b' ->
                        if op (a'+b')  then
                            res <- a' + b' :: res))
        |> ignore

    let useCollectL op () =
        let op = if op then fun n -> n=2 else fun n -> n > 2

        let a, b = ([ 1 .. n ], [ 1 .. n ])

        a
        |> List.collect
            (fun a' ->
                b
                |> List.collect (fun b' -> if op (a'+b') then [ a' + b' ] else []))
        |> ignore

    let useForLoops op () =
        let op = if op then fun n -> n=2 else fun n -> n > 2
        let a, b = ([| 1 .. n |], [| 1 .. n |])
        let mutable res = []

        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
                if op (a[ i ] + b[ j ])  then
                    res <- [ a[ i ] + b[ j ] ] :: res

        |> ignore



    let test timedFunc =
        $"%.2f{getTimeOfInMs runTime (timedFunc true)}                  %.2f{getTimeOfInMs runTime (timedFunc false )}"
    printfn $"\nComparing speed filtering an input list, array, seq of size {n*n}."
    printfn $"The output is the same type as the input except for iter and for which change a mutable output value."
    printfn $"In those cases the output must be a list: arrays cannot incrementally add one element quickly."
    printfn $"Iter and For result in a reversed list as output: for non-reversed add on time of List.rev.\n"
    printfn $"       Code Fragment           Time (output size=1)  Time (output size={n*n-1}) "
    printfn $"       sequence (seq) time:    {test useSeq}"
    printfn $"       list time:              {test useList}"
    printfn $"       array time:             {test useArray}"
    printfn $"       array collect time:     {test useCollectA}"
    printfn $"       list collect time:      {test useCollectL}"
    printfn $"       list iter->list time:   {test useIterL}"
    printfn $"       array iter->list time:  {test useIterA}"
    printfn $"       array for loop time:    {test useForLoops}"

let printInfo() =
    printfn "Timing information:"
    //Wires.testSpeed 10
    testCollections 500 300
    testMapRead 3000

