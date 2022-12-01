module AoC22.Day1.Solution

let private sumCaloriesPerElf (input: string) =
    input.Split "\n\n"
    |> Seq.map (fun x -> x.Split "\n")
    |> Seq.map (Seq.map int)
    |> Seq.map Seq.sum

let calculateAnswer analyseCalories input =
    sumCaloriesPerElf input
    |> analyseCalories

let part1 () =
    let calories =
        System.IO.File.ReadAllText "Day1/input.txt"
        |> calculateAnswer Seq.max
    printfn $"Max calories: {calories}"

let findSumOfTop3 =
    Seq.sortDescending
    >> Seq.take 3
    >> Seq.sum

let part2 () =
    let calories =
        System.IO.File.ReadAllText "Day1/input.txt"
        |> calculateAnswer findSumOfTop3
    printfn $"Sum of top 3: {calories}"
