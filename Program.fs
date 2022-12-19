open aoc_2022

let time fn =
    let sw = System.Diagnostics.Stopwatch()
    
    do sw.Start()
    
    let res = fn()
    
    do sw.Stop()
    
    $"%s{res} (in %i{sw.ElapsedMilliseconds}ms)"

printfn $"Day 1, part 1: %s{Day1.part1}"
printfn $"Day 1, part 2: %s{Day1.part2}"

printfn $"Day 2, part 1: %s{Day2.part1}"
printfn $"Day 2, part 2: %s{Day2.part2}"

printfn $"Day 3, part 1: %s{time Day3.part1}"
printfn $"Day 3, part 2: %s{time Day3.part2}"

printfn $"Day 4, part 1: %s{time Day4.part1}"
printfn $"Day 4, part 2: %s{time Day4.part2}"

printfn $"Day 5, part 1: %s{time Day5.part1}"
printfn $"Day 5, part 2: %s{time Day5.part2}"

printfn $"Day 6, part 1: %s{time Day6.part1}"
printfn $"Day 6, part 2: %s{time Day6.part2}"

printfn $"Day 7, part 1: %s{time Day7.part1}"
printfn $"Day 7, part 2: %s{time Day7.part2}"

printfn $"Day 8, part 1: %s{time Day8.part1}"
printfn $"Day 8, part 2: %s{time Day8.part2}"

printfn $"Day 9, part 1: %s{time Day9.part1}"
printfn $"Day 9, part 2: %s{time Day9.part2}"

printfn $"Day 10, part 1: %s{time Day10.part1}"
printfn $"Day 10, part 2: %s{time Day10.part2}"

printfn $"Day 11, part 1: %s{time Day11.part1}"
printfn $"Day 11, part 2: %s{time Day11.part2}"

printfn $"Day 12, part 1: %s{time Day12.part1}"
printfn $"Day 12, part 2: %s{time Day12.part2}"

printfn $"Day 13, part 1: %s{time Day13.part1}"
//printfn $"Day 13, part 2: %s{time Day13.part2}"

printfn $"Day 14, part 1: %s{time Day14.part1}"
printfn $"Day 14, part 2: %s{time Day14.part2}"

printfn $"Day 15, part 1: %s{time Day15.part1}"
printfn $"Day 15, part 2: %s{time Day15.part2}"
