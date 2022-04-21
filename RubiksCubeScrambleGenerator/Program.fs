namespace RubiksCubeScrambleGenerator

module RubiksCubeScrambleGenerator =
    [<Literal>]
    let MovesPerScramble = 20u

    [<Literal>]
    let CubeSideCount = 6u

    type RotationDirection =
        | CounterClockwise = -1
        | Clockwise = 1
        | HalfTurn = 2

    let randomEngine = new System.Random()
    let rng (maxInclusive: uint32) = uint32 (randomEngine.Next(1, int maxInclusive + 1))

    let randomCubeSide () = rng CubeSideCount
    let randomRotation () =
        let rotations = System.Enum.GetValues<RotationDirection>()
        let rotationIndex = int <| rng (uint32 (rotations.Length))

        rotations.[rotationIndex - 1]

    let getCubeSideLetter = function
        | 1u -> "U"
        | 2u -> "D"
        | 3u -> "R"
        | 4u -> "L"
        | 5u -> "F"
        | 6u -> "B"
        | _ -> raise <| System.ArgumentException("Invalid cube side number")

    let getRotationSuffix = function
        | RotationDirection.CounterClockwise -> "'"
        | RotationDirection.Clockwise -> ""
        | RotationDirection.HalfTurn -> "2"
        | _ -> raise <| System.ArgumentException("Invalid rotation value")

    let rec generateTurn = function
        | Some (previousTurn: string) ->
            let cubeSide = randomCubeSide()
            let cubeSideLetter = getCubeSideLetter cubeSide

            if cubeSideLetter.[0] = previousTurn.[0] then
                generateTurn (Some previousTurn)
            else
                let rotation = randomRotation()
                let rotationSuffix = getRotationSuffix rotation

                cubeSideLetter + rotationSuffix
        | None -> (getCubeSideLetter (randomCubeSide())) + (getRotationSuffix (randomRotation()))

    let rec generateScramble () =
        let rec generateScramble' (list: string list) =
            if list.Length = 0 then
                let firstTurn = generateTurn None

                generateScramble' [firstTurn]
            elif list.Length = (int MovesPerScramble) then
                list
            else
                let newTurn = generateTurn (Some list.Head)

                generateScramble' (newTurn :: list)

        []
        |> generateScramble'
        |> List.rev

    let getScrambleCount (args: string[]) =
        let scrambleCount =
            match args.Length with
            | 0 -> 1
            | 1 ->
                match System.Int32.TryParse args.[0] with
                | true, value -> value
                | _ -> raise <| System.ArgumentException("Invalid number passed as scramble count")
            | _ -> raise <| System.ArgumentException("Invalid command line arguments. Proper usage: RubiksCubeScrambleGenerator [numberOfScrambles]")

        scrambleCount

    [<EntryPoint>]
    let main args =
        try
            let scrambleCount = getScrambleCount args

            for i = 1 to scrambleCount do
                for turn in generateScramble() do
                    printf "%s " turn

                printfn ""

            0
        with
            | :? System.ArgumentException as error ->
                printfn "[ERROR]: %s" error.Message

                1
