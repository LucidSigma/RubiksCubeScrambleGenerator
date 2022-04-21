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

    [<EntryPoint>]
    let main args =
        let turns = [for i in 1u..MovesPerScramble -> (randomCubeSide(), randomRotation())]

        for (cubeSide, rotation) in turns do
            printf "%s%s " (getCubeSideLetter cubeSide) (getRotationSuffix rotation)

        0
