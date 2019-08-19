
#time 

#I __SOURCE_DIRECTORY__

#load  ".paket/load/netcoreapp2.2/main.group.fsx"

type Observation =
    | Unresponsive
    | NoSignsOfLife
    | SignsOfLife
    | Shockable
    | NonShockable
    | ChangeToNonShockable
    | ChangeToShockable
    | ChangeToROSC
    | ROSC

type Evaluation =
    | CheckForSignsOfLife of Observation list
    | CheckRhythm of Observation list
    | Finished

type Intervention =
    | BLS
    | CPR
    | ChargeDefib
    | UnChargeDefib
    | Shock
    | Adrenalin
    | Amiodarone

type InterventionBlock = 
    { 
        Interventions : Intervention list 
        Evaluation : Evaluation
    } 

type ProtocolItem = 
    | Repeatable of InterventionBlock
    | NonRepeatable of InterventionBlock

type ProtocolBlock = Observation * ProtocolItem list

type Protocol = ProtocolBlock list

type Event =
    | Observed of Observation
    | Intervened of Intervention

type Command = 
    | Observe of Observation
    | Intervene of Intervention

type GetCurrentProtocolBlock = 
    Protocol -> Event list -> ProtocolBlock Option

type RemoveNonRepeatableFromBlock =
    Event list -> ProtocolBlock -> (int * ProtocolItem list)

type GetCurrentProtocolItem = (int * ProtocolItem list) -> ProtocolItem Option

type GetCommandsFromProtocolItem =
    Event list -> ProtocolItem -> Command list

type ProcessCommand = Command -> Event list -> Event list


module Implementation =


    let getCurrentProtocolBranch : GetCurrentProtocolBlock =
        fun p es ->
            let lastObs =
                es 
                |> List.rev
                |> List.map (fun e ->
                    match e with
                    | Observed o -> o |> Some
                    | Intervened _ -> None
                ) 
                |> List.filter Option.isSome
                |> List.map Option.get
                |> List.tryHead

            match lastObs with
            | None -> None
            | Some o ->
                p
                |> List.tryFind (fst >> ((=) o))

    let removeFromProtocolItemList n protocolItems =
            let c = n // + 1 // Store the original observation count
            let rec remove (n, r) xs =
                if n = 0 then (c - r, xs) // return number of remaining observations - removed 
                else
                    match xs with
                    | [] -> (0, [])
                    | h::tail ->
                        match h with
                        | Repeatable _ -> remove (n - 1, r) xs
                        | NonRepeatable _ -> remove (n - 1, r + 1) tail

            remove (n, 0) protocolItems

    let removeNonRepeatableFromBranch : RemoveNonRepeatableFromBlock =
        fun es pb  ->
            let obs, ps = pb
            let n =
                es 
                |> List.filter (fun e ->
                    match e with
                    | Intervened _ -> false
                    | Observed o -> o = obs
                )
                |> List.length
            
            ps 
            |> removeFromProtocolItemList (n - 1)


    let getCurrentProtocolItem : GetCurrentProtocolItem =
        fun (n, xs) ->
            if List.isEmpty xs then None
            else
                let i = n % (xs |> List.length)
                xs.[i] |> Some


    let getCommandsFromProtocolItem : GetCommandsFromProtocolItem =
        fun es pi ->
            let n =
                es 
                |> List.rev
                |> List.fold (fun a e ->
                    match e with
                    | Observed _   -> (false, a |> snd)
                    | Intervened _ -> 
                        if a |> fst |> not then a
                        else (true, (a |> snd) + 1)
                ) (true, 0)
                |> snd

            let ib = 
                match pi with
                | Repeatable b -> b
                | NonRepeatable b -> b

            if n >= 0 && ((ib.Interventions |> List.length) > n) then 
                ib.Interventions.[n]
                |> Intervene
                |> List.singleton
            else 
                ib.Evaluation
                |> function 
                | CheckForSignsOfLife obs 
                | CheckRhythm obs -> obs |> List.map Observe 
                | Finished -> []


module Protocol =

    let unresponsive : ProtocolBlock =
        Unresponsive,
        [
            {
                Interventions = []
                Evaluation = 
                    [ SignsOfLife; NoSignsOfLife ]
                    |> CheckForSignsOfLife
            }
            |> NonRepeatable
        ]

    let noSignsOfLife : ProtocolBlock =
        NoSignsOfLife,
        [
                {
                    Interventions = [ BLS ] // Start BLS
                    Evaluation = 
                        [ ROSC; NonShockable; ChangeToShockable ]
                        |> CheckRhythm
                } |> NonRepeatable
        ]

    let nonShockable : ProtocolBlock =
        NonShockable,
        [
            {
                Interventions = [ CPR ]
                Evaluation = 
                    [ ROSC; NonShockable; ChangeToShockable ]
                    |> CheckRhythm
            } |> Repeatable

            {
                Interventions = [ CPR; Adrenalin ]
                Evaluation = 
                    [ ROSC; NonShockable; ChangeToShockable ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let changeToShockable : ProtocolBlock =
        ChangeToShockable ,
        [
            {
                Interventions = [ CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let shockable : ProtocolBlock =
        Shockable,
        [
            // First Shock
            {
                Interventions = [ Shock; CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Second Shock
            {
                Interventions = [ Shock; CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Third Shock
            {
                Interventions = [ Shock; CPR; Adrenalin; Amiodarone; ChargeDefib ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Fourth Shock
            {
                Interventions = [ Shock; CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Fifth Shock
            {
                Interventions = [ Shock; CPR; Adrenalin; Amiodarone; ChargeDefib ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Continue ...
            {
                Interventions = [ Shock; CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> Repeatable
            {
                Interventions = [ Shock; CPR; Adrenalin; ChargeDefib ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let changeToNonShockable : ProtocolBlock =
        ChangeToNonShockable,
        [
            {
                Interventions = [ UnChargeDefib; CPR ]
                Evaluation = 
                    [ ROSC; NonShockable; ChangeToShockable ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let changeToROSC : ProtocolBlock =
        ChangeToROSC,
        [
            {
                Interventions = [ UnChargeDefib ]
                Evaluation = Finished
            } |> NonRepeatable
        ]

    let resuscitation : Protocol =
        [
            unresponsive            // Start with an unresponsive patient
            noSignsOfLife           // When there are no signs of life
            changeToShockable       // Change to the shockable rhythm block 
            nonShockable            // Start with a non shockable rhythm
            shockable               // Continue in the shockable rhythm block
            changeToNonShockable    // Change to a non shockable rhythm
            changeToROSC            // Change from shockable to ROSC
        ]

module Tests =

    open System
    open Implementation

    open Expecto

    let expectIsTrue msg b = Expect.isTrue b msg

    let expectExists ass msg act = Expect.exists act ass msg

    let expectEqual act msg exp = Expect.equal act exp msg

    let es = [ Unresponsive |> Observed ]
    
    let run es =
        es
        |> getCurrentProtocolBranch  Protocol.resuscitation
        |> Option.bind ((removeNonRepeatableFromBranch es) >> Some)
        |> Option.bind (getCurrentProtocolItem)
        |> Option.bind ((getCommandsFromProtocolItem es) >> Some)
        |> Option.defaultValue []


    let runRandom n =
        printfn "--- Start run %i" n

        let rand = n |> Random

        let pickRand xs =
            let c = xs |> List.length

            if c = 0 then None
            else 
                xs.[ c |> rand.Next ]
                |> Some

        let procEvs es = 
            es
            |> run
            |> List.map (fun c ->
                match c with
                | Observe o -> Observed o
                | Intervene i -> Intervened i
            )
            |> pickRand

        let rec run b es =
            if b then 
                printfn "--- Finished\n\n"
                es
            else
                match es |> procEvs with
                | Some e ->
                    printfn "Event: %A" e
                    [ e ] |> List.append es |> run false
                | None -> 
                    run true es

        printfn "Event: %A" (Observed Unresponsive)
        [ Observed Unresponsive ] 
        |> run false


    let checkCmdsLength n cmds =
        let c = cmds |> List.length
        
        c
        |> expectEqual n (sprintf "Should have %i commands, but has %i" n c)


    let tests =
        testList "Test resuscitation protocol" [
        
            test "For an unresponsive patient" {
                es
                |> run
                |> fun cmds ->
                    cmds
                    |> checkCmdsLength 2
                    
                    cmds
                    |> expectExists ((=) (Observe SignsOfLife))
                                    "Should check for signs of life"

                    cmds 
                    |> expectExists ((=) (Observe NoSignsOfLife))
                                    "Should check for signs of life"
            }
        
            test "When a patient has signs of life" {
                [ Observed SignsOfLife ]
                |> List.append es
                |> run
                |> fun cmds ->
                    cmds
                    |> checkCmdsLength 0

            }

            test "When a patient has no signs of life" {
                [ Observed NoSignsOfLife ]
                |> List.append es
                |> run
                |> fun cmds ->
                    cmds
                    |> checkCmdsLength 1
                    
                    cmds 
                    |> expectExists ((=) (Intervene BLS)) 
                                    "Should start BLS"

            }

            test "When a patient has had basic life support" {
                [ Observed NoSignsOfLife; Intervened BLS ]
                |> List.append es
                |> run
                |> fun cmds -> 
                    cmds
                    |> checkCmdsLength 3
                    // Check command 1
                    cmds
                    |> expectExists ((=) (Observe ChangeToShockable)) 
                                         "Should check for a change to shockable rhythm"
                    // Check command 2
                    cmds
                    |> expectExists ((=) (Observe NonShockable)) 
                                         "Should check for a non shockable rhythm"
                    // Check command 3
                    cmds
                    |> expectExists ((=) (Observe ROSC)) 
                                         "Should check for a return of spontaneous circulation"
            }

            test "When a patient has a non shockable rhythm" {
                [ Observed NoSignsOfLife; Intervened BLS; Observed NonShockable ]
                |> List.append es
                |> run
                |> fun cmds -> 
                    cmds
                    |> checkCmdsLength 1
                    // Check command 1
                    cmds
                    |> expectExists ((=) (Intervene CPR)) 
                                         "Should start cardiopulmonary resuscitation"
            }

            test "When a patient has a shockable rhythm" {
                [ Observed NoSignsOfLife; Intervened BLS; Observed ChangeToShockable ]
                |> List.append es
                |> run
                |> fun cmds -> 
                    cmds
                    |> checkCmdsLength 1
                    // Check command 1
                    cmds
                    |> expectExists ((=) (Intervene CPR)) 
                                         "Should start cardiopulmonary resuscitation"
            }

            test "When a patient has a shockable rhythm and CPR" {
                [ Observed NoSignsOfLife
                  Intervened BLS
                  Observed ChangeToShockable
                  Intervened CPR ]
                |> List.append es
                |> run
                |> fun cmds -> 
                    cmds
                    |> checkCmdsLength 1
                    // Check command 1
                    cmds
                    |> expectExists ((=) (Intervene ChargeDefib)) 
                                         "Should charge the defibrillator"
            }

        ]



open Expecto

let run () =
    Tests.tests
    |> runTests defaultConfig 

        