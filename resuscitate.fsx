
#time 

#I __SOURCE_DIRECTORY__

type Observation =
    | Unresponsive
    | NoSignsOfLife
    | SignsOfLife
    | Shockable
    | NonShockable
    | SwitchToNonShockable
    | SwitchToShockable
    | ROSC

type Evaluation =
    | CheckForSignsOfLife of Observation list
    | CheckRhythm of Observation list

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

type ProtocolBranch = Observation * ProtocolItem list

type Protocol = ProtocolBranch list

type Event =
    | Observed of Observation
    | Intervened of Intervention

type Command = 
    | Observe of Observation
    | Intervene of Intervention

type GetCurrentProtocolBranch = 
    Protocol -> Event list -> ProtocolBranch Option

type RemoveNonRepeatableFromBranch =
    Event list -> ProtocolBranch -> (int * ProtocolItem list)

type GetCurrentProtocolItem = (int * ProtocolItem list) -> ProtocolItem Option

type GetCommandsFromProtocolItem =
    Event list -> ProtocolItem -> Command list

type ProcessCommand = Command -> Event list -> Event list


module Implementation =

    let unresponsive : ProtocolBranch =
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

    let noSignsOfLife : ProtocolBranch =
        NoSignsOfLife,
        [
                {
                    Interventions = [ BLS ] // Start BLS
                    Evaluation = 
                        [ ROSC; NonShockable; SwitchToShockable ]
                        |> CheckRhythm
                } |> NonRepeatable
        ]

    let nonShockable : ProtocolBranch =
        NonShockable,
        [
            {
                Interventions = [ CPR ]
                Evaluation = 
                    [ ROSC; NonShockable; SwitchToShockable ]
                    |> CheckRhythm
            } |> Repeatable

            {
                Interventions = [ CPR; Adrenalin ]
                Evaluation = 
                    [ ROSC; NonShockable; SwitchToShockable ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let switchToShockable : ProtocolBranch =
        SwitchToShockable ,
        [
            {
                Interventions = [ CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; SwitchToNonShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let shockable : ProtocolBranch =
        Shockable,
        [
            // First Shock
            {
                Interventions = [ Shock; CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; SwitchToNonShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Second Shock
            {
                Interventions = [ Shock; CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; SwitchToNonShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Third Shock
            {
                Interventions = [ Shock; CPR; Adrenalin; Amiodarone; ChargeDefib ]
                Evaluation = 
                    [ Shockable; SwitchToNonShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Fourth Shock
            {
                Interventions = [ Shock; CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; SwitchToNonShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Fifth Shock
            {
                Interventions = [ Shock; CPR; Adrenalin; Amiodarone; ChargeDefib ]
                Evaluation = 
                    [ Shockable; SwitchToNonShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Continue ...
            {
                Interventions = [ Shock; CPR; ChargeDefib ]
                Evaluation = 
                    [ Shockable; SwitchToNonShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable
            {
                Interventions = [ Shock; CPR; Adrenalin; ChargeDefib ]
                Evaluation = 
                    [ Shockable; SwitchToNonShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let switchToNonShockable : ProtocolBranch =
        SwitchToNonShockable,
        [
            {
                Interventions = [ UnChargeDefib; CPR ]
                Evaluation = 
                    [ ROSC; NonShockable; SwitchToShockable ]
                    |> CheckRhythm
            } |> Repeatable
        ]


    let resuscitation : Protocol =
        [
            unresponsive
            noSignsOfLife
            switchToShockable
            nonShockable
            shockable
            switchToNonShockable
        ]

    let getCurrentProtocolBranch : GetCurrentProtocolBranch =
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

    let removeNonRepeatableFromBranch : RemoveNonRepeatableFromBranch =
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


    module Tests =

        let es = [ Unresponsive |> Observed ]
        
        let run es =
            es
            |> getCurrentProtocolBranch resuscitation
            |> Option.bind ((removeNonRepeatableFromBranch es) >> Some)
            |> Option.bind (getCurrentProtocolItem)
            |> Option.bind ((getCommandsFromProtocolItem es) >> Some)


open Implementation

let es = 
    [
        Observed Unresponsive 
        Observed NoSignsOfLife
        Intervened BLS 
        Observed NonShockable 
        Intervened CPR 
        Observed NonShockable
        Intervened CPR
        Intervened Adrenalin
        Observed SwitchToShockable
        Intervened CPR
        Intervened ChargeDefib
        Observed Shockable
        Intervened Shock
        Intervened CPR
        Intervened ChargeDefib
        Observed Shockable
        Intervened Shock
        Intervened CPR
        Intervened ChargeDefib
        Observed Shockable
        Intervened Shock
        Intervened CPR
        Intervened ChargeDefib
        Observed Shockable
        Intervened Shock
        Intervened CPR
        Intervened Adrenalin
        Intervened Amiodarone
        Intervened ChargeDefib
        Observed Shockable
        Intervened Shock
    ]

es
|> Tests.run

open System

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
        |> Tests.run
        |> Option.defaultValue []
        |> List.map (fun c ->
            match c with
            | Observe o -> Observed o
            | Intervene i -> Intervened i
        )
        |> pickRand

    let rec run b es =
        if b then printfn "--- Finished\n\n"
        else
            match es |> procEvs with
            | Some e ->
                printfn "Event: %A" e
                [ e ] |> List.append es |> run false
            | None -> run true es

    [ Observed Unresponsive ] |> run false


        