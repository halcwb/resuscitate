namespace Shared

type Description = string

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
    | DischargeDefib
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
    Event list -> ProtocolItem -> Description list * Command list

type ProcessCommand = Command -> Event list -> Description list * Command list * Event list

module Protocol =

    let unresponsive : ProtocolBlock =
        Unresponsive,
        [
            {
                Interventions = []
                Evaluation = 
                    [ NoSignsOfLife; SignsOfLife ]
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
                        [ NonShockable; ChangeToShockable; ROSC ]
                        |> CheckRhythm
                } |> NonRepeatable
        ]

    let nonShockable : ProtocolBlock =
        NonShockable,
        [
            {
                Interventions = [ CPR ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable

            {
                Interventions = [ CPR; Adrenalin ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
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
                Interventions = [ CPR; DischargeDefib ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let changeToROSC : ProtocolBlock =
        ChangeToROSC,
        [
            {
                Interventions = [ DischargeDefib ]
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


    let printEvent e =
        match e with
        | Observed o ->
            match o with
            | Unresponsive -> "The patient was Non Responsive"
            | NoSignsOfLife -> "The patient showed No Signs of Life"
            | SignsOfLife -> "The patient showed Signs of Life"
            | Shockable -> "A Shockable rhythm was observed"
            | NonShockable -> "A NON Shockable rhythm was observed"
            | ChangeToNonShockable -> "The rhythm changed to a NON Shockable rhythm"
            | ChangeToShockable -> "The rhythm changed to a Shockable rhythm"
            | ChangeToROSC -> "There was a Return Of Spontaneous Circulation"
            | ROSC -> "There was a Return Of Spontaneous Circulation"
        | Intervened i ->
            match i with
            | BLS -> "Basic Life Support was given"
            | CPR -> "Continous Pulmonary Resuscitation was started"
            | ChargeDefib -> "The Defibrillator was Charged"
            | DischargeDefib -> "The Defibrillator was Discharged"
            | Shock -> "A Shock was given"
            | Adrenalin -> "Adrenalin was given"
            | Amiodarone -> "Amiodarone was given"


    let printCommand c =
        match c with
        | Observe o ->
            match o with
            | Unresponsive -> "The patient is NON RESPONSIVE"
            | NoSignsOfLife -> "The patient shows NO SIGNS OF LIFE"
            | SignsOfLife -> "The patient shows SIGNS OF LIFE"
            | Shockable -> "A SHOCKABLE rhythm is observed"
            | NonShockable -> "A NON SHOCKABLE rhythm is observed"
            | ChangeToNonShockable -> "A NON SHOCKABLE rhythm is observed"
            | ChangeToShockable -> "A SHOCKABLE rhythm is observed"
            | ChangeToROSC -> "There is a ROSC"
            | ROSC -> "There is a ROSC"
        | Intervene i ->
            match i with
            | BLS -> "Start BLS"
            | CPR -> "Resume CPR"
            | ChargeDefib -> "CHARGE the Defibrillator"
            | DischargeDefib -> "DISCHARGE the Defibrillator"
            | Shock -> "Apply a SHOCK"
            | Adrenalin -> "Give ADRENALIN"
            | Amiodarone -> "Give AMIODARONE"


    let getInterventionDescr = function
        | BLS ->    
            [
                "Give 5 Initial Breaths"
                "Start CPR (15:2)"
                "Attach Defibrillator/Monitor"
            ]
        | CPR -> 
            [
                "Resume CPR (15:2) for 2 min"
            ]
        | ChargeDefib -> 
            [
                "Charge Defibrillator 4 Joules/kg"
            ]
        | DischargeDefib -> 
            [ 
                "Discharge Defibrillator"
            ]
        | Shock -> 
            [ 
                "Apply Shock"
            ]
        | Adrenalin -> 
            [
                "Give Adrenalin 10 mcg/kg"
            ]
        | Amiodarone -> 
            [ 
                "Give Amiodarone 5 mg/kg"
            ]


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
                let cs =
                    ib.Interventions.[n]
                    |> List.singleton
                cs |> List.collect Protocol.getInterventionDescr ,
                cs |> List.map Intervene

            else 
                ib.Evaluation
                |> function 
                | CheckForSignsOfLife obs ->
                    [ "Check for Signs Of Life" ], 
                    obs |> List.map Observe 
                | CheckRhythm obs -> 
                    [ "Check Rhythm" ],
                    obs |> List.map Observe 
                | Finished -> [], []


    let getCommands es =
        es
        |> getCurrentProtocolBranch  Protocol.resuscitation
        |> Option.bind ((removeNonRepeatableFromBranch es) >> Some)
        |> Option.bind (getCurrentProtocolItem)
        |> Option.bind ((getCommandsFromProtocolItem es) >> Some)
        |> Option.defaultValue ([], [])

    let init es =
        es
        |> getCommands

    let processCommand : ProcessCommand =
        fun cmd es ->
            let es =
                match cmd with
                | Observe x -> Observed x
                | Intervene x -> Intervened x
                |> List.singleton
                |> List.append es
            let (ds, cs) = es |> getCommands

            ds, cs, es
