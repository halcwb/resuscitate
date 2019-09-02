namespace Shared

open System

type Description = string * string

type Observation =
    | NonResponsive
    | NoSignsOfLife
    | SignsOfLife
    | NoROSC
    | Shockable
    | NonShockable
    | ChangeToNonShockable
    | ChangeToShockable
    | ChangeToROSC
    | ROSC

type Evaluation =
    | CheckForSignsOfLife of Observation list
    | CheckForCirculation of Observation list
    | CheckRhythm of Observation list
    | Finished

type Intervention =
    | Initial5
    | Monitor
    | CPRStart
    | CPR2MinStart
    | CPRStop
    | CPR2MinStop
    | ChargeDefib
    | DischargeDefib
    | Shock
    | VascularAccess
    | Adrenalin
    | Amiodarone
    | Consider5H5T

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
    | Observed of ObservedEvent
    | Intervened of IntervenedEvent
and ObservedEvent =
    {
        DateTime : DateTime
        Observation : Observation
    }
and IntervenedEvent =
    {
        DateTime : DateTime
        Intervention : Intervention
    }

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

type ProcessedCommand =
    {
        Descriptions : Description list
        Commands : Command list
        Events : Event list
    }

type ProcessCommand = 
    Command 
        -> Event list 
        -> ProcessedCommand

module Protocol =

    let nonresponsive : ProtocolBlock =
        NonResponsive,
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
                    Interventions = [ Initial5 ] // Start BLS
                    Evaluation = 
                        [ NoROSC; ROSC ]
                        |> CheckForCirculation
                } |> NonRepeatable
        ]

    let noROSC : ProtocolBlock =
        NoROSC,
        [
                {
                    Interventions = [ CPRStart; Monitor; CPRStop ] // Start BLS
                    Evaluation = 
                        [ NonShockable; ChangeToShockable; ROSC ]
                        |> CheckRhythm
                } |> NonRepeatable
        ]

    let nonShockable : ProtocolBlock =
        NonShockable,
        [
            {
                Interventions = [ CPR2MinStart; VascularAccess; CPR2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable

            {
                Interventions = [ CPR2MinStart; Adrenalin; CPR2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable

            {
                Interventions = [ CPR2MinStart; Consider5H5T; CPR2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable

            {
                Interventions = [ CPR2MinStart; Adrenalin; CPR2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable

            {
                Interventions = [ CPR2MinStart; CPR2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let changeToShockable : ProtocolBlock =
        ChangeToShockable ,
        [
            {
                Interventions = [ CPRStart; ChargeDefib; CPRStop ]
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
                Interventions = [ Shock; CPR2MinStart; VascularAccess; ChargeDefib; CPR2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Second Shock
            {
                Interventions = [ Shock; CPR2MinStart; Consider5H5T; ChargeDefib; CPR2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Third Shock
            {
                Interventions = [ Shock; CPR2MinStart; Adrenalin; Amiodarone; ChargeDefib; CPR2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Fourth Shock
            {
                Interventions = [ Shock; CPR2MinStart; ChargeDefib; CPR2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Fifth Shock
            {
                Interventions = [ Shock; CPR2MinStart; Adrenalin; Amiodarone; ChargeDefib; CPR2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Continue ...
            {
                Interventions = [ Shock; CPR2MinStart; ChargeDefib; CPR2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> Repeatable
            {
                Interventions = [ Shock; CPR2MinStart; Adrenalin; ChargeDefib; CPR2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let changeToNonShockable : ProtocolBlock =
        ChangeToNonShockable,
        [
            {
                Interventions = [ CPR2MinStart; DischargeDefib; CPR2MinStop ]
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
            nonresponsive           // Start with an non responsive patient
            noSignsOfLife           // When there are no signs of life
            noROSC                  // No ROSC after inital 5 breaths
            changeToShockable       // Change to the shockable rhythm block 
            nonShockable            // Start with a non shockable rhythm
            shockable               // Continue in the shockable rhythm block
            changeToNonShockable    // Change to a non shockable rhythm
            changeToROSC            // Change from shockable to ROSC
        ]


    let printEvent e =
        match e with
        | Observed o ->
            match o.Observation with
            | NonResponsive -> "The patient was Non Responsive"
            | NoSignsOfLife -> "The patient showed No Signs of Life"
            | SignsOfLife -> "The patient showed Signs of Life"
            | NoROSC -> "The patient showed No Return Of Spontaneous Circulation"
            | Shockable -> "A Shockable rhythm was observed"
            | NonShockable -> "A NON Shockable rhythm was observed"
            | ChangeToNonShockable -> "The rhythm changed to a NON Shockable rhythm"
            | ChangeToShockable -> "The rhythm changed to a Shockable rhythm"
            | ChangeToROSC -> "There was a Return Of Spontaneous Circulation"
            | ROSC -> "There was a Return Of Spontaneous Circulation"
            |> sprintf "%s. %s" (o.DateTime.ToString("HH:mm:ss"))
        | Intervened i ->
            match i.Intervention with
            | Initial5 -> "5 Initial Breaths were Given"
            | Monitor -> "Defibrillator/Monitor was Attached"
            | CPRStart | CPR2MinStart -> "Cardio Pulmonary Resuscitation was Started"
            | CPRStop | CPR2MinStop -> "Cardio Pulmonary Resuscitation was Stopped"
            | ChargeDefib -> "The Defibrillator was Charged"
            | DischargeDefib -> "The Defibrillator was Discharged"
            | Shock -> "A Shock was given"
            | VascularAccess -> "Vascular Access was obtained"
            | Adrenalin -> "Adrenalin was given"
            | Amiodarone -> "Amiodarone was given"
            | Consider5H5T -> "5H and 5T were considered"
            |> sprintf "%s. %s" (i.DateTime.ToString("HH:mm:ss"))


    let printCommand c =
        match c with
        | Observe o ->
            match o with
            | NonResponsive -> "The patient is NON RESPONSIVE"
            | NoSignsOfLife -> "The patient shows NO SIGNS OF LIFE"
            | SignsOfLife -> "The patient shows SIGNS OF LIFE"
            | NoROSC -> "No Signs of Circulation"
            | Shockable -> "A SHOCKABLE rhythm is observed"
            | NonShockable -> "A NON SHOCKABLE rhythm is observed"
            | ChangeToNonShockable -> "A NON SHOCKABLE rhythm is observed"
            | ChangeToShockable -> "A SHOCKABLE rhythm is observed"
            | ChangeToROSC -> "There is a ROSC"
            | ROSC -> "There is a ROSC"
        | Intervene i ->
            match i with
            | Initial5 -> "5 Initial Breaths Given"
            | Monitor -> "Defibrillator/Monitor Attached"
            | CPRStart | CPR2MinStart -> "CPR Resumed"
            | CPRStop | CPR2MinStop -> "CPR Paused"
            | ChargeDefib -> "Defibrillator Charged"
            | DischargeDefib -> "Defibrillator Discharged"
            | Shock -> "SHOCK Given"
            | VascularAccess -> "Vascular Access Obtained"
            | Adrenalin -> "ADRENALIN Given"
            | Amiodarone -> "AMIODARONE Given"
            | Consider5H5T -> "Causes were considered"


    let getInterventionDescr : Intervention -> Description list = function
        | Initial5 ->    
            [
                "Give 5 Initial Breaths", "Give 5 Initial Breaths"
            ]
        | Monitor ->    
            [
                "Attach Defibrillator/Monitor", "Attach a Defibrillator and or monitor"
            ]
        | CPRStart -> 
            [
                "Resume CPR (15:2)", "Continue with CPR 15 on 2"
            ]
        | CPR2MinStart -> 
            [
                "Resume CPR (15:2) for 2 min", "Continue with CPR, time will show below"
            ]
        | CPRStop ->
            [
                "Pause CPR", "Pause CPR"
            ]
        | CPR2MinStop ->
            [
                "Pause CPR after 2 min", "Continue with CPR until 2 min are over"
            ]
        | ChargeDefib -> 
            [
                "Charge Defibrillator 4 Joules/kg", "Charge the defibrillator with 4 joules per kilogram"
            ]
        | DischargeDefib -> 
            [ 
                "Discharge Defibrillator", "Discharge the defibrillator"
            ]
        | Shock -> 
            [ 
                "Apply Shock", "Clear from patient? Apply Shock."
            ]
        | VascularAccess -> 
            [
                "Obtain Vascular Access", "Obtain vascular access"
            ]
        | Adrenalin -> 
            [
                "Give Adrenalin 10 mcg/kg", "Give Adrenalin 10 micro gram per kilogram"
            ]
        | Amiodarone -> 
            [ 
                "Give Amiodarone 5 mg/kg", "Give Amiodarone 5 milli gram per kilogram"
            ]
        | Consider5H5T -> 
            [ 
                "Consider causes", "Consider causes"
                "- Hypoxia", ""
                "- Hypovoleamia", ""
                "- Hypothermia", ""
                "- Hypo/Hyperkalaemia", ""
                "- Tensionpneumothorax", ""
                "- Thrombosis", ""
                "- Tamponade", ""
                "- Toxic", ""
            ]


module Implementation =

    let createObservedEvent o = { DateTime = DateTime.Now; Observation = o } |> Observed

    let createIntervededEvent i = { DateTime = DateTime.Now; Intervention = i } |> Intervened

    let eventEqsObservation o = function
        | Observed x -> x.Observation = o
        | Intervened _ -> false

    let eventEqsIntervantion i = function
        | Intervened x -> x.Intervention = i
        | Observed _ -> false

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
                |> List.tryFind (fst >> ((=) o.Observation))

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
                    | Observed o -> o.Observation = obs
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
                    [ "Check for Signs Of Life", "Check for signs of life" ], 
                    obs |> List.map Observe 
                | CheckForCirculation obs ->
                    [ "Check Circulation", "Check for signs of adequate circulation" ], 
                    obs |> List.map Observe 
                | CheckRhythm obs -> 
                    [ "Check Rhythm", "Check the rhythm on the monitor" ],
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
                | Observe x   -> { DateTime = DateTime.Now ; Observation = x }  |> Observed
                | Intervene x -> { DateTime = DateTime.Now ; Intervention = x } |> Intervened
                |> List.singleton
                |> List.append es
            let (ds, cs) = 
                es
                |> getCommands

            { 
                Descriptions = ds
                Commands  = cs
                Events = es 
            }
