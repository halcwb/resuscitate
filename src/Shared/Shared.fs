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
    | ShoutForHelp
    | OpenAirway
    | Initial5
    | CallForTeam
    | Monitor
    | CPRStart
    | CPR2MinStart
    | CPRStop
    | CPR2MinStop of int
    | ChargeDefib of int
    | DischargeDefib
    | Shock
    | VascularAccess
    | Lab
    | Adrenalin
    | Amiodarone
    | ConsiderCauses

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
    Event list -> Intervention list -> ProtocolItem -> Description list * Command list

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
                Interventions = [ ShoutForHelp; OpenAirway]
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
                    Interventions = [ CallForTeam; Initial5 ] // Start BLS
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
        let cpr2MinStop = CPR2MinStop 120
        NonShockable,
        [
            {
                Interventions = [ CPR2MinStart; VascularAccess; Lab; cpr2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable

            {
                Interventions = [ CPR2MinStart; Adrenalin; cpr2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable

            {
                Interventions = [ CPR2MinStart; ConsiderCauses; cpr2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> NonRepeatable

            {
                Interventions = [ CPR2MinStart; Adrenalin; cpr2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable

            {
                Interventions = [ CPR2MinStart; cpr2MinStop ]
                Evaluation = 
                    [ NonShockable; ChangeToShockable; ROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let changeToShockable : ProtocolBlock =
        ChangeToShockable ,
        [
            {
                Interventions = [ CPRStart; ChargeDefib 0; CPRStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let shockable : ProtocolBlock =
        let chargeDefib = ChargeDefib 105
        let cpr2MinStop = CPR2MinStop 120
        Shockable,
        [
            // First Shock
            {
                Interventions = [ Shock; CPR2MinStart; VascularAccess; Lab; chargeDefib; cpr2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Second Shock
            {
                Interventions = [ Shock; CPR2MinStart; ConsiderCauses; chargeDefib; cpr2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Third Shock
            {
                Interventions = [ Shock; CPR2MinStart; Adrenalin; Amiodarone; chargeDefib; cpr2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Fourth Shock
            {
                Interventions = [ Shock; CPR2MinStart; chargeDefib; cpr2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Fifth Shock
            {
                Interventions = [ Shock; CPR2MinStart; Adrenalin; Amiodarone; chargeDefib; cpr2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> NonRepeatable
            // Continue ...
            {
                Interventions = [ Shock; CPR2MinStart; chargeDefib; cpr2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> Repeatable
            {
                Interventions = [ Shock; CPR2MinStart; Adrenalin; chargeDefib; cpr2MinStop ]
                Evaluation = 
                    [ Shockable; ChangeToNonShockable; ChangeToROSC ]
                    |> CheckRhythm
            } |> Repeatable
        ]

    let changeToNonShockable : ProtocolBlock =
        ChangeToNonShockable,
        [
            {
                Interventions = [ CPR2MinStart; DischargeDefib; CPR2MinStop 120 ]
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
            | ShoutForHelp -> "Help was Invoked"
            | OpenAirway -> "Airway was Opened"
            | CallForTeam -> "Resuscitation Team was Called"
            | Initial5 -> "5 Initial Breaths were Given"
            | Monitor -> "Defibrillator/Monitor was Attached"
            | CPRStart | CPR2MinStart -> "Cardio Pulmonary Resuscitation was Started"
            | CPRStop | CPR2MinStop _ -> "Cardio Pulmonary Resuscitation was Stopped"
            | ChargeDefib _ -> "The Defibrillator was Charged"
            | DischargeDefib -> "The Defibrillator was Discharged"
            | Shock -> "A Shock was given"
            | VascularAccess -> "Vascular Access was obtained"
            | Lab -> "Lab was requested"
            | Adrenalin -> "Adrenalin was given"
            | Amiodarone -> "Amiodarone was given"
            | ConsiderCauses -> "Causes were considered"
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
            | ShoutForHelp -> "Help Called"
            | OpenAirway -> "Airway Opened"
            | CallForTeam -> "Resuscitation Team Called"
            | Initial5 -> "5 Initial Breaths Given"
            | Monitor -> "Defibrillator/Monitor Attached"
            | CPRStart | CPR2MinStart -> "CPR Resumed"
            | CPRStop | CPR2MinStop _ -> "CPR Paused"
            | ChargeDefib _ -> "Defibrillator Charged"
            | DischargeDefib -> "Defibrillator Discharged"
            | Shock -> "SHOCK Given"
            | VascularAccess -> "Vascular Access Obtained"
            | Lab -> "Lab was Requested"
            | Adrenalin -> "ADRENALIN Given"
            | Amiodarone -> "AMIODARONE Given"
            | ConsiderCauses -> "Causes were considered"


    let getInterventionDescr : Intervention -> Description list = function
        | ShoutForHelp -> 
            [
                "Get Help", "Get help"
            ]
        | OpenAirway ->
            [
                "Open Airway", "Open the airway"
            ]
        | CallForTeam ->
            [
                "Call the Resuscitation Team", "Call the resuscitation team"
            ]
        | Initial5 ->    
            [
                "Give 5 Initial Breaths", "Give 5 initial breaths"
            ]
        | Monitor ->    
            [
                "Attach Defibrillator/Monitor", "Attach a defibrillator and or monitor"
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
        | CPR2MinStop _ ->
            [
                "Pause CPR after 2 min", "Continue with CPR until 2 min are over"
            ]
        | ChargeDefib _ -> 
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
        | Lab ->
            [
                "Request Lab", "Request lab"
                "Glucose", ""
                "Electrolytes", ""
                "Blood Culture", ""
                "Blood Type", ""
                "Blood Gas", ""
            ]

        | Adrenalin -> 
            [
                "Give Adrenalin 10 mcg/kg", "Give Adrenalin 10 micro gram per kilogram"
            ]
        | Amiodarone -> 
            [ 
                "Give Amiodarone 5 mg/kg", "Give Amiodarone 5 milli gram per kilogram"
            ]
        | ConsiderCauses -> 
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
        fun es is pi ->
            // calculate with intervention should be run
            let n =
                es 
                |> List.rev
                |> List.fold (fun a e ->
                    match e with
                    | Observed _   -> (false, a |> snd)
                    | Intervened x -> 
                        if a |> fst |> not then a
                        else (true, (a |> snd) + 1)
                ) (true, 0)
                |> snd

            let ib = 
                match pi with
                | Repeatable b
                | NonRepeatable b -> b

            // skip the intervention that only runs once 
            let filtered = 
                ib.Interventions
                |> List.filter (fun i ->
                    if is |> List.exists ((=) i) |> not then true
                    else 
                        // use only events from previous intervention blocks
                        es
                        |> List.rev
                        |> List.fold (fun acc e ->
                            if acc |> fst then (true, e::(acc |> snd))
                            else
                                match e with
                                | Observed _   -> (true, [])
                                | Intervened _ -> acc
                        ) (false, [])
                        |> snd
                        // look if the intervention allready happened
                        |> List.exists (fun e ->
                            match e with
                            | Observed _ -> false
                            | Intervened x -> x.Intervention = i 
                        ) |> not
                )

            if n >= 0 && ((filtered |> List.length) > n) then 
                let cs =
                    filtered.[n]
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


    let getCommands is es =
        es
        |> getCurrentProtocolBranch  Protocol.resuscitation
        |> Option.bind ((removeNonRepeatableFromBranch es) >> Some)
        |> Option.bind (getCurrentProtocolItem)
        |> Option.bind ((getCommandsFromProtocolItem es is) >> Some)
        |> Option.defaultValue ([], [])

    let init is es =
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
                |> getCommands [ VascularAccess; Lab; ConsiderCauses ]

            { 
                Descriptions = ds
                Commands  = cs
                Events = es 
            }
