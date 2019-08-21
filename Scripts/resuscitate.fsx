
#time 

#I __SOURCE_DIRECTORY__

#load  "./../.paket/load/netstandard2.0/Test/test.group.fsx"
#load "./../src/Shared/Shared.fs"


module Tests =

    open System
    open Shared
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


    let runRandom f n =
        sprintf "--- Start run %i" n |> f

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
                sprintf "--- Finished\n\n" |> f
                es
            else
                match es |> procEvs with
                | Some e ->
                    sprintf "Event: %A" e |> f
                    [ e ] |> List.append es |> run false
                | None -> 
                    run true es

        sprintf "Event: %A" (Observed Unresponsive) |> f
        [ Observed Unresponsive ] 
        |> run false


    let checkCmdsLength n cmds =
        let c = cmds |> List.length
        
        c
        |> expectEqual n (sprintf "Should have %i commands, but has %i" n c)


    let testChargeDischarge es =
        es
        |> List.fold (fun a e ->
            if a |> fst |> not then a
            else
                match a |> snd, e with
                // Cannot charge twice at a row
                | Some (Intervened ChargeDefib), Intervened ChargeDefib -> 
                    false, None
                // Otherwise can charge
                | _, Intervened ChargeDefib -> 
                    true, Some e
                // When charge can shock or decharge
                | Some (Intervened ChargeDefib), Intervened DischargeDefib 
                | Some (Intervened ChargeDefib), Intervened Shock -> 
                    true, None
                // Otherwise cannot discharge or shock
                | _, Intervened DischargeDefib 
                | _, Intervened Shock -> 
                    false, None
                // Otherwise nothing relevant happened
                | _, _ -> a

        ) (true, None)
        |> function 
        | false, _ -> false
        | true, Some (Intervened ChargeDefib) -> false // Events end with charged but no discharge occurred
        | true, _ -> true
        |> expectIsTrue "Should always first be charged and then be discharged or used to shock"

    module LongRunning =

        let testChargeDischarge () =
            [1..10000]
            |> List.map (fun n -> n |> runRandom ignore)
            |> List.distinct
            |> List.iteri (fun i es ->
                printfn "test %i" i
                es
                |> testChargeDischarge
            )


    let tests =

        let testRun n =
            n 
            |> (fun n -> if n < 0 then -3 * n else n)
            |> runRandom ignore

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

            test "When the defibrillator has been charged" {
                [ Observed NoSignsOfLife
                  Intervened BLS
                  Observed ChangeToShockable
                  Intervened CPR
                  Intervened ChargeDefib]
                |> List.append es
                |> run
                |> fun cmds -> 
                    cmds
                    |> checkCmdsLength 3
                    // Check command 1
                    cmds
                    |> expectExists ((=) (Observe Shockable)) 
                                         "Should check for a shockable rhythm"
                    // Check command 2
                    cmds
                    |> expectExists ((=) (Observe ChangeToNonShockable)) 
                                         "Should check for a change to non shockable rhythm"
                    // Check command 3
                    cmds
                    |> expectExists ((=) (Observe ChangeToROSC)) 
                                         "Should check for a change to ROSC"
            }

            testList "Random runs" [
                
                testProperty "For all possible runs, BLS" <| fun n ->
                    n 
                    |> testRun
                    |> List.filter ((=) (Intervened BLS ))
                    |> List.length
                    |> (fun x -> x <= 1)
                    |> expectIsTrue "Should be intervened at most once"

                testProperty "For all possible runs, amiodarone" <| fun n ->
                    n 
                    |> testRun
                    |> List.filter ((=) (Intervened BLS ))
                    |> List.length
                    |> (fun x -> x <= 2)
                    |> expectIsTrue "Should be intervened at most twice"

                testProperty "For all possible runs, should end with" <| fun n ->
                    n 
                    |> testRun
                    |> List.rev
                    |> function
                    | [] 
                    | [_] -> false
                    | x1::x2::_ ->
                        x1 = (Observed SignsOfLife) ||
                        x1 = (Observed ROSC) ||
                        (x2 = (Observed ChangeToROSC) && x1 = (Intervened DischargeDefib))
                    |> expectIsTrue "Signs of life or ROSC or change to ROSC and uncharge"

                testProperty "For all possible runs, the defibrillator" <| fun n ->
                    n 
                    |> testRun
                    |> testChargeDischarge

            ]

        ]


open Shared
open Shared.Implementation

let printEvent e =
    match e with
    | Observed o ->
        match o with
        | Unresponsive -> "The patient is non responsive"
        | NoSignsOfLife -> "The patient shows no signs of life"
        | SignsOfLife -> "The patient shows signs of life"
        | Shockable -> "A shockable rhythm is observed"
        | NonShockable -> "A NON shockable rhythm is observed"
        | ChangeToNonShockable -> "The rhythm has changed to a NON shockable rhythm"
        | ChangeToShockable -> "The rhythm has changed to a shockable rhythm"
        | ChangeToROSC -> "There is a return of spontaneous circulation"
        | ROSC -> "There is a return of spontaneous circulation"
    | Intervened i ->
        match i with
        | BLS -> "Basic Life Support has been given"
        | CPR -> "Continous Pulmonary Resuscitation has started"
        | ChargeDefib -> "The defibrillator has been charged"
        | DischargeDefib -> "The defibrillator has been discharged"
        | Shock -> "A shock has been given"
        | Adrenalin -> "Adrenalin was given"
        | Amiodarone -> "Amiodarone was given"


let printCommand c =
    match c with
    | Observe o ->
        match o with
        | Unresponsive -> "The patient is non responsive"
        | NoSignsOfLife -> "The patient shows no signs of life"
        | SignsOfLife -> "The patient shows signs of life"
        | Shockable -> "A shockable rhythm is observed"
        | NonShockable -> "A NON shockable rhythm is observed"
        | ChangeToNonShockable -> "The rhythm has changed to a NON shockable rhythm"
        | ChangeToShockable -> "The rhythm has changed to a shockable rhythm"
        | ChangeToROSC -> "There is a return of spontaneous circulation"
        | ROSC -> "There is a return of spontaneous circulation"
    | Intervene i ->
        match i with
        | BLS -> "Start Basic Life support"
        | CPR -> "Resume Continous Pulmonary Resuscitation"
        | ChargeDefib -> "Charge the defibrillator"
        | DischargeDefib -> "Discharge the defibrillator"
        | Shock -> "Apply a shock"
        | Adrenalin -> "Give adrenalin"
        | Amiodarone -> "Give amiodarone"


open Expecto

let run () =
    Tests.tests
    |> runTests defaultConfig 

Tests.runRandom ignore 79
|> List.iter (printEvent >> (printfn "%s"))

Tests.LongRunning.testChargeDischarge ()

    