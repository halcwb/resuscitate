
#time 

#I __SOURCE_DIRECTORY__

#load  "./../../.paket/load/netstandard2.0/Test/test.group.fsx"
#load "./../../src/Shared/Shared.fs"


module Tests =

    open System
    open Shared
    open Implementation

    open Expecto

    let expectIsTrue msg b = Expect.isTrue b msg

    let expectExists ass msg act = Expect.exists act ass msg

    let expectEqual act msg exp = Expect.equal act exp msg

    let nonResponsive = [ NonResponsive |> createObservedEvent ]
    
    let run es =
        es
        |> getCurrentProtocolBranch  Protocol.resuscitation
        |> Option.bind ((removeNonRepeatableFromBranch es) >> Some)
        |> Option.bind (getCurrentProtocolItem)
        |> Option.bind ((fun pi -> getCommandsFromProtocolItem es [ VascularAccess ] pi |> snd) >> Some)
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
            |> pickRand

        let rec run b es =
            if b then 
                sprintf "--- Finished\n\n" |> f
                es
            else
                match es |> procEvs with
                | Some c ->
                    sprintf "Command: %A" c |> f
                    c 
                    |> function 
                    | Observe o -> [ o |> createObservedEvent ]
                    | Intervene i -> [ i |> createIntervededEvent ]
                    |> List.append es |> run false
                
                | None -> 
                    run true es

        let nonResp = NonResponsive |> createObservedEvent
        sprintf "Event: %A" nonResp |> f
        
        [ nonResp ] 
        |> run false


    let checkCmdsLength n cmds =
        let c = cmds |> List.length
        
        c
        |> expectEqual n (sprintf "Should have %i commands, but has %i" n c)

    let testEndEvent (es : Event list) =
        es
        |> List.rev
        |> function
        | [] 
        | [_] -> false
        | x1::x2::_ ->
            (x1 |> eventEqsObservation SignsOfLife) ||
            (x1 |> eventEqsObservation ROSC) ||
            (x2 |> eventEqsObservation ChangeToROSC && (x1 |> eventEqsIntervantion DischargeDefib))
        |> expectIsTrue "Signs of life or ROSC or change to ROSC and uncharge"

    let testChargeDischarge (es : Event list) =
        es
        |> List.fold (fun a e ->
            if a |> fst |> not then a
            else
                match a |> snd with
                // Cannot charge twice at a row
                | Some (Intervened x) when x.Intervention = ChargeDefib && e |> eventEqsIntervantion x.Intervention -> 
                    false, None
                // Otherwise can charge
                | _ when e |> eventEqsIntervantion ChargeDefib -> 
                    true, Some e
                // When charge can shock or decharge
                | Some (Intervened x) when x.Intervention = ChargeDefib 
                                           && (e |> eventEqsIntervantion DischargeDefib ||
                                               e |> eventEqsIntervantion Shock) ->
                    true, None
                // Otherwise cannot discharge or shock
                | _ when e |> eventEqsIntervantion DischargeDefib || e |> eventEqsIntervantion Shock ->
                    false, None
                // Otherwise nothing relevant happened
                | _ -> a

        ) (true, None)
        |> function 
        | false, _ -> false
        | true, Some (Intervened x) when x.Intervention = ChargeDefib-> false // Events end with charged but no discharge occurred
        | true, _ -> true
        |> fun b -> 
            if b then b
            else
                es |> List.iter (Protocol.printEvent >> printfn "%s")
                b
        |> expectIsTrue "Should always first be charged and then be discharged or used to shock"


    module LongRunning =

        let runTests () =
            [1..10000]
            |> List.map (fun n -> 
                if (n % 80) = 0 then printfn "." else printf "."
                n |> runRandom ignore
            )
            |> List.distinct
            |> List.iteri (fun i es ->
                printfn "test %i" i
                es
                |> testChargeDischarge
                es
                |> testEndEvent
            )


    let tests =

        let testRun n =
            n 
            |> (fun n -> if n < 0 then -3 * n else n)
            |> runRandom ignore

        testList "Test resuscitation protocol" [
        
            test "For an non responsive patient" {
                nonResponsive
                |> run
                |> fun cmds ->
                    cmds
                    |> checkCmdsLength 1
                    
                    cmds
                    |> expectExists ((=) (Intervene ShoutForHelp))
                                    "Should shout for help"
            }
        
            test "When help is called" {
                [ ShoutForHelp |> createIntervededEvent ]
                |> List.append nonResponsive
                |> run
                |> fun cmds ->
                    cmds
                    |> checkCmdsLength 1
    
                    cmds
                    |> expectExists ((=) (Intervene OpenAirway))
                                    "Should open airway"

            }

            test "When a patient has signs of life" {
                [ SignsOfLife |> createObservedEvent ]
                |> List.append nonResponsive
                |> run
                |> fun cmds ->
                    cmds
                    |> checkCmdsLength 0

            }

            test "When a patient has no signs of life" {
                [ NoSignsOfLife |> createObservedEvent ]
                |> List.append nonResponsive
                |> run
                |> fun cmds ->
                    cmds
                    |> checkCmdsLength 1
                    
                    cmds 
                    |> expectExists ((=) (Intervene CallForTeam)) 
                                    "Should call reanimation team"

            }

            test "When cpr is paused" {
                [ NoROSC |> createObservedEvent
                  CPRStart |> createIntervededEvent
                  Monitor |> createIntervededEvent
                  CPRStop |> createIntervededEvent ]
                |> List.append nonResponsive
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
                [ NonShockable |> createObservedEvent ]
                |> run
                |> fun cmds -> 
                    cmds
                    |> checkCmdsLength 1
                    // Check command 1
                    cmds
                    |> expectExists ((=) (Intervene CPR2MinStart)) 
                                         "Should start cardiopulmonary resuscitation for 2 minutes"
            }

            test "When a patient has a shockable rhythm" {
                [ NoSignsOfLife |> createObservedEvent
                  Initial5 |> createIntervededEvent
                  ChangeToShockable |> createObservedEvent ]
                |> List.append nonResponsive
                |> run
                |> fun cmds -> 
                    cmds
                    |> checkCmdsLength 1
                    // Check command 1
                    cmds
                    |> expectExists ((=) (Intervene CPRStart)) 
                                         "Should start cardiopulmonary resuscitation"
            }

            test "When a patient has a shockable rhythm and CPR" {
                [ ChangeToShockable |> createObservedEvent
                  CPRStart |> createIntervededEvent ]
                |> List.append nonResponsive
                |> run
                |> fun cmds -> 
                    cmds
                    |> checkCmdsLength 1
                    // Check command 1
                    cmds
                    |> expectExists ((=) (Intervene ChargeDefib)) 
                                         "Should charge the defibrillator"
            }

            test "When cpr has been paused" {
                [ ChangeToShockable |> createObservedEvent
                  CPRStart |> createIntervededEvent
                  ChargeDefib |> createIntervededEvent
                  CPRStop |> createIntervededEvent ]
                |> List.append nonResponsive
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
                
                testProperty "For all possible runs, initial 5 breaths" <| fun n ->
                    n 
                    |> testRun
                    |> List.filter (fun e -> 
                        match e with
                        | Observed o -> false
                        | Intervened i -> i.Intervention = Initial5
                    )
                    |> List.length
                    |> (fun x -> x <= 1)
                    |> expectIsTrue "Should be intervened at most once"

                testProperty "For all possible runs, amiodarone" <| fun n ->
                    n 
                    |> testRun
                    |> List.filter (fun e -> 
                        match e with
                        | Observed o -> false
                        | Intervened i -> i.Intervention = Amiodarone
                    )
                    |> List.length
                    |> (fun x -> x <= 2)
                    |> expectIsTrue "Should be intervened at most twice"

                testProperty "For all possible runs, should end with" <| fun n ->
                    n 
                    |> testRun
                    |> testEndEvent

                testProperty "For all possible runs, the defibrillator" <| fun n ->
                    n 
                    |> testRun
                    |> testChargeDischarge

                testProperty "For all possible runs, vascular acces" <| fun n ->
                    n
                    |> testRun
                    |> List.filter (fun e ->
                        match e with
                        | Intervened i when i.Intervention = VascularAccess -> true
                        | _ -> false
                    )
                    |> List.length
                    |> (fun n -> n <= 1)
                    |> expectIsTrue "Should be performed at most once"

            ]

        ]


open Shared
open Shared.Implementation

open Expecto

let run () =
    Tests.tests
    |> runTests defaultConfig 

Tests.runRandom ignore -3
|> List.iter (Protocol.printEvent >> (printfn "%s"))

Tests.runRandom ignore -3
|> List.iter (printfn "%A")


Tests.LongRunning.runTests ()

[ NonShockable |> createObservedEvent ]
|> Tests.run


run ()

