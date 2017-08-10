type Msg = Tick Float GetKeyState
         | StartGame
         | SubmitAnswer Answer Answer
         | Reset

type GameState = MainMenu
               | InGame
               | EndOfGame
               | Failure

type Answer = A | B | C | D

main =
    gameApp Tick {   model = init
                 ,   view = view
                 ,   update = update
                 }

--- MODEL ---

init = { state = MainMenu
       , levels = [ level1,
                    level2,
                    level3,
                    level4
                  ]
       , chances = 2
       , time = 0  -- This is specifically ANIMATION time.

         -- Below are a set of variables that aren't used in the template but
         -- maybe you can figure out how to use them? You can add more too!
       , score = 0
       , timelimit = 0
       , highscore = 0
       , current = 0
       }

--- VIEW ---

view model = case model.state of
                MainMenu -> collage 1000 500 (menuView model)
                InGame   -> collage 1000 500 (levelView (List.head model.levels) model.time model.chances)
                EndOfGame   -> collage 1000 500 (endView model)
                Failure  -> collage 1000 500 (failView model)

menuView model = [ group [ circle 100
                            |> filled green
                         , text "START"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (0,-15)
                         ] |> notifyMouseDown StartGame
                 ]

endView model = [ group [ circle 100
                            |> filled yellow
                         , text "RESET"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (0,-15)
                         ] |> notifyMouseDown Reset
                 ]

failView model = [ group [ circle 100
                            |> filled darkRed
                         , text "RESET"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (0,-15)
                         ] |> notifyMouseDown Reset
                 ]

levelView level t chances = case level of
                                 Nothing -> []
                                 Just lev ->  [ group (lev.image t)
                                            , option A lev.optionA
                                                |> move (-150,-120)
                                                |> notifyMouseDown (SubmitAnswer A lev.answer)
                                            , option B lev.optionB
                                                |> move (-150,-160)
                                                |> notifyMouseDown (SubmitAnswer B lev.answer)
                                            , option C lev.optionC
                                                |> move (150,-120)
                                                |> notifyMouseDown (SubmitAnswer C lev.answer)
                                            , option D lev.optionD
                                                |> move (150,-160)
                                                |> notifyMouseDown (SubmitAnswer D lev.answer)
                                            , text "Extra chances"
                                                |> filled red
                                                |> move (200,100)
                                            , group (displayChances chances)
                                                |> move (200,150)
                                           ]

displayChances chances = case chances of
                            0 -> []
                            _ -> [heart red
                                    |> scale (0.5)
                                    |> move (0 +  chances * 100,0) ] ++ (displayChances (chances - 1))

option ans tex = group [ rectangle 200 30
                            |> filled orange
                       , text ((toString ans) ++ ": " ++ tex)
                            |> size 20
                            |> filled white
                            |> move (-90,-7) ]


level1 = { image = level1_image
         , optionA = "iron man"
         , optionB = "thor"
         , optionC = "hulk"
         , optionD = "Captain America"
         , answer = D
         }

level1_image t = [ circle 70 |> filled red 
                 , circle 60 |> filled white, circle 50 |> filled red
                 , circle 30 |> filled blue
                 , star |> filled white |> scale 20 |> rotate (t/5) ]


level2 = { image = level2_image
         , optionA = "aquaman"
         , optionB = "superwoman"
         , optionC = "black widow"
         , optionD = "wonder woman"
         , answer = C
         }

level2_image t = [ circle 70 |> filled yellow, circle 55 |> filled black
                ,polygon  [(25,0),(0,20),(25,20),(0,0),(0,0),(0,0),(0,0)] |>filled red |>scale 2 |> move(-25, -20)]

level3 = { image = level3_image
         , optionA = "Wade Willson"
         , optionB = "Tony Stark"
         , optionC = "hulk"
         , optionD = "deadpool"
         , answer = D
         }

level3_image t = [circle 70 |>filled red, wedge 50 0.5 |>filled black |> rotate (degrees 180), wedge 50 0.5 |>filled black |> rotate(degrees 0), rect 100 10 |>filled red |>rotate (degrees 90), circle 10 |> filled white |>move (20,0), circle 10 |> filled white |>move (-20,0) ]

level4 = { image = level4_image
         , optionA = "clark"
         , optionB = "superman"
         , optionC = "Deadpool"
         , optionD = "clint"
         , answer = B
         }

level4_image t = [ roundedRect 210 10 20 |> filled green |>rotate (degrees 30),circle 50 |>filled blue, 
                    text "Daily Planet" |> size 50 |> filled darkCharcoal |>move (10,80) ]
heart c = group [circle 50
            |> filled c
            |> move (0,50)
         ,
          circle 50
            |> filled c
            |> move (50,0)
         ,
          square 100
            |> filled c ] |> rotate (degrees 45)


--- UPDATE ---

update msg model = case msg of
                        Tick t _ -> { model | state = if model.state == InGame && model.levels == []
                                                            then EndOfGame
                                                            else model.state
                                    ,         time = model.time + 1}
                        StartGame -> { model | state = InGame}
                        SubmitAnswer ans1 ans2 -> if ans1 == ans2
                                                    then nextLevel model
                                                    else wrongAnswer model
                        Reset -> init

nextLevel model = {model | levels = Maybe.withDefault [] (List.tail model.levels) , time = 0}

star = polygon [(cos(degrees 0),sin(degrees 0))
               ,(cos(degrees 144),sin(degrees 144))
               ,(cos(degrees 288),sin(degrees 288))
               ,(cos(degrees 432),sin(degrees 432))
               ,(cos(degrees 576),sin(degrees 576))]

wrongAnswer model = case model.chances of
                        0 -> {model | state = Failure}
                        _ -> {model | chances = model.chances - 1}
