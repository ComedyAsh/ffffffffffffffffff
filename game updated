type Msg = Tick Float GetKeyState
         | StartGame
         | Info
         | SubmitAnswer Answer Answer
         | Reset

type GameState = MainMenu
               | Instructions
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
                    level4,
                    level5,
                    level6,
                    level7,
                    level8
                  ]
       , chances = 2
       , time = 0  -- This is specifically ANIMATION time.

         -- Below are a set of variables that aren't used in the template but
         -- maybe you can figure out how to use them? You can add more too!
       , score =  100
       , timeremaining = 20
       , highscore = 0
       , current = 0
       }

--- VIEW ---

view model = case model.state of
                MainMenu -> collage 1000 500 (menuView model)
                Instructions -> collage 1000 500 (instructionsView model)
                InGame   -> collage 1000 500 (levelView (List.head model.levels) model.time model.chances model.timeremaining model.score)
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
                   , group [ rect 200 75
                              |> filled blue
                              |> move(0, -150)
                             , text "Instructions"
                              |> size 40
                              |> centered
                              |> filled white
                              |> move(0, -160)
                           ] |> notifyMouseDown Info
                 ]

instructionsView model = [ group [ rect 200 100
                                    |> filled green
                                   , text "Based on the image provided, select \n the corresponding superhero. \n Faster guesses give more score"
                                    |> size 12
                                    |> centered
                                    |> filled white
                                  ]
                           , group [ circle 50
                                      |> filled blue
                                      |> move (0, -150)
                                    , text "Back"
                                      |> size 40
                                      |> centered
                                      |> filled white
                                      |> move(0, -160)
                                   ] |> notifyMouseDown Reset
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

levelView level t chances timeremaining score = case level of
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
                                            , text (toString timeremaining)
                                                |> size 20
                                                |> filled black
                                                |> move (0,200)
                                                ,text(toString score)
                                                |> size 20
                                                |>filled black
                                                |>move (-200,0)
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
         , optionA = "Batman"
         , optionB = "Spiderman"
         , optionC = "Hulk"
         , optionD = "Batwoman"
         , answer = A
         }

level1_image t =  [ oval 200 120 
                     |> filled yellow
                     |> addOutline (solid 6) black
                   , oval 180 98
                     |> filled black
                   , curve (0,0) [Pull (0,0) (-40,0), Pull (-28,80) (0,0) ]
                     |> filled yellow
                     |> move(0, -50)
                   , curve (0,0) [Pull (0,0) (-40,0), Pull (-36,60) (0,0) ]
                     |> filled yellow
                     |> rotate (degrees -15)
                     |> move(-30, -50)
                   , curve (0,0) [Pull (0,0) (40,0), Pull (28,80) (0,0) ]
                     |> filled yellow
                     |> move(0, -50)
                   , curve (0,0) [Pull (0,0) (40,0), Pull (36,60) (0,0) ]
                     |> filled yellow
                     |> rotate (degrees 15)
                     |> move(30, -50)
                   , curve (0,0) [Pull (0,0) (-46,0), Pull (-20,-64) (-12,0) ]
                     |> filled yellow
                     |> move(-8, 50)
                     |> rotate (degrees 5)
                   , curve (0,0) [Pull (0,0) (46,0), Pull (20,-64) (12,0) ]
                     |> filled yellow
                     |> move(8, 50)
                     |> rotate (degrees -5)
                   , polygon [(-16,0),(16,0),(10,-16),(-10,-16)]
                     |> filled yellow
                     |> move(0, 50)
                 ]

level2 = { image = level2_image
         , optionA = "Aquaman"
         , optionB = "The Flash"
         , optionC = "Iron Man"
         , optionD = "War Machine"
         , answer = C
         }
         
ironmanblue t = rgb ((abs (sin (t / 10))) * 200) ((abs (sin (t / 10))) * 200 + 30) 255

level2_image t = [ circle 100
                     |> filled (rgb 38 38 38)
                     |> addOutline (solid 10) (ironmanblue t)
                     |> move(0, 10)
                   , triangle 60
                     |> filled black
                     |> addOutline (solid 10) blue
                     |> move (0, 10)
                     |> rotate (degrees -90)
                   , triangle 25
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> rotate (degrees -90)
                     |> move(0, 10)
                   , polygon [(-16,0),(16,0),(10,-16),(-10,-16)]
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(-65, -20)
                     |> rotate (degrees 120)
                   , polygon [(-16,0),(16,0),(10,-16),(-10,-16)]
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(65, -20)
                     |> rotate (degrees -120)
                   , polygon [(-16,0),(16,0),(10,-16),(-10,-16)]
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(0, 75)
                   , triangle 10
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(-30, -55)
                     |> rotate(degrees 90)
                   , triangle 10
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(30, -55)
                     |> rotate(degrees 90)
                   , triangle 10
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(-45, 65)
                     |> rotate(degrees 210)
                   , triangle 10
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(45, 65)
                     |> rotate(degrees 210)
                   , triangle 10
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(-75, 25)
                     |> rotate(degrees 210)
                   , triangle 10
                     |> filled (ironmanblue t)
                     |> addOutline (solid 4) blue
                     |> move(75, 25)
                     |> rotate(degrees 210)
                  ]

level3 = { image = level3_image
         , optionA = "Starhawk"
         , optionB = "The Flash"
         , optionC = "Storm"
         , optionD = "Electro"
         , answer = B
         }

level3_image t = [ circle 100 
                    |> filled red
                    |> addOutline (solid 10) black
                    |> move (0, 20)
                   , polygon [(5,40),(65,100),(45,60),(65,60),(30,20),(55,20),(-10,-50),(15,0),(-10,0),(25,40)]
                    |> filled yellow
                    |> addOutline (solid 5) black
                    |> scale 1.5
                    |> move (-40, -20)
                 ]

level4 = { image = level4_image
         , optionA = "Thor"
         , optionB = "Deadshot"
         , optionC = "Green Lantern"
         , optionD = "Hawkeye"
         , answer = D
         }

arrow t = group [ polygon [(-70,15),(-90,0),(-70,-15),(-75,-5),(90,-5),(90,5),(-75,5)]
                     |> filled black
                     |> move (-15, 30)
                     |> rotate (degrees -20)
                     |> scale 0.7
                  , polygon [(-70,0),(-45,0),(-45,10)]
                     |> filled (rgb 83 163 109)
                     |> scale 0.7
                     |> move (74, 1)
                     |> rotate (degrees -20)
                  , polygon [(-70,0),(-45,0),(-45,10)]
                     |> filled (rgb 83 163 109)
                     |> scale 0.7
                     |> mirrorY
                     |> move (72, -5)
                     |> rotate (degrees -20)
                 ]
                 
level4_image t = [ circle 100
                     |> filled (rgb 222 232 249)
                     |> move (-15, 5)
                     |> addOutline (solid 5) black
                   , polygon [(0,70),(-25,30),(-30,0),(-25,-30),(0,-70),(-15,-30),(-20,0),(-15,30)]
                     |> filled black
                     |> scale 1.2
                     |> rotate (degrees -15)
                   , arrow t
                     |> move ((abs(sin (t / 15)) * -140), (abs(sin (t / 15)) * 70) - 20)
                 ]

level5 = { image = level5_image
         , optionA = "iron man"
         , optionB = "thor"
         , optionC = "hulk"
         , optionD = "Captain America"
         , answer = D
         }
         
star = polygon [(cos(degrees 0),sin(degrees 0))
               ,(cos(degrees 144),sin(degrees 144))
               ,(cos(degrees 288),sin(degrees 288))
               ,(cos(degrees 432),sin(degrees 432))
               ,(cos(degrees 576),sin(degrees 576))]
               
level5_image t = [ circle 70 |> filled red 
                 , circle 60 |> filled white, circle 50 |> filled red
                 , circle 30 |> filled blue
                 , star |> filled white |> scale 20 |> rotate (t/5) ]


level6 = { image = level6_image
         , optionA = "aquaman"
         , optionB = "superwoman"
         , optionC = "black widow"
         , optionD = "wonder woman"
         , answer = C
         }

level6_image t = [ circle 70 |> filled yellow, circle 55 |> filled black
                ,polygon  [(25,0),(0,20),(25,20),(0,0),(0,0),(0,0),(0,0)] |>filled red |>scale 2 |> move(-25, -20)]

level7 = { image = level7_image
         , optionA = "Wade Willson"
         , optionB = "Tony Stark"
         , optionC = "hulk"
         , optionD = "deadpool"
         , answer = D
         }

level7_image t = [circle 70 |>filled red, wedge 50 0.5 |>filled black |> rotate (degrees 180), wedge 50 0.5 |>filled black |> rotate(degrees 0), rect 100 10 |>filled red |>rotate (degrees 90), circle 10 |> filled white |>move (20,0), circle 10 |> filled white |>move (-20,0) ]

level8 = { image = level8_image
         , optionA = "clark"
         , optionB = "superman"
         , optionC = "Deadpool"
         , optionD = "clint"
         , answer = B
         }

level8_image t = [ roundedRect 210 10 20 |> filled green |>rotate (degrees 30),circle 50 |>filled blue, 
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
                                                            then EndOfGame else if model.timeremaining <= 0 then
                                                            Failure
                                                            else model.state
  
                                    ,         time = model.time + 1
                                    ,         timeremaining =  if model.state == InGame then model.timeremaining - 0.100 else model.timeremaining}
                        StartGame -> { model | state = InGame}
                        SubmitAnswer ans1 ans2 -> if ans1 == ans2
                                                    then nextLevel model
                                                    else wrongAnswer model
                        Reset -> init
                        Info -> { model | state = Instructions}

nextLevel model = {model | levels = Maybe.withDefault [] (List.tail model.levels) , time = 0, score = model.score +100, timeremaining = 20}

wrongAnswer model = case model.chances of
                        0 -> {model | state = Failure}
                        _ -> {model | chances = model.chances - 1}
