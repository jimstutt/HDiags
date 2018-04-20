> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TypeFamilies #-}

> import Data.List.Split
> import Data.Maybe
> import Diagrams.BoundingBox
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.TwoD.Text
> import Graphics.SVGFonts

> dia1 = pad 1.1 . centerXY $ sSquare <> drawLines sSquare <> square 20
>                                 # fc whitesmoke
>                                 # scaleY 1.6
>                                 # translateX 8
>                                 # translateY (-4)

> box parts padding =
>     let padded =
>           strutY padding
>           ===
>           (strutX padding ||| centerXY parts ||| strutX padding)
>           ===
>           strutY padding
>         height = diameter (r2 (0,2)) padded
>         width  = diameter (r2 (3,0)) padded
>     in centerXY parts <> roundedRect width height 0.1

> textOpts n = TextOpts lin2 INSIDE_H KERN False 1 n

> text' :: String -> Double -> Diagram B
> text' s n = textSVG_ (textOpts n) s # fc black # lw none

>-- centredText ls n = vcat' (with & catMethod .~ Distrib & sep .~ n)
>--                      (map (\l -> centerX (text' l n)) ls)
>-- centredText' s = centredText (splitOn "\n" s)

> padAmount = 0.5

> top = r2 (7, 9)
> upright = r2 (7, 5)
> down = r2 (0, -10)
> right = r2 (15, 0)

> dia2 = atPoints locs circles

> circles =
>         [ aCircle "s"
>         , aCircle "h"
>         , aCircle "t"
>         , aCircle "p1"
>         , aCircle "p2"
>         , aCircle "f"
>         , aCircle "g"
>         ]

> locs = map p2 [(-6,2),(-2,3),(5,2),(-4,-2),(3,-2),(-5,-10),(4,-10)]

> aBox s n = (box (text' {-centredText'-} s 1) padAmount) # named n

> aCircle s = text s # fontSizeL 1 # fc black <> circle 1 # fc yellow

> sSquare :: Diagram B
> sSquare = fc lightgreen $ mconcat
>   [aBox "PULLBACK" "pbox"        # translate top
>   , aBox "V" "vbox"              # translate upright
>   , aBox "AB" "ab"               # translate (upright ^+^ down)
>   , aBox "A" "abox"              # translate down
>   , aBox "B" "bbox"              # translate (down ^+^ right)
>   , aBox "U" "ubox"              # translate (down ^+^ upright ^+^down)
>   ]

> drawLines :: Diagram B -> Diagram B
> drawLines square = foldr (.) id (map
>                                   (uncurry (connectOutside' arrowStyle))
>                                   arrs
>                                 ) square

> arrowStyle = (with & headLength .~ small & shaftStyle %~ lw thick)

> arrs = [("vbox","abox")
>        , ("vbox","ab")
>        , ("vbox","bbox")
>        , ("ab","abox")
>        , ("ab","bbox")
>        , ("abox","ubox")
>        , ("bbox","ubox")
>        ]

> main = mainWith (atop (dia2:: Diagram B) (dia1 :: Diagram B))
