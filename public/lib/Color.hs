module Color where

import Internal (removeFloat)

-- Data structure to represent CSS colors
data Color
    = RGB Int Int Int
    | RGBA Int Int Int Float
    | Hex String
    | HSL Int Int Int
    | HSLA Int Int Int Float
    | Transparent
    | AliceBlue
    | AntiqueWhite
    | Aqua
    | AquaMarine
    | Azure
    | Beige
    | Bisque
    | Black
    | BlanchedAlmond
    | Blue
    | BlueViolet
    | Brown
    | BurlyWood
    | CadetBlue
    | Chartreuse
    | Chocolate
    | Coral
    | CornflowerBlue
    | Cornsilk
    | Crimson
    | Cyan
    | DarkBlue
    | DarkCyan
    | DarkGoldenRod
    | DarkGray
    | DarkGreen
    | DarkGrey
    | DarkKhaki
    | DarkMagenta
    | DarkOliveGreen
    | DarkOrange
    | DarkOrchid
    | DarkRed
    | DarkSalmon
    | DarkSeaGreen
    | DarkSlateBlue
    | DarkSlateGray
    | DarkSlateGrey
    | DarkTurquoise
    | DarkViolet
    | DeepPink
    | DeepSkyBlue
    | DimGray
    | DimGrey
    | DodgerBlue
    | FireBrick
    | FloralWhite
    | ForestGreen
    | Fuchsia
    | Gainsboro
    | GhostWhite
    | Gold
    | GoldenRod
    | Gray
    | Green
    | GreenYellow
    | Grey
    | HoneyDew
    | HotPink
    | IndianRed
    | Indigo
    | Ivory
    | Khaki
    | Lavender
    | LavenderBlush
    | LawnGreen
    | LemonChiffon
    | LightBlue
    | LightCoral
    | LightCyan
    | LightGoldenRodYellow
    | LightGray
    | LightGreen
    | LightGrey
    | LightPink
    | LightSalmon
    | LightSeaGreen
    | LightSkyBlue
    | LightSlateGray
    | LightSlateGrey
    | LightSteelBlue
    | LightYellow
    | Lime
    | LimeGreen
    | Linen
    | Magenta
    | Maroon
    | MediumAquaMarine
    | MediumBlue
    | MediumOrchid
    | MediumPurple
    | MediumSeaGreen
    | MediumSlateBlue
    | MediumSpringGreen
    | MediumTurquoise
    | MediumVioletRed
    | MidnightBlue
    | MintCream
    | MistyRose
    | Moccasin
    | NavajoWhite
    | Navy
    | Oldlace
    | Olive
    | OliveDrab
    | Orange
    | OrangeRed
    | Orchid
    | PaleGoldenRod
    | PaleGreen
    | PaleTurquoise
    | PaleVioletRed
    | PapayaWhip
    | PeachPuff
    | Peru
    | Pink
    | Plum
    | PowderBlue
    | Purple
    | Red
    | RosyBrown
    | RoyalBlue
    | SaddleBrown
    | Salmon
    | SandyBrown
    | SeaGreen
    | SeaShell
    | Sienna
    | Silver
    | SkyBlue
    | SlateBlue
    | SlateGray
    | SlateGrey
    | Snow
    | SpringGreen
    | SteelBlue
    | Tan
    | Teal
    | Thistle
    | Tomato
    | Turquoise
    | Violet
    | Wheat
    | White
    | WhiteSmoke
    | Yellow
    | YellowGreen
    deriving (Eq)

-- Convert a color to a JSON string
instance Show Color where
    show :: Color -> String
    show (RGB r g b) = "\"rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")\""
    show (RGBA r g b a) = "\"rgba(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ removeFloat a ++ ")\""
    show (Hex ('#' : hex)) = "\"#" ++ hex ++ "\""
    show (Hex hex) = "\"#" ++ hex ++ "\""
    show (HSL h s l) = "\"hsl(" ++ show h ++ "," ++ show s ++ "%," ++ show l ++ "%)\""
    show (HSLA h s l a) = "\"hsla(" ++ show h ++ "," ++ show s ++ "%," ++ show l ++ "%," ++ removeFloat a ++ ")\""
    show Transparent = "0"
    show AliceBlue = "1"
    show AntiqueWhite = "2"
    show Aqua = "3"
    show AquaMarine = "4"
    show Azure = "5"
    show Beige = "6"
    show Bisque = "7"
    show Black = "8"
    show BlanchedAlmond = "9"
    show Blue = "10"
    show BlueViolet = "11"
    show Brown = "12"
    show BurlyWood = "13"
    show CadetBlue = "14"
    show Chartreuse = "15"
    show Chocolate = "16"
    show Coral = "17"
    show CornflowerBlue = "18"
    show Cornsilk = "19"
    show Crimson = "20"
    show Cyan = "21"
    show DarkBlue = "22"
    show DarkCyan = "23"
    show DarkGoldenRod = "24"
    show DarkGray = "25"
    show DarkGreen = "26"
    show DarkGrey = "27"
    show DarkKhaki = "28"
    show DarkMagenta = "29"
    show DarkOliveGreen = "30"
    show DarkOrange = "31"
    show DarkOrchid = "32"
    show DarkRed = "33"
    show DarkSalmon = "34"
    show DarkSeaGreen = "35"
    show DarkSlateBlue = "36"
    show DarkSlateGray = "37"
    show DarkSlateGrey = "38"
    show DarkTurquoise = "39"
    show DarkViolet = "40"
    show DeepPink = "41"
    show DeepSkyBlue = "42"
    show DimGray = "43"
    show DimGrey = "44"
    show DodgerBlue = "45"
    show FireBrick = "46"
    show FloralWhite = "47"
    show ForestGreen = "48"
    show Fuchsia = "49"
    show Gainsboro = "50"
    show GhostWhite = "51"
    show Gold = "52"
    show GoldenRod = "53"
    show Gray = "54"
    show Green = "55"
    show GreenYellow = "56"
    show Grey = "57"
    show HoneyDew = "58"
    show HotPink = "59"
    show IndianRed = "60"
    show Indigo = "61"
    show Ivory = "62"
    show Khaki = "63"
    show Lavender = "64"
    show LavenderBlush = "65"
    show LawnGreen = "66"
    show LemonChiffon = "67"
    show LightBlue = "68"
    show LightCoral = "69"
    show LightCyan = "70"
    show LightGoldenRodYellow = "71"
    show LightGray = "72"
    show LightGreen = "73"
    show LightGrey = "74"
    show LightPink = "75"
    show LightSalmon = "76"
    show LightSeaGreen = "77"
    show LightSkyBlue = "78"
    show LightSlateGray = "79"
    show LightSlateGrey = "80"
    show LightSteelBlue = "81"
    show LightYellow = "82"
    show Lime = "83"
    show LimeGreen = "84"
    show Linen = "85"
    show Magenta = "86"
    show Maroon = "87"
    show MediumAquaMarine = "88"
    show MediumBlue = "89"
    show MediumOrchid = "90"
    show MediumPurple = "91"
    show MediumSeaGreen = "92"
    show MediumSlateBlue = "93"
    show MediumSpringGreen = "94"
    show MediumTurquoise = "95"
    show MediumVioletRed = "96"
    show MidnightBlue = "97"
    show MintCream = "98"
    show MistyRose = "99"
    show Moccasin = "100"
    show NavajoWhite = "101"
    show Navy = "102"
    show Oldlace = "103"
    show Olive = "104"
    show OliveDrab = "105"
    show Orange = "106"
    show OrangeRed = "107"
    show Orchid = "108"
    show PaleGoldenRod = "109"
    show PaleGreen = "110"
    show PaleTurquoise = "111"
    show PaleVioletRed = "112"
    show PapayaWhip = "113"
    show PeachPuff = "114"
    show Peru = "115"
    show Pink = "116"
    show Plum = "117"
    show PowderBlue = "118"
    show Purple = "119"
    show Red = "120"
    show RosyBrown = "121"
    show RoyalBlue = "122"
    show SaddleBrown = "123"
    show Salmon = "124"
    show SandyBrown = "125"
    show SeaGreen = "126"
    show SeaShell = "127"
    show Sienna = "128"
    show Silver = "129"
    show SkyBlue = "130"
    show SlateBlue = "131"
    show SlateGray = "132"
    show SlateGrey = "133"
    show Snow = "134"
    show SpringGreen = "135"
    show SteelBlue = "136"
    show Tan = "137"
    show Teal = "138"
    show Thistle = "139"
    show Tomato = "140"
    show Turquoise = "141"
    show Violet = "142"
    show Wheat = "143"
    show White = "144"
    show WhiteSmoke = "145"
    show Yellow = "146"
    show YellowGreen = "147"
