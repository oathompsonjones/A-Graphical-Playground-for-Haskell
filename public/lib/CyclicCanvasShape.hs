module CyclicCanvasShape (center) where

import Canvas (Canvas (Canvas))
import Maths (Vector (Vector), arg, mag)
import Shape (
    Shape (Curve, Empty, Group, Line, Rect, _options),
    ShapeOptions (_angle, _position),
    group,
    translate,
 )

-- This transformation requires the canvas as an input
center :: Canvas -> Shape -> Shape
center _ Empty = Empty
center canvas (Group shapes) = group shapes (center canvas)
center (Canvas w h _ _ _) line@(Line l opts) =
    line
        { _options =
            opts
                { _position =
                    Vector
                        ((w - l * cos (_angle opts)) / 2)
                        ((h - l * sin (_angle opts)) / 2)
                }
        }
center (Canvas w h _ _ _) rect@(Rect x y opts) =
    rect
        { _options =
            opts
                { _position =
                    Vector
                        (w / 2 - (x / 2 * cos (_angle opts) - y / 2 * sin (_angle opts)))
                        (h / 2 - (x / 2 * sin (_angle opts) + y / 2 * cos (_angle opts)))
                }
        }
center (Canvas w h _ _ _) curve@(Curve pts opts) =
    curve
        { _options =
            opts
                { _position =
                    Vector
                        ((w - mag (last pts) * cos (_angle opts + arg (last pts))) / 2)
                        ((h - mag (last pts) * sin (_angle opts + arg (last pts))) / 2)
                }
        }
center (Canvas w h _ _ _) shape = shape{_options = (_options shape){_position = Vector (w / 2) (h / 2)}}
