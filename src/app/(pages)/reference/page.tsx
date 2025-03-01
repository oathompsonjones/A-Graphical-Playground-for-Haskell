import { Box, Icon, Stack, Typography } from "@mui/material";
import { Contents } from "components/pages/reference/contents";
import { Controls } from "components/pages/reference/controls";
import { KeyboardReturn } from "@mui/icons-material";
import type { ReactNode } from "react";
import { Section } from "components/pages/reference/section";

// TODO: Add image examples for shapes and transformations.

export type SectionType = (() => ReactNode) | {
    [key: string]: SectionType;
    root?: () => ReactNode;
};

// Line, Ellipse, Rect, Polygon, Curve, Arc

const list = (items: ReactNode[]): Awaited<ReactNode> => <ul>{items.map((item, i) => <li key={i}>{item}</li>)}</ul>;

const docs: Record<string, SectionType> = {
    /* eslint-disable @typescript-eslint/naming-convention, sort-keys */
    Notation: {
        root: () => (
            <div>
                Please note that the following documentation makes use of Haskell type signatures to describe functions.
                <br />
                Haskell type signatures look like this: <code>identifier :: Type1 -&gt; Type2 -&gt; ... -&gt;
                    TypeN</code>.
                <br />
                The <code>::</code> symbol is read as "has type", and the <code>-&gt;</code> symbol is read as "to".
                <br />
                The <code>identifier</code> is the name of the function, and the <code>TypeX</code> values are the types
                    of the arguments and the return value. The last <code>TypeN</code> is always the return value.
                <br />
                If an argument's type looks like this: <code>(Type1 -&gt; Type2)</code>, it means that that argument is
                    a function that takes a <code>Type1</code> and returns a <code>Type2</code>.
                <br />
                Infix operators in Haskell are defined the same way as functions, but with the operator name in
                    parentheses.
            </div>
        ),
        Examples: () => (
            <div>
                <code>add :: Int -&gt; Int -&gt; Int</code> — A function that takes two integers and returns an integer
                    (<code>add 6 7</code> = <code>13</code>).
                <br />
                <code>(+) :: Int -&gt; Int -&gt; Int</code> — The addition operator, which takes two integers and
                    returns an integer (<code>6 + 7</code> = <code>13</code>).
                <br />
                <br />
                <code>map :: (a -&gt; b) -&gt; [a] -&gt; [b]</code> — A function that takes another function, which
                converts from type <code>a</code> to type <code>b</code>, and a list of <code>a</code>s and returns
                a list of <code>b</code>s, by applying the given function to each element of the list (<code>map
                    (add 5) [1, 2, 3, 4, 5]</code> = <code>[6, 7, 8, 9, 10]</code>).
            </div>
        ),
    },
    Canvas: () => (
        <div>
            The <code>Canvas</code> is the main component of the editor. It is a 2D drawing surface using uses a
                cartesian coordinate system, with the origin at the top-left corner. The x-axis increases to the right,
                while the y-axis increases downwards. All lengths are measured in pixels, while all angles are measured
                clockwise in radians. These are denoted by the <code>Length</code> and <code>Radians</code> types,
                respectively, which are both aliases for <code>Float</code>.
            <br />
            <br />
            To set up the canvas, use the <code>createCanvas :: Int -&gt; Int -&gt; Canvas</code> function, providing a
                width and a height. Once you've created the canvas, you can set a background color using
                the <code>background :: Color -&gt; Canvas -&gt; Canvas</code> function, providing a color, and the
                canvas you created earlier. You can use the <code>fps :: Int -&gt; Canvas -&gt; Canvas</code> function
                to set the frames per second of the animation. By default, the canvas has a transparent background and
                an fps of 24.
            <br />
            <br />
            Having created the canvas, you can draw shapes on it using the <code>(&lt;&lt;&lt;) :: Canvas -&gt; Shape
                -&gt; Canvas</code> operator. This operator adds a new frame to your animation. Chaining multiple shapes
                together will draw them in sequence, not on top of each other. You can append a list of frames by using
                Haskell's <code>foldl</code> function, like this: <code>foldl (&lt;&lt;&lt;) canvas frames</code>.
            <br />
            <br />
            Finally, to render the canvas in the editor, use the <code>render :: Canvas -&gt; IO ()</code> function.
        </div>
    ),
    Vectors: () => (
        <div>
            To represent a point, you use the <code>Vector</code> data type, which stores an <code>x</code> and
                a <code>y</code> value.
            <br />
            <br />
            Vectors support the following operators:
            {list([
                <><code>(^+^) :: Vector -&gt; Vector -&gt; Vector</code> — Adds the second vector to the first.</>,
                <><code>(^-^) :: Vector -&gt; Vector -&gt; Vector</code> — Subtracts the second vector from the
                    first.</>,
                <><code>(^*^) :: Vector -&gt; Float -&gt; Vector</code> — Multiplies each component of the vector by the
                    given scalar value.</>,
                <><code>(^/^) :: Vector -&gt; Float -&gt; Vector</code> — Divides each component of the vector by the
                    given scalar value.</>,
            ])}
            and the following functions:
            {list([
                <><code>mag :: Vector -&gt; Float</code> — Calculates the magnitude (length) of the vector.</>,
                <><code>arg :: Vector -&gt; Float</code> — Calculates the argument (angle) of the vector.</>,
                <><code>norm :: Vector -&gt; Vector</code> — Calculates the normal (unit) vector.</>,
                <><code>dot :: Vector -&gt; Vector -&gt; Float</code> — Calculates the dot product of the two
                    vectors.</>,
                <><code>cross :: Vector -&gt; Vector -&gt; Float</code> — Calculates the cross product of the two
                    vectors.</>,
            ])}
        </div>
    ),
    Shapes: {
        "2D Primitives": {
            root: () => (
                <div>
                    To draw 2D shapes, you can use any of the functions in this section. They all return
                        the <code>Shape</code> data type. By default, all shapes are drawn at the top left corner of the
                        canvas, have a stroke color of black, a stroke weight of 1, and no fill color.
                </div>
            ),
            line: () => (
                <div>
                    The <code>line :: Length -&gt; Shape</code> function takes a length and returns a line.
                    <br />
                    <br />
                    The line extends in the positive x-direction.
                </div>
            ),
            ellipse: () => (
                <div>
                    The <code>ellipse :: Length -&gt; Length -&gt; Shape</code> function takes a horizontal axis and a
                        vertical axis and returns an ellipse.
                    <br />
                    <br />
                    Just like circles, the ellipse's origin is at its center.
                </div>
            ),
            rect: () => (
                <div>
                    The <code>rect :: Length -&gt; Length -&gt; Shape</code> function takes a width and height and
                        returns a rectangle.
                    <br />
                    <br />
                    Just like squares, the rectangle's origin is at its top left corner.
                </div>
            ),
            polygon: () => (
                <div>
                    The <code>polygon :: [Vector] -&gt; Shape</code> function takes a list of points and returns a
                        polygon.
                    <br />
                    <br />
                    The polygon's origin is at (0, 0).
                </div>
            ),
            bezier2: () => (
                <div>
                    The <code>bezier2 :: Vector -&gt; Vector -&gt; Shape</code> function takes two points and returns a
                        quadratic Bezier curve.
                    <br />
                    <br />
                    The curve starts at the shape's origin and ends at the second point. The first point is the control
                        point.
                </div>
            ),
            bezier3: () => (
                <div>
                    The <code>bezier3 :: Vector -&gt; Vector -&gt; Vector -&gt; Shape</code> function takes three points
                        and returns a cubic Bezier curve.
                    <br />
                    <br />
                    The curve starts at the shape's origin and ends at the third point. The first two points are the
                        control points.
                </div>
            ),
            arc: () => (
                <div>
                    The <code>arc :: Length -&gt; Length -&gt; Radians -&gt; Radians -&gt; Shape</code> function takes a
                        horizontal radius, a vertical radius, a start angle, and an end angle, and returns an elliptical
                        arc.
                    <br />
                    <br />
                    As with ellipses, the arc's origin is at its center.
                </div>
            ),
            pie: () => (
                <div>
                    The <code>pie :: Length -&gt; Length -&gt; Radians -&gt; Radians -&gt; Shape</code> function takes a
                        horizontal radius, a vertical radius, a start angle, and an end angle, and returns an elliptical
                        arc with two straight lines connecting the center to the start and end points.
                    <br />
                    <br />
                    The pie's origin is at its center.
                </div>
            ),
            segment: () => (
                <div>
                    The <code>arc :: Length -&gt; Length -&gt; Radians -&gt; Radians -&gt; Shape</code> function takes a
                        horizontal radius, a vertical radius, a start angle, and an end angle, and returns an elliptical
                        arc with a straight line connecting the start and end points.
                    <br />
                    <br />
                    As with ellipses, the arc's origin is at its center.
                </div>
            ),
            empty: () => (
                <div>
                    The <code>empty :: Shape</code> function represents the empty shape.
                    <br />
                    <br />
                    This shape draws nothing.
                </div>
            ),
        },
        // Built in functions to make shapes which use the Primitives above
        "Non-Primatives": {
            root: () => (
                <div>The following functions create shapes using the 2D primitives above.</div>
            ),
            circle: () => (
                <div>
                    The <code>circle :: Length -&gt; Shape</code> function is a shorthand for <code>ellipse r r</code>,
                        where <code>r</code> is the radius of the circle.
                </div>
            ),
            square: () => (
                <div>
                    The <code>square :: Length -&gt; Shape</code> function is a shorthand for <code>rect s s</code>,
                        where <code>s</code> is the side length of the square.
                </div>
            ),
            regular: () => (
                <div>
                    The <code>regular :: Int -&gt; Length -&gt; Shape</code> function takes the number of sides and a
                        radius and returns a regular polygon, with each point lying on a circle with the given radius.
                    <br />
                    <br />
                    The polygon's origin is at its center, just like circles and ellipses.
                </div>
            ),
        },
        "Combining Shapes": () => (
            <div>
                Shapes can be combined using the <code>(&amp;) :: Shape -&gt; Shape -&gt; Shape</code> operator.
                The <code>empty</code> is the identity function for the <code>&amp;</code> operator.
                <br />
                <br />
                The resulting <code>Shape</code> is a list of <code>Shape</code>s that are drawn on top of each other,
                    in the order they are combined.
                Applying transformations to a group of shapes applies the transformation to each shape individually.
                Given that the shapes may have different relative origin points, applying transformations such as
                    a rotation or scaling may result in unexpected behavior.
            </div>
        ),
        Transformations: {
            root: () => (
                <div>
                    To modify a shape, you can use any of the functions in this section. They all take some argument,
                        the shape to be transformed, and return the transformed shape.
                    <br />
                    By default, shapes are drawn at the top left corner of the canvas, with no rotation or scaling, have
                        a stroke color of black, a stroke weight of 1, and no fill color.
                </div>
            ),
            fill: () => (
                <div>
                    The <code>fill :: Color -&gt; Shape -&gt; Shape</code> function sets the fill color of the shape.
                    <br />
                    <br />
                    The <code>noFill :: Shape -&gt; Shape</code> function is a shorthand for <code>fill
                        Transparent</code>.
                </div>
            ),
            stroke: () => (
                <div>
                    The <code>stroke :: Color -&gt; Shape -&gt; Shape</code> function sets the stroke color of the
                        shape.
                    <br />
                    <br />
                    The <code>noStroke :: Shape -&gt; Shape</code> function is a shorthand for <code>stroke
                        Transparent</code>.
                </div>
            ),
            strokeWeight: () => (
                <div>
                    The <code>strokeWeight :: Float -&gt; Shape -&gt; Shape</code> function sets the stroke thickness of
                        the shape.
                </div>
            ),
            translate: () => (
                <div>
                    The <code>translate :: Vector -&gt; Shape -&gt; Shape</code> function translates the shape by the
                        given offset.
                    <br />
                    <br />
                    The <code>translateX :: Float -&gt; Shape -&gt; Shape</code> and <code>translateY :: Float -&gt;
                        Shape -&gt; Shape</code> functions are shorthand for <code>translate
                            (Vector x 0)</code> and <code>translate (Vector 0 y)</code>, respectively.
                    <br />
                    <br />
                    This moves the shape's origin, so for circles and ellipses, it moves the center of the shape, for
                        lines, it moves the starting point, and for squares and rectangles, it moves the top left
                        corner. For polygons, each point moves with the origin, maintaining their same relative
                        positions to each other.
                </div>
            ),
            rotate: () => (
                <div>
                    The <code>rotate :: Radians -&gt; Shape -&gt; Shape</code> function rotates the shape clockwise by
                        the given angle, in radians, around its origin.
                    <br />
                    <br />
                    As with translations, the rotation is applied about the shape's origin point.
                </div>
            ),
            scale: () => (
                <div>
                    The <code>scale :: Float -&gt; Shape -&gt; Shape</code> function scales the shape by the given
                        scale factor.
                    <br />
                    <br />
                    Once again, the scale factor is applied about the shape's origin point.
                </div>
            ),
        },
        "Chaining Transformations": () => (
            <div>
                Transformations can be applied in two ways. The first is to simply apply the transformation function
                    to the shape directly (e.g. <code>fill Red (circle 50)</code>). <br />
                The second is to use the <code>(&gt;&gt;&gt;) :: Shape -&gt; (Shape -&gt; Shape) -&gt;
                    Shape</code> operator to chain transformations together (e.g. <code>circle 50 &gt;&gt;&gt; fill
                    Red</code>).
                The <code>id</code> function is the identity function for the <code>&gt;&gt;&gt;</code> operator.
                <br />
                <br />
                The following examples all produce the same result:
                {list([
                    <><code>fill Red (translate (Vector 50 50) (rotate (radians 45) (circle 50)))</code></>,
                    <><code>circle 50 &gt;&gt;&gt; fill Red &gt;&gt;&gt; translate (Vector 50 50) &gt;&gt;&gt;
                        rotate (radians 45)</code></>,
                    <><code>circle 50 &gt;&gt;&gt; (fill Red . translate (Vector 50 50) . rotate (radians
                        45))</code></>,
                    <><code>circle 50 &gt;&gt;&gt; (foldr (.) id [fill Red, translate (Vector 50 50), rotate
                        (radians 45)])</code></>,
                    <><code>foldl (&gt;&gt;&gt;) (circle 50) [fill Red, translate (Vector 50 50), rotate (radians
                        45)]</code></>,
                ])}
            </div>
        ),
    },
    // eslint-disable-next-line max-lines-per-function
    Colors: () => (
        <div>
            Colors are represented using the <code>Color</code> data type, which has the following constructors:
            {list([
                <><code>RGB Float Float Float</code> — Represents a color with red, green, and blue values.</>,
                <><code>RGBA Float Float Float Float</code> — Represents a color with red, green, blue, and alpha
                    values.</>,
                <><code>Hex String</code> — Represents a color using a hexadecimal string. You can prefix the string
                    with a hash (e.g. <code>"#ff0000"</code>) or you can leave it out (e.g. <code>"ff0000"</code>).</>,
                <><code>HSL Float Float Float</code> — Represents a color with hue, saturation,
                    and lightness values.</>,
                <><code>HSLA Float Float Float Float</code> — Represents a color with hue, saturation, lightness, and
                    alpha values.</>,
                <><code>Transparent</code> — Represents a transparent color.</>,
            ])}

            You can also use the following CSS named colors: {[
                "AliceBlue",
                "AntiqueWhite",
                "Aqua",
                "AquaMarine",
                "Azure",
                "Beige",
                "Bisque",
                "Black",
                "BlanchedAlmond",
                "Blue",
                "BlueViolet",
                "Brown",
                "BurlyWood",
                "CadetBlue",
                "Chartreuse",
                "Chocolate",
                "Coral",
                "CornflowerBlue",
                "Cornsilk",
                "Crimson",
                "Cyan",
                "DarkBlue",
                "DarkCyan",
                "DarkGoldenRod",
                "DarkGray",
                "DarkGreen",
                "DarkGrey",
                "DarkKhaki",
                "DarkMagenta",
                "DarkOliveGreen",
                "DarkOrange",
                "DarkOrchid",
                "DarkRed",
                "DarkSalmon",
                "DarkSeaGreen",
                "DarkSlateBlue",
                "DarkSlateGray",
                "DarkSlateGrey",
                "DarkTurquoise",
                "DarkViolet",
                "DeepPink",
                "DeepSkyBlue",
                "DimGray",
                "DimGrey",
                "DodgerBlue",
                "FireBrick",
                "FloralWhite",
                "ForestGreen",
                "Fuchsia",
                "Gainsboro",
                "GhostWhite",
                "Gold",
                "GoldenRod",
                "Gray",
                "Green",
                "GreenYellow",
                "Grey",
                "HoneyDew",
                "HotPink",
                "IndianRed",
                "Indigo",
                "Ivory",
                "Khaki",
                "Lavender",
                "LavenderBlush",
                "LawnGreen",
                "LemonChiffon",
                "LightBlue",
                "LightCoral",
                "LightCyan",
                "LightGoldenRodYellow",
                "LightGray",
                "LightGreen",
                "LightGrey",
                "LightPink",
                "LightSalmon",
                "LightSeaGreen",
                "LightSkyBlue",
                "LightSlateGray",
                "LightSlateGrey",
                "LightSteelBlue",
                "LightYellow",
                "Lime",
                "LimeGreen",
                "Linen",
                "Magenta",
                "Maroon",
                "MediumAquaMarine",
                "MediumBlue",
                "MediumOrchid",
                "MediumPurple",
                "MediumSeaGreen",
                "MediumSlateBlue",
                "MediumSpringGreen",
                "MediumTurquoise",
                "MediumVioletRed",
                "MidnightBlue",
                "MintCream",
                "MistyRose",
                "Moccasin",
                "NavajoWhite",
                "Navy",
                "Oldlace",
                "Olive",
                "OliveDrab",
                "Orange",
                "OrangeRed",
                "Orchid",
                "PaleGoldenRod",
                "PaleGreen",
                "PaleTurquoise",
                "PaleVioletRed",
                "PapayaWhip",
                "PeachPuff",
                "Peru",
                "Pink",
                "Plum",
                "PowderBlue",
                "Purple",
                "Red",
                "RosyBrown",
                "RoyalBlue",
                "SaddleBrown",
                "Salmon",
                "SandyBrown",
                "SeaGreen",
                "SeaShell",
                "Sienna",
                "Silver",
                "SkyBlue",
                "SlateBlue",
                "SlateGray",
                "SlateGrey",
                "Snow",
                "SpringGreen",
                "SteelBlue",
                "Tan",
                "Teal",
                "Thistle",
                "Tomato",
                "Turquoise",
                "Violet",
                "Wheat",
                "White",
                "WhiteSmoke",
                "Yellow",
                "YellowGreen",
            ].map((color, i, arr) => (
                <span key={i}>
                    <Box
                        component="code" sx={{
                            "&:hover": { backgroundColor: color },
                            "&::after": { content: `'${color}'`, mixBlendMode: "difference" },
                        }} />
                    {i < arr.length - 1 ? ", " : ". "}
                </span>
            ))}
        </div>
    ),
    Other: {
        Angles: () => (
            <div>
                The following functions convert beetween degrees and radians (<code>Degrees</code> and <code>Radians
                </code> are aliases for <code>Float</code>):
                {list([
                    <span key={0}><code>degrees :: Radians -&gt; Degrees</code> — Converts radians to degrees.</span>,
                    <span key={1}><code>radians :: Degrees -&gt; Radians</code> — Converts degrees to radians.</span>,
                ])}
            </div>
        ),
        "Random Numbers": () => (
            <div>
                The following functions can be used to generate random numbers:
                {list([
                    <span key={1}><code>randoms :: Int -&gt; [Double]</code> — Generates an infinite list of random
                        numbers between 0 and 1, using the given seed.</span>,
                    <span key={2}><code>seed :: IO Int</code> — Returns a random seed value, which you can use to
                        generate random numbers. The seed is generated using the current time, and is therefore wrapped
                        in the <code>IO</code> monad. You can use the seed value as follows:
                    <pre>{[
                        "main :: IO ()",
                        "main = do",
                        "  seed <- randomSeed",
                        "  let randoms = randoms seed",
                        "  print $ take 10 randoms",
                    ].join("\n")}</pre></span>,
                ])}
            </div>
        ),
        "Operator Precedence": () => (
            <div>
                Precedence for the <code>(&lt;&lt;&lt;)</code>, <code>(&amp;)</code>,
                    and <code>(&gt;&gt;&gt;)</code> operators is defined as follows:
                {list([
                    <><code>infixl 7 &lt;&lt;&lt;</code></>,
                    <><code>infixr 8 &amp;</code></>,
                    <><code>infixl 9 &gt;&gt;&gt;</code></>,
                ])}
                This means that expressions such as the following do not require parentheses.
                <pre>{[
                    "render $ createCanvas 500 500 <<< circle 100 >>> translate (Vector 250 250)",
                    "                                & square 200 >>> translate (Vector 250 250)",
                ].join("\n")}</pre>
            </div>
        ),
    },
    /* eslint-enable @typescript-eslint/naming-convention */
};

/**
 * This is the reference page.
 * @returns The reference page.
 */
export default function Reference(): ReactNode {
    return (
        <>
            <Typography variant="h2">Reference</Typography>
            <Stack direction={{ md: "row" }} alignItems="top" gap={5}>
                <Contents docs={docs} />
                <Controls controls={[
                    [<KeyboardReturn />, "Run your sketch."],
                    [<Icon>S</Icon>, "Save your sketch."],
                    [<Icon>O</Icon>, "Open one of your saved sketches."],
                    [<Icon>N</Icon>, "Create a new sketch."],
                    [<Icon>/</Icon>, "Comment/uncomment the current line."],
                ]} />
            </Stack>
            {Object.entries(docs).map(([title, content], i) => (
                <Section title={title} content={content} depth={0} colored={i % 2 === 0} key={i} />
            ))}
        </>
    );
}
