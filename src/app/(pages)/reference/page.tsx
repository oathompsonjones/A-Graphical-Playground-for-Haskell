"use client";

import { Icon, Stack, Tooltip, Typography } from "@mui/material";
import { Contents } from "components/pages/reference/contents";
import { Controls } from "components/pages/reference/controls";
import Image from "next/image";
import { KeyboardReturn } from "@mui/icons-material";
import type { ReactNode } from "react";
import { Section } from "components/pages/reference/section";
import arc from "assets/images/docs/arc.png";
import bezier2 from "assets/images/docs/bezier2.png";
import bezier3 from "assets/images/docs/bezier3.png";
import circle from "assets/images/docs/circle.png";
import ellipse from "assets/images/docs/ellipse.png";
import fill from "assets/images/docs/fill.png";
import line from "assets/images/docs/line.png";
import pie from "assets/images/docs/pie.png";
import polygon from "assets/images/docs/polygon.png";
import rect from "assets/images/docs/rect.png";
import regular from "assets/images/docs/regular.png";
import rotate from "assets/images/docs/rotate.png";
import scale from "assets/images/docs/scale.png";
import segment from "assets/images/docs/segment.png";
import square from "assets/images/docs/square.png";
import stroke from "assets/images/docs/stroke.png";
import strokeWeight from "assets/images/docs/strokeWeight.png";
import translate from "assets/images/docs/translate.png";

const namedToRGB = (color: string, doc: Document | null): [number, number, number] => {
    if (doc === null)
        return [255, 255, 255];

    const element = doc.createElement("div");

    element.style.color = color;
    doc.body.appendChild(element);
    const { color: rgb } = getComputedStyle(element);

    doc.body.removeChild(element);

    const [r, g, b] = rgb.match(/\d+/g)!.map(Number);

    return [r!, g!, b!];
};

const rgbToHex = (r: number, g: number, b: number): string => `#${[r, g, b]
    .map((c) => c.toString(16).padStart(2, "0")).join("")}`;

const rgbToHsl = (r: number, g: number, b: number): [number, number, number] => {
    const [R, G, B] = [r / 255, g / 255, b / 255];
    const cMax = Math.max(R, G, B);
    const cMin = Math.min(R, G, B);
    const d = cMax - cMin;
    let [h, s, l] = [0, 0, 0];

    // Calculate hue
    if (d === 0)
        h = 0;
    else if (cMax === r)
        h = (g - b) / d % 6;
    else if (cMax === g)
        h = (b - r) / d + 2;
    else
        h = (r - g) / d + 4;

    h = Math.round(h * 60);

    if (h < 0)
        h += 360;

    // Calculate lightness
    l = (cMax + cMin) / 2;
    l = Number((l * 100).toFixed(1));

    // Calculate saturation
    s = d === 0 ? 0 : d / (1 - Math.abs(2 * l - 1));
    s = Number((s * 100).toFixed(1));

    return [h, s, l];
};

export type SectionType = ((doc: Document | null) => ReactNode) | {
    [key: string]: SectionType;
    root?: (doc: Document | null) => ReactNode;
};

const list = (items: ReactNode[]): Awaited<ReactNode> => <ul>{items.map((item, i) => <li key={i}>{item}</li>)}</ul>;

const docs: Record<string, SectionType> = {
    /* eslint-disable @typescript-eslint/naming-convention, sort-keys */
    Haskell: {
        root: () => (
            <div>
                Haskell has a built in library called <code>Prelude</code> which contains many useful functions and
                    types. This library is automatically imported into every Haskell project, including your sketches on
                    this site. You can find the documentation for the <code>Prelude</code> library <a
                    href="https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html">here</a>.
            </div>
        ),
        Notation: {
            root: () => (
                <div>
                    Please note that the following documentation makes use of Haskell type signatures to describe
                        functions.
                    <br />
                    Haskell type signatures look like this: <code>identifier :: Type1 -&gt; Type2 -&gt; ... -&gt;
                        TypeN</code>.
                    <br />
                    The <code>::</code> symbol is read as "has type", and the <code>-&gt;</code> symbol is read as "to".
                    <br />
                    The <code>identifier</code> is the name of the function, and the <code>TypeX</code> values are the
                        types of the arguments and the return value. The last <code>TypeN</code> is always the return
                        value.
                    <br />
                    If an argument's type looks like this: <code>(Type1 -&gt; Type2)</code>, it means that that argument
                        is a function that takes a <code>Type1</code> and returns a <code>Type2</code>.
                    <br />
                    Infix operators in Haskell are defined the same way as functions, but with the operator name in
                        parentheses.
                </div>
            ),
            Examples: () => (
                <div>
                    <code>add :: Int -&gt; Int -&gt; Int</code> — A function that takes two integers and returns an
                        integer (<code>add 6 7</code> = <code>13</code>).
                    <br />
                    <code>(+) :: Int -&gt; Int -&gt; Int</code> — The addition operator, which takes two integers and
                    returns an integer (<code>6 + 7</code> = <code>13</code>).
                    <br />
                    <br />
                    <code>map :: (a -&gt; b) -&gt; [a] -&gt; [b]</code> — A function that takes another function, which
                        converts from type <code>a</code> to type <code>b</code>, and a list of <code>a</code>s and
                        returns a list of <code>b</code>s, by applying the given function to each element of the list
                        (<code>map (add 5) [1, 2, 3, 4, 5]</code> = <code>[6, 7, 8, 9, 10]</code>).
                </div>
            ),
        },
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
                canvas you created earlier. By default, the canvas has a transparent background.
            <br />
            <br />
            Finally, to render the canvas in the editor, use the <code>render :: Canvas -&gt; IO ()</code> function.
            <br />
            <br />
            To create a 500 pixel square canvas and set the background color to white, you can use the following code:
            <pre>{["canvas :: Canvas", "canvas = background White (createCanvas 500 500)"].join("\n")}</pre>
        </div>
    ),
    "Images and Animations": () => (
        <div>
            An image is just an animation with a single frame. A frame is just a shape, or multiple shapes combined
                using the <code>(&amp;) :: Shape -&gt; Shape -&gt; Shape</code> operator.
            <br />
            <br />
            To append a frame to the canvas, you can use the <code>(&lt;&lt;&lt;) :: Canvas -&gt; Shape -&gt;
                Canvas</code> operator. As this operator returns the canvas, you can chain multiple frames together by
                using it multiple times. You can append a list of frames by using the <code>(&lt;&lt;&lt;:) :: Canvas
                -&gt; [Shape] -&gt; Canvas</code> operator.
            <br />
            <br />
            Frames will be rendered in the order they are appended to the canvas. You can use the <code>fps :: Int -&gt;
                Canvas -&gt; Canvas</code> function to set the frames rate of the animation. By default, the fps is 24.
            <br />
            <br />
            To create a 500 pixel square canvas, set the background color to white and set the FPS to 60, you can use
                the following code:
            <pre>{["canvas :: Canvas", "canvas = fps 60 (background White (createCanvas 500 500))"].join("\n")}</pre>
            <br />
            <br />
            It should be noted that animations will automatically loop when they reach the end. Attempting to render an
                infinite animation will cause the editor to slow down and eventually crash, so this is not recommended.
        </div>
    ),
    Vectors: () => (
        <div>
            To represent a point, you use the <code>Vector</code> data type, which stores an <code>x</code> and
                a <code>y</code> value. To create a vector, you call the <code>Vector</code> constructor directly, like
                this: <code>Vector 1 2</code>.
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
                    <Image src={line} alt="line" width={500} height={500} />
                    The <code>line :: Length -&gt; Shape</code> function takes a length and returns a line.
                    <br />
                    <br />
                    The line extends in the positive x-direction.
                </div>
            ),
            ellipse: () => (
                <div>
                    <Image src={ellipse} alt="ellipse" width={500} height={500} />
                    The <code>ellipse :: Length -&gt; Length -&gt; Shape</code> function takes a horizontal axis and a
                        vertical axis and returns an ellipse.
                    <br />
                    <br />
                    Just like circles, the ellipse's origin is at its center.
                </div>
            ),
            rect: () => (
                <div>
                    <Image src={rect} alt="rect" width={500} height={500} />
                    The <code>rect :: Length -&gt; Length -&gt; Shape</code> function takes a width and height and
                        returns a rectangle.
                    <br />
                    <br />
                    Just like squares, the rectangle's origin is at its top left corner.
                </div>
            ),
            polygon: () => (
                <div>
                    <Image src={polygon} alt="polygon" width={500} height={500} />
                    The <code>polygon :: [Vector] -&gt; Shape</code> function takes a list of points and returns a
                        polygon.
                    <br />
                    <br />
                    The polygon's origin is at (0, 0).
                </div>
            ),
            bezier2: () => (
                <div>
                    <Image src={bezier2} alt="bezier2" width={500} height={500} />
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
                    <Image src={bezier3} alt="bezier3" width={500} height={500} />
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
                    <Image src={arc} alt="arc" width={500} height={500} />
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
                    <Image src={pie} alt="pie" width={500} height={500} />
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
                    <Image src={segment} alt="segment" width={500} height={500} />
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
                    <Image src={circle} alt="circle" width={500} height={500} />
                    The <code>circle :: Length -&gt; Shape</code> function is a shorthand for <code>ellipse r r</code>,
                        where <code>r</code> is the radius of the circle.
                </div>
            ),
            square: () => (
                <div>
                    <Image src={square} alt="square" width={500} height={500} />
                    The <code>square :: Length -&gt; Shape</code> function is a shorthand for <code>rect s s</code>,
                        where <code>s</code> is the side length of the square.
                </div>
            ),
            regular: () => (
                <div>
                    <Image src={regular} alt="regular" width={500} height={500} />
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
    },
    Transformations: {
        root: () => (
            <div>
                To modify a shape, you can use any of the functions in this section. They all take some argument, the
                    shape to be transformed, and return the transformed shape.
                <br />
                By default, shapes are drawn at the top left corner of the canvas, with no rotation or scaling, have a
                    stroke color of black, a stroke weight of 1, and no fill color.
            </div>
        ),
        fill: () => (
            <div>
                <Image src={fill} alt="fill" width={500} height={500} />
                The <code>fill :: Color -&gt; Shape -&gt; Shape</code> function sets the fill color of the shape.
            </div>
        ),
        stroke: () => (
            <div>
                <Image src={stroke} alt="stroke" width={500} height={500} />
                The <code>stroke :: Color -&gt; Shape -&gt; Shape</code> function sets the stroke color of the shape.
            </div>
        ),
        strokeWeight: () => (
            <div>
                <Image src={strokeWeight} alt="strokeWeight" width={500} height={500} />
                The <code>strokeWeight :: Float -&gt; Shape -&gt; Shape</code> function sets the stroke thickness of the
                    shape.
            </div>
        ),
        translate: () => (
            <div>
                <Image src={translate} alt="translate" width={500} height={500} />
                The <code>translate :: Vector -&gt; Shape -&gt; Shape</code> function translates the shape by the given
                    offset.
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
                <Image src={rotate} alt="rotate" width={500} height={500} />
                The <code>rotate :: Radians -&gt; Shape -&gt; Shape</code> function rotates the shape clockwise by the
                    given angle, in radians, around its origin.
                <br />
                <br />
                As with translations, the rotation is applied about the shape's origin point.
            </div>
        ),
        scale: () => (
            <div>
                <Image src={scale} alt="scale" width={500} height={500} />
                The <code>scale :: Float -&gt; Shape -&gt; Shape</code> function scales the shape by the given scale
                    factor.
                <br />
                <br />
                Once again, the scale factor is applied about the shape's origin point.
            </div>
        ),
        Shorthands: {
            noFill: () => (
                <div>
                    The <code>noFill :: Shape -&gt; Shape</code> function is a shorthand for <code>fill
                        Transparent</code>.
                </div>
            ),
            noStroke: () => (
                <div>
                    The <code>noStroke :: Shape -&gt; Shape</code> function is a shorthand for <code>stroke
                        Transparent</code>.
                </div>
            ),
            translateX: () => (
                <div>
                    The <code>translateX :: Float -&gt; Shape -&gt; Shape</code> function is shorthand
                        for <code>translate (Vector x 0)</code>.
                </div>
            ),
            translateY: () => (
                <div>
                    The <code>translateY :: Float -&gt; Shape -&gt; Shape</code> function is shorthand
                        for <code>translate (Vector 0 y)</code>.
                </div>
            ),
            center: () => (
                <div>
                    The <code>center :: Canvas -&gt; Shape -&gt; Shape</code> function is a more complex translation
                        shorthand, which moves the shape's center to the center of the canvas.
                    <br />
                    <br />
                    For shapes who's origin is at their center, this is the same as using <code>translate (Vector
                        (width / 2) (height / 2))</code> where <code>width</code>, and <code>height</code> are the
                        width and height of your canvas.
                    <br />
                    <br />
                    For other shapes, <code>center</code> calculates the required offset to account for the shape's
                        origin point and current angle of rotation.
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
                    <><pre>fill Red (translate (Vector 50 50) (rotate (radians 45) (circle 50)))</pre></>,
                    <><pre>circle 50 &gt;&gt;&gt; fill Red &gt;&gt;&gt; translate (Vector 50 50) &gt;&gt;&gt;
                        rotate (radians 45)</pre></>,
                    <><pre>circle 50 &gt;&gt;&gt; (fill Red . translate (Vector 50 50) . rotate (radians
                        45))</pre></>,
                    <><pre>circle 50 &gt;&gt;&gt; (foldr (.) id [fill Red, translate (Vector 50 50), rotate
                        (radians 45)])</pre></>,
                    <><pre>foldl (&gt;&gt;&gt;) (circle 50) [fill Red, translate (Vector 50 50), rotate (radians
                        45)]</pre></>,
                ])}
            </div>
        ),
    },
    // eslint-disable-next-line max-lines-per-function
    Colors: (doc) => (
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
            ].map((name) => {
                const rgb = namedToRGB(name, doc);
                const hex = rgbToHex(...rgb);
                const hsl = rgbToHsl(...rgb);
                const contrast = hsl[2] > 40 ? "black" : "white";

                return { contrast, hex, hsl, name, rgb };
            }).sort((a, b) => a.hsl[0] - b.hsl[0]).map(({ name, rgb, hsl, hex, contrast }, i) => (
                <Tooltip
                    title={[
                        `rgb(${rgb.join(", ")})`,
                        `hsl(${hsl[0]}, ${hsl[1]}%, ${hsl[2]}%)`,
                        hex,
                    ].join(" • ")} key={i} placement="top" arrow>
                    <span>
                        <code style={{ background: name, color: contrast }}>{name}</code>
                        {" "}
                    </span>
                </Tooltip>
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
                        "  s <- seed",
                        "  print $ take 10 (randoms s)",
                    ].join("\n")}</pre></span>,
                ])}
            </div>
        ),
        "Operator Precedence": () => (
            <div>
                Precedence for the <code>(&lt;&lt;&lt;)</code>, <code>(&amp;)</code>,
                    and <code>(&gt;&gt;&gt;)</code> operators is defined as follows:
                {list([
                    <><code>infixl 7 &lt;&lt;&lt;:</code></>,
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
 * @param props - The properties of the page.
 * @param props.hideControls - Whether to hide the controls.
 * @returns The reference page.
 */
export default function Reference({ hideControls }: { hideControls?: boolean; }): ReactNode {
    return (
        <>
            <div className="wrapper">
                <Typography variant="h2">Reference</Typography>
                <Stack direction={{ md: "row" }} alignItems="top" gap={5}>
                    <Contents docs={docs} />
                    {!(hideControls ?? false) && <Controls controls={[
                        [<KeyboardReturn />, "Run your sketch."],
                        [<Icon>S</Icon>, "Save your sketch."],
                        [<Icon>O</Icon>, "Open one of your saved sketches."],
                        [<Icon>N</Icon>, "Create a new sketch."],
                        [<Icon>/</Icon>, "Comment/uncomment the current line."],
                    ]} />}
                </Stack>
            </div>
            {Object.entries(docs).map(([title, content], i) => (
                <Section title={title} content={content} depth={0} colored={i % 2 === 0} key={i} />
            ))}
        </>
    );
}
