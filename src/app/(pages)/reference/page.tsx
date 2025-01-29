"use client";

import { Box, Grid2, Icon, Typography } from "@mui/material";
import { KeyboardCommandKey, KeyboardControlKey, KeyboardReturn } from "@mui/icons-material";
import { useEffect, useState } from "react";
import type { ReactNode } from "react";
import type { Variant } from "@mui/material/styles/createTypography";
import styles from "styles/pages/reference.module.css";

type Section = ((metaKey?: ReactNode) => ReactNode) | { [key: string]: Section; };

const toId = (title: string): string => title.toLowerCase().replace(/\s/g, "-");
const list = (items: ReactNode[]): Awaited<ReactNode> => <ul>{items.map((item, i) => <li key={i}>{item}</li>)}</ul>;

const docs: Record<string, Section> = {
    /* eslint-disable @typescript-eslint/naming-convention, sort-keys */
    Notation: () => (
        <div>
            Please note that the following documentation makes use of Haskell type signatures to describe functions.
            <br />
            Haskell type signatures look like this: <code>identifier :: Type1 -&gt; Type2 -&gt; ... -&gt; TypeN</code>.
            <br />
            The <code>::</code> symbol is read as "has type", and the <code>-&gt;</code> symbol is read as "to".
            <br />
            The <code>identifier</code> is the name of the function, and the <code>TypeX</code> values are the types of
            the arguments and the return value. The last <code>TypeN</code> is always the return value.
            <br />
            If an argument's type looks like this: <code>(Type1 -&gt; Type2)</code>, it means that that argument is a
            function that takes a <code>Type1</code> and returns a <code>Type2</code>.
            <br />
            Infix operators in Haskell are defined the same way as functions, but with the operator name in parentheses.
            <br />
            <br />
            <Typography variant="h4">Examples</Typography>
            <code>add :: Int -&gt; Int -&gt; Int</code> — A function that takes two integers and returns an integer (
            <code>add 6 7</code> = <code>13</code>).
            <br />
            <code>(+) :: Int -&gt; Int -&gt; Int</code> — The addition operator, which takes two integers and returns an
                integer (<code>6 + 7</code> = <code>13</code>).
            <br />
            <br />
            <code>map :: (a -&gt; b) -&gt; [a] -&gt; [b]</code> — A function that takes another function, which
                converts from type <code>a</code> to type <code>b</code>, and a list of <code>a</code>s and returns a
                list of <code>b</code>s, by applying the given function to each element of the list (<code>
                map (add 5) [1, 2, 3, 4, 5]</code> = <code>[6, 7, 8, 9, 10]</code>).
        </div>
    ),
    "Editor Controls": (metaKey: ReactNode) => (
        <div>
            <Grid2 container alignItems="center">
                <Grid2 size={2}><Typography variant="h4">{metaKey}<KeyboardReturn /></Typography></Grid2>
                <Grid2 size={10}>Run the code.</Grid2>
                <Grid2 size={2}><Typography variant="h4">{metaKey}<Icon>S</Icon></Typography></Grid2>
                <Grid2 size={10}>Save the code.</Grid2>
                <Grid2 size={2}><Typography variant="h4">{metaKey}<Icon>/</Icon></Typography></Grid2>
                <Grid2 size={10}>Comment/uncomment the line.</Grid2>
            </Grid2>
        </div>
    ),
    Canvas: () => (
        <div>
            The <code>Canvas</code> is the main component of the editor. It is a 2D drawing surface using uses a
                cartesian coordinate system, with the origin at the top-left corner. The x-axis increases to the right,
                while the y-axis increases downwards.
            <br />
            To set up the canvas, use the <code>createCanvas :: Int -&gt; Int -&gt; Canvas</code> function, providing a
                width and a height. Once you've created the canvas, you can set a background color using
                the <code>background :: Color -&gt; Canvas -&gt; Canvas</code> function, providing a color, and the
                canvas you created earlier.
            <br />
            Having created the canvas, you can draw shapes on it using the <code>(&lt;&lt;&lt;) :: Canvas -&gt; Shape
                -&gt; Canvas</code> operator.
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
        "2D Primitives": () => (
            <div>
                To draw 2D shapes, you can use any of the following functions, which all return
                    the <code>Shape</code> data type:
                {list([
                    <><code>circle :: Float -&gt; Shape</code> — Takes a radius and returns a circle.</>,
                    <><code>ellipse :: Float -&gt; Float -&gt; Shape</code> — Takes a width and height and returns an
                        ellipse.</>,
                    <><code>line :: Float -&gt; Shape</code> — Takes a length and returns a line.</>,
                    <><code>rect :: Float -&gt; Float -&gt; Shape</code> — Takes a width and height and returns a
                        rectangle.</>,
                    <><code>square :: Float -&gt; Shape</code> — Takes a side length and returns a square.</>,
                    <><code>polygon :: [Vector] -&gt; Shape</code> — Takes a list of points and returns a polygon.</>,
                ])}

                Shapes can be combined using the <code>(&amp;) :: Shape -&gt; Shape -&gt; Shape</code> operator.
                The <code>emptyShape :: Shape</code> function represents the empty shape, and is the identity
                    function for the <code>&amp;</code> operator.
            </div>
        ),
        "Shape Transformations": () => (
            <div>
                You can modify a shape using the following functions:
                {list([
                    <><code>fill :: Color -&gt; Shape -&gt; Shape</code> — Set the fill color of the shape.</>,
                    <><code>stroke :: Color -&gt; Shape -&gt; Shape</code> — Set the stroke color of the shape.</>,
                    <><code>strokeWeight :: Float -&gt; Shape -&gt; Shape</code> — Set the stroke thickness of the
                        shape.</>,
                    <><code>translate :: Vector -&gt; Shape -&gt; Shape</code> — Translate the shape by the given
                        offset. For circles and ellipses, this moves the center of the shape. For lines, this
                        moves the starting point. For squares and rectangles, this moves the top left corner. For
                        polygons, this translates each point individually, keeping them in the same position relative
                        to one another.</>,
                    <><code>rotate :: Float -&gt; Shape -&gt; Shape</code> — Rotate the shape by the given angle in
                        radians. For circles and ellipses, this rotates the shape about its center. For lines, this
                        roates about the starting point. For squares and rectangles, this rotates about the top left
                        corner. For polygons, this rotates about the shape's relative origin point.</>,
                    <><code>scale :: Float -&gt; Shape -&gt; Shape</code> — Scale the shape by the given factor.</>,
                    <><code>reflect :: Float -&gt; Shape -&gt; Shape</code> — Reflect the shape over an axis at the
                        given angle in radians.</>,
                ])}

                There are also a few shorthand functions for common transformations:
                {list([
                    <><code>noStroke :: Shape -&gt; Shape</code> = <code>stroke Transparent</code></>,
                    <><code>noFill :: Shape -&gt; Shape</code> = <code>fill Transparent</code></>,
                    <><code>translateX :: Float -&gt; Shape -&gt; Shape</code> = <code>translate (Vector x 0)</code></>,
                    <><code>translateY :: Float -&gt; Shape -&gt; Shape</code> = <code>translate (Vector 0 y)</code></>,
                ])}

                Transformations can be applied in two ways. The first is to simply apply the transformation function to
                    the shape directly (e.g. <code>fill Red (circle 50)</code>). <br />
                The second is to use the <code>(&gt;&gt;&gt;) :: Shape -&gt; (Shape -&gt; Shape) -&gt;
                    Shape</code> operator to chain transformations together (e.g. <code>circle 50 &gt;&gt;&gt; fill
                    Red</code>).
                The <code>identityTransformation :: Shape -&gt; Shape</code> function represents the identity
                    transformation, and is the identity function for the <code>&gt;&gt;&gt;</code> operator.
                <br />
                <br />
                To apply multiple transformations, you can do any of the following to achieve the same result:
                {list([
                    <><code>fill Red (translate (Vector 50 50) (rotate (radians 45) (circle 50)))</code></>,
                    <><code>circle 50 &gt;&gt;&gt; fill Red &gt;&gt;&gt; translate (Vector 50 50) &gt;&gt;&gt; rotate
                        (radians 45)</code></>,
                    <><code>circle 50 &gt;&gt;&gt; (fill Red . translate (Vector 50 50) . rotate (radians
                        45))</code></>,
                    <><code>circle 50 &gt;&gt;&gt; (foldr (.) identityTransformation [fill Red, translate (Vector 50
                        50), rotate (radians 45)])</code></>,
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
        "Utility Functions": () => (
            <div>
                The following functions convert beetween degrees and radians:
                {list([
                    <span key={0}><code>degrees :: Float -&gt; Float</code> — Converts radians to degrees.</span>,
                    <span key={1}><code>radians :: Float -&gt; Float</code> — Converts degrees to radians.</span>,
                ])}
            </div>
        ),
    },
    /* eslint-enable @typescript-eslint/naming-convention */
};

const contents = (section: Record<string, Section>): Awaited<ReactNode> => list(
    Object.entries(section).map(([title, content], i) => (
        <span key={i}>
            <a href={`#${toId(title)}`}>{title}</a>
            {typeof content === "object" && contents(content)}
        </span>
    )),
);
const section = (metaKey: ReactNode, title: string, content: Section, depth: number, i: number): Awaited<ReactNode> => (
    <div key={i} className={`${styles.wrapper} ${i % 2 === 0 && depth === 0 ? styles.colored : ""} edge wrapper`}>
        <br />
        <Typography variant={`h${depth + 3}` as Variant} id={toId(title)} className={styles.title!}>{title}</Typography>
        {content instanceof Function
            ? content(metaKey)
            : Object.entries(content)
                .map(([subtitle, subcontent], j) => section(metaKey, subtitle, subcontent, depth + 1, j))}
    </div>
);

/**
 * This is the reference page.
 * @returns The home reference.
 */
export default function Reference(): ReactNode {
    const [metaKey, setMetaKey] = useState(<KeyboardControlKey />);

    useEffect(() => {
        if (navigator.platform.includes("Mac"))
            setMetaKey(<KeyboardCommandKey />);
    }, []);

    return (
        <>
            <Typography variant="h2">Reference</Typography>
            {contents(docs)}
            {Object.entries(docs).map(([title, content], i) => section(metaKey, title, content, 0, i))}
        </>
    );
}
