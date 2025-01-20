"use client";

import { Grid2, Icon, Typography } from "@mui/material";
import { KeyboardCommandKey, KeyboardControlKey, KeyboardReturn } from "@mui/icons-material";
import { useEffect, useState } from "react";
import type { ReactNode } from "react";
import styles from "styles/pages/reference.module.css";

/**
 * This is the reference page.
 * @returns The home reference.
 */
export default function Reference(): ReactNode {
    const [metaKey, setMetaKey] = useState(<KeyboardControlKey />);

    useEffect(() => {
        if (navigator.platform.includes("Mac"))
            setMetaKey(<KeyboardCommandKey />);
    }, [navigator.platform]);

    return (
        <div className={styles.wrapper}>
            <Typography variant="h2">Reference</Typography>
            <br />
            <Typography variant="h4">Editor Controls</Typography>
            <Grid2 container alignItems="center">
                <Grid2 size={1}><Typography variant="h4">{metaKey}<KeyboardReturn /></Typography></Grid2>
                <Grid2 size={11}>Run the code.</Grid2>
                <Grid2 size={1}><Typography variant="h4">{metaKey}<Icon>S</Icon></Typography></Grid2>
                <Grid2 size={11}>Save the code.</Grid2>
                <Grid2 size={1}><Typography variant="h4">{metaKey}<Icon>/</Icon></Typography></Grid2>
                <Grid2 size={11}>Comment/uncomment the line.</Grid2>
            </Grid2>
            <br />
            <Typography variant="h4">Points</Typography>
            <Typography>
                The canvas uses a cartesian coordinate system, with the origin at the top-left corner,
                and the x-axis increasing to the right, and the y-axis increasing downwards.
                To represent a point, you use the <code>Point x y</code> data type, which stores an x and y coordinate.
            </Typography>
            <br />
            <Typography variant="h4">Shapes</Typography>
            <Typography variant="h5">2D Primitives</Typography>
            <Typography>
                To draw 2D shapes, you can use any of the following functions,
                which all return the <code>Shape</code> data type:
                <ul>
                    <li>
                        <code>circle :: Float -&gt; Shape</code> — Takes a radius and returns a circle.
                    </li>
                    <li>
                        <code>ellipse :: Float -&gt; Float -&gt; Shape</code> — Takes a width and height and
                        returns an ellipse.
                    </li>
                    <li>
                        <code>line :: Float -&gt; Shape</code> — Takes a length and returns a line.
                    </li>
                    <li>
                        <code>rect :: Float -&gt; Float -&gt; Shape</code> — Takes a width and height and
                        returns a rectangle.
                    </li>
                    <li>
                        <code>square :: Float -&gt; Shape</code> — Takes a side length and returns a square.
                    </li>
                    <li>
                        <code>polygon :: [Point] -&gt; Shape</code> — Takes a list of points and returns a polygon.
                    </li>
                </ul>
                You can concatenate shapes using the <code>&amp;</code> operator.
                The expression <code>(circle 10) &amp; (rect 20 30)</code> will draw a circle and a rectangle.
            </Typography>
            <Typography variant="h5">Shape Modifiers</Typography>
            <Typography>
                You can modify a shape using the following functions:
                <ul>
                    <li>
                        <code>fill :: Shape -&gt; Color -&gt; Shape</code> — Fill the shape with a color.
                    </li>
                    <li>
                        <code>rotate :: Shape -&gt; Float -&gt; Shape</code> — Rotate the shape by an angle in degrees.
                    </li>
                    <li>
                        <code>scale :: Shape -&gt; Float -&gt; Shape</code> — Scale the shape by a factor.
                    </li>
                    <li>
                        <code>translate :: Shape -&gt; Point -&gt; Shape</code> — Translate the shape by an offset.
                    </li>
                </ul>
            </Typography>
        </div>
    );
}
