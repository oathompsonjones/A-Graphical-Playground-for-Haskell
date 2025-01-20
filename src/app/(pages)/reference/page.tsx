"use client";

import { Grid2, Icon, Typography } from "@mui/material";
import { KeyboardCommandKey, KeyboardControlKey, KeyboardReturn } from "@mui/icons-material";
import { useEffect, useState } from "react";
import type { ReactNode } from "react";

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
        <div>
            <Typography variant="h2">Reference</Typography>
            <br />
            <Typography variant="h4">Hot Keys</Typography>
            <br />
            <Grid2 container alignItems="center">
                <Grid2 size={1}><Typography variant="h4">{metaKey}<KeyboardReturn /></Typography></Grid2>
                <Grid2 size={11}>Run the code.</Grid2>
                <Grid2 size={1}><Typography variant="h4">{metaKey}<Icon>S</Icon></Typography></Grid2>
                <Grid2 size={11}>Save the code.</Grid2>
                <Grid2 size={1}><Typography variant="h4">{metaKey}<Icon>/</Icon></Typography></Grid2>
                <Grid2 size={11}>Comment/uncomment the line.</Grid2>
            </Grid2>
            <br />
            <Typography variant="h4">Data Types</Typography>
            <br />
            <Typography variant="h6">
                {"data Point = Point { x :: Float, y :: Float }"}
            </Typography>
            <Typography>
                This represents a 2D point, with x and y coordinates.
            </Typography>
            <br />
            <Typography variant="h6">
                {"data Color = RGBA { r :: Float, g :: Float, b :: Float, a :: Float }"}
                <br />
                {"| RGB { r :: Float, g :: Float, b :: Float }"}
            </Typography>
            <Typography>
                This represents a color, with red, green, and blue components, and an optional alpha component.
            </Typography>
            <br />
            <Typography variant="h4">Functions</Typography>
            <br />
            <Typography variant="h6">
                background :: Color -&gt; IO ()
            </Typography>
            <Typography>
                This takes a color, and sets the background to that color.
            </Typography>
            <br />
            <Typography variant="h6">
                circle :: Point -&gt; Float -&gt; Maybe Color -&gt; Maybe Color -&gt; Maybe Float -&gt; IO ()
            </Typography>
            <Typography>
                This takes a center point, a radius, an optional fill color, an optional stroke color, an optional
                stroke width, and draws a circle.
            </Typography>
            <br />
            <Typography variant="h6">
                ellipse :: Point -&gt; Float -&gt; Float -&gt; Maybe Color -&gt; Maybe Color -&gt; Maybe Float -&gt;
                IO ()
            </Typography>
            <Typography>
                This takes a center point, a horizontal radius, a vertical radius, an optional fill color, an optional
                stroke color, an optional stroke width, and draws an ellipse.
            </Typography>
            <br />
            <Typography variant="h6">
                line :: Point -&gt; Point -&gt; Maybe Color -&gt; Maybe Color -&gt; Maybe Float -&gt; IO ()
            </Typography>
            <Typography>
                This takes two points, an optional fill color, an optional stroke color, an optional stroke width, and
                draws a line.
            </Typography>
            <br />
            <Typography variant="h6">
                point :: Point -&gt; Maybe Color -&gt; Maybe Color -&gt; Maybe Float -&gt; IO ()
            </Typography>
            <Typography>
                This takes a point, an optional fill color, an optional stroke color, an optional stroke width, and
                draws a point.
            </Typography>
            <br />
            <Typography variant="h6">
                quad :: Point -&gt; Point -&gt; Point -&gt; Point -&gt; Maybe Color -&gt; Maybe Color -&gt; Maybe Float
                -&gt; IO ()
            </Typography>
            <Typography>
                This takes four corner points, an optional fill color, an optional stroke color, an optional stroke
                width, and draws a quadrilateral.
            </Typography>
            <br />
            <Typography variant="h6">
                rect :: Point -&gt; Float -&gt; Float -&gt; Maybe Color -&gt; Maybe Color -&gt; Maybe Float -&gt; IO ()
            </Typography>
            <Typography>
                This takes a top left point, a width, a height, an optional fill color, an optional stroke color, an
                optional stroke width, and draws a rectangle.
            </Typography>
            <br />
            <Typography variant="h6">
                square :: Point -&gt; Float -&gt; Maybe Color -&gt; Maybe Color -&gt; Maybe Float -&gt; IO ()
            </Typography>
            <Typography>
                This takes a top left point, a size, an optional fill color, an optional stroke color, an optional
                stroke width, and draws a square.
            </Typography>
            <br />
            <Typography variant="h6">
                triangle :: Point -&gt; Point -&gt; Point -&gt; Maybe Color -&gt; Maybe Color -&gt; Maybe Float -&gt; IO
                ()
            </Typography>
            <Typography>
                This takes three corner points, an optional fill color, an optional stroke color, an optional stroke
                width, and draws a triangle.
            </Typography>
        </div>
    );
}
