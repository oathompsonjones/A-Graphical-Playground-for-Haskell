import type { ReactNode } from "react";

/**
 * Displays the canvas documentation section.
 * @returns A documentation section.
 */
export function Canvas(): ReactNode {
    return (
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
    );
}
