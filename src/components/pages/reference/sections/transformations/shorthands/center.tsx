import type { ReactNode } from "react";

/**
 * Displays the center documentation section.
 * @returns A documentation section.
 */
export function Center(): ReactNode {
    return (
        <div>
            The <code>center :: Canvas -&gt; Shape -&gt; Shape</code> function is a more complex translation shorthand,
                which moves the shape's center to the center of the canvas.
            <br />
            <br />
            For shapes who's origin is at their center, this is the same as using <code>translate (Vector (width / 2)
                (height / 2))</code> where <code>width</code>, and <code>height</code> are the width and height of your
                canvas.
            <br />
            <br />
            For other shapes, <code>center</code> calculates the required offset to account for the shape's origin point
                and current angle of rotation. As this function does more than just a simple translation, it is
                important to consider the order in which transformations are applied.
            <br />
            <br />
            As this function requires the canvas as an argument, it is recommended that you write your code like this:
            <pre>{[
                "canvas :: Canvas",
                "canvas = createCanvas 500 500",
                "",
                "main :: IO ()",
                "main = redner $ canvas <<< circle 100 >>> center canvas",
            ].join("\n")}</pre>
        </div>
    );
}
