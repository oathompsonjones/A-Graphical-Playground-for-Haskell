import type { ReactNode } from "react";

/**
 * Displays the images and animations documentation section.
 * @returns A documentation section.
 */
export function ImagesAndAnimations(): ReactNode {
    return (
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
            It should be noted that animations will automatically loop when they reach the end. Attempting to render an
                infinite animation will cause the editor to slow down and eventually crash, so this is not recommended.
        </div>
    );
}
