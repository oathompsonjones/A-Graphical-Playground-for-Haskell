import Image from "next/image";
import type { ReactNode } from "react";
import bezier3 from "assets/images/docs/bezier3.png";

/**
 * Displays the bezier2 documentation section.
 * @returns A documentation section.
 */
export function Bezier3(): ReactNode {
    return (
        <div>
            <Image alt="bezier3" height={500} src={bezier3} width={500} />
            The <code>bezier3 :: Vector -&gt; Vector -&gt; Vector -&gt; Shape</code> function takes three points and
            returns a cubic Bezier curve.
            <br />
            <br />
            The curve starts at the shape's origin and ends at the third point. The first two points are the control
            points.
        </div>
    );
}
