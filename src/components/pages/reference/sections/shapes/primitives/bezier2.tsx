import Image from "next/image";
import type { ReactNode } from "react";
import bezier2 from "assets/images/docs/bezier2.png";

/**
 * Displays the bezier2 documentation section.
 * @returns A documentation section.
 */
export function Bezier2(): ReactNode {
    return (
        <div>
            <Image src={bezier2} alt="bezier2" width={500} height={500} />
            The <code>bezier2 :: Vector -&gt; Vector -&gt; Shape</code> function takes two points and returns a
                quadratic Bezier curve.
            <br />
            <br />
            The curve starts at the shape's origin and ends at the second point. The first point is the control point.
        </div>
    );
}
