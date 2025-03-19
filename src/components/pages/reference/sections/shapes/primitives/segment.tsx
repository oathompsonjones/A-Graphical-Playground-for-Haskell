import Image from "next/image";
import type { ReactNode } from "react";
import segment from "assets/images/docs/segment.png";

/**
 * Displays the segment documentation section.
 * @returns A documentation section.
 */
export function Segment(): ReactNode {
    return (
        <div>
            <Image src={segment} alt="segment" width={500} height={500} />
            The <code>arc :: Length -&gt; Length -&gt; Radians -&gt; Radians -&gt; Shape</code> function takes a
                horizontal radius, a vertical radius, a start angle, and an end angle, and returns an elliptical arc
                with a straight line connecting the start and end points.
            <br />
            <br />
            As with ellipses, the arc's origin is at its center.
        </div>
    );
}
