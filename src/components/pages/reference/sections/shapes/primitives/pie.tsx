import Image from "next/image";
import type { ReactNode } from "react";
import pie from "assets/images/docs/pie.png";

/**
 * Displays the pie documentation section.
 * @returns A documentation section.
 */
export function Pie(): ReactNode {
    return (
        <div>
            <Image src={pie} alt="pie" width={500} height={500} />
            The <code>pie :: Length -&gt; Length -&gt; Radians -&gt; Radians -&gt; Shape</code> function takes a
                horizontal radius, a vertical radius, a start angle, and an end angle, and returns an elliptical arc
                with two straight lines connecting the center to the start and end points.
            <br />
            <br />
            The pie's origin is at its center.
        </div>
    );
}
