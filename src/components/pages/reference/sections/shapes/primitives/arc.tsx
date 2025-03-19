import Image from "next/image";
import type { ReactNode } from "react";
import arc from "assets/images/docs/arc.png";

/**
 * Displays the arc documentation section.
 * @returns A documentation section.
 */
export function Arc(): ReactNode {
    return (
        <div>
            <Image src={arc} alt="arc" width={500} height={500} />
            The <code>arc :: Length -&gt; Length -&gt; Radians -&gt; Radians -&gt; Shape</code> function takes a
                horizontal radius, a vertical radius, a start angle, and an end angle, and returns an elliptical arc.
            <br />
            <br />
            As with ellipses, the arc's origin is at its center.
        </div>
    );
}
