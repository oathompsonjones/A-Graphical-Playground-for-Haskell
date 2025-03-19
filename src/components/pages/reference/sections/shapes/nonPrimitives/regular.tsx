import Image from "next/image";
import type { ReactNode } from "react";
import regular from "assets/images/docs/regular.png";

/**
 * Displays the regular documentation section.
 * @returns A documentation section.
 */
export function Regular(): ReactNode {
    return (
        <div>
            <Image src={regular} alt="regular" width={500} height={500} />
            The <code>regular :: Int -&gt; Length -&gt; Shape</code> function takes the number of sides and a radius and
                returns a regular polygon, with each point lying on a circle with the given radius.
            <br />
            <br />
            The polygon's origin is at its center, just like circles and ellipses.
        </div>
    );
}
