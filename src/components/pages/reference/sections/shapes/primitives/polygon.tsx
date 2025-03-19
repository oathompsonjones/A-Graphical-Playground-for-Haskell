import Image from "next/image";
import type { ReactNode } from "react";
import polygon from "assets/images/docs/polygon.png";

/**
 * Displays the polygon documentation section.
 * @returns A documentation section.
 */
export function Polygon(): ReactNode {
    return (
        <div>
            <Image src={polygon} alt="polygon" width={500} height={500} />
            The <code>polygon :: [Vector] -&gt; Shape</code> function takes a list of points and returns a polygon.
            <br />
            <br />
            The polygon's origin is at (0, 0).
        </div>
    );
}
