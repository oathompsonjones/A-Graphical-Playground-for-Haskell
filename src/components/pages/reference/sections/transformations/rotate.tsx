import Image from "next/image";
import type { ReactNode } from "react";
import rotate from "assets/images/docs/rotate.png";

/**
 * Displays the rotate documentation section.
 * @returns A documentation section.
 */
export function Rotate(): ReactNode {
    return (
        <div>
            <Image src={rotate} alt="rotate" width={500} height={500} />
            The <code>rotate :: Radians -&gt; Shape -&gt; Shape</code> function rotates the shape clockwise by the given
                angle, in radians, around its origin.
            <br />
            <br />
            As with translations, the rotation is applied about the shape's origin point.
        </div>
    );
}
