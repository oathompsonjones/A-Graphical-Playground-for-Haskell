import Image from "next/image";
import type { ReactNode } from "react";
import translate from "assets/images/docs/translate.png";

/**
 * Displays the translate documentation section.
 * @returns A documentation section.
 */
export function Translate(): ReactNode {
    return (
        <div>
            <Image src={translate} alt="translate" width={500} height={500} />
            The <code>translate :: Vector -&gt; Shape -&gt; Shape</code> function translates the shape by the given
                offset.
            <br />
            <br />
            This moves the shape's origin, so for circles and ellipses, it moves the center of the shape, for lines, it
                moves the starting point, and for squares and rectangles, it moves the top left corner. For polygons,
                each point moves with the origin, maintaining their same relative positions to each other.
        </div>
    );
}
