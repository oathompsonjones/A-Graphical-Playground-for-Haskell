import Image from "next/image";
import type { ReactNode } from "react";
import rect from "assets/images/docs/rect.png";

/**
 * Displays the rect documentation section.
 * @returns A documentation section.
 */
export function Rect(): ReactNode {
    return (
        <div>
            <Image src={rect} alt="rect" width={500} height={500} />
            The <code>rect :: Length -&gt; Length -&gt; Shape</code> function takes a width and height and returns a
                rectangle.
            <br />
            <br />
            The rectangle's origin is at its top left corner.
        </div>
    );
}
