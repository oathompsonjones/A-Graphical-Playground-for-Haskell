import Image from "next/image";
import type { ReactNode } from "react";
import stroke from "assets/images/docs/stroke.png";

/**
 * Displays the stroke documentation section.
 * @returns A documentation section.
 */
export function Stroke(): ReactNode {
    return (
        <div>
            <Image alt="stroke" height={500} src={stroke} width={500} />
            The <code>stroke :: Color -&gt; Shape -&gt; Shape</code> function sets the stroke color of the shape.
        </div>
    );
}
