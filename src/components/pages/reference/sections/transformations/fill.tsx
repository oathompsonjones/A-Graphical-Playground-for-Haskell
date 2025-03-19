import Image from "next/image";
import type { ReactNode } from "react";
import fill from "assets/images/docs/fill.png";

/**
 * Displays the fill documentation section.
 * @returns A documentation section.
 */
export function Fill(): ReactNode {
    return (
        <div>
            <Image src={fill} alt="fill" width={500} height={500} />
            The <code>fill :: Color -&gt; Shape -&gt; Shape</code> function sets the fill color of the shape.
        </div>
    );
}
