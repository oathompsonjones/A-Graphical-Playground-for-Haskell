import Image from "next/image";
import type { ReactNode } from "react";
import circle from "assets/images/docs/circle.png";

/**
 * Displays the circle documentation section.
 * @returns A documentation section.
 */
export function Circle(): ReactNode {
    return (
        <div>
            <Image src={circle} alt="circle" width={500} height={500} />
            The <code>circle :: Length -&gt; Shape</code> function is a shorthand for <code>ellipse r r</code>,
                where <code>r</code> is the radius of the circle.
        </div>
    );
}
