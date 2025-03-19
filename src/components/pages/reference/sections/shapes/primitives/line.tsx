import Image from "next/image";
import type { ReactNode } from "react";
import line from "assets/images/docs/line.png";

/**
 * Displays the line documentation section.
 * @returns A documentation section.
 */
export function Line(): ReactNode {
    return (
        <div>
            <Image src={line} alt="line" width={500} height={500} />
            The <code>line :: Length -&gt; Shape</code> function takes a length and returns a line.
            <br />
            <br />
            The line extends in the positive x-direction.
        </div>
    );
}
