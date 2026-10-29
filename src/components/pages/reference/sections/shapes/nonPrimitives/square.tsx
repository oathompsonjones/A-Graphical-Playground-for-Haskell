import Image from "next/image";
import type { ReactNode } from "react";
import square from "assets/images/docs/square.png";

/**
 * Displays the square documentation section.
 * @returns A documentation section.
 */
export function Square(): ReactNode {
    return (
        <div>
            <Image alt="square" height={500} src={square} width={500} />
            The <code>square :: Length -&gt; Shape</code> function is a shorthand for <code>rect s s</code>,
            where <code>s</code> is the side length of the square.
        </div>
    );
}
