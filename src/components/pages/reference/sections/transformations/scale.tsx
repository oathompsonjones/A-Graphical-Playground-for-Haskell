import Image from "next/image";
import type { ReactNode } from "react";
import scale from "assets/images/docs/scale.png";

/**
 * Displays the scale documentation section.
 * @returns A documentation section.
 */
export function Scale(): ReactNode {
    return (
        <div>
            <Image src={scale} alt="scale" width={500} height={500} />
            The <code>scale :: Float -&gt; Shape -&gt; Shape</code> function scales the shape by the given scale factor.
            <br />
            <br />
            Once again, the scale factor is applied about the shape's origin point.
        </div>
    );
}
