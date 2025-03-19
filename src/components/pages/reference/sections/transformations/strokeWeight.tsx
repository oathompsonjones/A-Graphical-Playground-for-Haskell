import Image from "next/image";
import type { ReactNode } from "react";
import strokeWeight from "assets/images/docs/strokeWeight.png";

/**
 * Displays the strokeWeight documentation section.
 * @returns A documentation section.
 */
export function StrokeWeight(): ReactNode {
    return (
        <div>
            <Image src={strokeWeight} alt="strokeWeight" width={500} height={500} />
            The <code>strokeWeight :: Float -&gt; Shape -&gt; Shape</code> function sets the stroke thickness of the
                shape.
        </div>
    );
}
