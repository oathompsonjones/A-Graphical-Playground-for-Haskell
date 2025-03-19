import Image from "next/image";
import type { ReactNode } from "react";
import ellipse from "assets/images/docs/ellipse.png";

/**
 * Displays the ellipse documentation section.
 * @returns A documentation section.
 */
export function Ellipse(): ReactNode {
    return (
        <div>
            <Image src={ellipse} alt="ellipse" width={500} height={500} />
            The <code>ellipse :: Length -&gt; Length -&gt; Shape</code> function takes a horizontal axis and a
                vertical axis and returns an ellipse.
            <br />
            <br />
            The ellipse's origin is at its center.
        </div>
    );
}
