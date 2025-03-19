import type { ReactNode } from "react";

/**
 * Displays the noFill documentation section.
 * @returns A documentation section.
 */
export function NoFill(): ReactNode {
    return (
        <div>
            The <code>noFill :: Shape -&gt; Shape</code> function is a shorthand for <code>fill Transparent</code>.
        </div>
    );
}
