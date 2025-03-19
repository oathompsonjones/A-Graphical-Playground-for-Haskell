import type { ReactNode } from "react";

/**
 * Displays the noStroke documentation section.
 * @returns A documentation section.
 */
export function NoStroke(): ReactNode {
    return (
        <div>
            The <code>noStroke :: Shape -&gt; Shape</code> function is a shorthand for <code>stroke Transparent</code>.
        </div>
    );
}
