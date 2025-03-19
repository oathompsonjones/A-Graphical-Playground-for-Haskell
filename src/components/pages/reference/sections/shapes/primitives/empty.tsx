import type { ReactNode } from "react";

/**
 * Displays the segment documentation section.
 * @returns A documentation section.
 */
export function Empty(): ReactNode {
    return (
        <div>
            The <code>empty :: Shape</code> function represents the empty shape.
            <br />
            <br />
            This shape draws nothing.
        </div>
    );
}
