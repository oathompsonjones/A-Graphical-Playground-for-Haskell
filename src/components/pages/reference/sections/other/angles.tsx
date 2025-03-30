import type { ReactNode } from "react";

/**
 * Displays the angles documentation section.
 * @param props - The properties of the component.
 * @param props.list - A function to use to create a list element.
 * @returns A documentation section.
 */
export function Angles({ list }: { list: (elems: ReactNode[]) => ReactNode; }): ReactNode {
    return (
        <div>
            The following functions convert beetween degrees and radians (<code>Degrees</code> and <code>Radians
            </code> are aliases for <code>Float</code>):
            {list([
                <span key={0}><code>degrees :: Radians -&gt; Degrees</code> — Converts radians to degrees.</span>,
                <span key={1}><code>radians :: Degrees -&gt; Radians</code> — Converts degrees to radians.</span>,
            ])}
        </div>
    );
}
