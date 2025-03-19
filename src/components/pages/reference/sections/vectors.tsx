import type { ReactNode } from "react";

/**
 * Displays the vectors documentation section.
 * @param props - The properties of the component.
 * @param props.list - A function to use to create a list element.
 * @returns A documentation section.
 */
export function Vectors({ list }: { list: (elems: ReactNode[]) => ReactNode; }): ReactNode {
    return (
        <div>
            To represent a point, you use the <code>Vector</code> data type, which stores an <code>x</code> and
                a <code>y</code> value. To create a vector, you call the <code>Vector</code> constructor directly, like
                this: <code>Vector 1 2</code>.
            <br />
            <br />
            Vectors support the following operators:
            {list([
                <><code>(^+^) :: Vector -&gt; Vector -&gt; Vector</code> — Adds the second vector to the first.</>,
                <><code>(^-^) :: Vector -&gt; Vector -&gt; Vector</code> — Subtracts the second vector from the
                    first.</>,
                <><code>(^*^) :: Vector -&gt; Float -&gt; Vector</code> — Multiplies each component of the vector by the
                    given scalar value.</>,
                <><code>(^/^) :: Vector -&gt; Float -&gt; Vector</code> — Divides each component of the vector by the
                    given scalar value.</>,
            ])}
            and the following functions:
            {list([
                <><code>mag :: Vector -&gt; Float</code> — Calculates the magnitude (length) of the vector.</>,
                <><code>arg :: Vector -&gt; Float</code> — Calculates the argument (angle) of the vector.</>,
                <><code>norm :: Vector -&gt; Vector</code> — Calculates the normal (unit) vector.</>,
                <><code>dot :: Vector -&gt; Vector -&gt; Float</code> — Calculates the dot product of the two
                    vectors.</>,
                <><code>cross :: Vector -&gt; Vector -&gt; Float</code> — Calculates the cross product of the two
                    vectors.</>,
            ])}
        </div>
    );
}
