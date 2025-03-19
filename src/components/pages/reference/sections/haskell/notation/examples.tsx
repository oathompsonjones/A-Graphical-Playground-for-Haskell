import type { ReactNode } from "react";

/**
 * Displays the notation examples documentation section.
 * @returns A documentation section.
 */
export function NotationExamples(): ReactNode {
    return (
        <div>
            <code>add :: Int -&gt; Int -&gt; Int</code> — A function that takes two integers and returns an integer
                (<code>add 6 7</code> = <code>13</code>).
            <br />
            <code>(+) :: Int -&gt; Int -&gt; Int</code> — The addition operator, which takes two integers and returns an
                integer (<code>6 + 7</code> = <code>13</code>).
            <br />
            <br />
            <code>map :: (a -&gt; b) -&gt; [a] -&gt; [b]</code> — A function that takes another function, which converts
                from type <code>a</code> to type <code>b</code>, and a list of <code>a</code>s and returns a list
                of <code>b</code>s, by applying the given function to each element of the list (<code>map (add 5)
                [1, 2, 3, 4, 5]</code> = <code>[6, 7, 8, 9, 10]</code>).
        </div>
    );
}
