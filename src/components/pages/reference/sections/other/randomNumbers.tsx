import type { ReactNode } from "react";

/**
 * Displays the random numbers documentation section.
 * @param props - The properties of the component.
 * @param props.list - A function to use to create a list element.
 * @returns A documentation section.
 */
export function RandomNumbers({ list }: { list: (elems: ReactNode[]) => ReactNode; }): ReactNode {
    return (
        <div>
            The following functions can be used to generate random numbers:
            {list([
                <span key={1}><code>randoms :: Int -&gt; [Double]</code> — Generates an infinite list of random numbers
                    between 0 and 1, using the given seed.</span>,
                <span key={2}><code>seed :: IO Int</code> — Returns a random seed value, which you can use to generate
                    random numbers. The seed is generated using the current time, and is therefore wrapped in the <code>
                    IO</code> monad. You can use the seed value as follows:
                <pre>{[
                    "main :: IO ()",
                    "main = do",
                    "  s <- seed",
                    "  print $ take 10 (randoms s)",
                ].join("\n")}</pre></span>,
            ])}
        </div>
    );
}
