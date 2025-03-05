import { Example } from "./example";
import type { ReactNode } from "react";
import type { StaticImageData } from "next/image";
import styles from "styles/components/pages/home/examples.module.css";

/**
 * Render example sketches.
 * @param props - The properties of the component.
 * @param props.examples - The examples to render.
 * @returns The examples with images.
 */
export function Examples({ examples }: {
    examples: Array<{ code: string; image: StaticImageData; name: string; }>;
}): ReactNode {
    return (
        <div className={styles.examplesContainer}>
            <div className={styles.examplesScroll}>
                {examples.map(({ name, code, image }, i) => <Example key={i} code={code} image={image} name={name} />)}
            </div>
        </div>
    );
}
