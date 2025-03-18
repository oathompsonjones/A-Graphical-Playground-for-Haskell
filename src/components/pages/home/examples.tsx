"use client";

import { Example } from "./example";
import type { ReactNode } from "react";
import type { StaticImageData } from "next/image";
import styles from "styles/components/pages/home/examples.module.css";
import { useEffect } from "react";

/**
 * Render example sketches.
 * @param props - The properties of the component.
 * @param props.examples - The examples to render.
 * @returns The examples with images.
 */
export function Examples({ examples }: {
    examples: Array<{ code: string; image: StaticImageData; name: string; }>;
}): ReactNode {
    // Safari and Chrome can do this by just applying `justify-content: center;` to the container.
    // Firefox needs a little help.
    useEffect(() => {
        const containers = document.querySelectorAll(`.${styles.examplesScroll}`);

        for (const container of containers)
            container.scrollLeft = (container.scrollWidth - container.clientWidth) / 2;
    }, []);

    return (
        <div className={styles.examplesContainer}>
            <div className={styles.examplesScroll}>
                {examples.map(({ name, code, image }, i) => <Example key={i} code={code} image={image} name={name} />)}
            </div>
        </div>
    );
}
