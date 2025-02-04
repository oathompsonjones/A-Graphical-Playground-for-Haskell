import type { ReactNode } from "react";
import type { SectionType } from "app/(pages)/reference/page";
import { Typography } from "@mui/material";
import type { Variant } from "@mui/material/styles/createTypography";
import styles from "styles/components/pages/reference/section.module.css";

/**
 * Displays a section of the reference page.
 * @param props - The properties of the component.
 * @param props.title - The heading of the section.
 * @param props.content - The documentation of the section.
 * @param props.depth - The recursion depth of the section.
 * @param props.i - The index of the section.
 * @returns A section of the reference page.
 */
export function Section({ title, content, depth, i }: {
    title: string;
    content: SectionType;
    depth: number;
    i: number;
}): ReactNode {
    return (
        <div key={i} className={`${styles.wrapper} ${i % 2 === 0 && depth === 0 ? styles.colored : ""} edge wrapper`}>
            <br />
            {title !== "root" &&
                <Typography
                    variant={`h${depth + 3}` as Variant}
                    id={title.toLowerCase().replace(/\s/g, "-")}
                    className={styles.title!}
                >{title}<hr /></Typography>
            }
            {content instanceof Function
                ? content()
                : Object.entries(content)
                    .map(([subtitle, subcontent], j) => (
                        <Section title={subtitle} content={subcontent} depth={depth + 1} i={j} />
                    ))}
        </div>
    );
}
