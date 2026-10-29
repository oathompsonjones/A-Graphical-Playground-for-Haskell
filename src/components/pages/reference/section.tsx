import type { ReactNode } from "react";
import type { SectionType } from "app/(pages)/reference/page";
import { Typography } from "@mui/material";
import type { TypographyVariant } from "@mui/material";
import styles from "styles/components/pages/reference/section.module.css";

/**
 * Displays a section of the reference page.
 * @param props - The properties of the component.
 * @param props.title - The heading of the section.
 * @param props.content - The documentation of the section.
 * @param props.depth - The recursion depth of the section.
 * @param props.colored - Whether the section should be colored.
 * @returns A section of the reference page.
 */
export function Section({ title, content, depth, colored }: {
    readonly title: string;
    readonly content: SectionType;
    readonly depth: number;
    readonly colored: boolean;
}): ReactNode {
    const list = (items: ReactNode[]): Awaited<ReactNode> => <ul>{items.map((item, i) => <li key={i}>{item}</li>)}</ul>;

    return (
        <div className={`${styles.wrapper} ${colored && depth === 0 ? styles.colored : ""} edge wrapper`}>
            <br />
            {title !== "root" &&
                <Typography
                    className={styles.title}
                    id={title.toLowerCase().replace(/\s/g, "-")}
                    variant={`h${depth + 3}` as TypographyVariant}
                >{title}<hr />
                </Typography>}
            {content instanceof Function
                ? content(list)
                : Object.entries(content)
                    .map(([subtitle, subcontent], i) => (
                        <Section colored={colored} content={subcontent} depth={depth + 1} key={i} title={subtitle} />
                    ))}
        </div>
    );
}
