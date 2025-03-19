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
 * @param props.colored - Whether the section should be colored.
 * @returns A section of the reference page.
 */
export function Section({ title, content, depth, colored }: {
    title: string;
    content: SectionType;
    depth: number;
    colored: boolean;
}): ReactNode {
    const list = (items: ReactNode[]): Awaited<ReactNode> => <ul>{items.map((item, i) => <li key={i}>{item}</li>)}</ul>;

    return (
        <div className={`${styles.wrapper} ${colored && depth === 0 ? styles.colored : ""} edge wrapper`}>
            <br />
            {title !== "root" &&
                <Typography
                    variant={`h${depth + 3}` as Variant}
                    id={title.toLowerCase().replace(/\s/g, "-")}
                    className={styles.title!}
                >{title}<hr /></Typography>
            }
            {content instanceof Function
                ? content(list)
                : Object.entries(content)
                    .map(([subtitle, subcontent], i) => (
                        <Section title={subtitle} content={subcontent} depth={depth + 1} colored={colored} key={i} />
                    ))}
        </div>
    );
}
