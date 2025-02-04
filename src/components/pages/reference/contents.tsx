import { Accordion, AccordionDetails, AccordionSummary } from "@mui/material";
import { ExpandMore } from "@mui/icons-material";
import type { ReactNode } from "react";
import type { SectionType } from "app/(pages)/reference/page";
import styles from "styles/components/pages/reference/contents.module.css";

/**
 * Displays the contents of the reference page.
 * @param props - The properties of the component.
 * @param props.docs - The documentation to display.
 * @returns The contents of the reference page.
 */
export function Contents({ docs }: { docs: SectionType; }): ReactNode {
    return (
        <ul>
            {(Object.entries(docs) as Array<[string, SectionType]>).filter(([title]) => title !== "root")
                .map(([title, content], i) => (
                    <li key={i}>
                        {typeof content === "object"
                            ? (
                                <Accordion className={styles.accordion!}>
                                    <AccordionSummary className={styles.summary!} expandIcon={<ExpandMore />}>
                                        <a href={`#${title.toLowerCase().replace(/\s/g, "-")}`}>{title}</a>
                                    </AccordionSummary>
                                    <AccordionDetails className={styles.details!}>
                                        <Contents docs={content} />
                                    </AccordionDetails>
                                </Accordion>
                            )
                            : (
                                <p>
                                    <a href={`#${title.toLowerCase().replace(/\s/g, "-")}`}>{title}</a>
                                </p>
                            )}
                    </li>
                ))}
        </ul>
    );
}
