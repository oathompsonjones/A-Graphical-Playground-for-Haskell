import { Accordion, AccordionDetails, AccordionSummary, Typography } from "@mui/material";
import { ExpandMore } from "@mui/icons-material";
import Link from "next/link";
import type { ReactNode } from "react";
import type { SectionType } from "app/(pages)/reference/page";

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
                        <Accordion disableGutters>
                            <AccordionSummary expandIcon={typeof content === "object" && <ExpandMore />}>
                                <Typography>
                                    <Link href={`#${title.toLowerCase().replace(/\s/g, "-")}`}>{title}</Link>
                                </Typography>
                            </AccordionSummary>
                            {typeof content === "object" && (
                                <AccordionDetails>
                                    <Contents docs={content} />
                                </AccordionDetails>
                            )}
                        </Accordion>
                    </li>
                ))}
        </ul>
    );
}
