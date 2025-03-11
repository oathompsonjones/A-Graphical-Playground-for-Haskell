"use client";

import type { ReactElement, ReactNode } from "react";
import { Tab, Tabs } from "@mui/material";
import { PlainPaper } from "./plainPaper";
import styles from "styles/components/pages/editor/tabView.module.css";
import { useState } from "react";

/**
 * Creates a tab view.
 * @param props - The properties of the tab view.
 * @param props.children - The elements to tab between.
 * @param props.titles - The titles of the tabs.
 * @returns The tab view element.
 * @throws If the number of titles does not match the number of children.
 */
export function TabView({ children, titles }: { children: ReactElement[]; titles: ReactNode[]; }): ReactNode {
    const [tab, setTab] = useState(0);

    if (titles.length !== children.length)
        throw new Error("The number of titles must match the number of children.");

    return (
        <PlainPaper className={styles.container!}>
            <Tabs value={tab} onChange={(_, newTab: number) => setTab(newTab)}>
                {titles.map((title, i) => <Tab label={title} key={i} />)}
            </Tabs>
            {children.map((child, i) => (
                <div className={`${styles.tab} ${tab === i ? "" : styles.hidden}`} key={i}>
                    {child}
                </div>
            ))}
        </PlainPaper>
    );
}
