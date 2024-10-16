"use client";

import type { MouseEvent, ReactElement, ReactNode } from "react";
import { DragHandle } from "@mui/icons-material";
import styles from "styles/components/splitView.module.css";
import { useRef } from "react";

/**
 * Creates a resizable split view.
 * @param props - The properties of the split view.
 * @param props.className - Any additional CSS classes to apply.
 * @param props.children - The two elements to split.
 * @param props.vertical - Whether the split view is vertical.
 * @returns The split view element.
 */
export function SplitView({ className, children, vertical }: {
    className?: string;
    children: [ReactElement, ReactElement];
    vertical?: boolean;
}): ReactNode {
    const orientation = vertical ?? false ? "vertical" : "horizontal";
    const splitViewClass = `${styles.container} ${styles[orientation]}`;

    const container = useRef<HTMLDivElement>(null!);
    const handleDrag = (event: MouseEvent<HTMLDivElement>): void => {
        const firstSection = container.current.children[0] as HTMLDivElement;
        const secondSection = container.current.children[2] as HTMLDivElement;
        const containerRect = container.current.getBoundingClientRect();

        switch (orientation) {
            case "horizontal": {
                const offsetX = event.clientX - containerRect.left;
                const leftWidth = offsetX / containerRect.width * 100;
                const rightWidth = 100 - leftWidth;

                firstSection.style.width = `${leftWidth}%`;
                secondSection.style.width = `${rightWidth}%`;
                break;
            }
            case "vertical": {
                const offsetY = event.clientY - containerRect.top;
                const topHeight = offsetY / containerRect.height * 100;
                const bottomHeight = 100 - topHeight;

                firstSection.style.height = `${topHeight}%`;
                secondSection.style.height = `${bottomHeight}%`;
                break;
            }
        }
    };

    return (
        <div className={className === undefined ? splitViewClass : `${splitViewClass} ${className}`} ref={container}>
            {children[0]}
            <div className={styles.splitter} onDrag={handleDrag} draggable>
                <DragHandle className={styles.dragIcon!} />
            </div>
            {children[1]}
        </div>
    );
}
