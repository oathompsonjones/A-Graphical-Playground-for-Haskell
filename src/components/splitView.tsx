"use client";

import type { MouseEvent, ReactElement, ReactNode } from "react";
import { useRef, useState } from "react";
import { DragHandleRounded } from "@mui/icons-material";
import styles from "styles/components/splitView.module.css";

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

    const [canDrag, setCanDrag] = useState(false);
    const container = useRef<HTMLDivElement>(null!);
    const handleDrag = (event: MouseEvent<HTMLDivElement>): void => {
        if (canDrag) {
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
        }
    };

    return (
        <div className={`${splitViewClass} ${className}`} ref={container} onMouseMove={handleDrag}>
            {children[0]}
            <div className={styles.splitter} onMouseDown={() => setCanDrag(true)} onMouseUp={() => setCanDrag(false)}>
                <DragHandleRounded className={styles.dragIcon!} />
            </div>
            {children[1]}
        </div>
    );
}
