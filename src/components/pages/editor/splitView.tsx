"use client";

import type { MouseEvent, ReactElement, ReactNode } from "react";
import { useEffect, useRef, useState } from "react";
import { DragHandleRounded } from "@mui/icons-material";
import styles from "styles/components/splitView.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";
import { useWindowSize } from "hooks/useWindowSize";

/**
 * Creates a resizable split view.
 * @param props - The properties of the split view.
 * @param props.children - The two elements to split.
 * @param props.id - The unique identifier of the split view.
 * @param props.vertical - Whether the split view is vertical.
 * @returns The split view element.
 */
export function SplitView({ children, id, vertical }: {
    children: [ReactElement, ReactElement];
    id: string;
    vertical?: boolean;
}): ReactNode {
    const { height, width } = useWindowSize();
    const container = useRef<HTMLDivElement & { children: [HTMLDivElement, HTMLDivElement, HTMLDivElement]; }>(null!);
    const [size, setSize] = useLocalStorage(`${id}-size`, 0.5);
    const [canDrag, setCanDrag] = useState(false);

    const orientation = vertical ?? false ? "vertical" : "horizontal";

    const updateSizes = (): void => {
        const [div1, , div2] = container.current.children;

        switch (orientation) {
            case "horizontal":
                div1.style.width = `${100 * size}%`;
                div2.style.width = `${100 * (1 - size)}%`;
                break;
            case "vertical":
                div1.style.height = `${100 * size}%`;
                div2.style.height = `${100 * (1 - size)}%`;
                break;
        }
    };

    const handleDrag = (event: MouseEvent<HTMLDivElement>): void => {
        if (!canDrag)
            return;

        const rect = container.current.getBoundingClientRect();

        switch (orientation) {
            case "horizontal":
                setSize((event.clientX - rect.left) / rect.width);
                break;
            case "vertical":
                setSize((event.clientY - rect.top) / rect.height);
                break;
        }
    };

    useEffect(updateSizes, [height, width, size]);

    return (
        <div
            className={`${styles.container} ${styles[orientation]}`}
            ref={container}
            onMouseMove={handleDrag}
            onMouseLeave={setCanDrag.bind(null, false)}
            onMouseUp={setCanDrag.bind(null, false)}
        >
            {children[0]}
            <div className={styles.splitter} onMouseDown={setCanDrag.bind(null, true)}>
                <DragHandleRounded className={styles.dragIcon!} />
            </div>
            {children[1]}
        </div>
    );
}
