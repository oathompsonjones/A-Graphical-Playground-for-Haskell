"use client";

import { Grid2, Typography } from "@mui/material";
import { KeyboardCommandKey, KeyboardControlKey } from "@mui/icons-material";
import { useEffect, useState } from "react";
import type { ReactNode } from "react";

/**
 * Displays the editor controls section of the reference page.
 * @param props - The properties of the component.
 * @param props.controls - The editor controls to display.
 * @returns The editor controls section of the reference page.
 */
export function Controls({ controls }: { controls: Array<[ReactNode, ReactNode]>; }): ReactNode {
    const [metaKey, setMetaKey] = useState(<KeyboardControlKey />);

    useEffect(() => {
        if (navigator.platform.includes("Mac"))
            setMetaKey(<KeyboardCommandKey />);
    }, []);

    return (
        <Grid2 container alignItems="center">
            <Grid2 size={12} component={Typography} variant="h4">Editor Controls</Grid2>
            {controls.map(([icon, text]) => (
                <>
                    <Grid2 size={2} component={Typography} variant="h4">{metaKey}{icon}</Grid2>
                    <Grid2 size={10}>{text}</Grid2>
                </>
            ))}
        </Grid2>
    );
}
