"use client";

import { Grid, Typography } from "@mui/material";
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
        <div>
            <Typography variant="h3">Editor Controls</Typography>
            {controls.map(([icon, text], i) => (
                <Grid size={12} alignItems="center" container key={i}>
                    <Grid size={2} component={Typography} variant="h4">{metaKey}{icon}</Grid>
                    <Grid size={10}>{text}</Grid>
                </Grid>
            ))}
        </div>
    );
}
