"use client";

import { Dialog, IconButton, Typography } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { FileOpen, Save } from "@mui/icons-material";
import styles from "styles/components/pages/editor/menu.module.css";
import { useOutsideClick } from "hooks/useOutsideClick";

/**
 * This is the open warning menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the menu is open.
 * @param props.setOpen - The function to set the menu state.
 * @param props.openFn - The function to open a sketch.
 * @param props.save - The function to save the sketch.
 * @returns The open warning menu element.
 */
export function OpenWarningMenu({ open, setOpen, openFn, save }: {
    open: boolean;
    setOpen: Dispatch<SetStateAction<boolean>>;
    openFn: () => void;
    save: () => void;
}): ReactNode {
    const ref = useOutsideClick<HTMLDivElement>(() => setOpen(false));

    const newOptions: Array<{ action: () => void; icon: ReactNode; label: string; }> = [
        {
            action: (): void => {
                setOpen(false);
                openFn();
            },
            icon: <FileOpen />,
            label: "Open Sketch",
        },
        {
            action: (): void => {
                setOpen(false);
                save();
            },
            icon: <Save />,
            label: "Save Sketch",
        },
    ];

    return (
        <Dialog open={open} onClose={() => setOpen(false)} ref={ref}>
            <div className={styles.dialog}>
                <Typography variant="h5">You have unsaved changes.</Typography>
                <Typography variant="h6">
                    Would you like to save your changes before opening a sketch?
                </Typography>
                <div className={styles.options}>
                    {newOptions.map(({ action, icon, label }, i) => (
                        <div key={i}>
                            <IconButton onClick={action}>{icon}</IconButton>
                            <Typography>{label}</Typography>
                        </div>
                    ))}
                </div>
            </div>
        </Dialog>
    );
}
