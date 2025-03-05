"use client";

import { ArrowCircleRight, Cancel } from "@mui/icons-material";
import { Dialog, IconButton, Typography } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { redirect } from "next/navigation";
import styles from "styles/components/menu.module.css";
import { useOutsideClick } from "hooks/useOutsideClick";

/**
 * This is the unsaved code warning menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the warning menu is open.
 * @param props.setOpen - The function to set the warning menu state.
 * @param props.url - The URL to redirect to.
 * @returns The unsaved code warning menu element.
 */
export function UnsavedWarningMenu({ open, setOpen, url }: {
    open: boolean;
    setOpen: Dispatch<SetStateAction<boolean>>;
    url: string;
}): ReactNode {
    const ref = useOutsideClick<HTMLDivElement>(() => setOpen(false));

    const newOptions: Array<{ action: () => void; icon: ReactNode; label: string; }> = [
        {
            action: (): void => {
                setOpen(false);
            },
            icon: <Cancel />,
            label: "Cancel",
        },
        {
            action: (): void => {
                setOpen(false);
                redirect(url);
            },
            icon: <ArrowCircleRight />,
            label: "Continue",
        },
    ];

    return (
        <Dialog open={open} onClose={() => setOpen(false)} ref={ref}>
            <div className={styles.dialog}>
                <Typography variant="h5">You have unsaved changes.</Typography>
                <Typography variant="h6">
                    These changes will be lost if you continue. Are you sure you want to continue?
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
