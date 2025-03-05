"use client";

import { Cancel, Delete } from "@mui/icons-material";
import { Dialog, IconButton, Typography } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { deleteSketch, getSketches } from "database/index";
import type { Sketch } from "schemas/database";
import { UserContext } from "contexts/user";
import styles from "styles/components/menu.module.css";
import { useContext } from "react";
import { useOutsideClick } from "hooks/useOutsideClick";

/**
 * This is the delete warning menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the warning menu is open.
 * @param props.setOpen - The function to set the warning menu state.
 * @param props.id - The ID of the sketch to delete.
 * @param props.setSketches - The function to call to update the sketches.
 * @returns The delete warning menu element.
 */
export function DeleteWarningMenu({ open, setOpen, id, setSketches }: {
    open: boolean;
    setOpen: Dispatch<SetStateAction<boolean>>;
    id: string;
    setSketches: Dispatch<SetStateAction<Sketch[] | null>>;
}): ReactNode {
    const { user } = useContext(UserContext);
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

                if (user) {
                    deleteSketch(id)
                        .then(async () => getSketches(user._id.toString())
                            .then((json) => setSketches(JSON.parse(json) as Sketch[]))
                            .catch(() => undefined))
                        .catch(() => undefined);
                }
            },
            icon: <Delete />,
            label: "Delete",
        },
    ];

    return (
        <Dialog open={open} onClose={() => setOpen(false)} ref={ref}>
            <div className={styles.dialog}>
                <Typography variant="h5">Are you sure you want to continue?</Typography>
                <Typography variant="h6">
                    Deleting this sketch will remove it from your account permanently. This action cannot be undone.
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
