"use client";

/* eslint-disable react/jsx-no-bind */

import { Button, Dialog, FormControl, TextField } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { NotificationsContext } from "contexts/notifications";
import { SketchContext } from "contexts/sketch";
import { UserContext } from "contexts/user";
import { saveSketch } from "actions/code/saveSketch";
import styles from "styles/components/menu.module.css";
import { useContext } from "react";
import { useOutsideClick } from "hooks/useOutsideClick";

/**
 * This is the save menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the menu is open.
 * @param props.setOpen - The function to set the menu state.
 * @returns The save menu element.
 */
export function SaveMenu({ open, setOpen }: {
    readonly open: boolean;
    readonly setOpen: Dispatch<SetStateAction<boolean>>;
}): ReactNode {
    const { user } = useContext(UserContext);
    const { setNotification } = useContext(NotificationsContext);
    const { code, setSaved, setId } = useContext(SketchContext);
    const ref = useOutsideClick<HTMLDivElement>(() => setOpen(false));

    const saveSketchAction = (formData: FormData): void => {
        saveSketch(formData).then((id: string) => {
            setOpen(false);
            setSaved(true);
            setId(id);
            setNotification("Sketch saved.", "success");
        }).catch((e: unknown) => {
            setNotification(e instanceof Error ? e.message : "An error occurred.", "error");
        });
    };

    return user !== null && (
        <Dialog onClose={() => setOpen(false)} open={open} ref={ref}>
            <div className={styles.dialog}>
                <FormControl action={saveSketchAction} component="form">
                    <TextField name="content" sx={{ opacity: 0 }} type="hidden" value={code} />
                    <TextField name="authorId" sx={{ opacity: 0 }} type="hidden" value={user._id.toString()} />
                    <TextField label="Name" name="name" />
                    <br />
                    <Button type="submit">Save</Button>
                </FormControl>
            </div>
        </Dialog>
    );
}
