"use client";

import { Button, Dialog, FormControl, TextField } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { NotificationsContext } from "contexts/notifications";
import { UserContext } from "contexts/user";
import { saveSketch } from "actions/code/saveSketch";
import styles from "styles/components/pages/editor/menu.module.css";
import { useContext } from "react";
import { useOutsideClick } from "hooks/useOutsideClick";

/**
 * This is the save menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the menu is open.
 * @param props.setOpen - The function to set the menu state.
 * @param props.code - The code to save.
 * @param props.setSaved - The function to set the saved state.
 * @param props.setId - The function to set the ID of the sketch.
 * @returns The save menu element.
 */
export function SaveMenu({ open, setOpen, code, setSaved, setId }: {
    open: boolean;
    setOpen: Dispatch<SetStateAction<boolean>>;
    code: string;
    setSaved: Dispatch<SetStateAction<boolean>>;
    setId: Dispatch<SetStateAction<string | null>>;
}): ReactNode {
    const { user } = useContext(UserContext);
    const { setNotification } = useContext(NotificationsContext);
    const ref = useOutsideClick<HTMLDivElement>(() => setOpen(false));

    const saveSketchAction = (formData: FormData): void => {
        saveSketch(formData).then((id: string) => {
            setOpen(false);
            setSaved(true);
            setId(id);
            setNotification("success", "Sketch saved.");
        }).catch((e: unknown) => {
            setNotification("error", e instanceof Error ? e.message : "An error occurred.");
        });
    };

    return user !== null && (
        <Dialog open={open} onClose={() => setOpen(false)} ref={ref}>
            <div className={styles.dialog}>
                <FormControl action={saveSketchAction} component="form">
                    <TextField type="hidden" name="content" value={code} sx={{ opacity: 0 }} />
                    <TextField type="hidden" name="authorId" value={user._id.toString()} sx={{ opacity: 0 }} />
                    <TextField label="Name" name="name" />
                    <br />
                    <Button type="submit">Save</Button>
                </FormControl>
            </div>
        </Dialog>
    );
}
