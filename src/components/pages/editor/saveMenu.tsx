"use client";

import { Button, Dialog, FormControl, TextField } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { NotificationsContext } from "contexts/notifications";
import { UserContext } from "contexts/user";
import { saveSketch } from "actions/code/saveSketch";
import styles from "styles/components/pages/editor/saveMenu.module.css";
import { useContext } from "react";
import { useOutsideClick } from "hooks/useOutsideClick";

// TODO: Make this look better and add ability to update currently saved sketch.

/**
 * This is the share menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the share menu is open.
 * @param props.setOpen - The function to set the share menu state.
 * @param props.code - The code to share.
 * @returns The share menu element.
 */
export function SaveMenu({ open, setOpen, code }: {
    open: boolean;
    setOpen: Dispatch<SetStateAction<boolean>>;
    code: string;
}): ReactNode {
    const { user } = useContext(UserContext);
    const { setType, setMessage } = useContext(NotificationsContext);
    const ref = useOutsideClick<HTMLDivElement>(() => setOpen(false));

    const saveSketchAction = (formData: FormData): void => {
        saveSketch(formData).then(() => {
            setOpen(false);
            setType("success");
            setMessage("Sketch saved.");
        }).catch((e: unknown) => {
            setType("error");
            setMessage(e instanceof Error ? e.message : "An error occurred.");
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
