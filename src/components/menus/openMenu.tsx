"use client";

/* eslint-disable react/jsx-no-bind */

import { Dialog, Typography } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { useContext, useEffect, useState } from "react";
import type { Sketch } from "schemas/database";
import { UserContext } from "contexts/user";
import { getSketches } from "database/index";
import styles from "styles/components/menu.module.css";
import { useOutsideClick } from "hooks/useOutsideClick";

/**
 * This is the open menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the menu is open.
 * @param props.setOpen - The function to set the menu state.
 * @returns The open menu element.
 */
export function OpenMenu({ open, setOpen }: {
    readonly open: boolean;
    readonly setOpen: Dispatch<SetStateAction<boolean>>;
}): ReactNode {
    const { user } = useContext(UserContext);
    const ref = useOutsideClick<HTMLDivElement>(() => setOpen(false));
    const [sketches, setSketches] = useState<Sketch[] | null>(null);

    const fetchSketches = (): void => {
        if (user && sketches === null) {
            getSketches(user._id.toString())
                .then((json) => setSketches(JSON.parse(json) as Sketch[]))
                .catch(() => undefined);
        }
    };

    const sketchList = sketches === null || sketches.length === 0
        ? <Typography>You have no saved sketches.</Typography>
        : (
            <div>
                <Typography variant="h5">Your Sketches</Typography>
                {sketches.sort((a, b) => (a.name < b.name ? -1 : 1)).map((sketch, i) => (
                    <div key={i}>
                        <a href={`editor?id=${encodeURIComponent(sketch._id.toString())}`}>{sketch.name}</a>
                    </div>
                ))}
            </div>
        );

    useEffect(fetchSketches, [user]);

    return user !== null && (
        <Dialog onClose={() => setOpen(false)} open={open} ref={ref}>
            <div className={styles.dialog}>
                {sketchList}
            </div>
        </Dialog>
    );
}
