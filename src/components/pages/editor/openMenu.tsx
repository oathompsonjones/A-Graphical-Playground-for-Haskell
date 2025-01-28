"use client";

import { Dialog, Typography } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { useContext, useEffect, useState } from "react";
import type { Sketch } from "schemas/database";
import { UserContext } from "contexts/user";
import { getSketches } from "database/index";
import styles from "styles/components/pages/editor/openMenu.module.css";
import { useOutsideClick } from "hooks/useOutsideClick";

/**
 * This is the share menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the share menu is open.
 * @param props.setOpen - The function to set the share menu state.
 * @param props.author - The author of the code.
 * @returns The share menu element.
 */
export function OpenMenu({ open, setOpen, author }: {
    open: boolean;
    setOpen: Dispatch<SetStateAction<boolean>>;
    author: string | null;
}): ReactNode {
    const { user } = useContext(UserContext);
    const ref = useOutsideClick<HTMLDivElement>(() => setOpen(false));
    const [sketches, setSketches] = useState<Sketch[] | null>(null);

    const onSketchClick = (sketch: Sketch): void => {
        window.location.search = `code=${sketch.content}&title=${sketch.name}&author=${author}`;
    };

    const fetchSketches = (): void => {
        if (user && sketches === null) {
            getSketches(user._id.toString())
                .then((json) => setSketches(JSON.parse(json) as Sketch[]))
                .catch(() => undefined);
        }
    };

    useEffect(fetchSketches, []);

    return (
        <Dialog open={open} onClose={() => setOpen(false)} ref={ref}>
            {user !== null && <div className={styles.dialog}>
                {sketches === null || sketches.length === 0
                    ? <Typography>You have no saved sketches.</Typography>
                    : <>
                        <Typography variant="h6">Your Sketches</Typography>
                        {sketches.map((sketch, i) => (
                            <div key={i}>
                                <a onClick={() => onSketchClick(sketch)}>{sketch.name}</a>
                            </div>
                        ))}
                    </>}
            </div>}
        </Dialog>
    );
}
