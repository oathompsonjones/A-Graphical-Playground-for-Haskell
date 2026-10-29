"use client";

/* eslint-disable react/jsx-no-bind */

import { Button, Card, CardContent, IconButton, Tooltip, Typography } from "@mui/material";
import type { Dispatch, SetStateAction } from "react";
import { type ReactNode, useContext, useState } from "react";
import { Delete } from "@mui/icons-material";
import { DeleteWarningMenu } from "components/menus/deleteWarningMenu";
import { SketchContext } from "contexts/sketch";
import type { Sketch as SketchSchema } from "schemas/database";
import { UnsavedWarningMenu } from "components/menus/unsavedWarningMenu";
import { redirect } from "next/navigation";
import styles from "styles/components/pages/account/sketch.module.css";

/**
 * Displays a sketch.
 * @param props - The properties of the component.
 * @param props.sketch - The sketch to display.
 * @param props.setSketches - The function to call to update the sketches.
 * @returns The sketch element.
 */
export function Sketch({ sketch, setSketches }: {
    readonly sketch: SketchSchema;
    readonly setSketches: Dispatch<SetStateAction<SketchSchema[] | null>>;
}): ReactNode {
    const { saved } = useContext(SketchContext);
    const [openDelete, setOpenDelete] = useState(false);
    const [openUnsaved, setOpenUnsaved] = useState(false);
    const url = `/editor?id=${encodeURIComponent(sketch._id.toString())}`;

    const handleOpenClick = (): void => {
        if (saved)
            redirect(url);
        else
            setOpenUnsaved(true);
    };

    return (
        <>
            <DeleteWarningMenu
                id={sketch._id.toString()} open={openDelete}
                setOpen={setOpenDelete} setSketches={setSketches}
            />
            <UnsavedWarningMenu open={openUnsaved} setOpen={setOpenUnsaved} url={url} />
            <Card>
                <CardContent className={styles.sketch}>
                    <Typography variant="h5">
                        <Button color="secondary" onClick={handleOpenClick} variant="text">
                            <Typography variant="h6">
                                {sketch.name}
                            </Typography>
                        </Button>
                    </Typography>
                    <Tooltip arrow placement="left" title="Delete">
                        <IconButton color="error" onClick={() => setOpenDelete(true)} size="small">
                            <Delete />
                        </IconButton>
                    </Tooltip>
                </CardContent>
                <CardContent>
                    <Typography color="textDisabled" variant="caption">
                        <b>Created At: </b>
                        {new Date(parseInt(sketch.createdAt, 10)).toLocaleString()}
                    </Typography>
                    <br />
                    <Typography color="textDisabled" variant="caption">
                        <b>Modified At: </b>
                        {new Date(parseInt(sketch.modifiedAt, 10)).toLocaleString()}
                    </Typography>
                </CardContent>
            </Card>
        </>
    );
}
