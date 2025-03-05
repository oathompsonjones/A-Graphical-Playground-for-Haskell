"use client";

import { Card, CardActionArea, CardMedia, Typography } from "@mui/material";
import { useContext, useState } from "react";
import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import { SketchContext } from "contexts/sketch";
import type { StaticImageData } from "next/image";
import { UnsavedWarningMenu } from "../../menus/unsavedWarningMenu";
import { redirect } from "next/navigation";
import styles from "styles/components/pages/home/examples.module.css";

/**
 * Render an example sketch.
 * @param props - The properties of the component.
 * @param props.code - The url encoded code of the example.
 * @param props.image - The raw image of the example.
 * @param props.name - The raw name of the example.
 * @returns The example with an image.
 */
export function Example({ name, code, image }: { code: string; image: StaticImageData; name: string; }): ReactNode {
    const { saved } = useContext(SketchContext);

    const url = `/editor?code=${code}&title=${encodeURIComponent(name)}&author=examples`;
    const [open, setOpen] = useState(false);

    const handleClick = (): void => {
        if (saved)
            redirect(url);
        else
            setOpen(true);
    };

    return (
        <>
            <UnsavedWarningMenu open={open} setOpen={setOpen} url={url} />
            <Card className={styles.example!}>
                <CardActionArea LinkComponent={Link} onClick={handleClick}>
                    <CardMedia>
                        <Image src={image} alt={name} width={1000} height={600} />
                    </CardMedia>
                    <Typography component="p">{name}</Typography>
                </CardActionArea>
            </Card>
        </>
    );
}
