import { Card, CardActionArea, CardMedia, Typography } from "@mui/material";
import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import type { StaticImageData } from "next/image";
import styles from "styles/components/pages/home/examples.module.css";

/**
 * Render example sketches.
 * @param props - The properties of the component.
 * @param props.examples - The examples to render.
 * @returns The examples with images.
 */
export function Examples({ examples }: {
    examples: Array<{ code: string; image: StaticImageData; name: string; }>;
}): ReactNode {
    return (
        <div className={styles.examplesContainer}>
            <div className={styles.examplesScroll}>
                {examples.map(({ name, code, image }, i) => (
                    <Card className={styles.example!} key={i}>
                        <CardActionArea
                            LinkComponent={Link}
                            href={`/editor?code=${code}&title=${encodeURIComponent(name)}&author=examples`}>
                            <CardMedia>
                                <Image src={image} alt={name} width={1000} height={600} />
                            </CardMedia>
                            <Typography component="p">{name}</Typography>
                        </CardActionArea>
                    </Card>
                ))}
            </div>
        </div>
    );
}
