"use client";

import { Stack, Typography } from "@mui/material";
import Link from "next/link";
import type { ReactNode } from "react";
import styles from "styles/components/footer.module.css";

/**
 * Contains the footer element.
 * @returns The page footer.
 */
export function Footer(): ReactNode {
    const divider = <Typography className={styles.divider}>•</Typography>;

    return (
        <footer className={`${styles.footer} full-width`}>
            <Stack direction={{ md: "row" }} divider={divider} spacing={2} sx={{ justifyContent: "center" }}>
                <Typography>
                    <Link href="/dissertation">Dissertation</Link>
                </Typography>
                <Typography>
                    <Link href="/issues">Issue Tracker</Link>
                </Typography>
                <Typography>
                    <Link href="/privacy">Privacy Policy</Link>
                </Typography>
                <Typography>© 2024-{String(new Date().getFullYear()).slice(2)} Oliver Jones</Typography>
            </Stack>
            <Typography>
                Website Designed and Developed by <Link href="https://oathompsonjones.co.uk">Oliver Jones</Link>.
            </Typography>
        </footer>
    );
}
