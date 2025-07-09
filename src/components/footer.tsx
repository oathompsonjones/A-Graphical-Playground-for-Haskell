import { Stack, Typography } from "@mui/material";
import Link from "next/link";
import type { ReactNode } from "react";
import styles from "styles/components/footer.module.css";

/**
 * Contains the footer element.
 * @returns The page footer.
 */
export function Footer(): ReactNode {
    const divider = <Typography className={styles.divider!}>•</Typography>;

    return (
        <footer className={`${styles.footer} full-width`}>
            <Stack direction={{ md: "row" }} spacing={2} justifyContent="center" divider={divider}>
                <Typography component={Link} href="/dissertation">Dissertation</Typography>
                <Typography component={Link} href="/issues">Issue Tracker</Typography>
                <Typography component={Link} href="/privacy">Privacy Policy</Typography>
                <Typography>Copyright © 2024-{String(new Date().getFullYear()).slice(2)} Oliver Jones</Typography>
            </Stack>
        </footer>
    );
}
