import { Stack, Typography } from "@mui/material";
import Link from "next/link";
import type { ReactNode } from "react";
import styles from "styles/components/footer.module.css";

/**
 * Contains the footer element.
 * @returns The page footer.
 */
export function Footer(): ReactNode {
    return (
        <footer className={styles.footer}>
            <Stack direction="row" spacing={2} divider={<Typography>•</Typography>} justifyContent="center">
                <Typography component={Link} href="https://oathompsonjones.github.io/Honours-Project">
                    Dissertation
                </Typography>
                <Typography component={Link} href="/privacy">
                    Privacy Policy
                </Typography>
                <Typography>
                    Copyright © 2024-{new Date().getFullYear()} Oliver Jones
                </Typography>
            </Stack>
        </footer>
    );
}
