import Link from "next/link";
import type { ReactNode } from "react";
import { Typography } from "@mui/material";
import styles from "styles/components/footer.module.css";

/**
 * Contains the footer element.
 * @returns The page footer.
 */
export function Footer(): ReactNode {
    return (
        <footer className={styles.footer}>
            <Typography>
                <Link href="/privacy">Privacy Policy</Link> • Copyright © 2024 Oliver Jones
            </Typography>
        </footer>
    );
}
