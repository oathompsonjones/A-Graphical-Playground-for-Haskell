import { AppBar, MenuItem, Toolbar, Typography } from "@mui/material";
import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import logo from "assets/images/logo.png";
import styles from "styles/components/header.module.css";

/**
 * Contains the header element.
 * @returns The page header.
 */
export function Header(): ReactNode {
    return (
        <AppBar component="header" className={styles.header!}>
            <Toolbar component="nav" className={styles.nav!}>
                <Image src={logo} alt="Haskell Logo" style={{ height: "3rem", width: "auto" }} />
                <Typography variant="h5">A Graphical Playground for Haskell</Typography>
                <MenuItem component={Link} href="/">Home</MenuItem>
                <MenuItem component={Link} href="/editor">Editor</MenuItem>
            </Toolbar>
        </AppBar>
    );
}
