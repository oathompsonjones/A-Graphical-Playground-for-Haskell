"use client";

import { AppBar, Avatar, MenuItem, Toolbar, Typography, useMediaQuery } from "@mui/material";
import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import { UserContext } from "contexts/user";
import logo from "assets/images/logo.png";
import styles from "styles/components/header.module.css";
import { useContext } from "react";

/**
 * Contains the header element.
 * @returns The page header.
 */
export function Header(): ReactNode {
    const { user } = useContext(UserContext);
    const edge = useMediaQuery((theme) => theme.breakpoints.down("md"));

    let signInText = user?.username ?? user?.email.split("@")[0] ?? "Sign In";

    if (edge && signInText.length > 10)
        signInText = `${signInText.slice(0, 8)}...`;

    return (
        <AppBar component="header" className={styles.header!}>
            <Toolbar component="nav" className={`${styles.nav} ${edge ? "edge" : "full-width"}`}>
                <div className={styles.title}>
                    <Image src={logo} alt="Haskell Logo" />
                    <Typography variant="h5" component="h5">A Graphical Playground for Haskell</Typography>
                </div>
                <MenuItem className={styles.menuItem!} component={Link} href="/">Home</MenuItem>
                <MenuItem className={styles.menuItem!} component={Link} href="/editor">Editor</MenuItem>
                <MenuItem className={styles.menuItem!} component={Link} href="/reference">Reference</MenuItem>
                <div className={styles.spacer} />
                <MenuItem className={styles.account!} component={Link} href="/account">
                    {signInText}
                    <Avatar src={user?.avatar ?? ""} className={styles.avatar!} />
                </MenuItem>
            </Toolbar>
        </AppBar>
    );
}
