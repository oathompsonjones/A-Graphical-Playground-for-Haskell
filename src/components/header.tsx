"use client";

import { AppBar, Avatar, MenuItem, Toolbar, Typography } from "@mui/material";
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

    return (
        <AppBar component="header" className={styles.header!}>
            <Toolbar component="nav" className={`${styles.nav!} full-width`}>
                <Image src={logo} alt="Haskell Logo" style={{ height: "3rem", width: "auto" }} />
                <Typography variant="h5" sx={{ display: { md: "block", xs: "none" } }}>
                    A Graphical Playground for Haskell
                </Typography>
                <MenuItem component={Link} href="/">Home</MenuItem>
                <MenuItem component={Link} href="/editor">Editor</MenuItem>
                <MenuItem component={Link} href="/reference">Reference</MenuItem>
                <div style={{ flexGrow: 1 }} />
                <MenuItem component={Link} href="/account" sx={{ gap: "1rem" }}>
                    <Typography>
                        {user?.username ?? "Sign In"}
                    </Typography>
                    <Avatar src={user?.avatar ?? ""} />
                </MenuItem>
            </Toolbar>
        </AppBar>
    );
}
