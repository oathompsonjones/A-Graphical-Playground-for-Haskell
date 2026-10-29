"use client";

import { AppBar, Avatar, Button, Toolbar, Typography, useMediaQuery } from "@mui/material";
import { useContext, useEffect, useState } from "react";
import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import { SketchContext } from "contexts/sketch";
import { UserContext } from "contexts/user";
import logo from "assets/images/logo.png";
import styles from "styles/components/header.module.css";

/**
 * Contains the header element.
 * @returns The page header.
 */
export function Header(): ReactNode {
    const { user } = useContext(UserContext);
    const { open, saved, title } = useContext(SketchContext);
    const edge = useMediaQuery((theme) => theme.breakpoints.down("md"));
    const [mounted, setMounted] = useState(false);

    let signInText = user?.username ?? user?.email.split("@")[0] ?? "Sign In";

    if (edge && signInText.length > 10)
        signInText = `${signInText.slice(0, 8)}...`;

    useEffect(() => setMounted(true), []);

    let status: "none" | "open" | "unsaved" = "none";

    if (mounted && open) {
        status = "open";

        if (!saved)
            status = "unsaved";
    }

    const statusMarker = mounted && status !== "none"
        ? (
            <Typography
                className={styles[status]}
                title={`${title} — ${status === "open" ? "Saved" : "Unsaved"}`}
                variant="h4"
            >
                •
            </Typography>
        )
        : null;

    return (
        <AppBar className={styles.header} component="header">
            <Toolbar className={`${styles.nav} ${edge ? "edge" : "full-width"}`} component="nav">
                <div className={styles.title}>
                    <Image alt="Haskell Logo" src={logo} />
                    <Typography component="h5" variant="h5">A Graphical Playground for Haskell</Typography>
                </div>
                <Button className={styles.menuItem} color="inherit" component={Link} href="/" variant="text">
                    Home
                </Button>
                <Button className={styles.menuItem} color="inherit" component={Link} href="/reference" variant="text">
                    Reference
                </Button>
                <Button className={styles.menuItem} color="inherit" component={Link} href="/editor" variant="text">
                    Editor
                    {statusMarker}
                </Button>
                <div className={styles.spacer} />
                <Button className={styles.account} color="inherit" component={Link} href="/account" variant="text">
                    {signInText}
                    <Avatar className={styles.avatar} src={user?.avatar ?? ""}>
                        {user?.avatar === null && signInText.split(" ")
                            .map((word) => word[0])
                            .join("")
                            .slice(0, 3)}
                    </Avatar>
                </Button>
            </Toolbar>
        </AppBar>
    );
}
