"use client";

import { useActionState, useContext, useEffect, useState } from "react";
import type { AuthResponse } from "actions/auth/authenticate";
import { EditProfile } from "components/pages/account/editProfile";
import Link from "next/link";
import { LogOut } from "components/pages/account/logOut";
import type { ReactNode } from "react";
import { Sketch } from "components/pages/account/sketch";
import type { Sketch as SketchSchema } from "schemas/database";
import { Typography } from "@mui/material";
import { UserContext } from "contexts/user";
import { getSketches } from "database/index";
import { logout } from "actions/auth/logout";
import styles from "styles/pages/account.module.css";
import { updateProfile } from "actions/auth/updateProfile";

/**
 * This is the account page.
 * @returns The page element.
 */
export default function Account(): ReactNode {
    const [logOutState, logOutAction, logOutIsPending] = useActionState<AuthResponse, FormData>(logout, {
        error: null,
        success: false,
    });
    const [updateState, updateAction, updateIsPending] = useActionState<AuthResponse, FormData>(updateProfile, {
        error: null,
        success: false,
    });
    const { user } = useContext(UserContext);
    const [sketches, setSketches] = useState<SketchSchema[] | null>(null);

    const fetchSketches = (): void => {
        if (user && sketches === null) {
            getSketches(user._id.toString())
                .then((json) => setSketches(JSON.parse(json) as SketchSchema[]))
                .catch(() => undefined);
        }
    };

    useEffect(fetchSketches, [user]);

    useEffect(() => {
        if (logOutState.success)
            window.location.href = `${window.location.origin}/auth/login`;

        if (updateState.success)
            window.location.reload();
    }, [logOutState.success, updateState.success]);

    return user && (
        <div>
            <Typography variant="h2">
                Welcome back, <b>{user.username ?? user.email.split("@")[0]}</b>!
            </Typography>
            <br />
            <EditProfile action={updateAction} pending={updateIsPending} />
            <br />
            <Typography variant="h4">Your Saved sketches</Typography>
            {sketches === null || sketches.length === 0
                ? (
                    <div className={styles.noSketches}>
                        <Typography variant="h6">You have no saved sketches.</Typography>
                        <Typography variant="caption">
                            <Link href="/editor">Create a new sketch</Link>
                        </Typography>
                    </div>
                )
                : (
                    <div className={styles.sketches}>
                        {sketches.map((sketch, i) => <Sketch key={i} sketch={sketch} setSketches={setSketches} />)}
                    </div>
                )}
            <br />
            <LogOut action={logOutAction} pending={logOutIsPending} />
        </div>
    );
}
