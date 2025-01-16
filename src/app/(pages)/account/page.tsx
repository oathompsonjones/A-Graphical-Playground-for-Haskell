"use client";

import { Button, FormControl, Typography } from "@mui/material";
import { useContext, useState } from "react";
import Image from "next/image";
import type { ReactNode } from "react";
import type { Sketch } from "database/schemas/sketch";
import { UserContext } from "contexts/user";
import { getSketches } from "database/database";
import { logout } from "actions/auth/logout";

/**
 * This is the account page.
 * @returns The page element.
 */
export default function Account(): ReactNode {
    const { user } = useContext(UserContext);
    const [sketches, setSketches] = useState<Sketch[] | null>(null);

    // This should never happen, middleware should prevent it.
    if (user === null)
        return <></>;

    if (sketches === null) {
        getSketches(user._id.toString())
            .then(setSketches)
            .catch(() => undefined);
    }

    return (
        <div>
            <Typography variant="h2">Welcome back, <b>{user.username ?? user.email.split("@")[0]}</b>!</Typography>
            <br />
            <div>
                {user.avatar !== null && <Image
                    src={user.avatar}
                    alt="User avatar"
                    height={500}
                    width={500}
                    style={{ height: "auto", width: "500px" }}
                />}
            </div>
            <div>
                <Typography>Your saved code:</Typography>
                <ul>
                    {(sketches ?? []).map((sketch, i) => (
                        <li key={i}>
                            <Typography>{sketch.name}</Typography>
                            <Typography variant="caption">
                                    Created At: {new Date(sketch.createdAt).toUTCString()}
                            </Typography>
                            <Typography variant="caption">
                                    Modified At: {new Date(sketch.modifiedAt).toUTCString()}
                            </Typography>
                            <pre>{sketch.content}</pre>
                        </li>
                    ))}
                </ul>
            </div>
            <FormControl component="form" action={logout}>
                <Button type="submit">Sign Out</Button>
            </FormControl>
        </div>
    );
}
