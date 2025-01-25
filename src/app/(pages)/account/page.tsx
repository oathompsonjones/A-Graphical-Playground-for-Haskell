"use client";

import { Button, FormControl, Typography } from "@mui/material";
import { useContext, useEffect, useState } from "react";
import Image from "next/image";
import type { ReactNode } from "react";
import type { Sketch } from "schemas/database";
import { UserContext } from "contexts/user";
import { getSketches } from "database/index";
import { logout } from "actions/auth/logout";

/**
 * This is the account page.
 * @returns The page element.
 */
export default function Account(): ReactNode {
    const { user } = useContext(UserContext);
    const [sketches, setSketches] = useState<Sketch[] | null>(null);

    useEffect(() => {
        if (user && sketches === null) {
            getSketches(user._id.toString())
                .then((json) => setSketches(JSON.parse(json) as Sketch[]))
                .catch(() => undefined);
        }
    }, [sketches, user]);

    return user && (
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
                    {sketches?.map((sketch, i) => (
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
