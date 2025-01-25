"use server";

import { RedirectType, redirect } from "next/navigation";
import { cookies } from "next/headers";

/** Log the user out. */
export async function logout(): Promise<void> {
    (await cookies()).delete("user");
    redirect("/login", RedirectType.replace);
}
