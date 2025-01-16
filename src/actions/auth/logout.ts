"use server";

import { cookies } from "next/headers";
import { redirect } from "next/navigation";

/** Log the user out. */
export async function logout(): Promise<void> {
    (await cookies()).delete("user");
    redirect("/login");
}
