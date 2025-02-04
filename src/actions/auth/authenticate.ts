"use server";

import { RedirectType, redirect } from "next/navigation";
import { compare, hash } from "bcrypt";
import type { User } from "schemas/database";
import { authenticationSchema } from "schemas/forms";
import { cookies } from "next/headers";
import { getUserFromEmail } from "database/index";

// TODO: Try to fix issue with cookies not always loading correctly.
// TODO: Add ability to login with Google/Apple/GitHub/StackOverflow/etc.

/**
 * Authenticate the user with the server.
 * @param formData - The form data to send.
 */
export async function authenticate(formData: FormData): Promise<void> {
    try {
        const parsedData = authenticationSchema.parse({
            email: formData.get("email"),
            password: formData.get("password"),
        });

        const user = JSON.parse(await getUserFromEmail(parsedData.email)) as User;
        const passwordHash = await hash(parsedData.password, 11);
        const isCorrectPassword = await compare(user.passwordHash, passwordHash);

        if (isCorrectPassword)
            throw new Error("Invalid credentials.");

        (await cookies()).set("user", user._id.toString());
    } catch (err) {
        throw new Error(`authenticate-${err instanceof Error ? err.message : String(err)}`);
    } finally {
        redirect("/account", RedirectType.replace);
    }
}
