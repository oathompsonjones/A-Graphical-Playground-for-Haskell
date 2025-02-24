"use server";

import type { User } from "schemas/database";
import { authenticationSchema } from "schemas/forms";
import { compare } from "bcrypt";
import { cookies } from "next/headers";
import { getUserFromEmail } from "database/index";

export type AuthResponse = { success: false; error: string | null; } | { success: true; data: string | null; };

// TODO: Try to fix issue with cookies not always loading correctly.
// TODO: Add ability to login with Google/Apple/GitHub/StackOverflow/etc.

/**
 * Authenticate the user with the server.
 * @param _state - The previous state.
 * @param formData - The form data to send.
 * @returns Whether the user was authenticated.
 */
export async function authenticate(_state: AuthResponse, formData: FormData): Promise<AuthResponse> {
    let id: string | null = null;

    try {
        const { email, password } = authenticationSchema.parse({
            email: formData.get("email"),
            password: formData.get("password"),
        });

        const user = JSON.parse(await getUserFromEmail(email)) as User;
        const isCorrectPassword = await compare(password, user.passwordHash);

        if (!isCorrectPassword)
            throw new Error("Invalid credentials.");

        id = user._id.toString();
        (await cookies()).set("user", id);
    } catch (err) {
        return {
            error: err instanceof Error ? err.message : String(err),
            success: false,
        };
    }

    return { data: id, success: true };
}
