"use server";

import { authenticationSchema } from "database/schemas/forms";
import { cookies } from "next/headers";
import { getUser } from "database/database";
import { hash } from "bcrypt";

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

        const user = await getUser(parsedData.email);
        const passwordHash = await hash(parsedData.password, 11);

        if (user.passwordHash !== passwordHash)
            throw new Error("Invalid credentials.");

        cookies().set("user", user.email);
    } catch (err) {
        throw new Error(`authenticate-${err instanceof Error ? err.message : String(err)}`);
    }
}
