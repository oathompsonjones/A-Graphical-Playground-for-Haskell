"use server";

import { compare, hash } from "bcrypt";
import { authenticationSchema } from "schemas/forms";
import { cookies } from "next/headers";
import { getUser } from "database/index";
import { redirect } from "next/navigation";

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
        const isCorrectPassword = await compare(user.passwordHash, passwordHash);

        if (isCorrectPassword)
            throw new Error("Invalid credentials.");

        (await cookies()).set("user", user.email);
    } catch (err) {
        throw new Error(`authenticate-${err instanceof Error ? err.message : String(err)}`);
    } finally {
        redirect("/account");
    }
}
