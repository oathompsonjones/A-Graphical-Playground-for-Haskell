"use server";

import type { AuthResponse } from "./authenticate";
import { cookies } from "next/headers";

/**
 * Log the user out.
 * @param _state - The previous state.
 * @returns The new state.
 */
export async function logout(_state: AuthResponse): Promise<AuthResponse> {
    try {
        (await cookies()).delete("user");
    } catch (err) {
        return {
            error: err instanceof Error ? err.message : String(err),
            success: false,
        };
    }

    return { success: true };
}
