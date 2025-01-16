import type { WithId } from "mongodb";
import z from "zod";

export const userSchema = z.object({
    avatar: z.string().nullable(),
    email: z.string().email(),
    passwordHash: z.string(),
    username: z.string().nullable(),
});

export type UserWithoutId = z.infer<typeof userSchema>;
export type User = WithId<UserWithoutId>;
