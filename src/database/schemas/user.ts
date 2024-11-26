import { sketchSchema } from "./sketch";
import z from "zod";

export const userSchema = z.object({
    avatar: z.string().nullable(),
    email: z.string().email(),
    passwordHash: z.string(),
    sketches: z.array(sketchSchema),
    username: z.string().nullable(),
});

export type User = z.infer<typeof userSchema>;
