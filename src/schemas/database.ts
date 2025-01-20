import type { WithId } from "mongodb";
import z from "zod";

// Sketchs
export const sketchSchema = z.object({
    authorId: z.string(),
    content: z.string(),
    createdAt: z.string().datetime(),
    modifiedAt: z.string().datetime(),
    name: z.string(),
    public: z.boolean(),
});

export type SketchWithoutId = z.infer<typeof sketchSchema>;
export type Sketch = WithId<SketchWithoutId>;

// Users
export const userSchema = z.object({
    avatar: z.string().nullable(),
    email: z.string().email(),
    passwordHash: z.string(),
    username: z.string().nullable(),
});

export type UserWithoutId = z.infer<typeof userSchema>;
export type User = WithId<UserWithoutId>;
