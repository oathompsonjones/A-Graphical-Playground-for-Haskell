"use client";

import type { User } from "database/schemas/user";
import { createContext } from "react";

export const UserContext = createContext<{ user: User | null; setUser: (user: User | null) => void; }>({
    setUser: () => undefined,
    user: null,
});
