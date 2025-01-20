"use client";

import type { User } from "schemas/database";
import { createContext } from "react";

export const UserContext = createContext<{ user: User | null; setUser: (user: User | null) => void; }>(null!);
