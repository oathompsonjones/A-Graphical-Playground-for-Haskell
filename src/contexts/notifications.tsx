"use client";

import type { AlertColor } from "@mui/material";
import type { ReactNode } from "react";
import { createContext } from "react";

export const NotificationsContext = createContext<{
    message: ReactNode;
    setMessage: (message: ReactNode) => void;
    setType: (type: AlertColor) => void;
    type: AlertColor;
}>(null!);
