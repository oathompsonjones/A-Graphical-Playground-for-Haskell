"use client";

import { CssBaseline, StyledEngineProvider, ThemeProvider, createTheme, responsiveFontSizes } from "@mui/material";
import DefaultPropsProvider from "@mui/material/DefaultPropsProvider";
import type { ReactNode } from "react";

/**
 * Provides the theme to the application.
 * @param props - The properties of the component.
 * @param props.children - The children to render.
 * @returns The theme provider to wrap the application in.
 */
export function ThemeContextProvider({ children }: { children: ReactNode; }): ReactNode {
    const theme = responsiveFontSizes(
        createTheme({
            components: {
                MuiAccordion: { styleOverrides: { root: { background: "none", boxShadow: "none" } } },
                MuiButton: { styleOverrides: { root: { borderRadius: "999px", textTransform: "none" } } },
                MuiContainer: { styleOverrides: { root: { padding: "0" } } },
                MuiDivider: { styleOverrides: { root: { margin: "1.25% 0" } } },
                MuiInputBase: { styleOverrides: { root: { borderRadius: "1rem 1rem 0 0 !important" } } },
                MuiMenuItem: { styleOverrides: { root: { borderRadius: "999px" } } },
                MuiPaper: { styleOverrides: { rounded: { borderRadius: "2vmin" } } },
                MuiSkeleton: { styleOverrides: { root: { borderRadius: "2vmin" } } },
            },
            cssVariables: { colorSchemeSelector: "class" },
            defaultColorScheme: "dark",
            palette: {
                background: { default: "#666", paper: "#222" },
                mode: "dark",
                primary: { main: "#5e5086" },
                secondary: { main: "#999" },
                text: { disabled: "#666", primary: "#ddd", secondary: "#999" },
            },
        }),
        { breakpoints: ["xs", "sm", "md", "lg", "xl"] },
    );

    return (
        // Injects MUI styles before anything else.
        <StyledEngineProvider injectFirst>
            <ThemeProvider theme={theme}>
                <DefaultPropsProvider value={{
                    MuiButton: { variant: "contained" },
                    MuiButtonGroup: { variant: "contained" },
                    MuiPaper: { elevation: 5 },
                    MuiTextField: { fullWidth: true, required: true, variant: "filled" },
                }}>
                    <CssBaseline enableColorScheme />
                    {children}
                </DefaultPropsProvider>
            </ThemeProvider>
        </StyledEngineProvider>
    );
}
