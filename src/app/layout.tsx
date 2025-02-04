import "styles/global.css";
import "styles/layout.css";
import "styles/typography.css";
import type { Metadata, Viewport } from "next";
import { Footer } from "components/footer";
import { Header } from "components/header";
import { Providers } from "../contexts/providers";
import type { ReactNode } from "react";

// https://realfavicongenerator.net (remove the mask icon and msapplication stuff)
export const metadata: Metadata = {
    description: "Experience Haskell like never before. This educational tool allows you to create graphics and " +
        "animations using Haskell, all from within your browser; so no more installing libraries or setting up " +
        "environments — just start coding!",
    icons: {
        apple: "/favicon-96x96.png",
        icon: ["/favicon-96x96.png"],
        shortcut: "/favicon-96x96.png",
    },
    keywords: [
        "haskell",
        "graphics",
        "animation",
        "playground",
        "education",
        "learning",
        "functional programming",
        "functional",
        "programming",
        "code",
        "coding",
        "visual",
        "graphical",
        "graphical haskell",
        "graphical playground",
        "graphical haskell playground",
        "graphical haskell playground for education",
        "graphical haskell playground for learning",
        "graphical haskell playground for functional programming",
        "graphical haskell playground for code",
        "graphical haskell playground for coding",
        "a graphical playground for haskell",
    ],
    title: "A Graphical Playground for Haskell",
};

export const viewport: Viewport = {
    initialScale: 1,
    themeColor: "#5e5086",
    viewportFit: "cover",
    width: "device-width",
};

/**
 * A wrapper to build every page.
 * @param props - The props to pass to the layout.
 * @param props.children - The children to render.
 * @returns A page wrapper.
 */
export default function Layout({ children }: { children: ReactNode; }): ReactNode {
    return (
        <html lang="en">
            <body>
                <noscript>You need to enable JavaScript to run this app.</noscript>
                <Providers>
                    <Header />
                    <main>{children}</main>
                    <Footer />
                </Providers>
            </body>
        </html>
    );
}
