import "styles/global.css";
import type { Metadata, Viewport } from "next";
import { Footer } from "components/footer";
import { Header } from "components/header";
import type { ReactNode } from "react";

// https://realfavicongenerator.net (remove the mask icon and msapplication stuff)
export const metadata: Metadata = {
    description: "My website description.",
    icons: {
        apple: "https://cdn.worldvectorlogo.com/logos/next-js.svg",
        icon: [],
        shortcut: "https://cdn.worldvectorlogo.com/logos/next-js.svg",
    },
    keywords: [],
    title: "My Website",
};

export const viewport: Viewport = {
    initialScale: 1,
    themeColor: "",
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
                <Header />
                <main>{children}</main>
                <Footer />
            </body>
        </html>
    );
}
