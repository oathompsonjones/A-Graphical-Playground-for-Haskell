"use client";

import { ContentCopy, CopyAll, Download, Image } from "@mui/icons-material";
import { Dialog, IconButton, Typography } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { NotificationsContext } from "contexts/notifications";
import { SketchContext } from "contexts/sketch";
import { decompressFromEncodedURIComponent } from "lz-string";
import styles from "styles/components/menu.module.css";
import { useContext } from "react";
import { useOutsideClick } from "hooks/useOutsideClick";

/**
 * This is the share menu.
 * @param props - The properties of the component.
 * @param props.open - Whether the share menu is open.
 * @param props.setOpen - The function to set the share menu state.
 * @returns The share menu element.
 */
export function ShareMenu({ open, setOpen }: {
    open: boolean;
    setOpen: Dispatch<SetStateAction<boolean>>;
}): ReactNode {
    const ref = useOutsideClick<HTMLDivElement>(() => setOpen(false));
    const { setNotification } = useContext(NotificationsContext);
    const { code, title, author } = useContext(SketchContext);

    const copyCode = (): void => {
        void window.navigator.clipboard.writeText(decompressFromEncodedURIComponent(code)).then(() => {
            setNotification("Code copied to clipboard.", "success");
            setOpen(false);
        });
    };
    const copyUrl = (): void => {
        const codeURL = new URL(window.location.href);

        codeURL.searchParams.delete("id");
        codeURL.searchParams.set("code", code);

        if (title !== "untitled")
            codeURL.searchParams.set("title", encodeURIComponent(title));

        if (author !== null)
            codeURL.searchParams.set("author", author);

        void window.navigator.clipboard.writeText(codeURL.toString()).then(() => {
            setNotification("URL copied to clipboard.", "success");
            setOpen(false);
        });
    };
    const copyImage = (): void => {
        void window.navigator.clipboard.write([
            new ClipboardItem({
                /* eslint-disable */ "image/png": new Promise(async (resolve) => { /* eslint-enable */
                    const canvas = document.querySelector("canvas")!;
                    const image = canvas.toDataURL("image/png");
                    const blob = await (await fetch(image)).blob();

                    resolve(new Blob([blob], { type: "image/png" }));
                }),
            }),
        ]).then(() => {
            setNotification("Image copied to clipboard.", "success");
            setOpen(false);
        });
    };
    const downloadImage = (): void => {
        const canvas = document.querySelector("canvas")!;
        const image = canvas.toDataURL("image/png");
        const a = document.createElement("a");

        a.href = image;
        a.download = "image.png";
        a.click();
        setNotification("Image downloaded.", "success");
        setOpen(false);
    };

    const shareOptions: Array<{ action: () => void; icon: ReactNode; label: string; }> = [
        { action: copyCode, icon: <ContentCopy />, label: "Copy Code" },
        { action: copyUrl, icon: <CopyAll />, label: "Copy URL" },
        { action: copyImage, icon: <Image />, label: "Copy Image" },
        { action: downloadImage, icon: <Download />, label: "Download Image" },
    ];

    return (
        <Dialog open={open} onClose={() => setOpen(false)} ref={ref}>
            <div className={`${styles.dialog} ${styles.options}`}>
                {shareOptions.map(({ action, icon, label }, i) => (
                    <div key={i}>
                        <IconButton onClick={action}>{icon}</IconButton>
                        <Typography>{label}</Typography>
                    </div>
                ))}
            </div>
        </Dialog>
    );
}
