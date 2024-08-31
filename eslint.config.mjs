// @ts-check
import createConfig from "@oathompsonjones/eslint-config";

export default createConfig({
    configs: [
        { ignores: ["next-env.d.ts"] },
        { rules: { "require-unicode-regexp": "off" } },
    ],
    pagesDirectory: "src/app/(pages)",
    useNextJS: true,
});
