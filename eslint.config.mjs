// @ts-check
import createConfig from "@oathompsonjones/eslint-config";

export default createConfig({
    configs: [
        { ignores: ["next-env.d.ts"] },
        { rules: { "prefer-named-capture-group": "off", "require-unicode-regexp": "off" } },
    ],
    pagesDirectory: "src/app/(pages)",
    useNextJS: true,
});
