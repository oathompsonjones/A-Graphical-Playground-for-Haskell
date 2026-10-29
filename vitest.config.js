// @ts-check
import { defineConfig } from "vitest/config";
import { fileURLToPath } from "node:url";

export default defineConfig({
    resolve: { alias: { tools: fileURLToPath(new URL("./src/tools", import.meta.url)) } },
    test: {
        environment: "node",
        include: ["src/**/*.test.ts"],
    },
});
