// @ts-check
import { next } from "@million/lint";

/** @type {import('next').NextConfig} */
export default next({ rsc: true })({
    compiler: {
        emotion: true,
        styledComponents: true,
    },
    images: { remotePatterns: ["www.gravatar.com", "www.gchq.gov.uk"].map((hostname) => ({ hostname })) },
    // eslint-disable-next-line require-await
    async redirects() {
        return Object.entries({
            dissertation: "https://oathompsonjones.github.io/A-Graphical-Playground-for-Haskell",
            // eslint-disable-next-line max-len
            feedback: "https://forms.office.com/pages/responsepage.aspx?id=sAafLmkWiUWHiRCgaTTcYf1ipzRq35JMpCjA2DJSvktURDBEUzhETVlaTkpEUzdRNDY3SjI2OUtKQi4u&route=shorturl",
            issues: "https://github.com/oathompsonjones/A-Graphical-Playground-for-Haskell/issues",
        }).map(([source, destination]) => ({
            destination,
            permanent: true,
            source: `/${source}`,
        }));
    },
    transpilePackages: ["highlight.js"],
    webpack(config) {
        config.module.rules.push({
            test: /\.svg$/u,
            use: ["@svgr/webpack"],
        });

        return config;
    },
});
