// @ts-check
/** @type {import('next').NextConfig} */
export default {
    compiler: {
        emotion: true,
        styledComponents: true,
    },
    images: { remotePatterns: ["www.gravatar.com", "www.gchq.gov.uk"].map((hostname) => ({ hostname })) },
    // eslint-disable-next-line require-await
    async redirects() {
        return [
            {
                destination: "https://oathompsonjones.github.io/Honours-Project/",
                permanent: true,
                source: `/dissertation/`,
            },
        ];
    },
    transpilePackages: ["highlight.js"],
    webpack(config) {
        config.module.rules.push({
            test: /\.svg$/u,
            use: ["@svgr/webpack"],
        });

        return config;
    },
};
