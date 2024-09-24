/** @type {import('next').NextConfig} */
export default {
    compiler: {
        emotion: true,
        styledComponents: true,
    },
    redirects() {
        return [
            {
                destination: "https://oathompsonjones.github.io/Honours-Project/",
                permanent: true,
                source: `/dissertation/`,
            },
        ];
    },
    webpack(config) {
        config.module.rules.push({
            test: /\.svg$/u,
            use: ["@svgr/webpack"],
        });

        return config;
    },
};
