// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

import { themes as prismThemes } from "prism-react-renderer";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Bleep",
  tagline: "A Bleeping Fast Build Tool",
  url: "https://bleep.build",
  baseUrl: "/",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/bleep-logo-mark.svg",

  // GitHub pages deployment config.
  organizationName: "oyvindberg",
  projectName: "bleep",
  trailingSlash: true,

  i18n: {
    defaultLocale: "en",
    locales: ["en"],
  },

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: "./sidebars.js",
        },
        theme: {
          customCss: "./src/css/custom.css",
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: "Bleep",
        logo: {
          alt: "Bleep Logo",
          src: "img/bleep-logo-mark.svg",
        },
        items: [
          {
            type: "doc",
            docId: "readme",
            position: "left",
            label: "Documentation",
          },
          {
            href: "https://github.com/oyvindberg/bleep",
            label: "GitHub",
            position: "right",
          },
        ],
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Docs",
            items: [
              {
                label: "Documentation",
                to: "/docs",
              },
            ],
          },
          {
            title: "Community",
            items: [
              {
                label: "GitHub",
                href: "https://github.com/oyvindberg/bleep",
              },
              {
                label: "Discussions",
                href: "https://github.com/oyvindberg/bleep/discussions",
              },
              {
                label: "Releases",
                href: "https://github.com/oyvindberg/bleep/releases",
              },
            ],
          },
        ],
        copyright: `Copyright ${new Date().getFullYear()} Bleep Contributors. Built with Docusaurus.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.oceanicNext,
        additionalLanguages: ["java", "scala", "yaml", "bash"],
      },
    }),
};

export default config;
