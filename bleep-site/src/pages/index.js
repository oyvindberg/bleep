import React from "react";
import clsx from "clsx";
import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Layout from "@theme/Layout";
import HomepageFeatures from "@site/src/components/HomepageFeatures";

import styles from "./index.module.css";

function HomepageHeader() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <header className={clsx("hero hero--primary", styles.heroBanner)}>
      <div className="container">
        <img
          src="/img/bleep-logo.svg"
          alt="Bleep"
          className={styles.heroLogo}
        />
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <p className={styles.heroDescription}>
          Instant startup. Builds you can read. Works with your favorite tools.
        </p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/"
          >
            Get Started
          </Link>
          <Link
            className="button button--outline button--secondary button--lg"
            to="/docs/demos/cli-experience"
            style={{ marginLeft: "1rem" }}
          >
            See It In Action
          </Link>
        </div>
      </div>
    </header>
  );
}

// Sparkles icon for AI/automation section
const SparklesIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 24 24"
    fill="none"
    stroke="currentColor"
    strokeWidth="2"
    strokeLinecap="round"
    strokeLinejoin="round"
    className={styles.bannerIcon}
  >
    <path d="M12 3l1.5 4.5L18 9l-4.5 1.5L12 15l-1.5-4.5L6 9l4.5-1.5L12 3z" />
    <path d="M5 19l0.5 1.5L7 21l-1.5 0.5L5 23l-0.5-1.5L3 21l1.5-0.5L5 19z" />
    <path d="M19 14l0.5 1.5L21 16l-1.5 0.5L19 18l-0.5-1.5L17 16l1.5-0.5L19 14z" />
  </svg>
);

function AiBanner() {
  return (
    <section className={styles.aiBanner}>
      <div className="container">
        <div className={styles.aiBannerContent}>
          <SparklesIcon />
          <div className={styles.aiBannerText}>
            <h3>Built for Tomorrow's Workflows</h3>
            <p>Scripts that work as tools. Builds that machines can read. Ready for automation.</p>
          </div>
        </div>
      </div>
    </section>
  );
}

function DemoSection() {
  return (
    <section className={styles.demoSection}>
      <div className="container">
        <h2>Experience the Speed</h2>
        <p>See bleep in action with interactive terminal demos</p>
        <div className={styles.demoLinks}>
          <Link
            className="button button--primary button--lg"
            to="/docs/demos/cli-experience"
          >
            CLI Demo
          </Link>
          <Link
            className="button button--primary button--lg"
            to="/docs/demos/ide-import"
            style={{ marginLeft: "1rem" }}
          >
            IDE Import Demo
          </Link>
          <Link
            className="button button--primary button--lg"
            to="/docs/demos/importing-sbt-build"
            style={{ marginLeft: "1rem" }}
          >
            sbt Import Demo
          </Link>
        </div>
      </div>
    </section>
  );
}

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout
      title="A Bleeping Fast Build Tool"
      description="Bleep is a native-compiled build tool with instant startup, readable configuration, and one-second IDE imports."
    >
      <HomepageHeader />
      <main>
        <HomepageFeatures />
        <AiBanner />
        <DemoSection />
      </main>
    </Layout>
  );
}
