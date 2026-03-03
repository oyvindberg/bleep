import React from "react";
import clsx from "clsx";
import styles from "./styles.module.css";

// Lightning bolt with speed lines - represents instant startup
const LightningIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 64 64"
    fill="none"
    stroke="currentColor"
    strokeWidth="2"
    strokeLinecap="round"
    strokeLinejoin="round"
    className={styles.featureIcon}
  >
    {/* Speed lines */}
    <line x1="8" y1="24" x2="18" y2="24" />
    <line x1="4" y1="32" x2="16" y2="32" />
    <line x1="8" y1="40" x2="18" y2="40" />
    {/* Lightning bolt */}
    <polygon
      points="38,4 22,36 32,36 28,60 52,28 40,28 48,4"
      fill="currentColor"
      stroke="currentColor"
    />
  </svg>
);

// YAML document with curly braces - represents builds as data
const YamlDocIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 64 64"
    fill="none"
    stroke="currentColor"
    strokeWidth="2"
    strokeLinecap="round"
    strokeLinejoin="round"
    className={styles.featureIcon}
  >
    {/* Document outline */}
    <rect x="12" y="4" width="40" height="56" rx="3" />
    {/* YAML-like content lines */}
    <line x1="20" y1="16" x2="28" y2="16" />
    <line x1="24" y1="24" x2="44" y2="24" />
    <line x1="24" y1="32" x2="40" y2="32" />
    <line x1="20" y1="40" x2="32" y2="40" />
    <line x1="24" y1="48" x2="44" y2="48" />
  </svg>
);

// IDE window with checkmark - represents instant IDE imports
const IdeCheckIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 64 64"
    fill="none"
    stroke="currentColor"
    strokeWidth="2"
    strokeLinecap="round"
    strokeLinejoin="round"
    className={styles.featureIcon}
  >
    {/* Window frame */}
    <rect x="4" y="8" width="56" height="48" rx="3" />
    {/* Title bar */}
    <line x1="4" y1="18" x2="60" y2="18" />
    {/* Window buttons */}
    <circle cx="12" cy="13" r="2" fill="currentColor" />
    <circle cx="20" cy="13" r="2" fill="currentColor" />
    <circle cx="28" cy="13" r="2" fill="currentColor" />
    {/* Code lines */}
    <line x1="12" y1="28" x2="28" y2="28" />
    <line x1="12" y1="36" x2="36" y2="36" />
    <line x1="12" y1="44" x2="24" y2="44" />
    {/* Checkmark circle */}
    <circle cx="48" cy="40" r="10" fill="currentColor" stroke="currentColor" />
    <polyline
      points="43,40 46,44 54,36"
      stroke="white"
      strokeWidth="2.5"
      fill="none"
    />
  </svg>
);

const FeatureList = [
  {
    title: "Start Building, Not Waiting",
    Icon: LightningIcon,
    description: (
      <>
        Zero startup time. Run any command instantly. Your build tool should
        never be the bottleneck.
      </>
    ),
  },
  {
    title: "Builds You Can Understand",
    Icon: YamlDocIcon,
    description: (
      <>
        No DSL to learn. No magic to debug. Just data you can read, tools can
        modify, and anyone can review.
      </>
    ),
  },
  {
    title: "Your Tools, Instantly",
    Icon: IdeCheckIcon,
    description: (
      <>
        IDE imports in one second. CLI that responds immediately. Shell
        completions included. Everything just works.
      </>
    ),
  },
];

function Feature({ Icon, title, description }) {
  return (
    <div className={clsx("col col--4")}>
      <div className="text--center">
        <Icon />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
