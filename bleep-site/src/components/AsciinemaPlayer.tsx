import * as React from "react";
import BrowserOnly from "@docusaurus/core/lib/client/exports/BrowserOnly";
import "asciinema-player/dist/bundle/asciinema-player.css";

// https://github.com/asciinema/asciinema-player/issues/72#issuecomment-1051545675

interface AsciinemaPlayerProps {
  src: string;
  cols?: string;
  rows?: string;
  autoPlay?: boolean;
  preload?: boolean;
  loop?: boolean | number;
  startAt?: number | string;
  speed?: number;
  idleTimeLimit?: number;
  theme?: string;
  poster?: string;
  fit?: string;
  fontSize?: string;
}

const Player: React.FC<AsciinemaPlayerProps> = ({ src, ...opts }) => {
  const ref = React.useRef<HTMLDivElement>(null);

  React.useEffect(() => {
    const AsciinemaPlayerLibrary = require("asciinema-player");

    const currentRef = ref.current;
    if (currentRef) {
      AsciinemaPlayerLibrary.create(src, currentRef, opts);
    }
  }, [src, ref]);

  return <div ref={ref} />;
};

export const AsciinemaPlayer: React.FC<AsciinemaPlayerProps> = (props) => (
  <BrowserOnly>{() => <Player {...props} />}</BrowserOnly>
);
