import React from "react";
import styles from "./Layout.module.css";

const Layout: React.FC<{ classes ?: Array<string> }> = ({
  classes,
  children,
}) => {
  return (
    <div className={(classes || []).concat([styles.layout]).join(" ")}>
      <header>
        <nav className={styles.nav}>
            <ul>
                <li>WebCheck</li>
                <li>Specs</li>
            </ul>
        </nav>
      </header>
      <main>
          {children}
      </main>
    </div>
  );
};

export default Layout;
