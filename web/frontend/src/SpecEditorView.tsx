import React from "react";
import { useParams } from "react-router-dom";
import Layout from "./Layout";
import AceEditor from "react-ace";

import "ace-builds/src-noconflict/mode-haskell";
import "ace-builds/src-noconflict/theme-github";

export function SpecEditorView() {
  const { specId } = useParams();
  return (
    <Layout classes={["spec-editor"]}>
      <nav className="toolbar">
        <ul>
          <li>Run</li>
        </ul>
      </nav>
      <AceEditor
        mode="haskell"
        theme="github"
        value="module Specification where"
        fontSize="1rem"
        width="100%"
        height="100%"
      />
    </Layout>
  );
}
