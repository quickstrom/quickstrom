import React from "react";
import { useParams } from "react-router-dom";

export function SpecEditorView() {
  const { specId } = useParams();
  return <p>Spec: {specId}</p>;
}
