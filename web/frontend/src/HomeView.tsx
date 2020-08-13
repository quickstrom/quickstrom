import React from "react";

export function HomeView() {
  const specs = [{ id: "1" }, { id: "2" }, { id: "3" }];
  return (
    <ul>
      {specs.map((spec) => {
        return (
          <li>
            <a href={`/specs/${spec.id}`}>Spec #{spec.id}</a>
          </li>
        );
      })}
    </ul>
  );
}