import React from "react";
import { BrowserRouter as Router, Route, Switch } from "react-router-dom";
import "./App.css";
import { HomeView } from "./HomeView";
import { SpecEditorView } from "./SpecEditorView";

function App() {
  return (
    <Router>
      <Switch>
        <Route path="/specs/:specId">
          <SpecEditorView />
        </Route>
        <Route path="/">
          <HomeView />
        </Route>
      </Switch>
    </Router>
  );
}

export default App;
