import { h, render } from "https://unpkg.com/preact@latest?module";
import { useReducer, useState } from "https://unpkg.com/preact@latest/hooks/dist/hooks.module.js?module";
import htm from "https://unpkg.com/htm?module";

// Initialize htm with Preact
const html = htm.bind(h);


function reportReducer(state, action) {
    if (action.type === "previous") {
        const newIndex = state.index - 1;
        const newCurrent = state.all[newIndex];
        if (newCurrent) {
            return { ...state, index: newIndex, current: newCurrent };
        } else {
            return state;
        }
    } else if (action.type === "next") {
        const newIndex = state.index + 1;
        const newCurrent = state.all[newIndex];
        if (newCurrent) {
            return { ...state, index: newIndex, current: newCurrent };
        } else {
            return state;
        }
    } else {
        return state;
    }
}


function Report({ report }) {
    const [state, dispatch] = useReducer(reportReducer, { current: report.transitions[0], index: 0, all: report.transitions });
    return html`
      <${Header} report=${report} />
      <${Transition} state=${state} dispatch=${dispatch} />
      <${Footer} report=${report} />
    `;
}

function Header({ report }) {
    function Summary() {
        switch (report.summary.type) {
        case "failure":
            return html`
                <p class="summary failed">
                  Failed after ${report.summary.tests} test and
                  ${report.summary.shrinkLevels} level of shrinking.
                </p>`;
        case "success":
            return html`<p class="summary success">Passed ${report.summary.tests} tests.</p>`;
        }

    }

    return html`
    <header>
      <h1>Quickstrom Test Report</h1>
      <${Summary} />
    </header>
    `;
}

function Footer({ report }) {
    return html`
        <footer>
          Generated at <time>${report.generatedAt}</time>.
        </footer>
    `;
}
function Transition({ state, dispatch }) {
    const [selectedElement, setSelectedElement] = useState(null);
    const transition = state.current;
    return html`
    <main>
      <section class="controls">
        <button disabled=${state.index === 0} onClick=${() => dispatch({ type: "previous"})}>←</button>
      </section>
      <section class="content">
        <${Action} action=${transition.action} />
        <section class="states">
            <${State} state=${transition.states.from} extraClass="from" label="From" />
            <${State} state=${transition.states.to} extraClass="to" label="To" />
        </section>
        <section class="screenshots">
            <${Screenshot} state=${transition.states.from} extraClass="from" selectedElement=${selectedElement} setSelectedElement=${setSelectedElement} />
            <${Screenshot} state=${transition.states.to} extraClass="to" selectedElement=${selectedElement} setSelectedElement=${setSelectedElement} />
        </section>
        <section class="details">
            <div class="state-queries from">
            </div>
            <div class="state-queries to">
            </div>
        </section>
      </section>
      <section class="controls">
        <button disabled=${state.index === (state.all.length - 1)} onClick=${() => dispatch({ type: "next"})}>→</button>
      </section>
    </main>
    `;
}

function Action({ action }) {

    function renderKey(key) {
        switch(key) {
        case "\\57350": return html`⏎`;
        default: return key;
        }
    }
    function renderDetails() {
        switch (action.type) {
        case "KeyPress":
            return html`
          <div class="action-details">
            <h2><span class="name">${action.type}</span></h2>
            <div class="key">${renderKey(action.key)}</div>
          </div>
        `;
        case "Focus":
            return html`
          <div class="action-details">
            <h2><span class="name">${action.type}</span></h2>
          </div>
        `;
        }
    }

    return html`
    <section class="action">
      <div class="action-inner">
        <div class="label">Action</div>
        ${renderDetails()}
      </div>
    </section>
  `;
}

function State({ state, extraClass, label }) {
    return html`
    <div class=${"state " + extraClass}>
        <div class="label">${label}</div>
        <h2>${state.name}</h2>
    </div>
  `;
}

function Screenshot({ state, extraClass, selectedElement, setSelectedElement }) {
    function renderQueryMarkers(query) {
        return query.elements.map(element => html`
            <div 
              class="marker ${element.status} ${selectedElement && selectedElement.id === element.id ? "active" : "inactive"}" 
              onmouseenter=${(e => setSelectedElement(element))}
              onmouseleave=${(e => setSelectedElement(null))}
              style="
                top: ${element.position.y}px;
                left: ${element.position.x}px;
                width: ${element.position.w}px;
                height: ${element.position.h}px;
              ">
                <div class="marker-details">
                  <table>
                      ${element.state.map(elementState => html`
                        <tr>
                          <td>${elementState.name}</td>
                          <td>${elementState.value}</td>
                        </tr>
                      `)}
                  </table>
                </div>
            </div>
        `);
    }
    return html`
        <div class=${"state-screenshot " + extraClass}>
          <div class="state-screenshot-inner">
            ${state.queries.map(renderQueryMarkers)}
            <img src=${state.screenshot} />
          </div>
        </div>
  `;
}

(async () => {
    const r = await fetch("report.json");
    const report = await r.json();
    render(html`<${Report} report=${report} />`, document.body);
})();
