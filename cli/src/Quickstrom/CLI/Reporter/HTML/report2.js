import { h, render } from "https://unpkg.com/preact@latest?module";
import { useReducer, useState } from "https://unpkg.com/preact@latest/hooks/dist/hooks.module.js?module";
import htm from "https://unpkg.com/htm?module";

// Initialize htm with Preact
const html = htm.bind(h);


function reportReducer(state, action) {
  if (action.tag === "previous") {
    const newIndex = state.index - 1;
    const newCurrent = state.all[newIndex];
    if (newCurrent) {
      return { ...state, index: newIndex, current: newCurrent };
    } else {
      return state;
    }
  } else if (action.tag === "next") {
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
    switch (report.summary.tag) {
      case "Failure":
        return html`
                <p class="summary failed">
                  Failed after ${report.summary.tests} test and ${report.summary.shrinkLevels} level of shrinking.
                </p>`;
      case "Success":
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
        <button disabled=${state.index === 0} onClick=${() => dispatch({ tag: "previous" })}>←</button>
      </section>
      <section class="content">
        <${Action} action=${transition.action} />
        <section class="states">
          <${State} state=${transition.states.from} number=${state.index + 1} extraClass="from" label="From" />
          <${State} state=${transition.states.to} number=${state.index + 2} extraClass="to" label="To" />
        </section>
        <section class="screenshots">
          <${Screenshot} state=${transition.states.from} extraClass="from" selectedElement=${selectedElement}
            setSelectedElement=${setSelectedElement} />
          <${Screenshot} state=${transition.states.to} extraClass="to" selectedElement=${selectedElement}
            setSelectedElement=${setSelectedElement} />
        </section>
        <section class="details">
          <div class="state-queries from">
          </div>
          <div class="state-queries to">
          </div>
        </section>
      </section>
      <section class="controls">
        <button disabled=${state.index === (state.all.length - 1)} onClick=${() => dispatch({ tag: "next" })}>→</button>
      </section>
    </main>
    `;
}

function Action({ action }) {
  function renderKey(key) {
    switch (key) {
      case "\ue006": return html`⏎`;
      default: return html`${key}`;
    }
  }
  function renderDetails() {
    switch (action.tag) {
      case "Click":
        return html`
          <div class="action-details">
            <h2><span class="name">${action.tag}</span></h2>
            <div class="selector">${action.contents[0]}[${action.contents[1]}]</div>
          </div>
        `;
      case "KeyPress":
        return html`
          <div class="action-details">
            <h2><span class="name">${action.tag}</span></h2>
            <div class="key">${renderKey(action.contents)}</div>
          </div>
        `;
      case "Focus":
        return html`
          <div class="action-details">
            <h2><span class="name">${action.tag}</span></h2>
            <div class="selector">${action.contents[0]}[${action.contents[1]}]</div>
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

function State({ state, number, extraClass, label }) {
  return html`
    <div class=${"state " + extraClass}>
       <div class="label">${label}</div>
      <h2>State ${number}</h2>
    </div>
  `;
}

function MarkerDim({ screenshot, element }) {
  if (element) {
    return html`
          <svg class="marker-dim active" viewBox="0 0 ${screenshot.width} ${screenshot.height}">
            <mask id="myMask">
              <rect x="0" y="0" width="${screenshot.width}" height="${screenshot.height}" fill="white" />
              <rect x="${element.position.x}" y="${element.position.y}" width="${element.position.width}"
                height="${element.position.height}" fill="black" />
            </mask>
          
            <rect x="0" y="0" width="${screenshot.width}" height="${screenshot.height}" fill="rgba(0,0,0,.4)"
              mask="url(#myMask)" />
          </svg>
      `;
  } else {

    return html`
          <svg class="marker-dim inactive" viewBox="0 0 ${screenshot.width} ${screenshot.height}"></svg>
      `;
  }
}

function Screenshot({ state, extraClass, selectedElement, setSelectedElement }) {
  function isActive(element) {
    return selectedElement && selectedElement.id === element.id;
  }
  const activeElement = state.queries.flatMap(q => q.elements).find(isActive);

  function renderDim(element) {
    return html`
            <${MarkerDim} screenshot=${state.screenshot} element=${element} />
        `;
  }
  function percentageOf(x, total) {
    return `${(x / total) * 100}%`;
  }
  function renderQueryMarkers(query) {
    return query.elements.map(element => {
      return html`
        <div key=${element.id} class="marker ${element.status} ${isActive(element) ? " active" : "inactive"}"
          onmouseenter=${(e =>
          setSelectedElement(element))}
          onmouseleave=${(e => setSelectedElement(null))}
          style="
          top: ${percentageOf(element.position.y, state.screenshot.height)};
          left: ${percentageOf(element.position.x, state.screenshot.width)};
          width: ${percentageOf(element.position.width, state.screenshot.width)};
          height: ${percentageOf(element.position.height, state.screenshot.height)};
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
            `;
    });
  }
  const dim = renderDim(activeElement);
  return html`
        <div class=${"state-screenshot " + extraClass}>
                                          <div class=" state-screenshot-inner">
          ${state.queries.map(renderQueryMarkers)}
          <img src="data:image/png;base64,${state.screenshot.encoded}" />
          ${dim}
        </div>
        </div>
  `;
}

(async () => {
  const r = await fetch("report2.json");
  const report = await r.json();
  render(html`<${Report} report=${report} />`, document.body);
})();
