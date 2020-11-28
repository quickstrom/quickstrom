const traceSteps = document.querySelectorAll(".trace li");
const screenshotImg = document.querySelector(".screenshot img");
const detailElements = document.querySelectorAll(".queries .elements li");
const marker = document.querySelector(".marker");

function activeTraceStep(element) {
    const traceIndex = element.dataset.traceIndex;

    traceSteps.forEach(other => other.classList.remove("active"));
    element.classList.add("active");

    screenshotImg.src = `test-${traceIndex}.png`;

    const details = document.querySelector(".details");

    if (element.classList.contains("state")) {
        details.classList.add("active");
    } else {
        details.classList.remove("active");
    }

    details.querySelectorAll(".queries").forEach(queries => {
        queries.classList.remove("active");
    });

    const queries = details.querySelector(`.queries[data-trace-index="${traceIndex}"]`);
    if (element.classList.contains("state") && queries) {
        queries.classList.add("active");
    }
}

function markFromElement(element) {
    try {
        const x = Number.parseInt(element.dataset.x);
        const y = Number.parseInt(element.dataset.y);
        const w = Number.parseInt(element.dataset.w);
        const h = Number.parseInt(element.dataset.h);

        if (w > 0 && h > 0) {
            marker.classList.add("visible");
            marker.style.left = `${x}px`;
            marker.style.top = `${y}px`;
            marker.style.width = `${w}px`;
            marker.style.height = `${h}px`;
        }
    } catch (err) {
        console.error(err);
    }
}

function unmark() {
    marker.classList.remove("visible");
}

traceSteps.forEach(step => {
    step.addEventListener("click", e => {
        const traceIndex = step.dataset.traceIndex;
        activeTraceStep(step);

        unmark();
        if (step.classList.contains("action")) {
            markFromElement(step);
        }
    });
});

detailElements.forEach(element => {
    element.addEventListener("mouseenter", e => {
        markFromElement(element);
    });
    element.addEventListener("mouseleave", e => {
        unmark();
    });
});
