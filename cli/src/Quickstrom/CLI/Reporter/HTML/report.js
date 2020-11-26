const traceSteps = document.querySelectorAll(".trace li");
const screenshotImg = document.querySelector(".screenshot img");
const detailElements = document.querySelectorAll(".queries .elements li");
const marker = document.querySelector(".marker");

function activeTraceStep(element) {
    const traceIndex = element.dataset.traceIndex;

    traceSteps.forEach(other => other.classList.remove("active"));
    element.classList.add("active");

    if (element.classList.contains("state")) {
        screenshotImg.src = `test-${traceIndex}.png`;
    } else {
        screenshotImg.src = "";
    }

    const details = document.querySelector(".details");
    details.classList.add("active");

    details.querySelectorAll(".queries").forEach(queries => {
        queries.classList.remove("active");
    });

    const queries = details.querySelector(`.queries[data-trace-index="${traceIndex}"]`);
    if (element.classList.contains("state") && queries) {
        queries.classList.add("active");
    }
}

traceSteps.forEach(step => {
    step.addEventListener("click", e => {
        const traceIndex = step.dataset.traceIndex;
        activeTraceStep(step);
    });
});

detailElements.forEach(element => {
    element.addEventListener("mouseenter", e => {
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
    });
    element.addEventListener("mouseleave", e => {
        marker.classList.remove("visible");
    });
});
