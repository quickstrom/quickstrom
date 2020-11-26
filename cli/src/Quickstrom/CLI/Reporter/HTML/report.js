const traceElements = document.querySelectorAll(".trace li");
const screenshotImg = document.querySelector(".screenshot img");

function activateTraceElement(element) {
    const traceIndex = element.dataset.traceIndex;

    traceElements.forEach(other => other.classList.remove("active"));
    element.classList.add("active");

    screenshotImg.src = `test-${traceIndex}.png`;

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

traceElements.forEach(element => {
    element.addEventListener("click", e => {
        const traceIndex = element.dataset.traceIndex;
        activateTraceElement(element);
    });
});
