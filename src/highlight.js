export const highlight = (className) => {
    for (const e of document.querySelectorAll(".highlight")) {
        e.classList.remove("highlight");
    }
    for (const e of document.querySelectorAll(`.${className}`)) {
        e.classList.add("highlight");
    }
};
export const unhighlight = (className) => {
    for (const e of document.querySelectorAll(`.${className}`)) {
        e.classList.remove("highlight");
    }
};