"use strict";

exports.unsafeCata = (init) => (next) => (algebra) => (initialIndex) => {
    let stack = [];
    let current = init(initialIndex);
    while (true) {
        if (current.type === "yield") {
            stack.push(current.value.qcj);
            current = init(current.value.j);
        } else if (current.type === "return") {
            if (stack.length === 0) {
                return algebra(current.value);
            } else {
                current = next(stack.pop())(algebra(current.value));
            }
        } else {
            throw new Error("Failed pattern match in unsafeCata.")
        }
    }
}
