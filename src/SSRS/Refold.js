"use strict";

exports.unsafeHylo = (init) => (next) => (algebra) => (coalgebra) => (seed) => {
    let stack = [];
    let current = init(coalgebra(seed));
    while (true) {
        if (current.type === "yield") {
            stack.push(current.value.qcj);
            current = init(coalgebra(current.value.j));
        } else if (current.type === "return") {
            if (stack.length === 0) {
                return algebra(current.value);
            } else {
                current = next(stack.pop())(algebra(current.value));
            }
        } else {
            throw new Error("Failed pattern match in unsafeHylo.")
        }
    }
}
