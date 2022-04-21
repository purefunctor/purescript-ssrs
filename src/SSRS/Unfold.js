"use strict";

exports.unsafeAna = (init) => (next) => (coalgebra) => (seed) => {
    let stack = [];
    let current = init(coalgebra(seed));
    while (true) {
        if (current.type === "yield") {
            stack.push(current.value.qcj);
            current = init(coalgebra(current.value.j));
        } else if (current.type === "return") {
            if (stack.length === 0) {
                return current.value;
            } else {
                current = next(stack.pop())(current.value);
            }
        } else {
            throw new Error("Failed pattern match in unsafeAna.")
        }
    }
}
