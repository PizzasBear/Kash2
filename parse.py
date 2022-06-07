from typing import Callable, Type, Union

LEVELS: dict[str, int] = {
    '**': 0,
    '*': 1,
    '/': 1,
    '%': 1,
    '+': 2,
    '-': 2,
    '<<': 3,
    '>>': 3,
    '^': 4,
    '&': 5,
    '|': 6,
    '<': 7,
    '>':  7,
    '<=': 7,
    '>=': 7,
    '==': 7,
    '!=': 7,
    '&&': 8,
    '||': 9,
}
PRE_UNIOPS: dict[str, Callable[[int], int]] = {
    '-': lambda a: -a,
}
EXPR: Type = Union[int, tuple[str, 'EXPR', 'EXPR']]
OPS: dict[str, Callable[[int, int], int]] = {
    '**': lambda a, b: a ** b,
    '*': lambda a, b: a * b,
    '/': lambda a, b: a // b,
    '%': lambda a, b: a % b,
    '+': lambda a, b: a + b,
    '-': lambda a, b: a - b,
    '<<': lambda a, b: a << b,
    '>>': lambda a, b: a >> b,
    '^': lambda a, b: a ^ b,
    '&': lambda a, b: a & b,
    '|': lambda a, b: a | b,
    '<': lambda a, b: +(a < b),
    '>': lambda a, b: +(a > b),
    '<=': lambda a, b: +(a <= b),
    '>=': lambda a, b: +(a >= b),
    '==': lambda a, b: +(a == b),
    '!=': lambda a, b: +(a != b),
    '&&': lambda a, b: +(a and b),
    '||': lambda a, b: +(a or b),
}

# def parse_num(tokens: list[str]) -> tuple[int, int]:
#     for i, token in enumerate(tokens):
#         if i:

def parse(s: str) -> int:
    values: list[EXPR] = []
    ops: list[str] = []
    for token in s.split(' '):
        if token.isdigit() or token[0] in ('-', '+') and token[1:].isdigit():
            values.append(int(token))
        else:
            lvl = LEVELS[token]
            while ops and LEVELS[ops[-1]] <= lvl:
                right, left = values.pop(), values.pop()
                values.append((ops.pop(), left, right))
            ops.append(token)

    while ops:
        right, left = values.pop(), values.pop()
        values.append((ops.pop(), left, right))
    assert len(values) == 1
    return values[-1]

while True:
    s = input('expr: ').strip().lower()
    if not s or s == 'exit':
        break
    print('result:', parse(s))

