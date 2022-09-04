#!/usr/bin/env python3

import sys

ID = str


class Block:
    def __init__(self, id: ID, x0: int, y0: int, x1: int, y1: int) -> None:
        self.id = id
        self.x0 = x0
        self.y0 = y0
        self.x1 = x1
        self.y1 = y1
        self.alive = True

    def __str__(self) -> str:
        return f"Block({self.id}, {self.x0}, {self.y0}, {self.x1}, {self.y1})"

    def size(self) -> tuple[int, int]:
        return abs(self.x1 - self.x0), abs(self.y1 - self.y0)

    def color(self, r: int, g: int, b: int, a: int) -> None:
        print(f"color [{self.id}] [{r},{g},{b},{a}]")

    def cut(self, x: int, y: int) -> tuple["Block", "Block", "Block", "Block"]:
        print(f"cut [{self.id}] [{x},{y}]")
        self.alive = False
        return (
            Block(f"{self.id}.0", self.x0, self.y0, x, y),
            Block(f"{self.id}.1", x, self.y0, self.x1, y),
            Block(f"{self.id}.2", x, y, self.x1, self.y1),
            Block(f"{self.id}.3", self.x0, y, x, self.y1),
        )

    def cut_x(self, x: int) -> tuple["Block", "Block"]:
        print(f"cut [{self.id}] [x] [{x}]")
        return (
            Block(f"{self.id}.0", self.x0, self.y0, x, self.y1),
            Block(f"{self.id}.1", x, self.y0, self.x1, self.y1),
        )

    def cut_y(self, y: int) -> tuple["Block", "Block"]:
        print(f"cut [{self.id}] [y] [{y}]")
        return (
            Block(f"{self.id}.0", self.x0, self.y0, self.x1, y),
            Block(f"{self.id}.1", self.x0, y, self.x1, self.y1),
        )

    def contains(self, other: "Block") -> bool:
        return (
            self.x0 <= other.x0
            and self.y0 <= other.y0
            and self.x1 >= other.x1
            and self.y1 >= other.y1
        )

    def swap(self, other: "Block") -> None:
        if self.size() != other.size():
            print("WARNING: swapping non-equal sized quads", file=sys.stderr)
        print(f"swap [{self.id}] [{other.id}]")


root = Block(ID("0"), 0, 0, 400, 400)

WW = [400 * i // 10 for i in range(1, 10)]

root.color(0, 74, 173, 255)
board9 = root.cut(WW[-1], WW[0])[3]
board9.color(255, 255, 255, 255)
bottom, hole, right, board8 = board9.cut(WW[-2], WW[1])
hole.color(0, 74, 173, 255)
right.color(0, 0, 0, 255)

is_black = True
whites = []
blacks = []
a1v = board8.cut(WW[3], WW[5])
a1v[0].color(0, 0, 0, 255)
a1v[3].color(0, 0, 0, 255)
for a1 in a1v:
    for a2 in a1.cut((a1.x0 + a1.x1) // 2, (a1.y0 + a1.y1) // 2):
        for a3 in a2.cut((a2.x0 + a2.x1) // 2, (a2.y0 + a2.y1) // 2):
            on_left = a1v[0].contains(a3) or a1v[3].contains(a3)
            if is_black and not on_left:
                blacks.append(a3)
            elif not is_black and on_left:
                whites.append(a3)
            is_black = not is_black

is_black = True
for a1 in right.cut_y((right.y0 + right.y1) // 2):
    for a2 in a1.cut_y((a1.y0 + a1.y1) // 2):
        for a3 in a2.cut_y((a2.y0 + a2.y1) // 2):
            if not is_black:
                whites.append(a3)
            is_black = not is_black

is_black = True
for a1 in bottom.cut_x((bottom.x0 + bottom.x1) // 2):
    for a2 in a1.cut_x((a1.x0 + a1.x1) // 2):
        for a3 in a2.cut_x((a2.x0 + a2.x1) // 2):
            if not is_black:
                blacks.append(a3)
            is_black = not is_black

for w, b in zip(whites, blacks):
    w.swap(b)
