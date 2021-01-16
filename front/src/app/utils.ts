import * as PIXI from 'pixi.js';

export interface Point {
    x: number;
    y: number;
}

export interface Line {
    p1: Point;
    p2: Point;
}

export interface Rectangle {
    leftTop: Point;
    rightBottom: Point;
}

export interface Circle {
    p: Point;
    r: number;
}

export interface Triangle {
    a: Point;
    b: Point;
    c: Point;
}

export type Polygon = Point[];
export type Geometry = Figure[];
export type Triangulation = Triangle[];
export type Voronoi = Polygon[];

export interface Figure {
    points: Point[];
    io: boolean;
}

export function drawTest(app: PIXI.Application) {
    drawPoint(app, {x: 100, y: 100}, 1, 0x000000)
    drawPoint(app, {x: 200, y: 100}, 2, 0x0000FF)
    drawPoint(app, {x: 300, y: 100}, 3, 0x00FF00)
    drawPoint(app, {x: 400, y: 100}, 4, 0x00FFFF)
    drawPoints(app, [{x: 500, y: 100}, {x: 600, y: 100}, {x: 700, y: 100}], 4, 0xFF0000)

    drawLine(app, {p1: {x: 100, y: 200}, p2: {x: 150, y: 200}}, 1, 0x000000);
    drawLine(app, {p1: {x: 200, y: 200}, p2: {x: 250, y: 200}}, 2, 0x000000);

    drawLines(app, [
      {p1: {x: 400, y: 200}, p2: {x: 450, y: 200}},
      {p1: {x: 500, y: 200}, p2: {x: 550, y: 200}}
    ], 2, 0xFF0000);

    drawRectangle(app, {leftTop: {x: 100, y: 250}, rightBottom: {x: 150, y: 300}}, 1, 0x000000)
    drawRectangle(app, {leftTop: {x: 200, y: 250}, rightBottom: {x: 250, y: 300}}, 1, 0x000000, 0xCCCCCC)

    drawRectangle(app, {leftTop: {x: 300, y: 250}, rightBottom: {x: 350, y: 300}}, 1, 0x000000)
    drawPoints(app, [{x: 300, y: 250}, {x: 350, y: 250}, {x: 350, y: 300}, {x: 300, y: 300}], 2.5, 0xFF0000)

    drawRectangle(app, {leftTop: {x: 400, y: 250}, rightBottom: {x: 450, y: 300}}, 1, 0x000000, 0xCCCCCC)
    drawPoints(app, [{x: 400, y: 250}, {x: 450, y: 250}, {x: 450, y: 300}, {x: 400, y: 300}], 2.5, 0xFF0000)

    drawCircle(app, {p: {x: 125, y: 350}, r: 25}, 1, 0x000000)
    drawCircle(app, {p: {x: 225, y: 350}, r: 25}, 1, 0x000000, 0xCCCCCC)

    drawCircle(app, {p: {x: 325, y: 350}, r: 25}, 1, 0x000000)
    drawPoint(app, {x: 325, y: 350}, 2.5, 0xFF0000)

    drawCircle(app, {p: {x: 425, y: 350}, r: 25}, 1, 0x000000, 0xCCCCCC)
    drawPoint(app, {x: 425, y: 350}, 2.5, 0xFF0000)

    drawTriangle(app, {a: {x: 100, y: 425}, b: {x: 150, y: 425}, c: {x: 125, y: 450}}, 1, 0x000000)
    drawTriangle(app, {a: {x: 225, y: 425}, b: {x: 200, y: 450}, c: {x: 250, y: 450}}, 1, 0x000000, 0xCCCCCC)

    drawPolygon(app, [{x: 100, y: 525}, {x: 125, y: 475}, {x: 150, y: 525}, {x: 125, y: 550}], 1, 0x000000, 0xCCCCCC)
}

export function drawTriangles(app: PIXI.Application, triangles: Triangle[], size: number, colorBorder: number , colorFill?: number) {
    for (const triangle of triangles) {
        drawTriangle(app, triangle, size, colorBorder, colorFill);
    }
}

export function drawTriangle(app: PIXI.Application, triangle: Triangle, size: number, colorBorder: number , colorFill?: number) {
    drawPolygon(app, [triangle.a, triangle.b, triangle.c], size, colorBorder , colorFill);
}

export function drawPolygons(app: PIXI.Application, polygons: Polygon[], size: number, colorBorder: number , colorFill?: number) {
    for (const polygon of polygons) {
        drawPolygon(app, polygon, size, colorBorder, colorFill);
    }
}

export function drawPolygon(app: PIXI.Application, polygon: Polygon, size: number, colorBorder: number , colorFill?: number) {
    let figure = new PIXI.Graphics();
    figure.lineStyle(size, colorBorder, 1);
    if (colorFill) {
        figure.beginFill(colorFill);
    }

    let points = [];
    for (const point of polygon) {
        points.push(point.x);
        points.push(point.y);
    }
    figure.drawPolygon(points);
    figure.endFill();
    app.stage.addChild(figure);
}

export function drawCircle(app: PIXI.Application, circle: Circle, size: number, colorBorder: number , colorFill?: number) {
    let figure = new PIXI.Graphics();
    figure.lineStyle(size, colorBorder, 1);
    if (colorFill) {
        figure.beginFill(colorFill);
    }
    figure.drawCircle(circle.p.x, circle.p.y, circle.r);
    figure.endFill();
    app.stage.addChild(figure);
}

export function drawRectangle(app: PIXI.Application, rectangle: Rectangle, size: number, colorBorder: number , colorFill?: number) {
    let figure = new PIXI.Graphics();
    figure.lineStyle(size, colorBorder, 1);
    if (colorFill) {
        figure.beginFill(colorFill);
    }
    figure.drawRect(
        rectangle.leftTop.x,
        rectangle.leftTop.y,
        rectangle.rightBottom.x - rectangle.leftTop.x,
        rectangle.rightBottom.y - rectangle.leftTop.y
    );
    figure.endFill();
    app.stage.addChild(figure);
}

export function drawLines(app: PIXI.Application, lines: Line[], size: number, color: number) {
    for (const line of lines) {
        drawLine(app, line, size, color);
    }
}

export function drawLine(app: PIXI.Application, line: Line, size: number, color: number) {
    let figure = new PIXI.Graphics();
    figure.lineStyle(size, color, 1);
    figure.moveTo(line.p1.x, line.p1.y);
    figure.lineTo(line.p2.x, line.p2.y);
    app.stage.addChild(figure);
}

export function drawPoints(app: PIXI.Application, points: Point[], size: number, color: number) {
    for (const point of points) {
        drawPoint(app, point, size, color);
    }
}

export function drawPoint(app: PIXI.Application, point: Point, size: number, color: number) {
    let figure = new PIXI.Graphics();
    figure.beginFill(color);
    figure.drawCircle(0, 0, size);
    figure.endFill();
    figure.x = point.x;
    figure.y = point.y;
    app.stage.addChild(figure);
}

export function spreadPoints(x1: number, x2: number, i: number, n: number): number {
    return x1 + (x2 - x1) * (i / n);
}
