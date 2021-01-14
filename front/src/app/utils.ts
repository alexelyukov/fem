import * as PIXI from 'pixi.js';

interface Point {
    x: number;
    y: number;
}

interface Line {
    p1: Point;
    p2: Point;
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
