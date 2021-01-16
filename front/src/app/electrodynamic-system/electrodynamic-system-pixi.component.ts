import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy, Output, EventEmitter } from '@angular/core';
import * as PIXI from 'pixi.js';
import { concat } from 'rxjs';
import { drawPoints, drawPolygon, drawPolygons, drawTest, drawTriangles, Geometry, getTriangulationPoints, getVoronoiPoints, Point, spreadPoints, Triangulation, Voronoi } from '../utils';

@Component({
  selector: 'electrodynamic-system-pixi',
  template: ''
})
export class ElectrodynamicSystemPixiComponent implements OnInit, OnDestroy {
  public app: PIXI.Application;
  private geometry: Geometry;

  @Input()
  public devicePixelRatio = window.devicePixelRatio || 1;

  @Input()
  public applicationOptions: {} = { width: 900, height: 900, backgroundColor: 0xFFFFFF };

  @Output() notify: EventEmitter<Geometry> = new EventEmitter<Geometry>();

  constructor(private elementRef: ElementRef, private ngZone: NgZone) {}

  init() {
    this.ngZone.runOutsideAngular(() => {
      this.app = new PIXI.Application(this.applicationOptions);
    });
    this.elementRef.nativeElement.appendChild(this.app.view);

    this.getGeometry();

    this.notify.emit(this.geometry);

    this.drawGeometry();

  }

  getGeometry() {
    const n = 10;
    const alpha = 2*Math.PI/n;
    const beta = alpha*0.7;
    const gamma = alpha-beta;
    const r1 = 300;
    const r2 = 150;
    const c = {x: 400, y: 400};

    const innerRound = [...Array(2).keys()].map((value) => ({
      x: c.x+r2*Math.cos(gamma*(value+1)/3),
      y: c.y+r2*Math.sin(gamma*(value+1)/3)
    }));
    const outerRound = [...Array(9).keys()].map((value) => ({
      x: c.x+r1*Math.cos(beta*(value+1)/10),
      y: c.y+r1*Math.sin(beta*(value+1)/10)
    }));
    const border = [...Array(16).keys()].map((value) => ({x: c.x+r2+(r1-r2)*value/15, y: c.y}));

    const step =
              border.map((value) => (this.rotate(-beta, value, c)))
      .concat(outerRound.map((value) => (this.rotate(-beta, value, c))))
      .concat(border.reverse())
      .concat(innerRound);

    const angles = [...Array(n-1).keys()].map((value) => (value+1) * alpha);

    let points1 = angles.reduce((acc, angle) => {
      return acc.concat(step.map((value) => (this.rotate(angle, value, c))))
    }, step);

    let points2 = [...Array(50).keys()].map((value) => ({
      x: c.x + 100 * Math.cos(2 * Math.PI * (value+1) / 50),
      y: c.y + 100 * Math.sin(2 * Math.PI * (value+1) / 50),
    }));

    this.geometry = [{points: points1, io: true}, {points: points2, io: false}];
  }

  drawGeometry() {
    const figure1 = this.geometry[0];
    const figure2 = this.geometry[1];

    drawPolygon(this.app, figure1.points, 1, 0x000000, 0xCCCCCC);
    drawPoints(this.app, figure1.points, 2.5, 0xFF0000);

    drawPolygon(this.app, figure2.points, 1, 0x000000, 0xFFFFFF);
    drawPoints(this.app, figure2.points, 2.5, 0xFF0000);
  }

  ngOnInit(): void {
    this.init();
  }

  destroy() {
    while (this.app.stage.children[0]) {
      this.app.stage.removeChild(this.app.stage.children[0]);
    }

    this.app.destroy(true);
    this.app = null;
  }

  ngOnDestroy(): void {
    this.destroy();
  }

  clearAll() {
    this.destroy();

    this.ngZone.runOutsideAngular(() => {
      this.app = new PIXI.Application(this.applicationOptions);
    });
    this.elementRef.nativeElement.appendChild(this.app.view);

    this.drawGeometry();
  }

  drawTriangulation(triangulation: Triangulation) {
    drawTriangles(this.app, triangulation, 1, 0x000000);
    drawPoints(this.app, getTriangulationPoints(triangulation), 2.5, 0xFF0000);
  }

  drawVoronoi(voronoi: Voronoi) {
    drawPolygons(this.app, voronoi, 1, 0x000000);
    drawPoints(this.app, getVoronoiPoints(voronoi), 2.5, 0xFF0000);
  }

  rotate(fi: number, point: Point, c: Point) {
    const sinA = Math.sin(fi);
    const cosA = Math.cos(fi);
    const x1 = point.x - c.x;
    const y1 = point.y - c.y;

    return {x: c.x + x1 * cosA - y1 * sinA, y: c.y + x1 * sinA + y1 * cosA}
  }
}