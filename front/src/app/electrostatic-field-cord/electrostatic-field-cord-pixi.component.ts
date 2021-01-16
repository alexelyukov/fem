import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy, EventEmitter, Output } from '@angular/core';
import * as PIXI from 'pixi.js';
import { Circle, drawPoints, drawPolygon, drawPolygons, drawTest, drawTriangles, Geometry, getTriangulationPoints, getVoronoiPoints, Point, spreadPoints, Triangulation, Voronoi } from '../utils';

@Component({
  selector: 'electrostatic-field-cord-pixi',
  template: ''
})
export class ElectrostaticFieldCordPixiComponent implements OnInit, OnDestroy {
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

    this.getGeometry(
      {p: {x: 400, y: 400}, r: 300},
      {p: {x: 400 + 0.5*300*Math.cos(Math.PI/2), y: 400 - 0.5*300*Math.sin(Math.PI/2)}, r: 70},
      {p: {x: 400 - 0.5*300*Math.cos(Math.PI/6), y: 400 + 0.5*300*Math.sin(Math.PI/6)}, r: 70},
      {p: {x: 400 + 0.5*300*Math.cos(Math.PI/6), y: 400 + 0.5*300*Math.sin(Math.PI/6)}, r: 70}
    );

    this.notify.emit(this.geometry);

    this.drawGeometry();
  }

  getGeometry(circle1: Circle, circle2: Circle, circle3: Circle, circle4: Circle) {
    const n1 = 100;
    const n2 = 25;

    let points1 = [...Array(n1).keys()].map((value) => ({
      x: circle1.p.x + circle1.r * Math.cos(2 * Math.PI * (value+1) / n1),
      y: circle1.p.y + circle1.r * Math.sin(2 * Math.PI * (value+1) / n1),
    }));

    let points2 = [...Array(n2).keys()].map((value) => ({
      x: circle2.p.x + circle2.r * Math.cos(2 * Math.PI * (value+1) / n2),
      y: circle2.p.y + circle2.r * Math.sin(2 * Math.PI * (value+1) / n2),
    }));

    let points3 = [...Array(n2).keys()].map((value) => ({
      x: circle3.p.x + circle3.r * Math.cos(2 * Math.PI * (value+1) / n2),
      y: circle3.p.y + circle3.r * Math.sin(2 * Math.PI * (value+1) / n2),
    }));

    let points4 = [...Array(n2).keys()].map((value) => ({
      x: circle4.p.x + circle4.r * Math.cos(2 * Math.PI * (value+1) / n2),
      y: circle4.p.y + circle4.r * Math.sin(2 * Math.PI * (value+1) / n2),
    }));

    this.geometry = [
      {points: points1, io: true},
      {points: points2, io: false},
      {points: points3, io: false},
      {points: points4, io: false}
    ];
  }

  drawGeometry() {
    const figure1 = this.geometry[0];
    const figure2 = this.geometry[1];
    const figure3 = this.geometry[2];
    const figure4 = this.geometry[3];

    drawPolygon(this.app, figure1.points, 1, 0x000000, 0xCCCCCC);
    drawPoints(this.app, figure1.points, 2.5, 0xFF0000);

    drawPolygon(this.app, figure2.points, 1, 0x000000, 0xFFFFFF);
    drawPoints(this.app, figure2.points, 2.5, 0xFF0000);

    drawPolygon(this.app, figure3.points, 1, 0x000000, 0xFFFFFF);
    drawPoints(this.app, figure3.points, 2.5, 0xFF0000);

    drawPolygon(this.app, figure4.points, 1, 0x000000, 0xFFFFFF);
    drawPoints(this.app, figure4.points, 2.5, 0xFF0000);
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
}