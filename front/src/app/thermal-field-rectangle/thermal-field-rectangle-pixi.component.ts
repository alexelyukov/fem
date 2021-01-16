import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy, Output, EventEmitter } from '@angular/core';
import * as PIXI from 'pixi.js';
import { drawPoints, drawPolygon, drawPolygons, drawTriangles, Geometry, getTriangulationPoints, getVoronoiPoints, Rectangle, spreadPoints, Triangulation, Voronoi } from '../utils';

@Component({
  selector: 'thermal-field-rectangle-pixi',
  template: ''
})
export class ThermalFieldRectanglePixiComponent implements OnInit, OnDestroy {
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

    this.getGeometry({leftTop: {x: 100, y: 100}, rightBottom: {x: 700, y: 700}});

    this.notify.emit(this.geometry);

    this.drawGeometry();

  }

  getGeometry(rectangle: Rectangle) {
    const leftTop = rectangle.leftTop;
    const rightBottom = rectangle.rightBottom;

    let points =
              [...Array(50+1).keys()].map((value) => ({x: spreadPoints(leftTop.x, rightBottom.x, value, 50), y: leftTop.y}))
      .concat([...Array(50-1).keys()].map((value) => ({x: rightBottom.x, y: spreadPoints(leftTop.y, rightBottom.y, value + 1, 50)})))
      .concat([...Array(50+1).keys()].map((value) => ({x: spreadPoints(rightBottom.x, leftTop.x, value, 50), y: rightBottom.y})))
      .concat([...Array(50-1).keys()].map((value) => ({x: leftTop.x, y: spreadPoints(rightBottom.y, leftTop.y, value+1, 50)})));

    this.geometry = [{points, io: true}];
  }

  drawGeometry() {
    const figure1 = this.geometry[0];

    drawPolygon(this.app, figure1.points, 1, 0x000000, 0xCCCCCC);
    drawPoints(this.app, figure1.points, 2.5, 0xFF0000);
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