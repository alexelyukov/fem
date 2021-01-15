import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy } from '@angular/core';
import * as PIXI from 'pixi.js';
import { drawPoints, drawPolygon, drawTest, Geometry, Rectangle, spreadPoints } from '../utils';

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

  constructor(private elementRef: ElementRef, private ngZone: NgZone) {}

  init() {
    this.ngZone.runOutsideAngular(() => {
      this.app = new PIXI.Application(this.applicationOptions);
    });
    this.elementRef.nativeElement.appendChild(this.app.view);

    this.getGeometry({leftTop: {x: 100, y: 100}, rightBottom: {x: 700, y: 700}});

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

    this.geometry = [{points, inout: true}];
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
    this.app.destroy();
  }

  ngOnDestroy(): void {
    this.destroy();
  }
}