import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy, Output, EventEmitter } from '@angular/core';
import * as PIXI from 'pixi.js';
import { drawPoints, drawPolygon, drawTest, Geometry, Point, Rectangle, spreadPoints } from '../utils';

@Component({
  selector: 'electromagnetic-field-magnet-pixi',
  template: ''
})
export class ElectromagneticFieldMagnetPixiComponent implements OnInit, OnDestroy {
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

    this.getGeometry({leftTop: {x: 300, y: 200}, rightBottom: {x: 500, y: 700}});

    this.notify.emit(this.geometry);

    this.drawGeometry();

  }

  getGeometry(rectangle: Rectangle) {
    const leftTop = rectangle.leftTop;
    const rightBottom = rectangle.rightBottom;

    let points =
              [...Array(15+1).keys()].map((value) => ({x: spreadPoints(leftTop.x, rightBottom.x, value, 15), y: leftTop.y}))
      .concat([...Array(35-1).keys()].map((value) => ({x: rightBottom.x, y: spreadPoints(leftTop.y, rightBottom.y, value + 1, 35)})))
      .concat([...Array(15+1).keys()].map((value) => ({x: spreadPoints(rightBottom.x, leftTop.x, value, 15), y: rightBottom.y})))
      .concat([...Array(35-1).keys()].map((value) => ({x: leftTop.x, y: spreadPoints(rightBottom.y, leftTop.y, value+1, 35)})));

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
    this.app.destroy();
  }

  ngOnDestroy(): void {
    this.destroy();
  }
}