import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy } from '@angular/core';
import * as PIXI from 'pixi.js';
import { drawPoints, drawPolygon, drawTest, Geometry, Point, Rectangle, spreadPoints } from '../utils';

@Component({
  selector: 'electromagnetic-field-transformer-pixi',
  template: ''
})
export class ElectromagneticFieldTransformerPixiComponent implements OnInit, OnDestroy {
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

    this.getGeometry(
      {leftTop: {x: 50, y: 100}, rightBottom: {x: 750, y: 600}},
      {leftTop: {x: 150, y: 200}, rightBottom: {x: 350, y: 500}},
      {leftTop: {x: 450, y: 200}, rightBottom: {x: 650, y: 500}},
    );

    this.drawGeometry();

  }

  getGeometry(rectangle1: Rectangle, rectangle2: Rectangle, rectangle3: Rectangle) {
    const leftTop1 = rectangle1.leftTop;
    const rightBottom1 = rectangle1.rightBottom;

    const leftTop2 = rectangle2.leftTop;
    const rightBottom2 = rectangle2.rightBottom;

    const leftTop3 = rectangle3.leftTop;
    const rightBottom3 = rectangle3.rightBottom;

    let points1 =
              [...Array(50+1).keys()].map((value) => ({x: spreadPoints(leftTop1.x, rightBottom1.x, value, 50), y: leftTop1.y}))
      .concat([...Array(40-1).keys()].map((value) => ({x: rightBottom1.x, y: spreadPoints(leftTop1.y, rightBottom1.y, value + 1, 40)})))
      .concat([...Array(50+1).keys()].map((value) => ({x: spreadPoints(rightBottom1.x, leftTop1.x, value, 50), y: rightBottom1.y})))
      .concat([...Array(40-1).keys()].map((value) => ({x: leftTop1.x, y: spreadPoints(rightBottom1.y, leftTop1.y, value+1, 40)})));

    let points2 =
              [...Array(15+1).keys()].map((value) => ({x: spreadPoints(leftTop2.x, rightBottom2.x, value, 15), y: leftTop2.y}))
      .concat([...Array(25-1).keys()].map((value) => ({x: rightBottom2.x, y: spreadPoints(leftTop2.y, rightBottom2.y, value + 1, 25)})))
      .concat([...Array(15+1).keys()].map((value) => ({x: spreadPoints(rightBottom2.x, leftTop2.x, value, 15), y: rightBottom2.y})))
      .concat([...Array(25-1).keys()].map((value) => ({x: leftTop2.x, y: spreadPoints(rightBottom2.y, leftTop2.y, value+1, 25)})));

    let points3 =
              [...Array(15+1).keys()].map((value) => ({x: spreadPoints(leftTop3.x, rightBottom3.x, value, 15), y: leftTop3.y}))
      .concat([...Array(25-1).keys()].map((value) => ({x: rightBottom3.x, y: spreadPoints(leftTop3.y, rightBottom3.y, value + 1, 25)})))
      .concat([...Array(15+1).keys()].map((value) => ({x: spreadPoints(rightBottom3.x, leftTop3.x, value, 15), y: rightBottom3.y})))
      .concat([...Array(25-1).keys()].map((value) => ({x: leftTop3.x, y: spreadPoints(rightBottom3.y, leftTop3.y, value+1, 25)})));

    this.geometry = [
      {points: points1, inout: true},
      {points: points2, inout: false},
      {points: points3, inout: false}
    ];
  }

  drawGeometry() {
    const figure1 = this.geometry[0];
    const figure2 = this.geometry[1];
    const figure3 = this.geometry[2];

    drawPolygon(this.app, figure1.points, 1, 0x000000, 0xCCCCCC);
    drawPoints(this.app, figure1.points, 2.5, 0xFF0000);

    drawPolygon(this.app, figure2.points, 1, 0x000000, 0xFFFFFF);
    drawPoints(this.app, figure2.points, 2.5, 0xFF0000);

    drawPolygon(this.app, figure3.points, 1, 0x000000, 0xFFFFFF);
    drawPoints(this.app, figure3.points, 2.5, 0xFF0000);
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