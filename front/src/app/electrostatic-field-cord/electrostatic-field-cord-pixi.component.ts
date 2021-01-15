import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy } from '@angular/core';
import * as PIXI from 'pixi.js';
import { Circle, drawPoints, drawPolygon, drawTest, Geometry, Point, spreadPoints } from '../utils';

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

  constructor(private elementRef: ElementRef, private ngZone: NgZone) {}

  init() {
    this.ngZone.runOutsideAngular(() => {
      this.app = new PIXI.Application(this.applicationOptions);
    });
    this.elementRef.nativeElement.appendChild(this.app.view);

    this.getGeometry(
      {x: 400, y: 400, r: 300},
      {x: 400 + 0.5*300*Math.cos(Math.PI/2), y: 400 - 0.5*300*Math.sin(Math.PI/2), r: 70},
      {x: 400 - 0.5*300*Math.cos(Math.PI/6), y: 400 + 0.5*300*Math.sin(Math.PI/6), r: 70},
      {x: 400 + 0.5*300*Math.cos(Math.PI/6), y: 400 + 0.5*300*Math.sin(Math.PI/6), r: 70}
    );

    this.drawGeometry();
  }

  getGeometry(circle1: Circle, circle2: Circle, circle3: Circle, circle4: Circle) {
    const n1 = 100;
    const n2 = 25;

    let points1 = [...Array(n1).keys()].map((value) => ({
      x: circle1.x + circle1.r * Math.cos(2 * Math.PI * (value+1) / n1),
      y: circle1.y + circle1.r * Math.sin(2 * Math.PI * (value+1) / n1),
    }));

    let points2 = [...Array(n2).keys()].map((value) => ({
      x: circle2.x + circle2.r * Math.cos(2 * Math.PI * (value+1) / n2),
      y: circle2.y + circle2.r * Math.sin(2 * Math.PI * (value+1) / n2),
    }));

    let points3 = [...Array(n2).keys()].map((value) => ({
      x: circle3.x + circle3.r * Math.cos(2 * Math.PI * (value+1) / n2),
      y: circle3.y + circle3.r * Math.sin(2 * Math.PI * (value+1) / n2),
    }));

    let points4 = [...Array(n2).keys()].map((value) => ({
      x: circle4.x + circle4.r * Math.cos(2 * Math.PI * (value+1) / n2),
      y: circle4.y + circle4.r * Math.sin(2 * Math.PI * (value+1) / n2),
    }));

    this.geometry = [
      {points: points1, inout: true},
      {points: points2, inout: false},
      {points: points3, inout: false},
      {points: points4, inout: false}
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
    this.app.destroy();
  }

  ngOnDestroy(): void {
    this.destroy();
  }
}