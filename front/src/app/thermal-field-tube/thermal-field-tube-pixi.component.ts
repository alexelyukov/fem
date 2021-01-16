import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy, Output, EventEmitter } from '@angular/core';
import * as PIXI from 'pixi.js';
import { Circle, drawPoints, drawPolygon, drawTest, Geometry, Point, spreadPoints } from '../utils';

@Component({
  selector: 'thermal-field-tube-pixi',
  template: ''
})
export class ThermalFieldTubePixiComponent implements OnInit, OnDestroy {
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

    this.getGeometry({p: {x: 400, y: 400}, r: 300}, {p: {x: 400, y: 400}, r: 150});

    this.notify.emit(this.geometry);

    this.drawGeometry();

  }

  getGeometry(circle1: Circle, circle2: Circle) {
    const n1 = 100;
    const n2 = 50;

    let points1 = [...Array(n1).keys()].map((value) => ({
      x: circle1.p.x + circle1.r * Math.cos(2 * Math.PI * (value+1) / n1),
      y: circle1.p.y + circle1.r * Math.sin(2 * Math.PI * (value+1) / n1),
    }));

    let points2 = [...Array(n2).keys()].map((value) => ({
      x: circle2.p.x + circle2.r * Math.cos(2 * Math.PI * (value+1) / n2),
      y: circle2.p.y + circle2.r * Math.sin(2 * Math.PI * (value+1) / n2),
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
    this.app.destroy();
  }

  ngOnDestroy(): void {
    this.destroy();
  }
}