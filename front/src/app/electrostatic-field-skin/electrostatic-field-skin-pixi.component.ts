import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy, Output, EventEmitter } from '@angular/core';
import * as PIXI from 'pixi.js';
import { Circle, drawPoints, drawPolygon, drawTest, Geometry, Point, spreadPoints } from '../utils';

@Component({
  selector: 'electrostatic-field-skin-pixi',
  template: ''
})
export class ElectrostaticFieldSkinPixiComponent implements OnInit, OnDestroy {
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

    this.getGeometry({p: {x: 400, y: 400}, r: 300});

    this.notify.emit(this.geometry);

    this.drawGeometry();

  }

  getGeometry(circle: Circle) {
    const n1 = 100;

    let points = [...Array(n1).keys()].map((value) => ({
      x: circle.p.x + circle.r * Math.cos(2 * Math.PI * (value+1) / n1),
      y: circle.p.y + circle.r * Math.sin(2 * Math.PI * (value+1) / n1),
    }));

    this.geometry = [{points: points, io: true}];
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