import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy } from '@angular/core';
import * as PIXI from 'pixi.js';
import { drawLine, drawLines, drawPoint, drawPoints } from '../utils';

@Component({
  selector: 'app-pixi',
  template: ''
})
export class PIXIComponent implements OnInit, OnDestroy {
  public app: PIXI.Application;

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

    drawPoint(this.app, {x: 100, y: 100}, 1, 0x000000)
    drawPoint(this.app, {x: 200, y: 100}, 2, 0x0000FF)
    drawPoint(this.app, {x: 300, y: 100}, 3, 0x00FF00)
    drawPoint(this.app, {x: 400, y: 100}, 4, 0x00FFFF)
    drawPoints(this.app, [{x: 500, y: 100}, {x: 600, y: 100}, {x: 700, y: 100}], 4, 0xFF0000)

    drawLine(this.app, {p1: {x: 100, y: 200}, p2: {x: 150, y: 200}}, 1, 0x000000);
    drawLine(this.app, {p1: {x: 200, y: 200}, p2: {x: 250, y: 200}}, 2, 0x000000);

    drawLines(this.app, [
      {p1: {x: 400, y: 200}, p2: {x: 450, y: 200}},
      {p1: {x: 500, y: 200}, p2: {x: 550, y: 200}}
    ], 2, 0xFF0000);
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