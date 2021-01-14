import { OnInit, Component, ElementRef, Input, NgZone, OnDestroy } from '@angular/core';
import * as PIXI from 'pixi.js';
import { drawTest } from '../utils';

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

    

    drawTest(this.app);

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