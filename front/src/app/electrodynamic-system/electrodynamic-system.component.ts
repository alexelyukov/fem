import { Component } from '@angular/core';

@Component({
  selector: 'electrodynamic-system',
  templateUrl: './electrodynamic-system.component.html',
  styleUrls: ['./electrodynamic-system.component.sass']
})
export class ElectrodynamicSystemComponent {
  public isTriangulationWaiting = false;
  public isSolveWaiting = false;
}