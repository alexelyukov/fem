import { Component } from '@angular/core';

@Component({
  selector: 'electrostatic-field-cord',
  templateUrl: './electrostatic-field-cord.component.html',
  styleUrls: ['./electrostatic-field-cord.component.sass']
})
export class ElectrostaticFieldCordComponent {
  public isTriangulationWaiting = false;
  public isSolveWaiting = false;
}