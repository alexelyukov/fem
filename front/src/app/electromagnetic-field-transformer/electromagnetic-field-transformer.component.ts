import { Component } from '@angular/core';

@Component({
  selector: 'electromagnetic-field-transformer',
  templateUrl: './electromagnetic-field-transformer.component.html',
  styleUrls: ['./electromagnetic-field-transformer.component.sass']
})
export class ElectromagneticFieldTransformerComponent {
  public isTriangulationWaiting = false;
  public isSolveWaiting = false;
}