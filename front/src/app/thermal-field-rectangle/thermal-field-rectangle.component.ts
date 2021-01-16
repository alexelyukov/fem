import { Component } from '@angular/core';
import { ApiService } from '../api.service';

@Component({
  selector: 'thermal-field-rectangle',
  templateUrl: './thermal-field-rectangle.component.html',
  styleUrls: ['./thermal-field-rectangle.component.sass']
})
export class ThermalFieldRectangleComponent {
  public isTriangulationWaiting = false;
  public isSolveWaiting = false;

  constructor(private apiService: ApiService) { }

  getTriangulation() {
    console.log(11)
    this.apiService.getThermalFieldRectangleTriangulation(12)
    .subscribe((data: any) => console.log(data));
  }
}