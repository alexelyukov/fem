import { Component } from '@angular/core';
import { ApiService } from '../api.service';
import { Geometry, Triangulation, Voronoi } from '../utils';

@Component({
  selector: 'electrostatic-field-cord',
  templateUrl: './electrostatic-field-cord.component.html',
  styleUrls: ['./electrostatic-field-cord.component.sass']
})
export class ElectrostaticFieldCordComponent {
  public isTriangulationWaiting = false;
  public isSolveWaiting = false;

  private geometry: Geometry = null;
  private triangulation: Triangulation = null;
  private voronoi: Voronoi = null

  constructor(private apiService: ApiService) { }

  getTriangulation() {
    this.isTriangulationWaiting = true;
    this.apiService.getElectrostaticFieldCordTriangulation(this.geometry)
      .subscribe(
        (data: string) => {
          const parsedData: {triangulation: Triangulation, voronoi: Voronoi} = JSON.parse(data);

          this.triangulation = parsedData.triangulation;
          this.voronoi = parsedData.voronoi;

          this.isTriangulationWaiting = false;
        },
        (error: Error) => {
          console.log(error);
          this.isTriangulationWaiting = false;
        }
      );
  }

  onNotify(geometry: Geometry) {
    this.geometry = geometry;
  }
}