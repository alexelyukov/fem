import { Component } from '@angular/core';
import { ApiService } from '../api.service';
import { Geometry, Triangulation, Voronoi } from '../utils';

@Component({
  selector: 'electromagnetic-field-magnet',
  templateUrl: './electromagnetic-field-magnet.component.html',
  styleUrls: ['./electromagnetic-field-magnet.component.sass']
})
export class ElectromagneticFieldMagnetComponent {
  public isTriangulationWaiting = false;
  public isSolveWaiting = false;

  private geometry: Geometry = null;
  private triangulation: Triangulation = null;
  private voronoi: Voronoi = null

  constructor(private apiService: ApiService) { }

  getTriangulation() {
    this.isTriangulationWaiting = true;
    this.apiService.getElectromagneticFieldMagnetTriangulation(this.geometry)
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