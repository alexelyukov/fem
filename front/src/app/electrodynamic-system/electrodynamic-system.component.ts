import { Component } from '@angular/core';
import { ApiService } from '../api.service';
import { Geometry, Triangulation, Voronoi } from '../utils';

@Component({
  selector: 'electrodynamic-system',
  templateUrl: './electrodynamic-system.component.html',
  styleUrls: ['./electrodynamic-system.component.sass']
})
export class ElectrodynamicSystemComponent {
  public isTriangulationWaiting = false;
  public isSolveWaiting = false;

  private geometry: Geometry = null;
  private triangulation: Triangulation = null;
  private voronoi: Voronoi = null

  constructor(private apiService: ApiService) { }

  getTriangulation() {
    this.isTriangulationWaiting = true;
    this.apiService.getElectrodynamicSystemTriangulation(this.geometry)
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